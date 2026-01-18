#include <algorithm>
#include <cstdio>
#include <cstdlib>
#include <functional>
#include <sstream>
#include <stdexcept>
#include <string>
#include <string_view>
#include <vector>

#ifdef _WIN32
#include <fcntl.h>
#include <io.h>
#include <windows.h>
#else
#include <fcntl.h>
#include <unistd.h>
#endif

#include "Repl_stub.h"
#include "xeus-haskell/mhs_repl.hpp"

namespace xeus_haskell {

namespace {

bool mhs_repl_initialized = false;

// Helper struct to hold protocol parsing results
struct ParsedOutput {
  std::string mime_type;
  std::string content;
};

// Function to parse the format: STX(0x02) ... US(0x1F) ... ETX(0x03)
ParsedOutput parse_protocol_output(std::string_view raw_output) {
  const char STX = '\x02';
  const char US = '\x1F';
  const char ETX = '\x03';

  // Relaxed parsing: Find US first
  size_t us_pos = raw_output.find(US);
  if (us_pos != std::string_view::npos) {
    std::string_view mime = raw_output.substr(0, us_pos);
    std::string_view content = raw_output.substr(us_pos + 1);

    // Trim STX from mime start if present (or search for it?)
    // We assume standard protocol: STX + MIME + US + CONTENT + ETX
    if (!mime.empty() && mime.front() == STX) {
      mime.remove_prefix(1);
    } else {
      // If STX is not at front, maybe trim whitespace?
      // For now, assume it's clean or STX is missing
      size_t stx = mime.find(STX);
      if (stx != std::string_view::npos) {
        mime = mime.substr(stx + 1);
      }
    }

    // Trim ETX from content end
    while (!content.empty() &&
           (content.back() == ETX ||
            std::isspace(static_cast<unsigned char>(content.back())))) {
      content.remove_suffix(1);
    }

    return {std::string(mime), std::string(content)};
  }

  // If protocol does not match, treat as standard text/plain
  return {"text/plain", std::string(raw_output)};
}

// Helper function to count matching delimiters
std::size_t find_matching_etx(std::string_view sv, std::size_t start_pos = 0) {
  const char STX = '\x02';
  const char ETX = '\x03';
  int depth = 0;
  for (std::size_t i = start_pos; i < sv.size(); ++i) {
    if (sv[i] == STX) {
      depth++;
    } else if (sv[i] == ETX) {
      depth--;
      if (depth == 0) {
        return i;
      }
    }
  }
  return std::string_view::npos;
}

std::string capture_stdout(std::function<void()> fn) {
#ifdef _WIN32
  // Windows implementation (Fixed to capture Win32 and C runtime stdout)
  HANDLE hRead, hWrite;
  SECURITY_ATTRIBUTES sa = {sizeof(SECURITY_ATTRIBUTES), NULL, TRUE};

  if (!CreatePipe(&hRead, &hWrite, &sa, 0)) {
    throw std::runtime_error("Failed to create pipe");
  }

  // Duplicate the write handle for the C runtime
  HANDLE hWriteDup;
  if (!DuplicateHandle(GetCurrentProcess(), hWrite, GetCurrentProcess(),
                       &hWriteDup, 0, FALSE, DUPLICATE_SAME_ACCESS)) {
    CloseHandle(hRead);
    CloseHandle(hWrite);
    throw std::runtime_error("Failed to duplicate write handle");
  }

  // Save original stdout handles (both Win32 and C runtime)
  HANDLE hStdout = GetStdHandle(STD_OUTPUT_HANDLE);
  int stdout_fd = _dup(1); // Save original C runtime stdout (FD 1)
  if (stdout_fd == -1) {
    throw std::runtime_error("Failed to dup original stdout");
  }

  // Create a C runtime file descriptor for the duplicated pipe handle
  int pipe_fd = _open_osfhandle((intptr_t)hWriteDup, _O_TEXT);
  if (pipe_fd == -1) {
    _close(stdout_fd);
    CloseHandle(hRead);
    CloseHandle(hWrite);
    throw std::runtime_error("Failed to open osfhandle");
  }

  fflush(stdout); // Flush any existing buffers

  // 1. Redirect Win32 stdout
  if (!SetStdHandle(STD_OUTPUT_HANDLE, hWrite)) {
    _close(pipe_fd);
    _close(stdout_fd);
    CloseHandle(hRead);
    CloseHandle(hWrite);
    throw std::runtime_error("Failed to set std handle");
  }

  // 2. Redirect C runtime stdout
  if (_dup2(pipe_fd, 1) == -1) {
    // Restore Win32 handle before throwing
    SetStdHandle(STD_OUTPUT_HANDLE, hStdout);
    _close(pipe_fd);
    _close(stdout_fd);
    CloseHandle(hRead);
    CloseHandle(hWrite);
    throw std::runtime_error("Failed to dup2 stdout");
  }

  // We've duplicated pipe_fd onto 1, so we can close the original
  // This also closes the underlying hWriteDup handle
  _close(pipe_fd);

  try {
    fn(); // run the function
  } catch (...) {
    // Restore stdouts before re-throwing
    fflush(stdout);
    _dup2(stdout_fd, 1);
    _close(stdout_fd);
    SetStdHandle(STD_OUTPUT_HANDLE, hStdout);
    CloseHandle(hWrite);
    CloseHandle(hRead);
    throw; // Re-throw the original exception
  }

  // Restore original stdouts
  fflush(stdout);

  // 1. Restore C runtime stdout
  _dup2(stdout_fd, 1);
  _close(stdout_fd);

  // 2. Restore Win32 stdout
  SetStdHandle(STD_OUTPUT_HANDLE, hStdout);

  // Close the Win32 pipe write handle.
  // Reading can now begin.
  CloseHandle(hWrite);

  // Read from pipe
  std::string output;
  DWORD bytesRead;
  char buffer[4096];
  while (ReadFile(hRead, buffer, sizeof(buffer) - 1, &bytesRead, NULL) &&
         bytesRead > 0) {
    buffer[bytesRead] = '\0';
    output += buffer;
  }
  CloseHandle(hRead);
  return output;
#else
  // POSIX implementation
  int pipefd[2];
  if (pipe(pipefd) == -1)
    return "";

  int stdout_fd = dup(fileno(stdout));
  fflush(stdout);
  dup2(pipefd[1], fileno(stdout));
  close(pipefd[1]);

  fn(); // execute function that writes to stdout

  fflush(stdout);
  dup2(stdout_fd, fileno(stdout));
  close(stdout_fd);

  // Read captured output
  std::string output;
  char buffer[4096];
  ssize_t n;
  while ((n = read(pipefd[0], buffer, sizeof(buffer) - 1)) > 0) {
    buffer[n] = '\0';
    output += buffer;
  }
  close(pipefd[0]);
  return output;
#endif
}

std::string capture_stderr(std::function<void()> fn) {
#ifdef _WIN32
  // Windows implementation
  HANDLE hRead, hWrite;
  SECURITY_ATTRIBUTES sa = {sizeof(SECURITY_ATTRIBUTES), NULL, TRUE};

  if (!CreatePipe(&hRead, &hWrite, &sa, 0)) {
    throw std::runtime_error("Failed to create pipe");
  }

  HANDLE hWriteDup;
  if (!DuplicateHandle(GetCurrentProcess(), hWrite, GetCurrentProcess(),
                       &hWriteDup, 0, FALSE, DUPLICATE_SAME_ACCESS)) {
    CloseHandle(hRead);
    CloseHandle(hWrite);
    throw std::runtime_error("Failed to duplicate write handle");
  }

  HANDLE hStderr = GetStdHandle(STD_ERROR_HANDLE);
  int stderr_fd = _dup(2);
  if (stderr_fd == -1) {
    throw std::runtime_error("Failed to dup original stderr");
  }

  int pipe_fd = _open_osfhandle((intptr_t)hWriteDup, _O_TEXT);
  if (pipe_fd == -1) {
    _close(stderr_fd);
    CloseHandle(hRead);
    CloseHandle(hWrite);
    throw std::runtime_error("Failed to open osfhandle");
  }

  fflush(stderr);

  if (!SetStdHandle(STD_ERROR_HANDLE, hWrite)) {
    _close(pipe_fd);
    _close(stderr_fd);
    CloseHandle(hRead);
    CloseHandle(hWrite);
    throw std::runtime_error("Failed to set std handle");
  }

  if (_dup2(pipe_fd, 2) == -1) {
    SetStdHandle(STD_ERROR_HANDLE, hStderr);
    _close(pipe_fd);
    _close(stderr_fd);
    CloseHandle(hRead);
    CloseHandle(hWrite);
    throw std::runtime_error("Failed to dup2 stderr");
  }

  _close(pipe_fd);

  try {
    fn();
  } catch (...) {
    fflush(stderr);
    _dup2(stderr_fd, 2);
    _close(stderr_fd);
    SetStdHandle(STD_ERROR_HANDLE, hStderr);
    CloseHandle(hWrite);
    CloseHandle(hRead);
    throw;
  }

  fflush(stderr);

  _dup2(stderr_fd, 2);
  _close(stderr_fd);

  SetStdHandle(STD_ERROR_HANDLE, hStderr);
  CloseHandle(hWrite);

  std::string output;
  DWORD bytesRead;
  char buffer[4096];
  while (ReadFile(hRead, buffer, sizeof(buffer) - 1, &bytesRead, NULL) &&
         bytesRead > 0) {
    buffer[bytesRead] = '\0';
    output += buffer;
  }
  CloseHandle(hRead);
  return output;
#else
  // POSIX implementation
  int pipefd[2];
  if (pipe(pipefd) == -1)
    return "";

  int stderr_fd = dup(fileno(stderr));
  fflush(stderr);
  dup2(pipefd[1], fileno(stderr));
  close(pipefd[1]);

  fn();

  fflush(stderr);
  dup2(stderr_fd, fileno(stderr));
  close(stderr_fd);

  std::string output;
  char buffer[4096];
  ssize_t n;
  while ((n = read(pipefd[0], buffer, sizeof(buffer) - 1)) > 0) {
    buffer[n] = '\0';
    output += buffer;
  }
  close(pipefd[0]);
  return output;
#endif
}
} // namespace

MicroHsRepl::MicroHsRepl() {
  if (!mhs_repl_initialized) {
    mhs_init();
    mhs_repl_initialized = true;
  }

  std::string microhs_runtime_dir = "";
  auto mhsdir = std::getenv("MHSDIR");
  auto conda_prefix = std::getenv("CONDA_PREFIX");

  if (mhsdir != nullptr) {
    microhs_runtime_dir = mhsdir;
  } else if (conda_prefix != nullptr) {
    microhs_runtime_dir = std::string(conda_prefix) + "/share/microhs";
  }

  context = mhs_repl_new(const_cast<char *>(microhs_runtime_dir.c_str()),
                         static_cast<uintptr_t>(microhs_runtime_dir.size()));

  auto warmup = execute("0");
  (void)warmup;
}

MicroHsRepl::~MicroHsRepl() { mhs_repl_free(context); }

repl_result MicroHsRepl::execute(std::string_view code) {
  std::string raw_output;
  std::string raw_error;

  try {
    raw_error = capture_stderr([&]() {
      raw_output = capture_stdout([&]() {
        std::string code_str(code);
        char *err = nullptr;
        intptr_t rc =
            mhs_repl_execute(context, const_cast<char *>(code_str.c_str()),
                             static_cast<uintptr_t>(code_str.size()),
                             reinterpret_cast<void *>(&err));

        if (rc != 0 && err) {
          std::string err_str(err);
          mhs_repl_free_cstr(err);
          throw std::runtime_error(err_str);
        }
        if (err)
          mhs_repl_free_cstr(err);
      });
    });
  } catch (const std::runtime_error &e) {
    // Return default mime_type (text/plain) on error
    return {false, std::string(), std::string(), std::string(), "text/plain",
            std::string(e.what())};
  }

  // Normalize line endings: remove \r (CR)
  // This ensures consistent output across platforms (Windows uses \r\n, others
  // \n)
  raw_output.erase(std::remove(raw_output.begin(), raw_output.end(), '\r'),
                   raw_output.end());
  raw_error.erase(std::remove(raw_error.begin(), raw_error.end(), '\r'),
                  raw_error.end());

  // Parse result wrapper
  // Marker: \x02application/vnd.xeus.haskell.value\x1F
  const std::string start_marker = "\x02" "application/vnd.xeus.haskell.value\x1F";
  std::string stdout_output;
  std::string result_content;
  std::string result_mime_type = "text/plain";

  size_t start_pos = raw_output.find(start_marker);
  if (start_pos != std::string::npos) {
    // Found the wrapper
    stdout_output = raw_output.substr(0, start_pos); // Part before

    size_t content_start = start_pos + start_marker.length();

    // Find matching ETX
    size_t end_pos = find_matching_etx(raw_output, start_pos);

    if (end_pos != std::string::npos) {
      result_content =
          raw_output.substr(content_start, end_pos - content_start);

      // Remaining stdout
      if (end_pos + 1 < raw_output.size()) {
        stdout_output += raw_output.substr(end_pos + 1);
      }

      // Now check if result_content is a Display protocol string
      ParsedOutput inner = parse_protocol_output(result_content);
      if (inner.mime_type != "text/plain" || inner.content != result_content) {
        // It was parsed as protocol
        result_mime_type = inner.mime_type;
        result_content = inner.content;
      }
    } else {
      result_content = raw_output.substr(content_start);
    }
  } else {
    // No wrapper, everything is stdout
    stdout_output = raw_output;
  }

  return {
      true,
      std::move(stdout_output),
      std::move(raw_error),
      std::move(result_content),
      std::move(result_mime_type),
      std::string() // No error
  };
}

std::vector<std::string> MicroHsRepl::completion_candidates() {
  std::string output;
  try {
    output = capture_stdout([&]() { mhs_repl_completion_candidates(context); });
  } catch (const std::runtime_error &) {
    return {};
  }

  std::vector<std::string> candidates;
  std::stringstream ss(output);
  std::string line;
  while (std::getline(ss, line)) {
    if (!line.empty() && line.back() == '\r') {
      line.pop_back();
    }
    if (!line.empty()) {
      candidates.push_back(line);
    }
  }
  return candidates;
}

std::string MicroHsRepl::inspect(std::string_view name) {
  std::string name_str(name);
  char *info = nullptr;
  intptr_t rc = mhs_repl_inspect(context, const_cast<char *>(name_str.c_str()),
                                 static_cast<uintptr_t>(name_str.size()),
                                 reinterpret_cast<void *>(&info));

  if (rc == 0 && info) {
    std::string result(info);
    mhs_repl_free_cstr(info);
    return result;
  }
  if (info)
    mhs_repl_free_cstr(info);
  return "";
}

std::string MicroHsRepl::is_complete(std::string_view code) {
  std::string code_str(code);
  char *status = static_cast<char *>(
      mhs_repl_is_complete(context, const_cast<char *>(code_str.c_str()),
                           static_cast<uintptr_t>(code_str.size())));
  if (status) {
    std::string result(status);
    mhs_repl_free_cstr(status);
    return result;
  }
  return "unknown";
}

} // namespace xeus_haskell
