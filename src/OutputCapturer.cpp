#include "OutputCapturer.hpp"
#include <cstdio>
#include <cstdlib>
#include <stdexcept>
#include <vector>

#ifdef _WIN32
#include <fcntl.h>
#include <io.h>
#include <windows.h>
#else
#include <fcntl.h>
#include <unistd.h>
#endif

namespace xeus_haskell {

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

} // namespace xeus_haskell
