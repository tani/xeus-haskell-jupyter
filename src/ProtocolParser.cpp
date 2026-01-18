#include "ProtocolParser.hpp"
#include <algorithm>
#include <cctype>

namespace xeus_haskell {

ParsedOutput parse_protocol_output(std::string_view raw_output) {
  const char STX = '\x02';
  const char US = '\x1F';
  const char ETX = '\x03';

  // Relaxed parsing: Find US first
  size_t us_pos = raw_output.find(US);
  if (us_pos != std::string_view::npos) {
    std::string_view mime = raw_output.substr(0, us_pos);
    std::string_view content = raw_output.substr(us_pos + 1);

    // Trim STX from mime start if present
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

} // namespace xeus_haskell
