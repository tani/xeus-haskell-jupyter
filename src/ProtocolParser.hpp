#pragma once

#include <string>
#include <string_view>

namespace xeus_haskell {

struct ParsedOutput {
  std::string mime_type;
  std::string content;
};

// Function to parse the format: STX(0x02) ... US(0x1F) ... ETX(0x03)
ParsedOutput parse_protocol_output(std::string_view raw_output);

} // namespace xeus_haskell
