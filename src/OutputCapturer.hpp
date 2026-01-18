#pragma once
#include <functional>
#include <string>

namespace xeus_haskell {

std::string capture_stdout(std::function<void()> fn);

}
