#include <boost/ut.hpp>
#include <xeus-haskell/mhs_repl.hpp>
#include <string>
#include <string_view>
#include <algorithm>
#include <cctype>
#include <ranges>

using namespace boost::ut;
using namespace std::string_literals;

auto trim(std::string_view s) -> std::string {
    auto is_space = [](unsigned char c) { return std::isspace(c); };

    auto trimmed = s 
        | std::views::drop_while(is_space)
        | std::views::reverse
        | std::views::drop_while(is_space)
        | std::views::reverse;

    return {trimmed.begin(), trimmed.end()};
}

auto expect_trim_eq = [](std::string_view a, std::string_view b) {
    expect(eq(trim(a), trim(b))) << "expected (ignoring spaces): " << a << " == " << b;
};

int main() {
    "positive repl test"_test = [] {
        xeus_haskell::MicroHsRepl repl;
        auto result = repl.execute("1 + 1");

        expect_trim_eq(result.value(), "2");
    };

    "basic test"_test = [] {
        expect(42_i == 42);
        expect(1_i != 2);
    };

    "sum test"_test = [] {
        constexpr auto sum = [](auto... values) { return (values + ...); };
        expect(sum(1, 2) == 3_i);
        expect(sum(0) == 0_i);
    };
}

