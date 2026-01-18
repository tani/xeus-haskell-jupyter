#include <boost/ut.hpp>
#include <xeus-haskell/mhs_repl.hpp>

#include <algorithm>
#include <chrono>
#include <cctype>
#include <iostream>
#include <ranges>
#include <string>
#include <string_view>

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

auto repl_instance = []() -> xeus_haskell::MicroHsRepl& {
    static xeus_haskell::MicroHsRepl repl;
    return repl;
};

int main() {
    "positive repl test"_test = [] {
        auto& repl = repl_instance();
        auto result = repl.execute("1 + 1");

        expect(result.ok);
        expect_trim_eq(result.result_content, "2");
    };

    "stdout is captured"_test = [] {
        auto& repl = repl_instance();
        auto res = repl.execute("putStrLn \"hello from repl\"");
        expect(res.ok);
        expect(that % res.stdout_output.find("hello from repl") != std::string::npos);
    };

    "leading comment expressions execute"_test = [] {
        auto& repl = repl_instance();
        auto res = repl.execute("-- Hello World\nprint \"Hello World!\"");
        expect(res.ok) << res.error;
        expect(that % res.stdout_output.find("Hello World!") != std::string::npos);
    };

    "leading block comment expressions execute"_test = [] {
        auto& repl = repl_instance();
        const auto code = R"({- block
comment -}
print "Hello World!")";
        auto res = repl.execute(code);
        expect(res.ok) << res.error;
        expect(that % res.stdout_output.find("Hello World!") != std::string::npos);
    };

    "comments with blank lines before expression execute"_test = [] {
        auto& repl = repl_instance();
        const auto code = R"(-- first

-- second

print "Hello World!")";
        auto res = repl.execute(code);
        expect(res.ok) << res.error;
        expect(that % res.stdout_output.find("Hello World!") != std::string::npos);
    };

    "definitions persist"_test = [] {
        auto& repl = repl_instance();
        auto def_result = repl.execute("xh_def_test = 40 + 2");
        expect(def_result.ok);

        auto res = repl.execute("xh_def_test");
        expect(res.ok);
        expect_trim_eq(res.result_content, "42");
    };

    "redefinitions replace old values"_test = [] {
        auto& repl = repl_instance();
        auto first = repl.execute("xh_redef_test = 1");
        expect(first.ok);

        auto redef = repl.execute("xh_redef_test = 5");
        expect(redef.ok);

        auto res = repl.execute("xh_redef_test");
        expect(res.ok);
        expect_trim_eq(res.result_content, "5");
    };

    "completion candidates include definitions and reserved ids"_test = [] {
        auto& repl = repl_instance();
        auto def_res = repl.execute("xh_completion_test_value = 7");
        expect(def_res.ok) << def_res.error;

        const auto candidates = repl.completion_candidates();
        auto has = [&](std::string_view needle) {
            return std::ranges::find(candidates, needle) != candidates.end();
        };

        expect(has("xh_completion_test_value"));
        expect(has("where")); // reserved keyword
    };

    "derived Typeable definitions stay usable"_test = [] {
        auto& repl = repl_instance();
        const auto type_def = R"(
data XhTypeableRecord = XhTypeableRecord
  { xhField :: Int
  } deriving (Show, Typeable)
)";
        auto def_res = repl.execute(type_def);
        expect(def_res.ok) << def_res.error;

        // Regression test for B@._mkTyCon lookup: evaluating after the definition
        // must continue to work.
        auto eval_res = repl.execute("xhField (XhTypeableRecord 42)");
        expect(eval_res.ok) << eval_res.error;
        expect_trim_eq(eval_res.result_content, "42");

        auto simple_res = repl.execute("20 + 22");
        expect(simple_res.ok) << simple_res.error;
        expect_trim_eq(simple_res.result_content, "42");
    };

    "expressions evaluate"_test = [] {
        auto& repl = repl_instance();
        auto res = repl.execute("let (a, b) = (10, 20) in a + b");
        expect(res.ok);
        expect_trim_eq(res.result_content, "30");
    };

    "expression errors are reported"_test = [] {
        auto& repl = repl_instance();
        auto res = repl.execute("1 + \"1\"");
        expect(!res.ok);
        expect(!res.error.empty());
    };

    "measure warm-up timing"_test = [] {
        auto& repl = repl_instance();
        auto measure = [&](std::string_view code) {
            const auto start = std::chrono::steady_clock::now();
            auto res = repl.execute(code);
            const auto end = std::chrono::steady_clock::now();
            const auto ms = std::chrono::duration_cast<std::chrono::milliseconds>(end - start).count();
            return std::pair{std::move(res), ms};
        };

        auto [first, first_ms] = measure("21 + 21");
        auto [second, second_ms] = measure("22 + 22");

        expect(first.ok);
        expect(second.ok);

        std::cout << "[timing] MicroHsRepl execute timings (ms): first="
                  << first_ms << " second=" << second_ms << std::endl;
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

    "io unit side effect separation"_test = [] {
        auto& repl = repl_instance();
        // IO () should run (stdout) and have empty result
        auto res = repl.execute("putStr \"side\"");
        expect(res.ok);
        // "side" should be in stdout, not result
        expect(res.stdout_output == "side");
        expect(res.result_content == "");
    };

    "pure value wrapped"_test = [] {
        auto& repl = repl_instance();
        auto res = repl.execute("42");
        expect(res.ok);
        // stdout empty, result "42"
        expect(res.stdout_output == "");
        expect_trim_eq(res.result_content, "42");
    };

    "multiple rich output in stdout"_test = [] {
        auto& repl = repl_instance();
        // Mimic user snippet with definitions for STX, US, ETX
        const auto code = R"(
          let stx = "\x02"
              us  = "\x1F"
              etx = "\x03"
          in do
            putStr $ stx ++ "text/html" ++ us ++ "<p>Hello</p>" ++ etx
            putStr $ stx ++ "text/csv" ++ us ++ "1,2,3,4" ++ etx
            putStr $ stx ++ "text/plain" ++ us ++ "hello" ++ etx
        )";
        auto res = repl.execute(code);
        expect(res.ok) << res.error;

        // Everything should be in stdout, result empty (IO ())
        expect(res.result_content == "");

        // Verify stdout contains the sequence
        std::string expected =
            "\x02" "text/html" "\x1F" "<p>Hello</p>" "\x03"
            "\x02" "text/csv" "\x1F" "1,2,3,4" "\x03"
            "\x02" "text/plain" "\x1F" "hello" "\x03";
        expect(res.stdout_output == expected) << "Actual: " << res.stdout_output;
    };

    "nested stx/etx in result"_test = [] {
        auto& repl = repl_instance();
        // Simulate a scenario where stdout has content AND we manually output a result wrapper
        // that contains nested STX/ETX. This tests the robust parsing.
        // We use IO () so Repl.hs doesn't add its own wrapper.
        const auto code = R"(
          let stx = "\x02"
              us  = "\x1F"
              etx = "\x03"
              wrapper = "\x02application/vnd.xeus.haskell.value\x1F"
          in do
            putStr "stdout_before"
            putStr wrapper
            putStr "result_start"
            putStr $ stx ++ "nested" ++ us ++ "data" ++ etx
            putStr "result_end"
            putStr etx
            putStr "stdout_after"
        )";
        auto res = repl.execute(code);
        expect(res.ok) << res.error;

        // Check separation
        expect(res.stdout_output == "stdout_beforestdout_after");

        std::string expected_result = "result_start\x02nested\x1F" "data\x03result_end";
        expect(res.result_content == expected_result);
    };

}
