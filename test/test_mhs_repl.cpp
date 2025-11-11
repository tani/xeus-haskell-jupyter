#include <boost/ut.hpp>
#include <xeus-haskell/mhs_repl.hpp>

int main() {
  using namespace boost::ut;
  using namespace std;
  xeus_haskell::MicroHsRepl repl;
  auto result = repl.execute("1 + 1");

  "positive repl test"_test = [result] {
    expect(result.value() == "2"s);
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
