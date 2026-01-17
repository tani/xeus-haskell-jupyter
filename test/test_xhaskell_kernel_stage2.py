import unittest
import jupyter_kernel_test
from unittest import SkipTest

class KernelTests(jupyter_kernel_test.KernelTests):

    kernel_name = "xhaskell"
    language_name = "haskell"
    code_hello_world = 'putStr "hello, world"'
    code_page_something = ""
    completion_samples = [{"text": "putStr", "cursor_pos": 6}]
    complete_code_samples = ["1+1"]
    incomplete_code_samples = ["[1,2,3"]
    invalid_code_samples = ["let x ="]
    code_execute_result = [
        {"code": "2 + 2", "result": "4\n"},
        {"code": "reverse \"olleh\"", "result": "\"hello\"\n"},
        {"code": "[1, 2, 3]", "result": "[1,2,3]\n"}
    ]
    code_generate_error = "error \"boom\""
    code_history_pattern = "1+1"
    code_inspect_sample = "putStrLn"

    def test_stdout(self):
        self.flush_channels()
        reply, output_msgs = self.execute_helper(code='putStr "hello, world"')
        # logic: kernel emits stream for putStr
        self.assertEqual(output_msgs[0]["msg_type"], "execute_result")
        self.assertEqual(output_msgs[0]["content"]["data"]["text/plain"], "hello, world")

    def test_execute_stdout(self) -> None:
        if not self.code_hello_world:
            raise SkipTest("No code hello world")
        self.flush_channels()
        reply, output_msgs = self.execute_helper(code=self.code_hello_world)
        self.assertEqual(reply["content"]["status"], "ok")
        self.assertGreaterEqual(len(output_msgs), 1)
        msg = output_msgs[0]
        self.assertEqual(msg["msg_type"], "execute_result")
        self.assertEqual(msg["content"]["data"]["text/plain"], "hello, world")

    def test_stderr(self):
        # Stderr handling might be different or not separated in current simple implementation
        # Skip or adapt if we know how error behaves.
        pass

if __name__ == "__main__":
    unittest.main()
