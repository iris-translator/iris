#![cfg(feature = "test")]

#[cfg(test)]
mod tests {
    use crate::shared::tester::{check_function_wrapped, check_transformed_code};

    #[test]
    fn test_ecma_binary_logical_op() {
        check_transformed_code("a + b", "a + b");
        check_transformed_code("a - b", "a - b");
        check_transformed_code("a * b", "a * b");
        check_transformed_code("a / b", "a / b");
        check_transformed_code("a % b", "a % b");
        check_transformed_code("a ** b", "a ** b");

        check_transformed_code("a >> b", "a >> b");
        check_transformed_code("a << b", "a << b");
        check_transformed_code("a >>> b", "(((a & 0xFFFFFFFF) >> b) & 0xFFFFFFFF)");

        check_transformed_code("a & b", "a & b");
        check_transformed_code("a | b", "a | b");
        check_transformed_code("a ^ b", "a ^ b");

        check_transformed_code("a && b", "a and b");
        check_transformed_code("a || b", "a or b");
        check_transformed_code("!a", "not a");

        check_transformed_code("a === b", "a == b");
        check_transformed_code("a !== b", "a != b");

        check_transformed_code("a == b", "a == b");
        check_transformed_code("a != b", "a != b");

        check_transformed_code("a < b", "a < b");
        check_transformed_code("a > b", "a > b");
        check_transformed_code("a <= b", "a <= b");
        check_transformed_code("a >= b", "a >= b");

        // check_transformed_code("a in b", "a in b");
        check_transformed_code("a instanceof b", "isinstance(a, b)");
    }

    #[test]
    fn test_ecma_unary_update_op() {
        check_transformed_code("delete a.b", "del a.b");
        check_transformed_code("void a", "(a, None)[-1]");
        check_transformed_code("typeof a", "type(a)");
        check_transformed_code("++a", "a = a + 1");
        check_transformed_code("--a", "a = a - 1");
        check_transformed_code("+a", "+a");
        check_transformed_code("-a", "-a");
        check_transformed_code("~a", "~a");
        check_transformed_code("!a", "not a");
    }

    // #[test]
    // fn test_ecma_assignment_op() {
    //     check_transformed_code("a = b", "a = b");
    //     check_transformed_code("a += b", "a = a + b");
    //     check_transformed_code("a -= b", "a = a - b");
    //     check_transformed_code("a *= b", "a = a * b");
    //     check_transformed_code("a /= b", "a = a / b");
    //     check_transformed_code("a %= b", "a = a % b");
    //
    //     check_transformed_code("a **= b", "a = a ** b");
    //     check_transformed_code("a <<= b", "a = a << b");
    //     check_transformed_code("a >>= b", "a = a >> b");
    //
    //     check_transformed_code("a++", "a = a + 1");
    // }

    #[test]
    fn test_ecma_return() {
        check_function_wrapped("return", "return");
        check_function_wrapped("return a", "return a");
    }

    #[test]
    fn test_ecma_yield() {
        check_function_wrapped("yield", "yield");
        check_function_wrapped("yield a", "yield a");
        check_function_wrapped("yield* a", "yield from a");
    }

    #[test]
    fn test_ecma_await() {
        check_function_wrapped("await a", "await a");
    }
}
