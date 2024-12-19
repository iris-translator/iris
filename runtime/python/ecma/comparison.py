from math import isnan, isinf


def ecma_loosely_equals(a, b):
    """
    1. If SameType(x, y) is true, then
       a. Return IsStrictlyEqual(x, y).
    2. If x is null and y is undefined, return true.
    3. If x is undefined and y is null, return true.
    4. NOTE: This step is replaced in section B.3.6.2.
    5. If x is a Number and y is a String, return ! IsLooselyEqual(x, ! ToNumber(y)).
    6. If x is a String and y is a Number, return ! IsLooselyEqual(! ToNumber(x), y).
    7. If x is a BigInt and y is a String, then
       a. Let n be StringToBigInt(y).
       b. If n is undefined, return false.
       c. Return ! IsLooselyEqual(x, n).
    8. If x is a String and y is a BigInt, return ! IsLooselyEqual(y, x).
    9. If x is a Boolean, return ! IsLooselyEqual(! ToNumber(x), y).
    10. If y is a Boolean, return ! IsLooselyEqual(x, ! ToNumber(y)).
    11. If x is either a String, a Number, a BigInt, or a Symbol and y is an Object, return ! IsLooselyEqual(x, ? ToPrimitive(y)).
    12. If x is an Object and y is either a String, a Number, a BigInt, or a Symbol, return ! IsLooselyEqual(? ToPrimitive(x), y).
    13. If x is a BigInt and y is a Number, or if x is a Number and y is a BigInt, then
        a. If x is not finite or y is not finite, return false.
        b. If ℝ(x) = ℝ(y), return true; otherwise return false.
    14. Return false.
    """
    if type(a) is type(b):
        return a == b
    if a is None and b is None:
        return True
    if isinstance(a, float) and isinstance(b, str):
        return ecma_loosely_equals(a, float(b))
    if isinstance(a, str) and isinstance(b, float):
        return ecma_loosely_equals(float(a), b)
    if isinstance(a, int) and isinstance(b, str):
        return ecma_loosely_equals(a, int(b))
    if isinstance(a, str) and isinstance(b, int):
        return ecma_loosely_equals(int(a), b)
    if isinstance(a, bool) and isinstance(b, str):
        return ecma_loosely_equals(float(a), b)
    if isinstance(a, str) and isinstance(b, bool):
        return ecma_loosely_equals(a, float(b))
    if isinstance(a, (str, int, float)) and isinstance(b, object):
        return ecma_loosely_equals(a, to_primitive(b))
    if isinstance(a, object) and isinstance(b, (str, int, float)):
        return ecma_loosely_equals(to_primitive(a), b)
    if isinstance(a, int) and isinstance(b, float):
        if isinf(b) or isnan(b):
            return False
        return float(a) == b
    if isinstance(a, float) and isinstance(b, int):
        if isinf(a) or isnan(a):
            return False
        return a == float(b)
    return False


def to_primitive(value):
    if isinstance(value, (int, float, str, bool)):
        return value
    elif isinstance(value, tuple):  # Convert tuples
        return tuple(to_primitive(item) for item in value)
    elif isinstance(value, list):  # Convert lists
        return [to_primitive(item) for item in value]
    elif isinstance(value, set):  # Convert sets
        return {to_primitive(item) for item in value}
    elif isinstance(value, dict):  # Convert dicts
        return {to_primitive(key): to_primitive(val) for key, val in value.items()}
    elif hasattr(value, "__int__"):
        return int(value)
    elif hasattr(value, "__float__"):
        return float(value)
    elif hasattr(value, "__str__"):
        return str(value)
    else:
        raise TypeError(f"Cannot convert {type(value).__name__} to a primitive type")


def ecma_loosely_inequals(a, b):
    return not ecma_loosely_equals(a, b)
