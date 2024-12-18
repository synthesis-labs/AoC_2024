import sys
from typing import Callable

sys.setrecursionlimit(10000)


def get_program(filepath: str) -> dict[str, int | list[int]]:
    result = dict()
    with open(filepath, "r") as txtin:
        for line in txtin:
            tmp = line.split(":")
            if "Register" in line:
                result[tmp[0][-1].lower()] = int(tmp[1])

            if "Program" in line:
                result["program"] = tuple(map(int, tmp[1].split(",")))
    return result


def compile_program(b: int, c: int, program: list[int]) -> Callable[[int], tuple[int]]:

    def inner(
        a: int, b: int = b, c: int = c, pointer: int = 0, accum: None | list[int] = None
    ) -> tuple[int]:
        accum = list() if accum is None else accum

        if pointer < 0 or pointer >= len(program):
            return tuple(accum)

        instruction = program[pointer]
        oper = program[pointer + 1]
        combo = a if oper == 4 else b if oper == 5 else c if oper == 6 else None
        combo = oper if oper <= 3 else combo
        p2 = pointer + 2

        match instruction:
            case 0:
                return inner(int(a / 2**combo), b, c, p2, accum)
            case 1:
                return inner(a, b ^ oper, c, p2, accum)
            case 2:
                return inner(a, combo % 8, c, p2, accum)
            case 3:
                return (
                    inner(a, b, c, oper, accum) if a != 0 else inner(a, b, c, p2, accum)
                )
            case 4:
                return inner(a, b ^ c, c, p2, accum)
            case 5:
                return inner(a, b, c, p2, accum + [combo % 8])
            case 6:
                return inner(a, int(a / 2**combo), c, p2, accum)
            case 7:
                return inner(a, b, int(a / 2**combo), p2, accum)

    return inner


def get_length_estimates(
    compiled_func: Callable[[int], tuple[int]], target: tuple[int], _guess: int = 0
) -> tuple[int]:

    result = compiled_func(_guess)

    if len(result) < len(target):
        _guess += _guess + 1
        return get_length_estimates(compiled_func, target, _guess)
    elif len(result) > len(target):
        _guess -= _guess + 2
        return get_length_estimates(compiled_func, target, _guess)

    _min, _max = _guess, _guess
    incr = _guess // 2

    while True:
        result = compiled_func(_min)
        if len(result) == len(target):
            if incr == 1:
                break
            _min -= incr
        else:
            _min += incr
            incr = max(1, incr // 2)

    incr = _guess // 2
    while True:
        result = compiled_func(_max)
        if len(result) == len(target):
            if incr == 1:
                break
            incr *= 2
            _max += incr
        else:
            _max -= incr
            incr = max(1, incr // 4)

    return _min, _max


def get_digit_estimate(
    compiled_func: Callable[[int], tuple[int]],
    target: tuple[int],
    idx: int,
    _min_allowed: int,
    _max_allowed: int,
) -> tuple[int]:

    if (
        compiled_func(_min_allowed)[idx] == target[idx]
        and compiled_func(_max_allowed)[idx] == target[idx]
    ):
        return _min_allowed, _max_allowed

    step = (_max_allowed - _min_allowed) // 100

    seed = None
    for _guess in range(_min_allowed, _max_allowed + step, step):
        result = compiled_func(_guess)
        if result[idx] == target[idx]:
            seed = _guess

    
    

    _min, _max = _min_allowed, _max_allowed


if __name__ == "__main__":
    init_prog = get_program(sys.argv[1])
    compiled = compile_program(init_prog["b"], init_prog["c"], init_prog["program"])
