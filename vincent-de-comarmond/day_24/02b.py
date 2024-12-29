from collections import Counter
from copy import deepcopy
from math import log10
from operator import add, lshift, and_, xor, or_
from pprint import pp
from sys import argv
from typing import Callable


def load_input(input_fp: str):
    inputs = dict()
    relations = dict()
    all_wires = set()

    def translate(input_str: str) -> str:
        output = input_str
        for o, r in [("AND", "&"), ("XOR", "^"), ("OR", "|")]:
            output = output.replace(o, r)
        return output

    input_defn = True
    with open(input_fp, "r") as flin:
        for ln in flin:
            ln = ln.strip()
            if len(ln) == 0:
                input_defn = False
                continue

            if input_defn:
                wire, value = map(str.strip, ln.split(":"))
                inputs[wire] = int(value)
                all_wires.add(wire)
            else:
                wiring, output = map(str.strip, ln.split("->"))
                input1, _, input2 = wiring.split(" ")
                wiring = translate(wiring)
                all_wires |= {input1, input2, output}
                relations[output] = wiring

    return inputs, relations, all_wires


def resolve_dependencies(relationships: dict[str, str]) -> dict[str, str]:

    out = deepcopy(relationships)
    unresolved = any(_ in out for v in out.values() for _ in v.split(" "))

    while unresolved:
        new_output = dict()

        for k, v in out.items():
            new_v = v
            for symb in v.split(" "):
                if symb in out:
                    new_v = new_v.replace(symb, out[symb])

            new_output[k] = new_v
        out = new_output
        unresolved = any(_ in out for v in out.values() for _ in v.split(" "))
    return out


def create_repr(input_num: int, input_symb: str, all_wires: set[str]) -> dict[str, int]:

    binary_num = bin(input_num)
    bin_list = [_ for _ in reversed(binary_num)]
    bin_list = bin_list[: bin_list.index("b")]
    output = dict()

    target_wires = sorted([_ for _ in all_wires if _.startswith(input_symb)])
    for idx, val in enumerate(target_wires):
        output[val] = int(bin_list[idx]) if idx < len(bin_list) else 0

    return output


def to_dec(bin_dict: dict, symb: str) -> int:
    output_tuple = sorted(filter(lambda _: _[0].startswith(symb), bin_dict.items()))

    dec_result = 0
    for p, (_, digit) in enumerate(output_tuple):
        dec_result += int(digit) * (2**p)
    return dec_result


def sequential_eval(input_str: str) -> int:
    cleaned = input_str.replace(" ", "")
    if len(cleaned) == 1:
        return cleaned
    execute = [cleaned[i : i + 4] for i in range(0, len(cleaned), 4)]
    result = ""
    for subseq in execute:
        result += str(eval(subseq[:3]))
        result += subseq[3] if len(subseq) == 4 else ""
    return sequential_eval(result)


def solve(inputs: dict[str, int], operations: dict[str, str]) -> dict[str, int]:

    resolved = resolve_dependencies(operations)

    def replace_inputs(target_str: str) -> str:
        for k, v in inputs.items():
            target_str = target_str.replace(k, str(v))
        return target_str

    # return {k: replace_inputs(v) for k, v in resolved.items()}
    return {k: sequential_eval(replace_inputs(v)) for k, v in resolved.items()}


def evaluate(x_dec: int, y_dec: int, all_wires: set[str], operators: dict[str, str]):
    return solve(
        create_repr(x_dec, "x", all_wires) | create_repr(y_dec, "y", all_wires),
        operators,
    )


def switch(relationships: dict[str, str], symbol1: str, symbol2: str) -> dict[str, str]:
    switched = relationships.copy()
    switched[symbol1] = relationships[symbol2]
    switched[symbol2] = relationships[symbol1]
    return switched


def search_switches(
    relationships: dict[str, str],
    search_space: int,
    operator: Callable[[int, int], int],
    all_wires: set[str],
):

    breakages = dict()
    for xbit in range(search_space):
        print(f"{100*xbit/search_space:.2f} %")
        for ybit in range(search_space):
            expectation = operator(2**xbit, 2**ybit)
            zbits = create_repr(expectation, "z", all_wires)
            evaluation = evaluate(2**xbit, 2**ybit, all_wires, relationships)

            for k in [_ for _ in evaluation if _.startswith("z")]:
                if int(zbits[k]) != int(evaluation[k]):
                    breakages[k] = {
                        _ for _ in relationships[k].split(" ") if _ in relationships
                    }

    print("Breakages found:")
    pp(breakages)

    break_list = [b for breakset in breakages.values() for b in breakset]
    break_counts = Counter(break_list)
    break_dependencies = resolve_dependencies(
        {k: v for k, v in relationships.items() if k in set(break_list)}
    )

    return breakages, break_counts, break_dependencies


def main(input_fp: str):
    inputs, relationships, all_wires = load_input(input_fp)

    # Should be the same
    x_wires = len([_ for _ in all_wires if _.startswith("x")])
    y_wires = len([_ for _ in all_wires if _.startswith("y")])

    return search_switches(relationships, x_wires, add, all_wires)


if __name__ == "__main__":
    breakages, break_counts, break_dependencies = main(argv[1])
