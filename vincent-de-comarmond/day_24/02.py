from copy import deepcopy
from math import log10
from operator import add, and_, xor, or_
from pprint import pp
from typing import Callable


def load_input(input_fp: str):
    resolved = dict()
    operations = dict()
    all_wires = set()

    inputs = True
    with open(input_fp, "r") as flin:
        for ln in flin:
            ln = ln.strip()
            if len(ln) == 0:
                inputs = False
                continue

            if inputs:
                wire, value = map(str.strip, ln.split(":"))
                resolved[wire] = int(value)
                all_wires.add(wire)
            else:
                wiring, output = map(str.strip, ln.split("->"))
                input1, operator, input2 = wiring.split(" ")
                op = and_ if operator == "AND" else or_ if operator == "OR" else xor
                all_wires |= {input1, input2, output}
                operations[output] = (op, (input1, input2))

    return resolved, operations, all_wires


def make_dependency_graph(operations: dict[str, tuple], input_symbols: tuple[str], output_symbols: str):

    new_ops = {k: ([v[0]], [v[1]]) for k, v in operations.items()}
    newnewops = dict()

    for k, v in new_ops.items():
        for dependencies in v[1]:
            for dependency in dependencies:
                if dependency not in new_ops:
                    newnewops[k]=v
                else:
                    
                    


                    
            for symb in input_symbols:
                if dependency.startswith(symb):
                    

        
    
    
    
    



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
        dec_result += digit * (2**p)
    return dec_result


def solve(
    inputs: dict[str, int],
    operations: dict[str, tuple[Callable[[int, int], int], tuple[str, str]]],
) -> dict[str, int]:

    ops = deepcopy(operations)
    results = deepcopy(inputs)

    while len(ops) > 0:
        solvable = {
            k: v for k, v in ops.items() if v[1][0] in results and v[1][1] in results
        }

        remove = set()
        for output, (operator, _inputs) in solvable.items():
            result = operator(*map(results.get, _inputs))
            results[output] = result
            remove.add(output)
            ops = {k: v for k, v in ops.items() if k not in remove}

    return results


def check_wiring(
    x_dec: int,
    y_dec: int,
    operator: Callable[[int, int], int],
    all_wires: set[str],
    operators: dict[str, tuple[Callable[[int, int], int], tuple[str, str]]],
):
    x_wires = len([_ for _ in all_wires if _.startswith("x")])
    y_wires = len([_ for _ in all_wires if _.startswith("y")])

    expectation = operator(x_dec, y_dec)
    xbin = create_repr(x_dec, "x", all_wires)
    ybin = create_repr(y_dec, "y", all_wires)
    artificial_inputs = xbin | ybin

    soln = solve(artificial_inputs, operators)

    dec_soln = to_dec(soln, "z")
    return expectation == dec_soln




if __name__ == "__main__":
    pass
