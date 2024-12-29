from collections import Counter, defaultdict
from copy import deepcopy
from functools import lru_cache, reduce
from itertools import combinations
from math import log10
from operator import add, lshift, and_, xor, or_
from pprint import pp
from sys import argv
from typing import Callable


def _formalize_inputs(input_defs: list[str]) -> dict[str, int]:
    input_vals = dict()

    for ln in input_defs:
        node, val = map(str.strip, ln.split(":"))
        input_vals[node] = int(val)
    return input_vals


def _formalize_relationships(
    raw_relationships: list[str],
) -> dict[str, tuple[Callable, set[str]]]:
    _func_map = {"AND": and_, "XOR": xor, "OR": or_}
    output = dict()
    for ln in raw_relationships:
        parents, child = map(str.strip, ln.split("->"))
        parent1, func, parent2 = parents.split(" ")
        output[child] = (_func_map[func], {parent1, parent2})
    return output


@lru_cache(maxsize=None)
def read_input(
    input_fp: str,
) -> tuple[dict[str, int], dict[str, tuple[Callable, set[str]]]]:
    input_defns: list[str] = list()
    relations: list[str] = list()

    with open(input_fp, "r") as flin:
        raw = flin.read().split("\n")

    part1_end = raw.index("")
    input_defns = raw[:part1_end]
    relations = [_ for _ in raw[part1_end + 1 :] if "->" in _]
    return _formalize_inputs(input_defns), _formalize_relationships(relations)


def get_roots_leaves(flat_input: dict[str, tuple[Callable, set[str]]]):
    roots, leaves = set(), set()

    for child, (func, parents) in flat_input.items():
        for parent in parents:
            if parent not in flat_input:
                roots.add(parent)

        for _, (func, parents) in flat_input.items():
            if child in parents:
                break
        else:
            leaves.add(child)

    return roots, leaves


def get_parents(
    flat_input: dict[str, tuple[Callable, set[str]]],
    target: str,
    add_root: bool = False,
) -> dict[str, dict]:
    result = dict()
    if target in flat_input:
        for parent in flat_input[target][1]:
            if parent not in result:
                result[parent] = get_parents(flat_input, parent)
            else:
                result[parent] |= get_parents(flat_input, parent)
    return {target: result} if add_root else result


def get_all_parents(nested: dict) -> dict:
    roots, leaves = get_roots_leaves(nested)
    return {leaf: get_parents(nested, leaf, add_root=False) for leaf in leaves}


def get_children(
    flat_input: dict[str, tuple[Callable, set[str]]],
    target: str,
    add_root: bool = False,
):
    result = dict()
    for k, v in flat_input.items():
        if target in v[1]:
            result[k] = dict()
    for k in deepcopy(result):
        result[k] = get_children(flat_input, k, False)
    return {target: result} if add_root else result


def get_all_children(nested: dict) -> dict:
    roots, leaves = get_roots_leaves(nested)
    return {root: get_children(nested, root, add_root=False) for root in roots}


def to_dec(bin_dict: dict, symb: str) -> int:
    output_tuple = sorted(filter(lambda _: _[0].startswith(symb), bin_dict.items()))

    dec_result = 0
    for p, (_, digit) in enumerate(output_tuple):
        dec_result += int(digit) * (2**p)
    return dec_result


def create_repr(input_num: int, max_symb: str) -> dict[str, int]:
    bin_list = [_ for _ in reversed(bin(input_num))]
    bin_list = bin_list[: bin_list.index("b")]

    output = dict()
    target_wires = [f"{max_symb[0]}{_:02d}" for _ in range(int(max_symb[1:]) + 1)]

    for idx, val in enumerate(target_wires):
        output[val] = int(bin_list[idx]) if idx < len(bin_list) else 0

    return output


def solve(
    inputs: dict[str, int],
    relationships: dict[str, tuple[Callable, set[str]]],
    out_symb: str = "z",
) -> tuple[dict[str, int], dict[str, int]]:

    allout = inputs.copy()
    finished = False
    while not finished:
        finished = True
        for child, (func, parents) in relationships.items():
            if all((parent in allout for parent in parents)):
                allout[child] = func(*(allout[_] for _ in parents))
            else:
                finished = False

    filtered = {k: v for k, v in allout.items() if k.startswith(out_symb)}
    return allout, dict(sorted(filtered.items()))


def flattened_parents(nested_dict: dict, item: str):
    parents = get_parents(nested_dict, item)

    accum = set()
    for k, v in parents.items():
        accum.add(k)
        if len(v) > 0:
            for p in v:
                accum |= flattened_parents(nested_dict, p)
    return accum


def flattened_children(nested_dict: dict, item):
    children = get_children(nested_dict, item)

    accum = set()
    for k, v in children.items():
        accum.add(k)
        if len(v) > 0:
            for p in v:
                accum |= flattened_children(nested_dict, p)
    return accum


def count_parent_generations(
    nested_dict: dict, accum: None | dict[str, int] = None
) -> dict[str, int]:
    accum = defaultdict(int) if accum is None else accum

    for k, parents in nested_dict.items():
        for parent, grandparents in parents.items():
            accum[parent] += 1

            if len(grandparents) > 0:
                count_parent_generations(grandparents, accum)
    return accum


def search_switches(
    relationships: dict[str, str],
    symbol_max: int,
    operator: Callable[[int, int], int],
):

    breakages = defaultdict(int)
    for xbit in range(symbol_max + 1):
        print(f"{100*xbit/symbol_max:.2f} %")
        for ybit in range(symbol_max + 1):
            expectation = operator(2**xbit, 2**ybit)
            zbits = create_repr(expectation, f"z{symbol_max+1:02d}")
            xin = create_repr(2**xbit, f"x{symbol_max:02d}")
            yin = create_repr(2**ybit, f"y{symbol_max:02d}")

            _, result = solve(xin | yin, relationships)

            if zbits != result:
                for k, v in result.items():
                    if v != zbits[k]:
                        breakages[k] += 1

    # print("Breakages found:")

    all_parents = get_all_parents(relationships)
    dependency_counts = defaultdict(int)
    for breakage in breakages:
        dependencies = all_parents[breakage]
        counts = count_parent_generations(dependencies)
        for k, v in counts.items():
            dependency_counts[k] += v
    # print("Dependency Frequency")

    dependency_counts = {
        k: v
        for k, v in sorted(dependency_counts.items(), key=lambda _: _[1], reverse=True)
    }

    # We can't swap the inputs ... so don't count these as dependencies
    dependency_counts = {
        k: v
        for k, v in dependency_counts.items()
        if not (k.startswith("y") or k.startswith("x"))
    }

    # pp(breakages)
    # pp(dependency_counts)
    # print(len(dependency_counts))

    return breakages, dependency_counts


if __name__ == "__main__":
    INPUT_FP = argv[1]
    defns, relations = read_input(INPUT_FP)
    breakages, dependency_counts = search_switches(relations, 44, add)

    results = dict()
    focus = {
        k: v
        for k, v in dependency_counts.items()
        if v == max(dependency_counts.values())
    }

    results["default"] = sum(breakages.values())
    pp(results)

    # for combo in combinations(list(focus), 8):
    #     print("New dependecy combination.")
    #     swapme = list()
    #     # print(combo)
    #     for pair in combinations(combo, 2):
    #         swapme.append(pair)
    #     print("New pair selection")
    #     print(len(swapme))
    #     print(swapme)

    #     nr = deepcopy(relations)
    #     for a, b in swapme:
    #         nr[a], nr[b] = nr.pop(b), nr.pop(a)
    #     nb, dep_counts = search_switches(nr, 44, add)
    #     results[tuple(swapme)] = sum(nb.values())
    #     pp(dict(sorted(results.items(), key=lambda _: _[1])))

    # relations["hcm"], relations["gfw"] = relations.pop("gfw"), relations.pop("hcm")

    for k in dependency_counts:
        if k in reduce(set.union, flattened_children(relations,
                                                     {_ for _ in dependency_counts if _ != k})):
            
