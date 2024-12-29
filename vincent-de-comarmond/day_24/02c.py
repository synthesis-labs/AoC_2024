from collections import Counter, defaultdict
from copy import deepcopy
from functools import lru_cache
from math import log10
from operator import add, lshift, and_, xor, or_
from pprint import pp
from sys import argv
from typing import Callable


@lru_cache(maxsize=None)
def read_input(input_fp: str) -> tuple[list[str]]:
    input_defns: list[str] = list()
    relations: list[str] = list()

    with open(input_fp, "r") as flin:
        raw = flin.read().split("\n")

    part1_end = raw.index("")
    input_defns = raw[:part1_end]
    relations = [_ for _ in raw[part1_end + 1 :] if "->" in _]
    return input_defns, relations


def input_defs(input_def_strs: list[str]) -> dict[str, int]:
    input_vals = dict()

    for ln in input_def_strs:
        node, val = map(str.strip, ln.split(":"))
        input_vals[node] = int(val)
    return input_vals


def determine_starting(
    raw_relations: list[str],
) -> tuple[dict[str, dict[str, dict]], dict[tuple[str, str, str], Callable]]:

    naive = dict()
    for ln in raw_relations:
        parents, child = map(str.strip, ln.split("->"))
        naive[child] = parents

    downstream = defaultdict(set)
    relations: dict[frozenset[str], Callable[[int, int], int]] = dict()

    for child, parents in naive.items():
        parent1, function, parent2 = parents.split(" ")
        downstream[parent1].add(child)
        downstream[parent2].add(child)

        func = and_ if function == "AND" else xor if function == "XOR" else or_
        relations[(parent1, parent2, child)] = func
    return downstream, relations


def get_roots_and_leaves(nested_dict) -> set[str]:
    roots, leaves = set(), set()
    for node, relations in nested_dict.items():
        for relation in relations:
            if relation not in nested_dict:
                leaves.add(relation)

    for node, relations in nested_dict.items():
        for k, v in nested_dict.items():
            if k == node:
                continue
            if node in v:
                break
        else:
            roots.add(node)

    return roots, leaves


def get_parents(nested: dict, target: str, add_root: bool = True):
    result = dict()

    for k, v in nested.items():
        if target in v:
            result[k] = dict()

    for k in deepcopy(result):
        # print(f"Searching {k}")
        result[k] = get_parents(nested, k, False)

    return {target: result} if add_root else result


def get_all_parents(nested: dict) -> dict:
    roots, leaves = get_roots_and_leaves(nested)
    return {leaf: get_parents(nested, leaf, add_root=False) for leaf in leaves}


def get_children(nested: dict, target: str, add_root: bool = True):
    result = dict()
    if target in nested:
        for child in nested[target]:
            if child not in result:
                result[child] = get_children(nested, child, False)
            else:
                result[child] |= get_children(nested, child, False)

    return {target: result} if add_root else result


def get_all_children(nested: dict) -> dict:
    roots, leaves = get_roots_and_leaves(nested)
    return {root: get_children(nested, root, add_root=False) for root in roots}


def solve(
    inputs: dict[str, int],
    relationships: dict[tuple[str, str, str], Callable[[int, int], int]],
    output_symbol: str = "z",
):

    output = inputs.copy()
    resolved = False

    while not resolved:
        resolved = True
        for (parent1, parent2, child), v in relationships.items():
            if parent1 in output and parent2 in output:
                output[child] = v(output[parent1], output[parent2])
            else:
                resolved = False
    return output, dict(
        sorted({k: v for k, v in output.items() if k.startswith(output_symbol)}.items())
    )
