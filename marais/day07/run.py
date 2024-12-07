import itertools

with open('data.txt', 'r') as f:
    data = f.readlines()

data = [x.strip().split() for x in data]
data = [[x[0][:-1]] + x[1:] for x in data]
data = [list(map(int, x)) for x in data]
# print(data)

def calculate(op_string, arguments) -> int:
    # print(f"op_string: {op_string}, arguments: {arguments}")
    result = arguments[0]
    for i in range(len(op_string)):
        if op_string[i] == '+':
            result += arguments[i + 1]
        elif op_string[i] == '*':
            result *= arguments[i + 1]
        elif op_string[i] == '|':
            result = int(str(result) + str(arguments[i + 1]))
    return result

def solve(result, arguments, operators):
    # print(arguments)
    positions = len(arguments) - 1

    perms = [''.join(p) for p in itertools.product(operators, repeat=positions)]
    # print(perms)
    for op_string in perms:
        calc = calculate(op_string, arguments)
        if calc == result:
            return True
    return False


acc = 0
for x in data:
    if solve(x[0], x[1:], ['+', '*']):
        acc += x[0]
        # print(f"adding {x}")
print(f"Part 1: {acc}")

# part 2
acc = 0
for x in data:
    if solve(x[0], x[1:], ['+', '*', '|']):
        acc += x[0]
print(f"Part 2: {acc}")