# read data.txt
import re

with open('data.txt', 'r') as f:
    data = f.readlines()

# Create a 2 x 2 matrix of characters from data
data = [list(x.strip()) for x in data]
# print(data)

def find_horizontal(word: str, matrix: list) -> list:
    result = []
    for i in range(len(matrix)):
        for j in range(len(matrix[i]) - len(word) + 1):
            if matrix[i][j] == word[0]:
                if all([matrix[i][j + k] == word[k] for k in range(1, len(word))]):
                    result.append((i, j, i, j + len(word)-1))
    return result

def find_vertical(word: str, matrix: list) -> list:
    result = []
    for i in range(len(matrix) - len(word) + 1):
        for j in range(len(matrix[i])):
            if matrix[i][j] == word[0]:
                if all([matrix[i + k][j] == word[k] for k in range(1, len(word))]):
                    result.append((i, j, i + len(word)-1, j))
    return result

def find_diagonal(word: str, matrix: list) -> list:
    result = []
    for i in range(len(matrix) - len(word) + 1):
        for j in range(len(matrix[i]) - len(word) + 1):
            if matrix[i][j] == word[0]:
                if all([matrix[i + k][j + k] == word[k] for k in range(1, len(word))]):
                    result.append((i, j, i + len(word)-1, j + len(word)-1))
    return result

def find_diagonal_reverse(word: str, matrix: list) -> list:
    result = []
    for i in range(len(matrix) - len(word) + 1):
        for j in range(len(word) - 1, len(matrix[i])):
            if matrix[i][j] == word[0]:
                if all([matrix[i + k][j - k] == word[k] for k in range(1, len(word))]):
                    result.append((i, j, i + len(word)-1, j - (len(word)-1)))
    return result

find_diagonal_reverse('SAMX', data)

acc = []
for word in ['XMAS', 'SAMX']:
    acc.extend(find_horizontal(word, data))
    acc.extend(find_vertical(word, data))
    acc.extend(find_diagonal(word, data))
    acc.extend(find_diagonal_reverse(word, data))

def print_matrix(mask, matrix):
    # Print the elements in the matrix if it falls within a range
    # defined by the mask. The mask is a list of tuples where each
    # tuple is a range of indices to print.
    for i in range(len(matrix)):
        for j in range(len(matrix[i])):
            if any([m[0] <= i <= m[2] and m[1] <= j <= m[3] for m in mask]):
                print(matrix[i][j], end='')
            else:
                print('.', end='')
        print()

# print(acc)
# print()
# print_matrix(acc, data)
# print()
print(f"Part 1: {len(acc)}")
#
# print( f"Horizontal XMAS: {find_horizontal('XMAS', data)}")
# print( f"Horizontal SAMX: {find_horizontal('SAMX', data)}")
# print( f"Vertical XMAS: {find_vertical('XMAS', data)}")
# print( f"Vertical SAMX: {find_vertical('SAMX', data)}")
# print( f"Diagonal XMAS: {find_diagonal('XMAS', data)}")
# print( f"Diagonal SAMX: {find_diagonal('SAMX', data)}")
# print( f"Diagonal Reverse XMAS: {find_diagonal_reverse('XMAS', data)}")
# print( f"Diagonal Reverse SAMX: {find_diagonal_reverse('SAMX', data)}")

# part2
#
# print()
# print()

# print("Diagonal MAS: ", find_diagonal('MAS', data))
# print("Reverse Diag MAS: ", find_diagonal_reverse('MAS', data))

def find_x_mas(matrix):
    acc = []
    for i in range(len(matrix) - 2):
        for j in range(len(matrix[i]) - 2):
            if matrix[i][j] == 'M' and matrix[i][j+2] == 'M' and matrix[i+1][j+1] == 'A' and matrix[i+2][j] == 'S' and matrix[i+2][j+2] == 'S':
                acc.append((i, j, i+2, j+2))

    for i in range(len(matrix) - 2):
        for j in range(len(matrix[i]) - 2):
            if matrix[i][j] == 'S' and matrix[i][j+2] == 'S' and matrix[i+1][j+1] == 'A' and matrix[i+2][j] == 'M' and matrix[i+2][j+2] == 'M':
                acc.append((i, j, i+2, j+2))

    for i in range(len(matrix) - 2):
        for j in range(len(matrix[i]) - 2):
            if matrix[i][j] == 'S' and matrix[i][j+2] == 'M' and matrix[i+1][j+1] == 'A' and matrix[i+2][j] == 'S' and matrix[i+2][j+2] == 'M':
                acc.append((i, j, i+2, j+2))

    for i in range(len(matrix) - 2):
        for j in range(len(matrix[i]) - 2):
            if matrix[i][j] == 'M' and matrix[i][j+2] == 'S' and matrix[i+1][j+1] == 'A' and matrix[i+2][j] == 'M' and matrix[i+2][j+2] == 'S':
                acc.append((i, j, i+2, j+2))

    return acc

print(f"Part 2: {len(find_x_mas(data))}")
