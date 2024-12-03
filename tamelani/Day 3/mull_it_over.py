import re

with open('input.txt', 'r') as file:
    big_data = file.read()

# Use Python Regex to find the specific pattern
found_muls = re.findall("mul+\([0-9]*,[0-9]*+\)", big_data)
total_muls = 0

for x in found_muls:
    multiplied_muls = 0
    formatted_muls = str.replace(x, 'mul(', '')
    formatted_muls = str.replace(formatted_muls, ')', '')
    temp_multipliers = list(map(int, formatted_muls.split(',')))
    multiplied_muls = temp_multipliers[0] * temp_multipliers[1]
    total_muls += multiplied_muls

print(total_muls)