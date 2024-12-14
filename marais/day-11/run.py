with open('data.txt', 'r') as f:
    data = f.readlines()

numbers = list(map(int,(data[0].split())))

def is_even(n: int) -> bool:
    return n >= 10 and not is_even(n // 10) or False


def add_to_dict(numbers: {}, key: int, count: int) -> {}:
    if key in numbers:
        numbers[key] += count
    else:
        numbers[key] = count
    return numbers


def apply_transformation2(old_numbers: {}):
    new_numbers = {}
    for number in old_numbers:
        if number == 0:
            add_to_dict(new_numbers, 1, old_numbers[number])
        elif is_even(number):
            nr_str = str(number)
            nr1 = int(nr_str[:len(nr_str)//2])
            nr2 = int(nr_str[len(nr_str)//2:])
            add_to_dict(new_numbers, nr1, old_numbers[number])
            add_to_dict(new_numbers, nr2, old_numbers[number])
        else:
            add_to_dict(new_numbers, number*2024, old_numbers[number])

    return new_numbers

number_count = {}
for number in numbers:
    number_count = add_to_dict(number_count, number, 1)

for i in range(25):
    number_count = apply_transformation2(number_count)
print(f"Part 1: {sum(list(number_count.values()))}")

for i in range(50):
    number_count = apply_transformation2(number_count)
print(f"Part 2: {sum(list(number_count.values()))}")