# Collect data from text file
with open('input.txt', "r") as file:
    big_list = file.read().splitlines()

list_1 = []
list_2 = []
for x in big_list: 
    # Have to convert to integer as string is implied by default
    temp_array = list(map(int, x.split('  ')))
    list_1.append(temp_array[0])
    list_2.append(temp_array[1])

# Order lists - default is ascending order
list_1.sort()
list_2.sort()

if (len(list_1) == len(list_2)):
    # Initialize the count and distance_tracker
    distance_tracker = []
    total_difference = 0
    for x in range(0, len(list_1)):
        temp_array = [list_1[x], list_2[x]]
        temp_array.sort()
        temp_distance = temp_array[1] - temp_array[0]
        distance_tracker.append(temp_distance)
        
        # To track difference per list index
        same_difference_multiply = 0

        list_same_count = list_2.count(list_1[x])
        same_difference_multiply = list_1[x] * list_same_count
        total_difference += same_difference_multiply

    if len(distance_tracker) > 0:
        distance_total = 0
        for x in distance_tracker:
            distance_total += x

        print(distance_total) # Answer to Part 1
        print(total_difference) # Answer to Part 2
