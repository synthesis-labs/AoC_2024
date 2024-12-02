# Collect data from text file
with open('input.txt', "r") as file:
    big_list = file.read().splitlines()

list_1 = []
list_2 = []
for x in big_list:
    temp_array = x.split('  ')
    # Have to convert to integer as string is implied by default
    list_1.append(int(temp_array[0]))
    list_2.append(int(temp_array[1]))

# Order lists - default is ascending order
list_1.sort()
list_2.sort()

if (len(list_1) == len(list_2)):
    # Initialize the count and distance_tracker
    distance_tracker = []
    for x in range(0, len(list_1)):
        temp_array = [list_1[x], list_2[x]]
        temp_array.sort()
        temp_distance = temp_array[1] - temp_array[0]
        distance_tracker.append(temp_distance)
    
    print(distance_tracker)
    if len(distance_tracker) > 0:
        distance_total = 0
        for x in distance_tracker:
            distance_total += x

        print(distance_total)
