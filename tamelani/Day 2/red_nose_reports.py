with open('input.txt', 'r') as file:
    big_list = file.read().splitlines()
# Empty passing reports
passing_reports = 0

# Iterate through big list
for x in big_list:
    # Create temporary array to compare line items
    temp_list = list(map(int, x.split(' ')))
    # use first difference to determine whether increase/decrease 
    polar_values = []
    for r in range(0, len(temp_list)):
        if r != 0:
            # Create variable to track differences
            diff = temp_list[r] - temp_list[r-1] 
            if 1 <= diff <= 3:
                polar_values.append('positive')
            elif -3 <= diff <= -1:
                polar_values.append('negative')
            else:
                # Need this to track big number jumps(besides negative and postitive changes)
                polar_values.append('too big')
            
    if ("positive" in polar_values and "negative" in polar_values) or "too big" in polar_values:
        continue
    else:
        passing_reports+=1

print(passing_reports)
