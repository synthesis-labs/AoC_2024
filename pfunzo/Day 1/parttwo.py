
def GetNumbers():
    left = []
    right = []

    f = open("input2.txt", "r")

    for line in f:
        temp = line.split("  ")

        left.append(int(temp[0]))

        right.append(int(temp[1]))

    return left, right



def Solution(left, right):
    sum = 0
    for i in range(len(left)):
        count = 0
        for num in right:
            if num == left[i]:
                count+=1
        
        sum+=(left[i] * count)

    return sum





left, right = GetNumbers()

Locations = Solution(left, right)

print(Locations)

# print(f"{left} are the left numbers")
# print(f"{right} are the left numbers")


