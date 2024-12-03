
def GetNumbers():
    left = []
    right = []

    f = open("input.txt", "r")

    for line in f:
        temp = line.split("  ")

        left.append(int(temp[0]))

        right.append(int(temp[1]))

    return left, right



def Solution(left, right):
    sum = 0
    x = sorted(left)
    y = sorted(right)
    cnt = len(left)  
    for i in range(cnt):
        dist = abs( x[i] - y[i] )
        sum+=dist
        
    return sum




      


left, right = GetNumbers()

Locations = Solution(left, right)

print(Locations)

# print(f"{left} are the left numbers")
# print(f"{right} are the left numbers")


