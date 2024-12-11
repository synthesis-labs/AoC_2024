def GetLevels():

    f = open("input.txt", "r")

    levels = []

    for line in f:
        temp = line.split(" ")
        level = [int(num) for num in temp]
        levels.append(level)
    
    f.close()
            

    return levels


def FailDirectionCheck(level):
    diff = (level[1] - level[0])
    for i in range(1, len(level) -1):
        diff = (level[i+1] - level[i])
        if level[0] < level[1]: ## we are increasing
            if diff < 0: 
                return True
        elif level[0] > level[1]:  ## we are decreasing
            if diff > 0: 
                return True
    return False


def FailSumCheck(level):
    for i in range(len(level) - 1):
        diff = abs(level[i] - level[i+1])
        if ((diff > 3) or (diff < 1)):
            return True
    return False

def Solution(levels):
    count = 0
    for level in levels:
        if (FailSumCheck(level) or (FailDirectionCheck(level) == True)):
            print(f"{level} : UnSafe")
        else :
            print(f"{level} : Safe")        
            count+=1
            
    return count

# test = [[7, 6, 4, 2, 1 ] ,[1, 2, 7, 8, 9 ] ,[9, 7, 6, 2, 1 ] ,[1, 3, 2, 4, 5 ] ,[8, 6, 4, 4, 1 ] ,
# [1, 3, 6, 7, 9 ] ,
# ]
# levels = test

levels = GetLevels()

SafeReports = Solution(levels)

print(SafeReports)