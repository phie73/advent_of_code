# open file
depths = []
increases = 0
file = open("input.txt", "r")

#get rid of \n in depths array

for lines in file:
    depths.append(lines)

print(len(depths))
print(type(depths[1]))

for i in range(0, (len(depths)-1), 1):
    if (int(depths[i+1]) > int(depths[i])):
        increases = increases +1

print(increases)