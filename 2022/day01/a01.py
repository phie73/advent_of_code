#!/usr/bin/env python

file = open("input01.txt")
elfCurrent = caloriesCurrent = elfHighest = caloriesHighest = 0

for line in file:
    if (line == "\n"):
        if (caloriesCurrent > caloriesHighest):
            caloriesHighest = caloriesCurrent
            elfHighest = elfCurrent
            elfCurrent = elfCurrent + 1
        caloriesCurrent = 0
    else:
        caloriesCurrent = caloriesCurrent + int(line)


print('{}{}{}{}'.format("highest Calories: ", caloriesHighest, " highest Elf: ", elfHighest))

        

