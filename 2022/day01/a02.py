#!/usr/bin/env python

file = open("input01.txt")
caloriesCurrent = caloriesHighest = 0
caloriesSecond = calories3 = 0

for line in file:
    if (line == "\n"):
        if (caloriesCurrent > caloriesHighest):
            calories3 = caloriesSecond
            caloriesSecond = caloriesHighest
            caloriesHighest = caloriesCurrent
        elif (caloriesCurrent > caloriesSecond):
            calories3 = caloriesSecond
            caloriesSecond = caloriesCurrent
        elif (caloriesCurrent > calories3):
            calories3 = caloriesCurrent
        caloriesCurrent = 0
    else:
        caloriesCurrent = caloriesCurrent + int(line)


print('{}{}{}{}{}{}'.format("highest Calories: ", caloriesHighest, " second: ", caloriesSecond, " third: ", calories3))
print('{}{}'.format("sum: ", (calories3 + caloriesHighest + caloriesSecond)))