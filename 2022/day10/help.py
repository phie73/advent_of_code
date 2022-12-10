def parse():
	with open("input.txt") as file:
		return file.read()

input = [line.split() for line in parse().splitlines()]

X = cycle = 1
crt = 0
signal_strengths = []
pixels = ""

def do_cycle():
    global crt
    global pixels

    if (cycle - 20) % 40 == 0:
        signal_strengths.append(cycle * X)

    pixels += "#" if X - 1 <= crt <= X + 1 else "."
    crt += 1 if crt < 39 else -39

    if crt == 0:
        pixels += "\n"

    return 1


for instruction in input:
    cycle += do_cycle()

    if instruction[0] == "addx":
        cycle += do_cycle()
        X += int(instruction[1])

print(f"pfff: {sum(signal_strengths)}")
print(f"uff: \n{pixels}")