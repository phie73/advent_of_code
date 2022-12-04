# using Pkg
# Pkg.add("ResumableFunctions")

using ResumableFunctions

@resumable function getRanges()
    for line in readlines("input.txt")
        elf1, elf2 = split(line, ",")
        e1, e2 = parse(Int, split(elf1, "-")[1]), parse(Int, split(elf1, "-")[2])
        e3, e4 = parse(Int, split(elf2, "-")[1]), parse(Int, split(elf2, "-")[2])
        @yield e1, e2, e3, e4
    end
end

function problem1()::Int
    sum = 0
    for rg in getRanges()
        e1, e2, e3, e4 = rg
        if (e1 >= e3 && e2 <= e4) || (e1 <= e3 && e2 >= e4)
            sum += 1
        end
    end
    return sum
end

function problem2()::Int
    pairs = 0
    for rg in getRanges()
        e1, e2, e3, e4 = rg
        if !(e2 < e3 || e1 > e4)
            pairs += 1
        end
    end
    return pairs
end 

solution1 = problem1()
solution2 = problem2()

print("assignments pairs fully contain other: ", solution1, "\n")
print("overlaping assignments in general: ", solution2, "\n")