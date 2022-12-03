-- open file
local function readFile(filename)
    local file = assert(io.open(filename, "r"))
    local content = file:read("*all")
    file:close()
    return content
end


local values = {
    a = 1,
    b = 2,
    c = 3,
    d = 4,
    e = 5,
    f = 6,
    g = 7,
    h = 8,
    i = 9,
    j = 10,
    k = 11,
    l = 12,
    m = 13,
    n = 14,
    o = 15,
    p = 16,
    q = 17,
    r = 18,
    s = 19,
    t = 20,
    u = 21,
    v = 22,
    w = 23,
    x = 24,
    y = 25,
    z = 26,
    A = 27,
    B = 28,
    C = 29,
    D = 30,
    E = 31,
    F = 32,
    G = 33,
    H = 34,
    I = 35,
    J = 36,
    K = 37,
    L = 38,
    M = 39,
    N = 40,
    O = 41,
    P = 42,
    Q = 43,
    R = 44,
    S = 45,
    T = 46,
    U = 47,
    V = 48,
    W = 49,
    X = 50,
    Y = 51,
    Z = 52
}

local elfpack = {

}
local called = 0


local function firstProblem(fileContent)
    local sum = 0
    local known = ""
    for line in fileContent:gmatch("[^\r\n]+") do
        local string1 = line:sub(1, (line:len(2)/2))
        local string2 = line:sub(((line:len(2)/2)+1))
        for i = 1, string.len(string1) do
            local character = string.sub(string1, i, i)
            if (string.find(string2, character) and not string.find(known, character)) then
                sum = sum + values[character]
            end
            known = known .. character
        end
        known = ""
    end
    return sum
end

local function iteration(elf1, elf2, elf3)
    for i = 1, string.len(elf1) do
        local character = string.sub(elf1, i, i)
        if (string.find(elf2, character) and string.find(elf3, character)) then
            return values[character]
        end
    end
    print(called)
    called = called + 1
    return 0
end

local function secondProblem(fileContent)
    local sum = 0
    local counter = 1
    for line in fileContent:gmatch("[^\r\n]+") do
        elfpack[counter] = line 
        if (counter == 3) then
            local elf1 = elfpack[1]
            local elf2 = elfpack[2]
            local elf3 = elfpack[3]
            if ((string.len(elf1) > string.len(elf2)) and (string.len(elf1) > string.len(elf3))) then
                long = elf1
                if (string.len(elf2) > string.len(elf3)) then
                    middel = elf2
                    short = elf3
                else
                    middel = elf3
                    short = elf2
                end 
            elseif ((string.len(elf2) > string.len(elf1)) and (string.len(elf2) > string.len(elf3))) then
                long = elf2
                if (string.len(elf1) > string.len(elf3)) then
                    middel = elf1
                    short = elf3
                else 
                    middel = elf3
                    short = elf1
                end
            else
                long = elf3
                if (string.len(elf1) > string.len(elf2)) then
                    middle = elf1
                    short = elf2
                else
                    middel = elf2
                    short = elf1
                end
            end
            counter = 0
            sum = sum + iteration(long, middel, short)
        end
        counter = counter + 1
    end
    return sum
end

local solution1 = firstProblem(readFile("input03.txt"))
local solution2 = secondProblem(readFile("input03.txt"))


print("solution1: ", solution1)
print("solution2: ", solution2)