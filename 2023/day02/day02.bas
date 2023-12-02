DECLARE SUB getword (line$, w$)
DECLARE FUNCTION max! (a!, b!)
Rem red green blue
Dim cubes(3)

Cls
sum = 0
sumb = 0

Open "/home/sophia/repos/advent_of_code/2023/day02/input.txt" For Input As #1
While Not EOF(1)
    cubes(1) = 0
    cubes(2) = 0
    cubes(3) = 0
    Line Input #1, line$
    Call getword(line$, w$)
    Call getword(line$, w$)
    id = Val(w$)
    While Len(line$)
        Call getword(line$, w$)
        num = Val(w$)
        Call getword(line$, w$)
        color$ = Left$(w$, 1)
        Select Case color$
            Case "r"
                cubes(1) = max(cubes(1), num)
            Case "g"
                cubes(2) = max(cubes(2), num)
            Case Else
                cubes(3) = max(cubes(3), num)
        End Select
    Wend
    If cubes(1) <= 12 And cubes(2) <= 13 And cubes(3) <= 14 Then
        sum = sum + id
    End If
    sumb = sumb + cubes(1) * cubes(2) * cubes(3)
Wend
Print "hmm1: "; sum
Print "hmm2: "; sumb
Sub getword (line$, w$)
    cur = InStr(line$, " ")
    If cur Then
        w$ = Left$(line$, cur - 1)
        line$ = Mid$(line$, cur + 1)
    Else
        w$ = line$
        line$ = ""
    End If
End Sub
Function max (a, b)
    If a > b Then
        max = a
    Else
        max = b
    End If
End Function

