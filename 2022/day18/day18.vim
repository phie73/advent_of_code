vim9script

var  input = readfile('/home/sophia/repos/advent_of_code/2022/day18/input.txt')
echo "hello world"
var counter = 6 
var surface_area = 0
var surface_area_b = 0
var visited = []

for line in input
  counter = 6
  var s =  split(line, ',')
  var dxh = join([string(str2nr(s[0]) + 1), s[1], s[2]], ',')
  var dxl = join([string(str2nr(s[0]) - 1), s[1], s[2]], ',')
  var dyh = join([s[0], string(str2nr(s[1]) + 1), s[2]], ',') 
  var dyl = join([s[0], string(str2nr(s[1]) - 1), s[2]], ',')
  var dzh = join([s[0], s[1], string(str2nr(s[2]) + 1)], ',')
  var dzl = join([s[0], s[1], string(str2nr(s[2]) - 1)], ',') 
  var dlow = join([string(str2nr(s[0]) - 1), string(str2nr(s[1]) - 1), string(str2nr(s[2]) - 1)], ',')
  var dhigh = join([string(str2nr(s[0]) + 1), string(str2nr(s[1]) + 1), string(str2nr(s[2]) + 1)], ',')
 
  if index(input, dxh) >= 0 
    counter = counter - 1
  endif
  if index(input, dxl) >= 0
    counter = counter - 1
  endif
  if index(input, dyh) >= 0
    counter = counter - 1
  endif
  if index(input, dyl) >= 0
    counter = counter - 1
  endif
  if index(input, dzh) >= 0
    counter = counter - 1
  endif
  if index(input, dzl) >= 0
    counter = counter - 1
  endif
  surface_area = surface_area + counter
  surface_area_b = surface_area_b + counter
  if (!(index(visited, dxh) >= 0) && !(index(visited, dxl) >= 0)) && (!(index(visited, dyl) >= 0) && !(index(visited, dyh) >= 0)) && (index(input, dzl) >= 0 && index(input, dzh) >= 0) 
    surface_area_b = surface_area_b - 3    
  else 
    if (!(index(visited, dzh) >= 0) && !(index(visited, dzl) >= 0)) && (!(index(visited, dyl) >= 0) && !(index(visited, dyh) >= 0)) && (index(input, dxl) >= 0 && index(input, dxh) >= 0)
   surface_area_b = surface_area_b - 3
    else 
      if (!(index(visited, dxh) >= 0) && !(index(visited, dxl) >= 0)) && (!(index(visited, dzl) >= 0) && !(index(visited, dzh) >= 0)) && (index(input, dyl) >= 0 && index(input, dyh) >= 0)
    surface_area_b = surface_area_b - 3
      endif 
    endif
  endif
  insert(visited, line)
endfor

echo surface_area
echo surface_area_b
