const std = @import("std");
const input = @embedFile("input.txt");

// trial and error how big this must be
const grid = struct
{
    x: u32 = 1000,
    y: u32 = 1000,
};

fn distance(x: u32, y: u32) u32
{
    return if (x > y) x-y else y-x;
}

fn part1() !void
{
    var head = grid{};
    var tail = grid{};

    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    var positions = std.AutoHashMap(grid, void).init(gpa.allocator());
    defer positions.deinit();

    var lines = std.mem.split(u8, input, "\n");
    while (lines.next()) |line| 
    {
        var move = std.mem.split(u8, line, " ");
        var direction: u8 = line[0];
        _ = move.next().?;
        var step: u32 = try std.fmt.parseUnsigned(u32, move.next().?, 10);

        var i: u32 = 0;
        while (i < step) : (i += 1)
        {
            switch (direction) {
                'R' => head.y += 1,
                'U' => head.x -= 1,
                'L' => head.y -= 1,
                'D' => head.x += 1,
                else => unreachable,
            }

            if (distance(head.x, tail.x) < 2 and distance(head.y, tail.y) < 2) 
            {
                continue; 
            }

            switch (direction) {
                'R' => tail.y += 1,
                'U' => tail.x -= 1,
                'L' => tail.y -= 1,
                'D' => tail.x += 1,
                else => unreachable,
            }

            if (direction == 'R' or direction == 'L') {
                if (head.x > tail.x) 
                {
                    tail.x += 1;
                }
                if (head.x < tail.x)
                {
                    tail.x -= 1;
                }
            }

            if (direction == 'U' or direction == 'D') {
                if (head.y > tail.y) 
                {
                    tail.y += 1;
                }
                if (head.y < tail.y)
                {
                    tail.y -= 1;
                }
            }

            try positions.put(tail, {});
        }
    }
    std.debug.print("Tail positions {d}\n", .{positions.count() + 1});
}

fn part2() !void
{
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    var positions = std.AutoHashMap(grid, void).init(gpa.allocator());
    defer positions.deinit();

    var knots: [10]grid = [_]grid {grid{}, grid{}, grid{}, grid{}, grid{}, grid{}, grid{}, grid{}, grid{}, grid{} };

    var lines = std.mem.split(u8, input, "\n");
    while (lines.next()) |line| 
    {
        var move = std.mem.split(u8, line, " ");
        var direction: u8 = line[0];
        _ = move.next().?;
        var step: u32 = try std.fmt.parseUnsigned(u32, move.next().?, 10);

        var i: u32 = 0;
        while (i < step) : (i += 1)
        {
            switch (direction) {
                'R' => knots[0].y += 1,
                'U' => knots[0].x -= 1,
                'L' => knots[0].y -= 1,
                'D' => knots[0].x += 1,
                else => unreachable,
            }

            var j: u32 = 1;
            while (j < knots.len) : (j +=1)
            {
                var front = knots[j-1];
                if (distance(front.x, knots[j].x) < 2 and distance(front.y, knots[j].y) < 2)
                {
                    continue;
                }
                if (front.x > knots[j].x)
                {
                    knots[j].x += 1;
                }
                if (front.x < knots[j].x)
                {
                    knots[j].x -= 1;
                }
                if (front.y > knots[j].y)
                {
                    knots[j].y += 1;
                }
                if (front.y < knots[j].y)
                {
                    knots[j].y -= 1;
                }
            }
            
            try positions.put(knots[9], {});
        }
    }
    std.debug.print("Tail positions {d}\n", .{positions.count()});
}


pub fn main() !void
{
    try part1();
    try part2();
}