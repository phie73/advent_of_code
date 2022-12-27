from pathlib import Path

import numpy as np
import scipy.ndimage as ndi


DATA = Path(__file__).with_name('input.txt').read_text()


def exposed_faces(cubes):
    n_exposed = 0

    for x, y, z in cubes:
        for dx, dy, dz in (
            (1, 0, 0),
            (-1, 0, 0),
            (0, 1, 0),
            (0, -1, 0),
            (0, 0, 1),
            (0, 0, -1),
        ):

            if (x + dx, y + dy, z + dz) not in cubes:
                n_exposed += 1

    return n_exposed


def part1(s: str):
    cubes = {tuple(int(val) for val in line.split(',')) for line in s.splitlines()}
    return exposed_faces(cubes)


def part2(s: str):
    cubes = np.array([[int(val) for val in line.split(',')] for line in s.splitlines()])

    space = np.zeros(cubes.max(axis=0) + 1)

    i, j, k = cubes.T
    space[i, j, k] = 1

    space = ndi.binary_fill_holes(space)

    cubes = set(zip(*np.where(space)))

    return exposed_faces(cubes)


print(part1(DATA))
print(part2(DATA))
