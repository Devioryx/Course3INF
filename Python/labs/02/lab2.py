# Write your code here:
moves = [(1, 2), (2, 1), (2, -1), (1, -2), (-1, -2), (-2, -1), (-2, 1), (-1, 2)]

def possible_moves1(collumn, row):
    collumn_number = ord(collumn) - ord('a') + 1

    return ((chr(new_col_number + ord('a') - 1), new_row) for (col, rol) in moves if 1 <= (new_col_number := (col + collumn_number)) <= 8 and 1 <= (new_row := rol + row) <= 8)
 
def possible_moves(collumn, row):
    collumn_number = ord(collumn) - ord('a') + 1
    
    for col, rol in moves:
        new_col_number = col + collumn_number
        new_row = rol + row

        if 1 <= new_col_number <= 8 and 1 <= new_row <= 8:
            new_col = chr(new_col_number + ord('a') - 1)
            yield (new_col, new_row)


    
# 2 possible moves
assert set(possible_moves('a', 1)) == {('c', 2), ('b', 3)}
assert set(possible_moves('h', 1)) == {('f', 2), ('g', 3)}
assert set(possible_moves('h', 8)) == {('f', 7), ('g', 6)}
assert set(possible_moves('a', 8)) == {('c', 7), ('b', 6)}

# 3 possible moves
assert set(possible_moves('a', 2)) == {('c', 3), ('b', 4), ('c', 1)}
assert set(possible_moves('a', 7)) == {('c', 6), ('b', 5), ('c', 8)}
assert set(possible_moves('h', 2)) == {('g', 4), ('f', 1), ('f', 3)}
assert set(possible_moves('h', 7)) == {('f', 6), ('g', 5), ('f', 8)}

# 4 possible moves
assert set(possible_moves('a', 3)) == {('c', 4), ('b', 5), ('b', 1), ('c', 2)}
assert set(possible_moves('h', 6)) == {('g', 8), ('f', 5), ('g', 4), ('f', 7)}
assert set(possible_moves('g', 2)) == {('e', 1), ('f', 4), ('h', 4), ('e', 3)}

# 6 possible moves
assert set(possible_moves('b', 3)) == {('c', 5), ('d', 2), ('c', 1), ('d', 4), ('a', 5), ('a', 1)}
assert set(possible_moves('g', 6)) == {('f', 4), ('h', 4), ('e', 5), ('e', 7), ('h', 8), ('f', 8)}

# 8 possible moves
assert set(possible_moves('d', 4)) == {('b', 3), ('b', 5), ('c', 2), ('c', 6), ('e', 2), ('e', 6), ('f', 3), ('f', 5)}
assert set(possible_moves('f', 6)) == {('h', 7), ('g', 8), ('e', 8), ('g', 4), ('d', 5), ('e', 4), ('h', 5), ('d', 7)}

# Generator tests
for move in possible_moves('a', 1):
    assert move in {('c', 2), ('b', 3)}

generator = possible_moves('a', 1)
assert next(generator) in {('c', 2), ('b', 3)}
assert next(generator) in {('c', 2), ('b', 3)}

try:
    next(generator)
    assert False
except StopIteration:
    pass