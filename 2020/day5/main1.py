
def seatId(seat):
    row, col = seat
    return row * 8 + col

def seat(seat):
    _min = 0
    _max = 127
    for i in range(7):
        cur = seat[i]
        half = int((_max-_min)/2)+1
        if cur == "F":
            _max -= half
        if cur == "B":
            _min += half
    row = _max
    _min = 0
    _max = 7
    for i in range(7,10):
        cur = seat[i]
        half = int((_max-_min)/2)+1
        if cur == "L":
            _max -= half
        if cur == "R":
            _min += half
    print((_max, _min))
    col = _max
    return (row, col)


highest = 0
with open('input.txt') as f:
    for l in f:
        seatId = seatId(seat(l))
        highest = max(highest, seatId)




print(highest)
