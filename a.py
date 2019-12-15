from collections import defaultdict
from hashlib import md5
import itertools
from itertools import tee


class V2:
    def __init__(self, x, y):
        self.x = x
        self.y = y

    def __add__(self, other):
        return V2(self.x + other.x, self.y + other.y)

    def __str__(self):
        return f"<{self.x},{self.y}>"

    def __repr__(self):
        return f"<{self.x},{self.y}>"

    def __hash__(self):
        return hash((self.x, self.y))

    def __eq__(self, other):
        return self.x == other.x and self.y == other.y


def year2015day01():
    n = 0
    p = 0
    first = None
    for line in open("input2015-01.txt").readlines():
        for c in line:
            p += 1
            if c == "(":
                n += 1
            else:
                n -= 1
            if n < 0 and first is None:
                first = p

    print("1a:", n)
    print("1b:", first)


def year2015day02():
    paper = 0
    ribbon = 0
    for line in open("input2015-02.txt").readlines():
        l, w, h = map(int, line.split("x"))
        a, b, c = l * w, w * h, h * l
        paper += 2 * (a + b + c) + min(a, b, c)
        p1, p2, p3 = 2 * (l + w), 2 * (w + h), 2 * (h + l)
        ribbon += min(p1, p2, p3) + l * w * h

    print("1a:", paper)
    print("1b:", ribbon)


def year2015day03():
    v = defaultdict(lambda: 0)
    p = V2(0, 0)
    v[p] += 1
    for line in open("input2015-03.txt").readlines():
        for c in line:
            p += {"^": V2(0, 1), "v": V2(0, -1), "<": V2(-1, 0), ">": V2(1, 0)}[c]
            v[p] += 1

    print("1a:", len(v))

    v = defaultdict(lambda: 0)
    p1 = V2(0, 0)
    p2 = V2(0, 0)
    v[V2(0, 0)] += 1
    for line in open("input2015-03.txt").readlines():
        for c in line:
            p1 += {"^": V2(0, 1), "v": V2(0, -1), "<": V2(-1, 0), ">": V2(1, 0)}[c]
            v[p1] += 1
            p1, p2 = p2, p1

    print("1b:", len(v))


def year2015day04():
    i = open("input2015-04.txt").read().strip()

    def vs(z):
        for n in itertools.count():
            d = md5(i.encode("ascii") + str(n).encode("ascii")).hexdigest()
            if all(x == "0" for x in d[0:z]):
                yield n

    print("1a:", next(vs(5)))
    print("1b:", next(vs(6)))


# https://stackoverflow.com/questions/6822725/rolling-or-sliding-window-iterator
def window(iterable, size):
    iters = tee(iterable, size)
    for i in range(1, size):
        for each in iters[i:]:
            next(each, None)
    return zip(*iters)


def year2015day05():
    def nice1(s):
        return all(
            [
                len([c for c in s if c in "aeiou"]) >= 3,
                any(len(set(x)) == 1 for x in window(s, 2)),
                all(ban not in s for ban in ["ab", "cd", "pq", "xy"]),
            ]
        )

    def nice2(s):
        return all(
            [
                any(s[i : i + 2] in s[i + 2 :] for i in range(0, len(s) - 1)),
                any(s[i] == s[i + 2] for i in range(0, len(s) - 2)),
            ]
        )

    n1 = 0
    n2 = 0
    for line in open("input2015-05.txt").readlines():
        line = line.strip()
        if nice1(line):
            n1 += 1
        if nice2(line):
            n2 += 1

    print("1a:", n1)
    print("1b:", n2)


def year2015day06():
    def p1():
        ls = [[False for x in range(0, 1000)] for y in range(0, 1000)]

        def cmd(f, x1, y1, x2, y2):
            for x in range(x1, x2 + 1):
                for y in range(y1, y2 + 1):
                    ls[x][y] = f(ls[x][y])

        for line in open("input2015-06.txt").readlines():
            line = line.strip()
            if "toggle" in line:
                line = line[len("toggle ") :]
                l, r = line.split(" through ")
                x1, y1 = l.split(",")
                x2, y2 = r.split(",")
                cmd(lambda x: not x, int(x1), int(y1), int(x2), int(y2))
            if "turn on" in line:
                line = line[len("turn on ") :]
                l, r = line.split(" through ")
                x1, y1 = l.split(",")
                x2, y2 = r.split(",")
                cmd(lambda x: True, int(x1), int(y1), int(x2), int(y2))
            if "turn off" in line:
                line = line[len("turn off ") :]
                l, r = line.split(" through ")
                x1, y1 = l.split(",")
                x2, y2 = r.split(",")
                cmd(lambda x: False, int(x1), int(y1), int(x2), int(y2))

        on = 0
        for y in range(0, 1000):
            for x in range(0, 1000):
                if ls[y][x]:
                    on += 1
        print("1a:", on)

    def p2():
        ls = [[0 for x in range(0, 1000)] for y in range(0, 1000)]

        def cmd(f, x1, y1, x2, y2):
            for x in range(x1, x2 + 1):
                for y in range(y1, y2 + 1):
                    ls[x][y] = f(ls[x][y])

        for line in open("input2015-06.txt").readlines():
            line = line.strip()
            if "toggle" in line:
                line = line[len("toggle ") :]
                l, r = line.split(" through ")
                x1, y1 = l.split(",")
                x2, y2 = r.split(",")
                cmd(lambda x: x + 2, int(x1), int(y1), int(x2), int(y2))
            if "turn on" in line:
                line = line[len("turn on ") :]
                l, r = line.split(" through ")
                x1, y1 = l.split(",")
                x2, y2 = r.split(",")
                cmd(lambda x: x + 1, int(x1), int(y1), int(x2), int(y2))
            if "turn off" in line:
                line = line[len("turn off ") :]
                l, r = line.split(" through ")
                x1, y1 = l.split(",")
                x2, y2 = r.split(",")
                cmd(lambda x: max(0, x - 1), int(x1), int(y1), int(x2), int(y2))

        b = 0
        for y in range(0, 1000):
            for x in range(0, 1000):
                b += ls[y][x]
        print("1b:", b)

    p1()
    p2()


def year2015day07():
    x = {}
    for line in open("input2015-07.txt").readlines():
        line = line.strip()
        l, var = line.split(" -> ")
        assert var not in x
        x[var] = l

    s = {}

    def signal(wire):
        if wire in s:
            return s[wire]
        try:
            v = int(wire)
            s[wire] = v
            return v
        except:
            if "AND" in x[wire]:
                l, _, r = x[wire].split(" ")
                ss = signal(l) & signal(r)
            elif "OR" in x[wire]:
                l, _, r = x[wire].split(" ")
                ss = signal(l) | signal(r)
            elif "NOT" in x[wire]:
                _, v = x[wire].split(" ")
                ss = ~signal(v)
            elif "LSHIFT" in x[wire]:
                l, _, r = x[wire].split(" ")
                ss = signal(l) << signal(r)
            elif "RSHIFT" in x[wire]:
                l, _, r = x[wire].split(" ")
                ss = signal(l) >> signal(r)
            else:
                ss = signal(x[wire])
            s[wire] = ss
            return ss

    z = signal("a") & 0b1111111111111111
    print("1a:", z)
    s = {"b": z}
    print("1b:", signal("a"))


def year2019day14():
    unit = {"ORE": 1}
    ingredients = {"ORE": []}

    for line in open("input14.txt").readlines():
        l, r = line.strip().split(" => ")

        def parse(s):
            unit, name = s.split()
            return {"unit": int(unit), "name": name}

        unit[parse(r)["name"]] = parse(r)["unit"]
        ingredients[parse(r)["name"]] = [parse(s) for s in l.split(", ")]

    def solve(n, c):
        bought = defaultdict(lambda: 0)
        leftover = defaultdict(lambda: 0)

        def satisfy(n, c):
            quantity = 0
            if n <= leftover[c]:
                leftover[c] -= n
            else:
                n -= leftover[c]
                quantity = (n + unit[c] - 1) // unit[c]
                bought[c] += quantity * unit[c]
                leftover[c] = (quantity * unit[c]) - n

            for ingredient in ingredients[c]:
                satisfy(quantity * ingredient["unit"], ingredient["name"])

        satisfy(n, c)
        return bought["ORE"]

    print("1a:", solve(1, "FUEL"))

    lo = 0
    hi = int(1e12)
    while hi - lo > 1:
        mid = (lo + hi) // 2
        if solve(mid, "FUEL") < int(1e12):
            lo = mid
        else:
            hi = mid
    print("1b:", lo)


year2015day07()
