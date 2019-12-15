from collections import defaultdict

with open("input14.txt") as f:
    unit = {"ORE": 1}
    ingredients = {"ORE": []}

    for line in f.readlines():
        l, r = line.strip().split(" => ")

        def parse(s):
            unit, name = s.split()
            return {"unit": int(unit), "name": name}

        unit[parse(r)["name"]] = parse(r)["unit"]
        ingredients[parse(r)["name"]] = [parse(s) for s in l.split(", ")]

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

    satisfy(1, "FUEL")
    print(bought["ORE"])
