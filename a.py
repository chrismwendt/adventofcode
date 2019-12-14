from collections import defaultdict
from operator import itemgetter

with open("input14.txt") as f:
    recipes = {"ORE": {"unit": 1, "ingredients": []}}
    for line in f.readlines():
        l, r = line.strip().split(" => ")

        def parse(s):
            unit, name = s.split()
            return {"unit": int(unit), "name": name}

        recipes[parse(r)["name"]] = {
            "unit": parse(r)["unit"],
            "ingredients": [parse(s) for s in l.split(", ")],
        }

    inventory = defaultdict(lambda: {"bought": 0, "leftover": 0})

    def satisfy(n, chemical):
        leftover, bought = itemgetter("leftover", "bought")(inventory[chemical])
        unit, ingredients = itemgetter("unit", "ingredients")(recipes[chemical])

        quantity = 0
        if n <= leftover:
            inventory[chemical]["leftover"] -= n
        else:
            n -= inventory[chemical]["leftover"]
            quantity = (n + (unit - 1)) // unit
            inventory[chemical]["bought"] += quantity * unit
            inventory[chemical]["leftover"] = (quantity * unit) - n

        for ingredient in ingredients:
            satisfy(quantity * ingredient["unit"], ingredient["name"])

    satisfy(1, "FUEL")
    print(inventory["ORE"]["bought"])
