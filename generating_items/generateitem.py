#!/usr/bin/env python3

import random

# Properties
#
# &#9679; = circle, &#9632; = square, &#9650; = triangle
shapes = ["&#9679;", "&#9632;", "&#9650;"]
colors = ["red", "blue", "green"]

# List of all possible objects
all_objects = [{"shape": shape,
                "color": color}
               for shape in shapes
               for color in colors]

# Quantifiers
#
# NB: A quantifier Q takes a property P and a box B and returns true iff Q
# objects in B are P. A "rule" is a property of boxes, e.g. lambda B: Q(P)(B),
# or lambda B: not Q1(P)(B) and not Q2(P)(B), for some quantifiers Q, Q1, Q2 and
# some property P.
Some = lambda P: lambda B: 1 <= len([x for x in B if P(x)])
All = lambda P: lambda B: len([x for x in B if P(x)]) == len(B)
NotAll = lambda P: lambda B: not(All(P)(B))
SBNA = lambda P: lambda B: Some(P)(B) and NotAll(P)(B)

Exactly3 = lambda P: lambda B: len([x for x in B if P(x)]) == 3
AtLeast3 = lambda P: lambda B: len([x for x in B if P(x)]) >= 3
AtMost3 = lambda P: lambda B: len([x for x in B if P(x)]) <= 3

Exactly4 = lambda P: lambda B: len([x for x in B if P(x)]) == 4
AtLeast4 = lambda P: lambda B: len([x for x in B if P(x)]) >= 4
AtMost4 = lambda P: lambda B: len([x for x in B if P(x)]) <= 4

# Minimal and maximal allowable box sizes
box_min = 5
box_max = 6

# Minimal and maximal allowable group sizes
grp_min = 3
grp_max = 4

# Functions

def qua2str(Q):
    """Convert a quantifier to a string."""
    if Q == Some:
        return "Some"
    elif Q == All:
        return "All"
    elif Q == NotAll:
        return "NotAll"
    elif Q == SBNA:
        return "SBNA"

    elif Q == Exactly3:
        return "Exactly3"
    elif Q == AtLeast3:
        return "AtLeast3"
    elif Q == AtMost3:
        return "AtMost3"

    elif Q == Exactly4:
        return "Exactly4"
    elif Q == AtLeast4:
        return "AtLeast4"
    elif Q == AtMost4:
        return "AtMost4"

def create_item(Q1, Q2, P, condition, group_y, group_n, box_crit):
    """Return an experimental item given two quantifiers, a property, a
    condition, two groups, and a box."""
    return {"Q1": qua2str(Q1),
            "Q2": qua2str(Q2),
            "P": P,
            "condition": condition,
            "yes group": group_y,
            "no group": group_n,
            "critical box": box_crit}

def gen_random_object():
    """Return a random object."""
    return random.choice(all_objects)

def gen_random_box(R):
    """Return a random box (of objects) that satisfies rule R."""
    # Initialize empty result.
    result = []

    # Keep generating random boxes of objects until we find one that satisfies
    # R. NB: We need "or not R(result)" because [] trivially satisfies
    # "AtMost3(red)", etc.
    while result == [] or not R(result):
        # Pick a random box size within the allowable range.
        box_size = random.randrange(box_min, box_max + 1)

        # Reinitialize result, and fill with box_size random objects.
        result = []
        while len(result) < box_size:
            random_object = gen_random_object()
            result.append(random_object)

    # Return result.
    return result

def gen_random_group(R):
    """Return a random group of boxes that all satisfy rule R."""
    # Pick a random group size within the allowable range.
    grp_size = random.randrange(grp_min, grp_max + 1)

    # Initialize empty result.
    result = []

    # Fill result with grp_size random boxes that all satisfy R.
    while len(result) < grp_size:
        random_box = gen_random_box(R)
        result.append(random_box)

    # Return result.
    return result

def gen_random_item(Q1, Q2, condition):
    """Return a random item based on two quantifiers and a condition."""
    # Pick a random property and then a random value of that property. Form a
    # predicate (of objects) P from that value.
    random_property = random.choice([shapes, colors])
    random_value = random.choice(random_property)
    P = lambda x: random_value in x.values()
    # NB: We could also manually set P = lambda x: "red" in x.values().

    # Create two rules based on Q1, Q2, and P.
    R1 = lambda B: Q1(P)(B)
    R2 = lambda B: Q2(P)(B)

    # Generate two random "yes" and "no" groups based on R1, R2.
    group_y = gen_random_group(lambda B: R1(B) and R2(B))
    group_n = gen_random_group(lambda B: not R1(B) and not R2(B))
    # NB: If we want the two groups to have the same number of boxes, we need to
    # embed this in a "while len(group_y) != len(group_n)" loop.

    # Generate the critical box, based on R1, R2, and condition.
    if condition == "true":
        # Must satisfy R1 and R2, just like boxes in the "yes" group.
        box_crit = gen_random_box(lambda B: R1(B) and R2(B))
    elif condition == "false":
        # Must satisfy neither R1 nor R2, just like boxes in the "no" group.
        box_crit = gen_random_box(lambda B: not R1(B) and not R2(B))
    else: # condition == "target"
        # Must satisfy exactly one of the two rules.
        box_crit = gen_random_box(lambda B: (R1(B) and not R2(B)) or (R2(B) and not R1(B)))

    # Return an item based on these values.
    return create_item(Q1, Q2, random_value, condition, group_y, group_n, box_crit)

def obj2html(obj):
    """Convert an object to HTML."""
    return '<span class="object" style="color: ' + obj["color"] + '">' + obj["shape"] + '</span>'

def box2html(box, P, horizontal=False):
    """Convert a box of objects to HTML, with those satisfying P sorted first."""
    if horizontal:
        result = '<div class="horizontal-box">\n'
    else:
        result = '<div class="vertical-box">\n'

    result += '<ul>\n'
    for obj in sorted(box, key=lambda k: P in k.values(), reverse=True):
        result += '<li>' + obj2html(obj) + '</li>\n'
    result += '</ul>\n'
    result += '</div>'

    return result

def grp2html(grp, P):
    """Convert a group of boxes to HTML."""
    result = ''
    for box in grp:
        result += box2html(box, P) + '\n'
    result += '<div class="clear-both"></div>'

    return result

def item2html(item):
    """Convert an item to HTML."""
    result = '<html>\n'
    result += '<link rel="stylesheet" href="global_experiment.css">\n'
    result += '<body>\n'
    result += '<!-- Quantifier 1: ' + item["Q1"] + ' -->\n'
    result += '<!-- Quantifier 2: ' + item["Q2"] + ' -->\n'
    result += '<!-- Property: ' + item["P"] + ' -->\n'
    result += '<!-- Condition: ' + item["condition"] + ' -->\n'
    result += '<!-- Number of boxes in yes group: ' + str(len(item["yes group"])) + ' -->\n'
    result += '<!-- Number of boxes in no group: ' + str(len(item["no group"])) + ' -->\n'
    result += '<!-- Number of objects in critical box: ' + str(len(item["critical box"])) + ' -->\n'
    result += '<h1 class="yes-group">Each of these boxes satisfies the rule.</h1>\n'
    result += grp2html(item["yes group"], item["P"]) + '\n'
    result += '<h1 class="no-group">None of these boxes satisfies the rule.</h1>\n'
    result += grp2html(item["no group"], item["P"]) + '\n'
    result += '<h1 class="critical-box">Does this box satisfy the rule?</h1>\n'
    result += box2html(item["critical box"], item["P"], horizontal=True) + '\n'
    result += '</body>\n'
    result += '</html>'

    return result

# Main

if __name__ == "__main__":
    import sys

    Q1 = eval(sys.argv[1])
    Q2 = eval(sys.argv[2])
    condition = sys.argv[3]

    try:
        print(item2html(gen_random_item(Q1, Q2, condition)))
    except:
        print("Something went wrong.")
