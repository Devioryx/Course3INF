class Person:
    def __init__(self, name, age):
        self._name = name
        self._age = age

    
    @property
    def name(self):
        return self._name
    
    @property
    def age(self):
        return self._age

    def __repr__(self):
        return f"Person({self._name},{self._age})"
    
    def __str__(self):
        return f"{self._name.capitalize()} ({self._age})"
    
    def __gt__(self, other):
        return self._age > other._age
    
class FamilyTree:
    def __init__(self, root, children=None):
        self._root = root
        if children is None:
            self._children = []
        else:
            self._children = children

    @property
    def root(self):
        return self._root

    @property
    def children(self):
        return self._children

    def add_child_tree(self, child_tree):
        self._children.append(child_tree)

    def count_descendants(self):
        count = len(self._children)
        for child in self._children:
            count += child.count_descendants()

        return count

    def __str__(self):
        return self.recursive_helper()

    def recursive_helper(self, level = 0):
        result = "    " * level + f"> {self._root}\n"
        for child in self._children:
            result += child.recursive_helper(level + 1)
        return result


# tests

# Create dummies of class Person
john = Person("John", 50)
emily = Person("Emily", 30)
jake = Person("Jake", 18)
dan = Person("Dan", 3)
fiona = Person("Fiona", 7)

# Create family trees for each person
john_familiy_tree = FamilyTree(john)
emily_familiy_tree = FamilyTree(emily)
jake_familiy_tree = FamilyTree(jake)
dan_familiy_tree = FamilyTree(dan)
fiona_familiy_tree = FamilyTree(fiona)

# ---- Testing add_child_tree functionality ----

# Add children to John
john_familiy_tree.add_child_tree(jake_familiy_tree)
john_familiy_tree.add_child_tree(emily_familiy_tree)

# Add children to Emily
emily_familiy_tree.add_child_tree(dan_familiy_tree)
emily_familiy_tree.add_child_tree(fiona_familiy_tree)

assert john_familiy_tree.children[1] == emily_familiy_tree
assert john_familiy_tree.children[0] == jake_familiy_tree
assert emily_familiy_tree.children[0] == dan_familiy_tree
assert emily_familiy_tree.children[1] == fiona_familiy_tree

# ---- Testing __init__ functionality ----

assert john.name == "John"
assert john.age == 50


assert jake.name == "Jake"
assert jake.age == 18


assert john_familiy_tree.root == john
assert len(john_familiy_tree.children) == 2

assert jake_familiy_tree.root == jake
assert len(jake_familiy_tree.children) == 0

assert emily_familiy_tree.root == emily
assert dan_familiy_tree.root == dan
assert fiona_familiy_tree.root == fiona


# ---- Testing __str__functionality ----
expected_repr = "> John (50)\n    > Jake (18)\n    > Emily (30)\n        > Dan (3)\n        > Fiona (7)\n"
assert str(john_familiy_tree) == expected_repr

# # ---- Testing __gt__functionality ---- 
assert john > emily
assert john > jake
assert emily > jake
assert jake > dan

# # ---- Testing __gt__functionality ---- 
assert john_familiy_tree.count_descendants() == 4
assert jake_familiy_tree.count_descendants() == 0
assert emily_familiy_tree.count_descendants() == 2