from collections import namedtuple

Point = namedtuple('Point', ['x', 'y'])
Rectangle = namedtuple('Rectangle', ['start', 'end', 'area'])

def calculate_area(a, b):
    width = abs(a.x - b.x)
    height = abs(a.y - b.y)

    return width * height

def get_areas(starting_points, ending_points, n):
    rectangles = [
        Rectangle(starting_point, ending_point, area)
        for starting_point, ending_point in zip(starting_points, ending_points)
        if (area := calculate_area(starting_point, ending_point)) > n
    ]
    return sorted(rectangles, key=lambda rectangle: -rectangle.area)

starting_points = [
    Point(2, 3), 
    Point(0, 0), 
    Point(3, 4), 
    Point(5, 6),
    Point(3, 3),
]
ending_points = [
    Point(3, 4), 
    Point(-5, -9), 
    Point(7, 7), 
    Point(5, 6),
    Point(0, 0),
]

expected_result = [
    Rectangle(Point(x=0, y=0), Point(x=-5, y=-9), 45), 
    Rectangle(Point(x=3, y=4), Point(x=7, y=7), 12),
]

assert get_areas(starting_points, ending_points, 9) == expected_result

starting_points_2 = [
    Point(3, 4),
    Point(2, 3),
    Point(5, 6),
    Point(3, 3),
    Point(0, 0),
]
ending_points_2 = [
    Point(7, 7),
    Point(3, 4),
    Point(5, 6),
    Point(0, 0),
    Point(-5, -9),
]

expected_result_2 = [
    Rectangle(Point(x=0, y=0), Point(x=-5, y=-9), 45),
    Rectangle(Point(x=3, y=4), Point(x=7, y=7), 12),
]