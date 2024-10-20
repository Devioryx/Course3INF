from abc import ABC, abstractmethod

class Shape(ABC):
    def __init__(self, color):
        self._color = color

    @property
    def color(self):
        return self._color
    
    @abstractmethod
    def area(self):
        pass   



class Rectangle(Shape):
    def __init__(self, length, width, color):
        super().__init__(color)

        self._length = length
        self._width = width

    @property
    def length(self):
        return self._length
    
    @property
    def width(self):
        return self._width
    
    def area(self):
        return self._width * self._length
    
class Circle(Shape):
    def __init__(self, radius, color):
        super().__init__(color)

        self._radius = radius

    @property
    def radius(self):
        return self._radius    
     
    def area(self):
        return 3.14 * self._radius ** 2
    

class Shapes:
    def __init__(self, shapes=None):
        self._shapes = [] if shapes is None else shapes

    def add_circle(self, shape):
        self._shapes.append(shape)

    def add_rectangle(self, shape):
        self._shapes.append(shape)

    def rectangle_area_sum(self):
        total_sum = 0
        for shape in self._shapes:
            total_sum += shape.area() if isinstance(shape, Rectangle) else 0

        return total_sum
    
    def circle_area_sum(self):
        total_sum = 0
        for shape in self._shapes:
            total_sum += shape.area() if isinstance(shape, Circle) else 0

        return total_sum
    
    def __getitem__(self, index):
        return self._shapes[index]
    
rec1 = Rectangle(10, 20, "Blue")
rec2 = Rectangle(5, 5, "Black")
circl1 = Circle(3, "Red")
circl2 = Circle(4, "Green")

shapes = Shapes()
shapes.add_circle(circl1)
shapes.add_circle(circl2)
shapes.add_rectangle(rec1)
shapes.add_rectangle(rec2)


print(shapes.rectangle_area_sum())
print(shapes[3].width)

