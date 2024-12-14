from math import sqrt, sin, cos, atan  # you will need these
from math import pi, isclose  # the tests below will need these


class PolarCoordinate:
    def __init__(self, radius, angle):
        self.__radius = radius
        self.__angle = angle

    @property
    def r(self):
        return self.__radius
    
    @property
    def angle(self):
        return self.__angle
    
    def to_cartesian(self):
        return (self.r * cos(self.angle), self.r * sin(self.angle) )
    
    @classmethod
    def from_cartesian(cls, x, y):
        radius = sqrt(x*x + y*y)
        angle = atan(y/x)
        return cls(radius, angle)
    
    def __repr__(self):
        return f"PolarCoordinate({self.r}, {self.angle})"
    
    def __str__(self):
        return f"(r: {self.r}, angle: {self.angle})"

    def __hash__(self):
        return hash((self.r, self.angle))

    def __eq__(self, other):
        return self.r == other.r and self.angle == other.angle

    def __neq__(self, other):
        return not self == other   
