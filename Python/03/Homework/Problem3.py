from math import sqrt

class Vector3D:
    def __init__(self, x , y, z):
        self._x = x
        self._y = y
        self._z = z

    def __repr__(self):
        return f"Vector3D({self._x}, {self._y}, {self._z})"
    
    def __str__(self):
        return f"({self._x}, {self._y}, {self._z})"
    
    def __add__(self, other):
        if isinstance(other, Vector3D):
            return Vector3D(self._x + other._x, self._y + other._y, self._z + other._z)
        else: 
            return Vector3D(self._x + other, self._y + other, self._z + other)
        
    def __radd__(self, other):
        return self.__add__(other)

    def __iadd__(self, other):
        if isinstance(other, Vector3D):
            self._x += other._x
            self._y += other._y
            self._z += other._z
        else:
            self._x += other
            self._y += other
            self._z += other
        return self
    
    def __mul__(self, other):
        if isinstance(other, Vector3D):
            return Vector3D(self._x * other._x, self._y * other._y, self._z * other._z)
        else: 
            return Vector3D(self._x * other, self._y * other, self._z * other)
        
    
    def __rmul__(self, other):
        return self.__mul__(other)

    def __imul__(self, other):
        if isinstance(other, Vector3D):
            self._x *= other._x
            self._y *= other._y
            self._z *= other._z
        else:
            self._x *= other
            self._y *= other
            self._z *= other
        return self
    
    def __eq__(self, other):
        if isinstance(other, Vector3D):
            return self._x == other._x and self._y == other._y and self._z == other._z
        else:
            return False
    
    def __ne__(self, other):
        return not self == other
    
    def __abs__(self):
        return sqrt(self._x * self._x + self._y * self._y + self._z * self._z)
    
    def __getattr__(self, name):
        if name == "X":
            return self._x
        elif name == "Y":
            return self._y
        elif name == "Z":
            return self._z
        else:
            raise AttributeError(f"'Vector3D' object has no attribute '{name}'")
        
    def __setattr__(self, name, value):
        if name == "X":
            self._x = value
        elif name == "Y":
            self._y = value
        elif name == "Z":
            self._z = value
        else:
            raise AttributeError(f"'Vector3D' object has no attribute '{name}'")

        
    def __iter__(self):
        return iter((self._x, self._y, self._z))