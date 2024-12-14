from math import log2

class Player:
    def __init__(self, name, hp=10, xp=0):
        self.__name = name
        self._hp = hp
        self._xp = xp

    @property
    def level(self):
        return 1 if self._xp < 300 else 2 + log2(int(self._xp / 300))
    
    @property
    def name(self):
        return self.__name
    

    def increment_xp(self, increment):
        self._xp += increment

    
    
    