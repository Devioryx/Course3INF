class Counter:
    def __init__(self, initial=0, step=1):
        self._initial = initial
        self._step = step
        self._counter = initial

    def increment(self):
        self._counter += self._step
    
    @property
    def total(self):
        return self._counter
    
    @property
    def step(self):
        return self._step
    
class TwoWayCounter(Counter):
    def __init__(self, initial=0, step=1):
        super().__init__(initial, step)

    def decrement(self):
        self._counter -= self._step
    
class LimitedCounter(Counter):
    def __init__(self, max, initial=0, step=1):
        super().__init__(initial, step)
        self._max = max

    def increment(self):
        if self.total < self._max:
            super().increment()

    @property
    def get_max(self):
        return self._max

class LimitedTwoWayCounter(TwoWayCounter, LimitedCounter):
    def __init__(self, min, max, initial=0, step=1):
        super().__init__(initial, step)
        super(TwoWayCounter, self).__init__(max, initial, step)

        self._min = min

    def increment(self):
        super(TwoWayCounter, self).increment()
    
    def decrement(self):
        if self.total > self._min:
            super().decrement()

    @property    
    def get_min(self):
        return self._min
    

class Sophomore(LimitedTwoWayCounter):
    def __init__(self, is_available=False):
        super().__init__(0, 1, 1 if is_available else 0 , 1)
        
    @property
    def is_available(self):
        return self.total > 0

    def wait(self):
        super().decrement()

        
    def signal(self):
        super.increment()      



c = LimitedTwoWayCounter(0, 10, 0, 1)
c.increment()
c.increment()
print(c.total)
c.increment()
print(c.total)
c.decrement()
print(c.total)

