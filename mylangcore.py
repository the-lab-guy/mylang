class Stack:

    def __init__(self, size) -> None:
        self.buf = [0 for _ in range(size)]
        self.sp = -1

    def push (self, number):
        self.sp += 1
        self.buf[self.sp] = number

    def pop (self):
        number = self.buf[self.sp]
        self.sp -= 1
        return number

    def top (self):
        return self.buf[self.sp]


class Heap:

    def __init__(self, size) -> None:
        self.buf = {}
        self.count = 0
        self.max_size = size

    def store (self, name, number):
        if self.count < self.max_size:
            self.buf[name] = number
            self.count += 1

    def fetch (self, name):
        number = self.buf[name]
        return number

    def size (self):
        return self.count
