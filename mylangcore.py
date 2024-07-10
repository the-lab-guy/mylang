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
