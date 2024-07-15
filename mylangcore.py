from enum import Enum

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


class Error:
    def __init__(self) -> None:
        pass

    def message (err_key=None, err_line=None, err_opcode=None) -> str:
        _key = err_key
        _line = err_line
        _opcode = err_opcode

        if _key is None:
            return "No error code specified"
        
        if _line is None:
            _line = ""
        
        if _opcode is None:
            _opcode = ""

        return f"ERROR: {_key.value} at [{_line}:{_opcode}]"
    

class Messages(Enum):
    M_OK       = "OK"
    E_DIV0   = "Division by zero"
    E_NOLBL  = "Label name not found"
    E_NOVAR  = "Variable name not found"
    E_OPCOD  = "Unknown instruction"
    E_MISS   = "Missing parameter"
    E_TYPE   = "Incorrect parameter type"
    F_NUMB   = "%lld"
    F_STR    = "%s"

if __name__ == "__main__":
    print("Mylang core objects library")
