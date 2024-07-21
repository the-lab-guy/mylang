from enum import Enum

class Stack:

    def __init__(self, size) -> None:
        self.buf = [0 for _ in range(size)]
        self.sp = -1

    def push (self, number):
        self.sp += 1
        self.buf[self.sp] = number
        #self._show()

    def pop (self):
        number = self.buf[self.sp]
        self.sp -= 1
        #self._show()
        return number

    def top (self):
        return self.buf[self.sp]
    
    def _show (self):
        print(self.buf[0:self.sp+1])

    def _size (self):
        return self.sp+1


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


import math
class Expression:

    ALLOWED_NAMES = {
        k: v for k, v in math.__dict__.items() if not k.startswith("__")
    }

    def tokenise_expression(ops:str, token_list:list) -> list:
        right_side = token_list
        parts_num = len(right_side)
        left_side = []
        skip_part = False

        for i, part in enumerate(right_side):
            if skip_part == True:
                skip_part = False
                continue
            if str(part) in ops:
                print(f"found operator {part}")
                left_op = left_side[-1] #if right_side[i-1].isnumeric() else "ERROR:left"
                operator = part
                right_op = right_side[i+1] #if right_side[i+1].isnumeric() else "ERROR:right"
                expr = Expression(left_op, operator, right_op)
                left_side = left_side[:-1]
                left_side.append(expr)
                skip_part = True
                print(f"left_side={left_side} right_side={right_side[i+2:]}")
            else:
                left_side.append(part)
        
        return left_side
    
    def __init__(self, left, operator, right):
        self.left = left
        self.operator = operator
        self.right = right

    def add_operator(self, operator):
        self.operator = str(operator)
    
    def add_right(self, right):
        self.right = str(right)

    def prepare(self):
        # substitute python ** for exponent ^ symbol
        if self.operator == "^": self.operator = "**"
        if type(self.left) is Expression:
            self.leftvalue = self.left.evaluate()
        else:
            self.leftvalue = self.left
        if type(self.right) is Expression:
            self.rightvalue = self.right.evaluate()
        else:
            self.rightvalue = self.right

        # compile expression
        self.expr = self.leftvalue + self.operator + self.rightvalue
        self.code = compile(self.expr, "<string>", "eval")
        # validate allowed names
        for self.name in self.code.co_names:
            raise NameError(f"The use of '{self.name}' is not allowed")

    def evaluate(self):
        self.prepare()
        result = eval(self.code, {"__builtins__": {}}, Expression.ALLOWED_NAMES)
        #print(result)
        return str(result)


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
    M_OK     = "OK"
    E_DIV0   = "Division by zero"
    E_NOLBL  = "Label name not found"
    E_NOVAR  = "Variable name not found"
    E_OPCOD  = "Unknown instruction"
    E_MISS   = "Missing parameter"
    E_TYPE   = "Incorrect parameter type"
    E_ILLEG  = "Illegal identifier name"
    F_NUMB   = "%lld"
    F_STR    = "%s"


if __name__ == "__main__":
    print("Mylang core objects library")
