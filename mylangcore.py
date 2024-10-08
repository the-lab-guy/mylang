from enum import Enum

class Stack:

    def __init__(self, size) -> None:
        self.buf = [0 for _ in range(size)]
        self.sp = 0

    def push (self, number):
        self.buf[self.sp] = number
        self.sp += 1
        # self._show()

    def pop (self):
        self.sp -= 1
        number = self.buf[self.sp]
        # self._show()
        return number

    def top (self):
        # self._show()
        return self.buf[self.sp]
    
    def _show (self):
        print(f"Size:{self.sp} Data:{self.buf[:self.sp]}")

    def _size (self):
        # self._show()
        return self.sp

    def _set_sp (self, index):
        self.sp = index
        # self._show()


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

    def __init__(self, left, operator, right):
        self.left = left
        self.operator = operator
        self.right = right

    def __str__(self):
        return f"({self.left} {self.operator} {self.right})"
    
    def __repr__(self):
        self.left_repr = repr(self.left) if type(self.left) is Expression else self.left
        self.operator_repr = repr(self.operator) if type(self.operator) is Expression else self.operator
        self.right_repr = repr(self.right) if type(self.right) is Expression else self.right

        return f"{self.left_repr} {self.right_repr} {self.operator_repr} "

    def add_operator(self, operator):
        self.operator = str(operator)
    
    def add_right(self, right):
        self.right = str(right)

    def prepare(self):
        if type(self.left) is Expression:
            self.leftvalue = self.left.evaluate()
        else:
            self.leftvalue = self.left
        if type(self.right) is Expression:
            self.rightvalue = self.right.evaluate()
        else:
            self.rightvalue = self.right

        # substitute python 'pow()' function for exponent ^ symbol
        if self.operator == "^":
            self.expr = f"pow({self.leftvalue},{self.rightvalue})"
        else:
            self.expr = self.leftvalue + self.operator + self.rightvalue
        # compile expression for evaluation
        self.code = compile(self.expr, "<string>", "eval")
        # validate allowed names
        for self.name in self.code.co_names:
            if self.name != 'pow':
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
    

class Parser:
    def __init__(self) -> None:
        pass

    def unary_minus(token_list:list) -> list:
        right_side = token_list
        left_side = []
        ops = '-'

        print(f"uminus received right_side token_list: {right_side}")
        skip_part = False
        it = enumerate(right_side)
        for i, part in it:
            if skip_part == True:
                skip_part = False
                continue
            if str(part) == ops and (len(left_side) == 0 or str(left_side[-1]) in "^*/+-"):
                print(f"uminus found operator {part} at {i}")
                print(f"left_side={left_side}")
                print(f"Type of right_side {type(right_side[i+1])}")
                if isinstance(right_side[i+1], Expression):
                    left_side.append('-1')
                    left_side.append('*')
                    print(f"left_side={left_side}")

            else:
                left_side.append(part)
                print(f"left_side={left_side}")
        
        return left_side


    def tokenise_expression(ops:str, token_list:list) -> list:
        right_side = token_list
        left_side = []
        skip_part = False

        for i, part in enumerate(right_side):
            if skip_part == True:
                skip_part = False
                continue
            if str(part) in ops:
                print(f"tokenise found operator {part}")
                left_op = left_side[-1]
                operator = part
                right_op = right_side[i+1]
                expr = Expression(left_op, operator, right_op)
                left_side = left_side[:-1]
                left_side.append(expr)
                skip_part = True
                print(f"left_side={left_side} right_side={right_side[i+2:]}")
            else:
                left_side.append(part)
        
        return left_side
    
    def parenthesise_expression(openers:str, closers:str, token_list:list) -> list:
        right_side = token_list
        left_side = []
        ops = openers + closers

        print(f"parenth received right_side token_list: {right_side}")
        skip = 0
        it = enumerate(right_side)
        for i, part in it:
            if str(part) in ops:
                print(f"parenth found operator {part} at {i}")
                if part in openers:
                    left_op = left_side
                    operator, skip = Parser.parenthesise_expression(openers, closers, right_side[i+1:])
                    right_op = right_side[i+skip+1:]
                    print(f"left_op={left_op} operator={operator} right_op={right_op}")
                    expr = Parser.unary_minus(operator)
                    expr = Parser.tokenise_expression('^', expr)
                    expr = Parser.tokenise_expression('*/', expr)
                    expr = Parser.tokenise_expression('+-', expr)
                    # join the two lists together
                    left_side = left_side + expr
                    print(f"Skipping {skip}+1")
                    [next(it, None) for _ in range(skip+1)]
                elif part in closers:
                    operator = left_side
                    print(f"operation={operator}")
                    print(f"i={i} : skip={skip}")
                    return operator, i
            else:
                left_side.append(part)
        
        return left_side


    # check if a simple expression
    def parse_expression(expr:list):   # returns an Expression object
        # do brackets
        openers = "("
        closers = ")"
        expr = Parser.parenthesise_expression(openers, closers, expr)

        # do unary minus
        expr = Parser.unary_minus(expr)

        # do exponents
        expr = Parser.tokenise_expression('^', expr)

        # do multiply and divide
        expr = Parser.tokenise_expression('*/', expr)

        # do add and subtract
        expr = Parser.tokenise_expression('+-', expr)

        if isinstance(expr[0], Expression):
            return expr[0]
        else:
            return None


class Messages(Enum):
    M_OK     = "OK"
    E_DIV0   = "Division by zero"
    E_NOLBL  = "Label name not found"
    E_NOVAR  = "Variable name not found"
    E_OPCOD  = "Unknown instruction"
    E_MISS   = "Missing parameter"
    E_TYPE   = "Incorrect parameter type"
    E_ILLEG  = "Illegal identifier name"
    E_EXPR   = "Invalid expression syntax"
    E_ELSE   = "Mismatched IF...ELSE"
    E_THEN   = "Mismatched IF...THEN"
    F_NUMB   = "%lld"
    F_STR    = "%s"
    F_FLOAT  = "%f"


class Check():
    def is_float(item: any) -> bool:
        if item is None:
            return False
        try:
            float(item)
            return True
        except ValueError:
            return False

    def is_integer(item: any) -> bool:
        if item is None:
            return False
        try:
            int(item)
            return True
        except ValueError:
            return False




if __name__ == "__main__":
    print("Mylang core objects library")
