import mylangcore as core
import textwrap


def isnamechar(char:chr) -> bool:
    return char.isalnum() or char in "_:"

def iswhitespace(char:chr) -> bool:
    return char in " \t\n"

def issinglesymbol(char:chr) -> bool:
    return char in "()*/^"

def ispairsymbol(chars:str) -> bool:
    if len(chars) == 0: return False
    if chars[0] in "+-=@#$":
        return True
    return False


def isseparator(char:chr) -> bool:
    return not isnamechar(char) and char not in "."
    #return (iswhitespace(char) or issymbol(char))


########################
#   Lexical Analysis
########################

def lexer(text_line:str="") -> list:

    # walk the single line of source text and scan for lexemes
    lexemes = []
    lexeme = ""
    in_quoted_string = False

    source = text_line.lstrip()   # remove leading spaces

    for index in range(len(source)):
        char = source[index]
        
        if char == '"':    # double quote chr(34)
            if in_quoted_string == False:
                in_quoted_string = True
                lexeme = char
                continue
            else:
                in_quoted_string = False
                lexeme = lexeme + char
                lexemes.append(lexeme[1:-1])    # remove quotes
                lexeme = ""
                continue

        if in_quoted_string == True:
            lexeme = lexeme + char
        elif (iswhitespace(char)):
        # skip multiple space chars
            if len(lexeme) > 0:
                lexemes.append(lexeme)
                lexeme = ""
            print(f"[{lexeme}] - was a space '{char}'")
        elif issinglesymbol(char):
            if len(lexeme) > 0:
                lexemes.append(lexeme)
                lexeme = ""
            lexeme = char
            lexemes.append(lexeme)
            lexeme = ""
            print(f"[{lexeme}] - symbol '{char}'")
        elif (ispairsymbol(char)):
            if len(lexeme) == 1:   # first symbol already caught
                if ispairsymbol(lexeme):
                    lexeme = lexeme + char    # add second symbol
                    lexemes.append(lexeme)    # save pair
                lexeme = ""
            else:
                if len(lexeme) > 0:   # first valid multi symbol
                    lexemes.append(lexeme)    # save previous lexeme
                    lexeme = ""
                lexeme = char

            print(f"[{lexeme}] - multi symbol '{char}'")
        elif (isnamechar(char)):
            if len(lexeme) > 0:
                if isseparator(lexeme[-1]):
                    print(f"found separator [{lexeme[-1]}]")
                    lexemes.append(lexeme)
                    lexeme = ""
            lexeme = lexeme + char
            print(f"[{lexeme}] - normal '{char}'")
        else:   # might be first of pair or illegal character
            if len(lexeme) > 0:
                lexemes.append(lexeme)
            lexeme = char
            print(f"[{lexeme}] - separator '{char}' - [{lexemes}]")

    # save last lexeme if any chars collected
    if len(lexeme) > 0:
        lexemes.append(lexeme)
    print(f"[{lexemes}] [{len(lexemes)}] - final lexeme list")
    #quit()
    return lexemes


########################
#   Tokenise Program
########################

def tokenise(program_filepath=None):

    # read file lines
    program_lines = []
    with open(program_filepath, "r") as program_file:
        program_lines = [line.strip() for line in program_file.readlines()]

    program = []
    token_counter = 0
    label_tracker = {}
    error_count = 0

    for index, line in enumerate(program_lines):
        line_number = index + 1
        #parts = line.split(" ")
        #print(lexer(line))
        if line == "":
            continue

        parts = lexer(line)
        print(f"line='{line}'")
        print(f"parts={parts}")
        opcode = str(parts[0])

        # check for empty line
        if opcode == "":
            continue

        # check if its a label
        if opcode.endswith(":"):
            label_tracker[opcode[:-1]] = token_counter
            if not opcode[:-1].isidentifier():
                warning_msg = core.Error.message(core.Messages.E_ILLEG, token_counter, opcode)
                print(f"{warning_msg} in line: {line_number} {line}")
                error_count += 1
            program.append(opcode)
            token_counter += 1
            continue
           
        # check if the line is a simple expression to be evaluated
        expr = core.Expression.parse_expression(parts)
        if not (expr is None):
            #print(f"Evaluation returned: {expr.evaluate()}")
            program.append(expr)
            token_counter += 1
            continue

        # store opcode token
        program.append(opcode)
        token_counter += 1

        try:
            # handle each opcode
            if opcode == "PUSH":
                if len(parts) > 2:
                    operand = core.Expression.parse_expression(parts[1:])
                    if operand is None:
                        warning_msg = core.Error.message(core.Messages.E_EXPR, token_counter, str(operand))
                        print(f"{warning_msg} in line: {line_number} {line}")
                        error_count += 1
                else:
                    operand = str(parts[1])
                    if not operand.isnumeric() and not operand.isidentifier():
                        warning_msg = core.Error.message(core.Messages.E_ILLEG, token_counter, operand)
                        print(f"{warning_msg} in line: {line_number} {line}")
                        error_count += 1
                program.append(operand)
                token_counter += 1
            elif opcode == "POP":
                variable_name = str(parts[1])
                program.append(variable_name)
                token_counter += 1
                if not variable_name.isidentifier():
                    warning_msg = core.Error.message(core.Messages.E_ILLEG, token_counter, variable_name)
                    print(f"{warning_msg} in line: {line_number} {line}")
                    error_count += 1
            elif opcode == "PRINT":
                # parse string literal
                if len(parts) > 1:
                    string_literal = ' '.join(parts[1:])
                else:
                    string_literal = "\\n"
                program.append(string_literal)
                token_counter += 1

            elif opcode == "JUMP":
                condition = ''.join(parts[1:3])
                label = parts[3]
                program.append(condition)
                token_counter += 1
                if condition == ".EQ.0":
                    program.append(label)
                    token_counter += 1
                elif condition == ".GT.0":
                    program.append(label)
                    token_counter += 1
                elif condition == ".LT.0":
                    program.append(label)
                    token_counter += 1
                elif condition == ".NE.0":
                    program.append(label)
                    token_counter += 1

        except IndexError:
            warning_msg = core.Error.message(core.Messages.E_MISS, token_counter, opcode)
            print(f"{warning_msg} in line: {line_number} {line}")
            error_count += 1
            continue    # skip to next line

    return error_count, program, label_tracker


#########################
#   Interpret Program
#########################

def interpret(program=[], label_tracker={}) -> str:

    pc = 0
    stack = core.Stack(256)
    heap = core.Heap(256)

    while program[pc] != "HALT":
        opcode = program[pc]
        pc += 1

        try:
            # handle Expression first because its not a string
            if isinstance(opcode, core.Expression):
                stack.push(float(opcode.evaluate()))
                continue
            # skip if its a label
            elif opcode.endswith(":"):
                continue
            elif opcode == "PUSH":
                operand = str(program[pc])
                if operand.isnumeric():
                    number = int(program[pc])
                    pc += 1
                    stack.push(number)
                elif operand.isidentifier():
                    variable_name = program[pc]
                    pc += 1
                    stack.push(heap.fetch(variable_name))
                elif isinstance(program[pc], core.Expression):
                    expression = program[pc]
                    pc += 1
                    stack.push(float(expression.evaluate()))
                else:
                    return core.Error.message(core.Messages.E_ILLEG, pc-1, opcode)
            elif opcode == "POP":
                variable_name = str(program[pc])
                if variable_name.isidentifier():
                    pc += 1
                    a = stack.pop()
                    heap.store(variable_name, a)
                else:
                    return core.Error.message(core.Messages.E_ILLEG, pc-1, opcode)
            elif opcode == "JUMP":
                condition = str(program[pc])
                pc += 1
                number = stack.top()
                if condition == ".EQ.0":
                    if number == 0:
                        pc = label_tracker[program[pc]]
                    else:
                        pc += 1
                elif condition == ".GT.0":
                    if number > 0:
                        pc = label_tracker[program[pc]]
                    else:
                        pc += 1
                elif condition == ".LT.0":
                    if number < 0:
                        pc = label_tracker[program[pc]]
                    else:
                        pc += 1
                elif condition == ".NE.0":
                    if number != 0:
                        pc = label_tracker[program[pc]]
                    else:
                        pc += 1
            elif opcode == "ADD":
                a = stack.pop()
                b = stack.pop()
                stack.push(a+b)
            elif opcode == "SUB":
                a = stack.pop()
                b = stack.pop()
                stack.push(b-a)
            elif opcode == "MUL":
                a = stack.pop()
                b = stack.pop()
                stack.push(a*b)
            elif opcode == "DIV":
                a = stack.pop()
                if a == 0:
                    return core.Error.message(core.Messages.E_DIV0, pc-1, opcode)
                b = stack.pop()
                stack.push(b//a)
            elif opcode == "DUP":
                stack.push(stack.top())
            elif opcode == "SWAP":
                a = stack.pop()
                b = stack.pop()
                stack.push(a)
                stack.push(b)
            elif opcode == "FLOAT":
                a = stack.pop()
                stack.push(float(a))
            elif opcode == "FLOOR":
                a = stack.pop()
                stack.push(int(a))
            elif opcode == "PRINT":
                string_literal = program[pc]\
                    .replace('\\n', '\n').replace('\\t', '\t')
                pc += 1
                if "@#" in string_literal:
                    a = stack.pop()
                    string_literal = string_literal.replace("@#", str(a))
                elif "@$" in string_literal:
                    a = stack.pop()
                    string_literal = string_literal.replace("@$", str(a))
                print(string_literal, end='')
            elif opcode == "READ":
                number = int(input())
                stack.push(number)
            else:
                return core.Error.message(core.Messages.E_OPCOD, pc-1, opcode)

        except (KeyError):
            message = core.Messages.E_NOVAR if opcode in ['POP', 'PUSH'] \
                else core.Messages.E_NOLBL
            opcode = opcode + ' ' + program[pc]
            return core.Error.message(message, pc-1, opcode)

    return "OK"


###############################
#   Pre-compiler Operations
###############################

def precompile(program):
    string_literals = []
    for ip in range(len(program)):
        if program[ip] == "PRINT":
            string_literal = program[ip+1]
            string_literal = string_literal.replace("@#", core.Messages.F_NUMB.value)
            string_literal = string_literal.replace("@$", core.Messages.F_FLOAT.value)
            program[ip+1] = len(string_literals)
            string_literals.append(string_literal)

    variable_names = set()
    for ip in range(len(program)):
        if program[ip] == "POP":
            variable_name = program[ip+1]
            variable_names.add(variable_name)


    return string_literals, variable_names

#######################
#   Compile Program
#######################

def compile(program_filepath=None, program=[], string_literals=[],
            variable_names=[]) -> str:

    asm_filepath = program_filepath.split('.')[0] + ".asm"
    out = open(asm_filepath, "w")

    # trailing slash on next line suppresses first blank
    out.write(textwrap.dedent("""\
    ; -- header --
    bits 64
    default rel
    """))

    out.write(textwrap.dedent("""
    ; -- variables --
    section .bss
    read_buffer resq 1  ; 64-bits integer = 8 bytes
    """))

    for variable_name in variable_names:
        out.write(f"{variable_name} resq 1  ; 64-bits integer = 8 bytes\n")

    # "%lld" returns a 64-bit unsigned integer
    out.write(textwrap.dedent("""
    ; -- constants --
    section .data
    """))

    # language runtime messages
    for message in core.Messages:
        out.write(f"{message.name:7} db \"{message.value}\", 0\n")

    out.write("\n")  # insert a blank line

    # source program literal strings
    for i, string_literal in enumerate(string_literals):
        out.write(f"string_literal_{i} db `{string_literal}`, 0\n")

    out.write(textwrap.dedent("""
    ; -- Entry Point --
    section .text
    global main
    extern ExitProcess
    extern printf
    extern scanf
    extern pow
            
    main:
    \tPUSH rbp
    \tMOV rbp, rsp
    """))

    ip = 0
    while ip < len(program):
        opcode = program[ip]
        ip += 1

        # handle Expression first because its not a string
        if isinstance(opcode, core.Expression):
            out.write(f"; -- Expression --\n")
            rpn_to_x64(opcode, out)
            continue    # get next program opcode

        if opcode.endswith(":"):
            out.write(f"; -- Label --\n")
            out.write(f"{opcode}\n")
        elif opcode == "PUSH":
            operand = program[ip]
            ip += 1
            
            # handle Expression first because its not a string
            if isinstance(operand, core.Expression):
                out.write(f"; -- {opcode} expr --\n")
                rpn_to_x64(operand, out)
            # number or variable?
            elif operand not in variable_names:
                out.write(f"; -- {opcode} num --\n")
                out.write(f"\tPUSH {operand}\n")
            else:
                out.write(f"; -- {opcode} var --\n")
                out.write(f"\tLEA rcx, {operand}\n")
                out.write(f"\tMOV rax, [rcx]\n")
                out.write(f"\tPUSH rax\n")
        elif opcode == "POP":
            variable_name = program[ip]
            ip += 1
            out.write(f"; -- {opcode} --\n")
            out.write(f"\tLEA rcx, {variable_name}\n")
            out.write(f"\tPOP rax\n")
            out.write(f"\tMOV [rcx], rax\n")
        elif opcode == "DUP":
            out.write(f"; -- {opcode} --\n")
            out.write(f"\tPOP rax\n")
            out.write(f"\tPUSH rax\n")
            out.write(f"\tPUSH rax\n")
        elif opcode == "SWAP":
            out.write(f"; -- {opcode} --\n")
            out.write(f"\tPOP rax\n")
            out.write(f"\tPOP rcx\n")
            out.write(f"\tPUSH rax\n")
            out.write(f"\tPUSH rcx\n")
        elif opcode == "ADD":
            out.write(f"; -- {opcode} --\n")
            out.write(f"\tPOP rax\n")
            out.write(f"\tADD qword [rsp], rax\n")
        elif opcode == "SUB":
            out.write(f"; -- {opcode} --\n")
            out.write(f"\tPOP rax\n")
            out.write(f"\tSUB qword [rsp], rax\n")
        elif opcode == "MUL":
            out.write(f"; -- {opcode} --\n")
            out.write(f"\tPOP rax\n")
            out.write(f"\tPOP rdx\n")
            out.write(f"\tIMUL rdx, rax\n")
            out.write(f"\tPUSH rdx\n")
        elif opcode == "DIV":
            out.write(f"; -- {opcode} --\n")
            out.write(f"\tPOP rcx\n")
            out.write(f"\tTEST rcx, rcx\n")
            out.write(f"\tJZ ERROR_LABEL\n")
            out.write(f"\tPOP rax\n")
            out.write(f"\tCQO  ; sign extend rax into rdx:rax\n")
            out.write(f"\tIDIV rcx\n")
            out.write(f"\tPUSH rax\n")
        elif opcode == "FLOAT":
            out.write(f"; -- {opcode} --\n")
            out.write(f"\tPOP rax\n")
            out.write(f"\tPXOR xmm0, xmm0\n")
            out.write(f"\tCVTSI2SD xmm0, rax\n")
            out.write(f"\tMOVQ rax, xmm0\n")
            out.write(f"\tPUSH rax\n")
        elif opcode == "FLOOR":
            out.write(f"; -- {opcode} --\n")
            out.write(f"\tPOP rax\n")
            out.write(f"\tPXOR xmm0, xmm0\n")
            out.write(f"\tMOVQ xmm0, rax\n")
            out.write(f"\tCVTTSD2SI rax, xmm0\n")
            out.write(f"\tPUSH rax\n")

        elif opcode == "PRINT":
            string_literal_index = program[ip]
            ip += 1
            out.write(f"; -- {opcode} --\n")
            if core.Messages.F_NUMB.value in string_literals[string_literal_index]:
                out.write(f"\tPOP rdx\n")  # value to print
            elif core.Messages.F_FLOAT.value in string_literals[string_literal_index]:
                out.write(f"\tPOP rdx\n")  # value to print

            out.write(f"\tSUB rsp, 40\n")
            out.write(f"\tLEA rcx, string_literal_{string_literal_index}\n")
            out.write(f"\tXOR eax, eax\n")
            out.write(f"\tCALL printf\n")
            out.write(f"\tADD rsp, 40\n")

        elif opcode == "READ":
            out.write(f"; -- {opcode} --\n")
            out.write(f"\tSUB rsp, 40\n")
            out.write(f"\tLEA rcx, F_NUMB\n")  # format string
            out.write(f"\tLEA rdx, read_buffer\n")
            out.write(f"\tXOR eax, eax\n")
            out.write(f"\tMOV [rdx], eax\n")
            out.write(f"\tCALL scanf\n")
            out.write(f"\tADD rsp, 40\n")
            out.write(f"\tPUSH qword [read_buffer]\n")

        elif opcode == "JUMP.EQ.0":
            label = program[ip]
            ip += 1

            out.write(f"; -- {opcode} --\n")
            out.write(f"\tCMP qword [rsp], 0\n")
            out.write(f"\tJE {label}\n")
        elif opcode == "JUMP.GT.0":
            label = program[ip]
            ip += 1

            out.write(f"; -- {opcode} --\n")
            out.write(f"\tCMP qword [rsp], 0\n")
            out.write(f"\tJG {label}\n")
        elif opcode == "JUMP.LT.0":
            label = program[ip]
            ip += 1

            out.write(f"; -- {opcode} --\n")
            out.write(f"\tCMP qword [rsp], 0\n")
            out.write(f"\tJL {label}\n")
        elif opcode == "JUMP.NE.0":
            label = program[ip]
            ip += 1

            out.write(f"; -- {opcode} --\n")
            out.write(f"\tCMP qword [rsp], 0\n")
            out.write(f"\tJNE {label}\n")
        elif opcode == "HALT":
            out.write(f"; -- {opcode} --\n")
            out.write(f"\tJMP EXIT_LABEL\n")
        else:
            return f"{core.Error.message(core.Messages.E_OPCOD, ip-1, opcode)} in {program_filepath}", ""


    out.write(f"ERROR_LABEL:\n")
    out.write(f"; -- ERROR --\n")
    out.write(f"\tSUB rsp, 40\n")
    out.write(f"\tLEA rcx, E_DIV0\n")
    out.write(f"\tXOR eax, eax\n")
    out.write(f"\tCALL printf\n")
    out.write(f"\tADD rsp, 40\n")
    
    out.write(f"EXIT_LABEL:\n")
    out.write(f"\tXOR rax, rax\n")
    out.write(f"\tCALL ExitProcess\n")

    out.close()
    return "OK", asm_filepath


def rpn_to_x64(opcode:core.Expression, out) -> None:
    out.write(f"; {opcode}\n")
    out.write(f"; {repr(opcode)}\n")
    params = repr(opcode).split()
    # test assembly output
    for param in params:
        if core.Check.is_float(param):
            out.write(f"\tMOV rax, __?float64?__({float(param)})\n")
            out.write(f"\tPUSH rax\n")
            continue

        if param in ("^*/+-"):
            out.write(f"\tPOP rax\n")
            out.write(f"\tMOVQ xmm1, rax\n")
            out.write(f"\tPOP rax\n")
            out.write(f"\tMOVQ xmm0, rax\n")

        if param == '+':
            out.write(f"\tADDSD xmm0, xmm1\n")
        elif param == '-':
            out.write(f"\tSUBSD xmm0, xmm1\n")
        elif param == '*':
            out.write(f"\tMULSD xmm0, xmm1\n")
        elif param == '/':
            out.write(f"\tDIVSD xmm0, xmm1\n")
        elif param == '^':
            out.write(f"\tSUB rsp, 40\n")
            out.write(f"\tcall pow\n")
            out.write(f"\tADD rsp, 40\n")

        out.write(f"\tMOVQ rax, xmm0\n")
        out.write(f"\tPUSH rax\n")

if __name__ == "__main__":
    print("Mylang tools library")
