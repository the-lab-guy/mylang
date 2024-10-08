import mylangcore as core
import textwrap
from pathlib import Path

def isnamechar(char:chr) -> bool:
    return char.isalnum() or char in "_:"

def iswhitespace(char:chr) -> bool:
    return char in " \t\n"

def isunarysymbol(char:chr) -> bool:
    return char in "-"

def isoperator(char:chr) -> bool:
    return False if len(char) == 0 else char in "()^*/+-=@#$"

def isoperatorpair(chars:str) -> bool:
    if len(chars) == 0: return False
    operators = ['++', '--', '==', '@#', '@$']
    if any(chars == s for s in operators):
        return True
    return False

def isseparator(char:chr) -> bool:
    return not isnamechar(char) and char not in "."

def isdecimalnumber(string:str) -> bool:
    string = string.lower()
    string = string.replace('-', '0').replace('.', '0').replace('e', '0')
    return string.isdecimal()

def ishexnumber(string:str) -> bool:
    string = string.lower()
    for char in "xabcdef":
        string = string.replace(char, '0')
    return string.isdecimal()

def isanumber(string:str) -> bool:
    if isdecimalnumber(string):
        return True
    elif ishexnumber(string):
        return True
    else:
        return False



########################
#   Lexical Analysis
########################

def lexer(text_line:str="") -> list:

    # walk the single line of source text and scan for lexemes
    lexemes = []
    lexeme = ""
    in_quoted_string = False
    in_literal_number = False

    source = text_line.lstrip()   # remove leading spaces
    prev_char = ""
    next_char = ""
    char = " "   # default start with supposed whitespace
    line_len = len(source)

    for index in range(len(source)):
        prev_char = char
        char = source[index]
        next_char = source[index+1] if line_len > (index+1) else " "
        
        if char == '"':    # double quote chr(34)
            if not in_quoted_string:
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
            continue

        if in_literal_number:
            if char.isdigit() or char in ".e":
                lexeme = lexeme + char
                continue
            else:
                in_literal_number = False

        if (iswhitespace(char)):
        # skip multiple space chars
            if len(lexeme) > 0:
                lexemes.append(lexeme)
                lexeme = ""
            print(f"[{lexeme}] - was a space '{char}'")
            continue

        elif (isoperator(char)):
            if isoperatorpair(prev_char + char):      # first symbol already caught?
                lexeme = lexeme + char    # add second multi symbol
                print(f"[{lexeme}] - operator pair '{char}'")
                lexemes.append(lexeme)    # save pair
                lexeme = ""
            else:
                if (isunarysymbol(char)):
                    #breakpoint()
                    if prev_char in " ^*/+-(" and next_char.isdigit():
                        in_literal_number = True
                if len(lexeme) > 0:            # previous is unrelated
                    lexemes.append(lexeme)   # save previous lexeme
                lexeme = char            # keep first valid multi symbol
                print(f"[{lexeme}] - operator '{char}'")

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
    if_stack = []
    if_counter = 0
    for_stack = []
    for_counter = 0


    for index, line in enumerate(program_lines):
        line_number = index + 1
        if line == "":
            continue

        print(f"lexing line='{line}'")
        parts = lexer(line)
        print(f"parsing parts={parts}")
        opcode = str(parts[0])

        # check for empty line
        if opcode == "":
            continue

        # convert ELSE to an ELSE_xx: label
        if opcode == "ELSE":
            if len(if_stack) > 0:
                opcode = opcode + f"_{if_stack[-1]}:"
            else:
                warning_msg = core.Error.message(core.Messages.E_ELSE, token_counter, str(opcode))
                print(f"{warning_msg} in line: {line_number} {line}")
                error_count += 1
                opcode = f"{opcode}_0:"

        # convert THEN to a THEN_xx: label
        elif opcode == "THEN":
            if len(if_stack) > 0:
                opcode = opcode + f"_{if_stack[-1]}:"
                if_stack.pop()    # drop last entry to close current IF block
            else:
                warning_msg = core.Error.message(core.Messages.E_THEN, token_counter, str(opcode))
                print(f"{warning_msg} in line: {line_number} {line}")
                error_count += 1
                opcode = f"{opcode}_0:"

        # convert FOR to an FOR_xx: label
        elif opcode == "FOR":
            for_counter += 1
            for_stack.append(for_counter)
            if len(for_stack) > 0:
                opcode = opcode + f"_{for_stack[-1]}:"
            else:
                warning_msg = core.Error.message(core.Messages.E_ELSE, token_counter, str(opcode))
                print(f"{warning_msg} in line: {line_number} {line}")
                error_count += 1
                opcode = f"{opcode}_0:"

        # convert NEXT to a NEXT_xx: label
        elif opcode == "NEXT":
            if len(for_stack) > 0:
                opcode = opcode + f"_{for_stack[-1]}:"
                for_stack.pop()    # drop last entry to close current IF block
            else:
                warning_msg = core.Error.message(core.Messages.E_THEN, token_counter, str(opcode))
                print(f"{warning_msg} in line: {line_number} {line}")
                error_count += 1
                opcode = f"{opcode}_0:"

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
           
        # check if it is an IF statement
        if opcode == "IF":
            if_counter += 1
            if_stack.append(if_counter)
            program.append(opcode)
            token_counter += 1
            continue

        # check if the line is a simple expression to be evaluated
        expr = core.Parser.parse_expression(parts)
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
                    operand = core.Parser.parse_expression(parts[1:])
                    if operand is None:
                        warning_msg = core.Error.message(core.Messages.E_EXPR, token_counter, str(operand))
                        print(f"{warning_msg} in line: {line_number} {line}")
                        error_count += 1
                else:
                    operand = str(parts[1])
                    if not isanumber(operand) and not operand.isidentifier():
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

    if len(if_stack) != 0:
        warning_msg = core.Error.message(core.Messages.E_THEN, token_counter, str(opcode))
        print(f"{warning_msg} in line: {line_number} {line}")
        error_count += 1

    return error_count, program, label_tracker,


#########################
#   Interpret Program
#########################

def interpret(program=[], label_tracker={}) -> str:

    pc = 0
    stack = core.Stack(256)
    heap = core.Heap(256)
    if_stack = []
    if_counter = 0
    for_stack = {}
    for_current = ""

    while program[pc] != "HALT":
        opcode = program[pc]
        pc += 1

        try:
            # handle Expression first because its not a string
            if isinstance(opcode, core.Expression):
                stack.push(float(opcode.evaluate()))
                continue
            # skip if its a user-defined label, process if it is autogenerated
            elif opcode.endswith(":"):
                # drop current level IF stack if label is a THEN_xx:
                if opcode.startswith("THEN_") and len(if_stack) > 0:
                    if_stack.pop()
                # handle ELSE_xx: label
                elif opcode.startswith("ELSE_") and len(if_stack) > 0:
                    pc = label_tracker[f"THEN_{if_stack[-1]}"]
                
                # loop back to FOR_xx: to iterate 
                elif opcode.startswith("NEXT_") and len(for_stack) > 0:
                    pc = label_tracker[f"FOR_{opcode[5:-1]}"]
                # drop current level NEXT stack if label is a NEXT_xx:
                elif opcode.startswith("FOR_"): #and len(for_stack) > 0:
                    if opcode not in for_stack:
                        for_stack[opcode] = stack._size()
                    else:
                        stack_pointer = int(for_stack[opcode])
                        for_inc = stack.buf[stack_pointer-1]
                        for_beg = stack.buf[stack_pointer-3] + for_inc
                        stack.buf[stack_pointer-3] = for_beg

                    stack_pointer = int(for_stack[opcode])
                    for_inc = stack.buf[stack_pointer-1]
                    for_end = stack.buf[stack_pointer-2]
                    for_beg = stack.buf[stack_pointer-3]
                    done = False
                    if for_inc < 0:
                        done = True if for_beg < for_end else False
                    else:
                        done = True if for_beg > for_end else False

                    if done:
                        stack._set_sp(stack_pointer-3)
                        _ = for_stack.pop(opcode)
                        pc = label_tracker[f"NEXT_{opcode[4:-1]}"]+1

                continue

            # stackops
            elif opcode == "DROP":
                _ = stack.pop()
            elif opcode == "PUSH":
                operand = str(program[pc])
                if isanumber(operand):
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
            elif opcode == "DUP":
                stack.push(stack.top())
            elif opcode == "SWAP":
                a = stack.pop()
                b = stack.pop()
                stack.push(a)
                stack.push(b)
            elif opcode == "ROT":
                a = stack.pop()
                b = stack.pop()
                c = stack.pop()
                stack.push(b)
                stack.push(a)
                stack.push(c)

            # branch control
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
            elif opcode == "IF":
                if_counter += 1
                if_stack.append(if_counter)
                if stack.pop() == 0:   # 0 = FALSE
                    if f"ELSE_{if_counter}" in label_tracker:
                        pc = label_tracker[f"ELSE_{if_counter}"]+1
                    else:
                        pc = label_tracker[f"THEN_{if_counter}"]+1
            elif opcode == "INDEX":
                if len(for_stack) > 0:
                    for_current = list(for_stack)[-1]
                    stack_pointer = int(for_stack[for_current])
                    stack.push(stack.buf[stack_pointer-3])
            
            # arithmetic
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
            elif opcode == "FLOAT":
                a = stack.pop()
                stack.push(float(a))
            elif opcode == "FLOOR":
                a = stack.pop()
                stack.push(int(a))
            elif opcode == "NEG":
                a = stack.pop()
                stack.push(-a)
            # logic ops
            elif opcode == "TRUE":
                stack.push(-1)
            elif opcode == "FALSE":
                stack.push(0)
            elif opcode == "AND":
                a = stack.pop()
                b = stack.pop()
                stack.push(a & b)
            elif opcode == "OR":
                a = stack.pop()
                b = stack.pop()
                stack.push(a | b)
            elif opcode == "XOR":
                a = stack.pop()
                b = stack.pop()
                stack.push(a ^ b)
            elif opcode == "NOT":
                a = stack.pop()
                stack.push(~a)
    
            # input / output
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

    print(f"\nStack size: {stack._size()}")
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
        if str(program[ip]) == "POP":
            variable_name = program[ip+1]
            variable_names.add(variable_name)
        elif str(program[ip]).startswith("FOR_"):    # create an Auto-Variable
            variable_name = "AV_" + program[ip]
            variable_names.add(variable_name)

    return string_literals, variable_names


#######################
#   Compile Program
#######################

def compile(source_filepath:Path=None, program=[], string_literals=[],
            variable_names=[], label_tracker=[]) -> str:

    asm_filepath = source_filepath.with_suffix(".asm")
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

    if_stack = []
    if_counter = 0
    for_stack = []

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
            if opcode.startswith("THEN_") and len(if_stack) > 0:
                if_stack.pop()
                out.write(f"; -- {opcode} --\n")
                out.write(f"{opcode}\n")
            elif opcode.startswith("ELSE_") and len(if_stack) > 0:
                if f"THEN_{if_stack[-1]}" in label_tracker:
                    label = f"THEN_{if_stack[-1]}"
                    out.write(f"\tJMP {label}\n")        # insert ELSE/THEN jump
                    out.write(f"; -- {opcode} --\n")
                    out.write(f"{opcode}\n")
                else:
                    out.write(f"; -- Error: Mismatched IF...THEN block --\n")
                    return f"{core.Error.message(core.Messages.E_THEN, ip-1, f"{opcode}_{if_counter}")} in {source_filepath}", ""
            
            elif opcode.startswith("NEXT_") and len(for_stack) > 0:
                label = f"FOR_{opcode[5:-1]}"
                out.write(f"; -- {opcode} --\n")
                out.write(f"\tJMP {label}\n")       # insert FOR/NEXT jump
                out.write(f"{opcode}\n")
                _ = for_stack.remove(label+":")
            elif opcode.startswith("FOR_"):
                for_stack.append(opcode)
                label = f"FOR_BEG_{opcode[4:-1]}"
                # save stackpointer to FOR vars
                variable_name = "AV_" + opcode[:-1]
                out.write(f"; -- {opcode} --\n")
                out.write(f"\tLEA rcx, {variable_name}\n")
                out.write(f"\tMOV qword[rcx], rsp\n")
                out.write(f"\tJMP {label}\n")       # skip increment on first iteration

                # save new index and continue
                out.write(f"{opcode}\n")                      # for label
                out.write(f"\tLEA rcx, {variable_name}\n")
                out.write(f"\tMOV rdx, qword[rcx]\n")
                out.write(f"\tMOV rax, qword[rdx+16]\n")       # load start
                out.write(f"\tADD rax, qword[rdx]\n")          # add increment
                out.write(f"\tMOV qword[rdx+16], rax\n")       # save index

                out.write(f"{label}:\n")
                out.write(f"\tLEA rcx, {variable_name}\n")
                out.write(f"\tMOV rdx, qword[rcx]\n")
                out.write(f"\tMOV rax, qword[rdx+16]\n")       # load start
                # decide which direction to test for
                out.write(f"\tMOV rcx, qword[rdx]\n")
                out.write(f"\tAND rcx, rcx\n")                 # check sign of increment
                out.write(f"\tJNS {"FOR_POS_"}{opcode[4:-1]}\n") # skip to positive inc
                # test for reverse limit
                out.write(f"\tCMP rax, qword[rdx+8]\n")        # compare with end
                out.write(f"\tJL {"NEXT_"}{opcode[4:-1]}\n")   # exit for loop
                out.write(f"\tJMP {"FOR_RUN_"}{opcode[4:-1]}\n")   # skip to execute
                # test for forward limit
                out.write(f"FOR_POS_{opcode[4:-1]}:\n")        # label
                out.write(f"\tCMP rax, qword[rdx+8]\n")        # compare with end
                out.write(f"\tJG {"NEXT_"}{opcode[4:-1]}\n")   # exit for loop
                # user code loop here
                out.write(f"FOR_RUN_{opcode[4:-1]}:\n")        # label
                #out.write(f"\tADD rax, qword[rdx]\n")          # add increment
                #out.write(f"\tMOV qword[rdx+16], rax\n")       # save index
            else:            
                out.write(f"; -- Label --\n")
                out.write(f"{opcode}\n")

        # stackops
        elif opcode == "INDEX":
            variable_name = f"AV_{for_stack[-1][:-1]}"
            out.write(f"; -- {opcode} --\n")
            out.write(f"\tLEA rcx, {variable_name}\n")
            out.write(f"\tMOV rdx, qword[rcx]\n")
            out.write(f"\tMOV rax, qword[rdx+16]\n")       # load start/index
            out.write(f"\tPUSH rax\n")

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
        elif opcode == "DROP":
            out.write(f"; -- {opcode} --\n")
            out.write(f"\tADD rsp, 8\n")
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
        elif opcode == "ROT":
            out.write(f"; -- {opcode} --\n")
            out.write(f"\tPOP rax\n")
            out.write(f"\tPOP rcx\n")
            out.write(f"\tPOP rdx\n")
            out.write(f"\tPUSH rcx\n")
            out.write(f"\tPUSH rax\n")
            out.write(f"\tPUSH rdx\n")

        # arithmetic
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
        elif opcode == "NEG":
            out.write(f"; -- {opcode} --\n")
            out.write(f"\tNEG qword [rsp]\n")

        # logic operations
        elif opcode == "TRUE":
            out.write(f"; -- {opcode} --\n")
            out.write(f"\tXOR rax, rax\n")
            out.write(f"\tNOT rax\n")
            out.write(f"\tPUSH rax\n")

        elif opcode == "FALSE":
            out.write(f"; -- {opcode} --\n")
            out.write(f"\tXOR rax, rax\n")
            out.write(f"\tPUSH rax\n")

        elif opcode == "AND":
            out.write(f"; -- {opcode} --\n")
            out.write(f"\tPOP rax\n")
            out.write(f"\tAND qword [rsp], rax\n")

        elif opcode == "OR":
            out.write(f"; -- {opcode} --\n")
            out.write(f"\tPOP rax\n")
            out.write(f"\tOR qword [rsp], rax\n")

        elif opcode == "XOR":
            out.write(f"; -- {opcode} --\n")
            out.write(f"\tPOP rax\n")
            out.write(f"\tXOR qword [rsp], rax\n")

        elif opcode == "NOT":
            out.write(f"; -- {opcode} --\n")
            out.write(f"\tNOT qword [rsp]\n")

        # input / output
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

        # branch control
        elif opcode == "JUMP":
            condition = program[ip]
            ip += 1
            out.write(f"; -- {opcode}{condition} --\n")
            label = program[ip]
            ip += 1
            if condition == ".EQ.0":
                out.write(f"\tCMP qword [rsp], 0\n")
                out.write(f"\tJE {label}\n")

            elif condition == ".GT.0":
                out.write(f"\tCMP qword [rsp], 0\n")
                out.write(f"\tJG {label}\n")

            elif condition == ".LT.0":
                out.write(f"\tCMP qword [rsp], 0\n")
                out.write(f"\tJL {label}\n")

            elif condition == ".NE.0":
                out.write(f"\tCMP qword [rsp], 0\n")
                out.write(f"\tJNE {label}\n")
            else:
                out.write(f"; -- Error: Unrecognised Branch Condition --\n")
                return f"{core.Error.message(core.Messages.E_OPCOD, ip-1, opcode+condition)} in {source_filepath}", ""

        elif opcode == "IF":
            if_counter += 1
            if_stack.append(if_counter)

            if f"ELSE_{if_counter}" in label_tracker:
                label = f"ELSE_{if_counter}"
            elif f"THEN_{if_counter}" in label_tracker:
                label = f"THEN_{if_counter}"
            else:
                out.write(f"; -- Error: Mismatched IF...THEN block --\n")
                return f"{core.Error.message(core.Messages.E_THEN, ip-1, f"{opcode}_{if_counter}")} in {source_filepath}", ""

            out.write(f"; -- {opcode}_{if_counter} --\n")
            out.write(f"\tPOP rax\n")
            out.write(f"\tAND rax, rax\n")          # test for FALSE
            out.write(f"\tJZ {label}\n")            # insert ELSE/THEN jump

        # system
        elif opcode == "HALT":
            out.write(f"; -- {opcode} --\n")
            out.write(f"\tJMP EXIT_LABEL\n")
        else:
            return f"{core.Error.message(core.Messages.E_OPCOD, ip-1, opcode)} in {source_filepath}", ""


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
