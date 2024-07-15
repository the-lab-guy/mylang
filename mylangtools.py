import mylangcore as core
import textwrap

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

    for line in program_lines:
        parts = line.split(" ")
        opcode = parts[0]

        # check for empty line
        if opcode == "":
            continue

        # check if its a label
        if opcode.endswith(":"):
            label_tracker[opcode[:-1]] = token_counter

        # store opcode token
        program.append(opcode)
        token_counter += 1

        # handle each opcode
        if opcode == "PUSH":
            # expecting a number or a variable name
            operand = str(parts[1])
            if operand.isnumeric():
                number = int(parts[1])
                program.append(number)
                token_counter += 1
            elif operand[0].isalpha():
                variable_name = operand
                program.append(variable_name)
                token_counter += 1
            else:
                print(f"Error: Illegal variable name [{operand}]")
                quit()
        elif opcode == "POP":
            # return value of an integer variable
            variable_name = str(parts[1])
            if variable_name[0].isalpha():
                program.append(variable_name)
                token_counter += 1
            else:
                print(f"Error: Illegal variable name [{variable_name}]")
                quit()
        elif opcode == "PRINT":
            # parse string literal
            string_literal = ' '.join(parts[1:])[1:-1]
            program.append(string_literal)
            token_counter += 1
        elif opcode == "JUMP.EQ.0":
            # read label
            label = parts[1]
            #print(f"label={label}")
            program.append(label)
            #print(f"last token={program[:-1]}")
            token_counter += 1
        elif opcode == "JUMP.GT.0":
            # read label
            label = parts[1]
            program.append(label)
            token_counter += 1
        elif opcode == "JUMP.LT.0":
            # read label
            label = parts[1]
            program.append(label)
            token_counter += 1

    return program, label_tracker


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

        # skip if its a label
        if opcode.endswith(":"):
            continue
        elif opcode == "PUSH":
            operand = str(program[pc])
            if operand.isnumeric():
                number = program[pc]
                pc += 1
                stack.push(number)
            elif operand[0].isalpha():
                variable_name = program[pc]
                pc += 1
                stack.push(heap.fetch(variable_name))
            else:
                return core.Error.message(core.Messages.E_NOVAR, pc-1, opcode)
        elif opcode == "POP":
            variable_name = program[pc]
            if variable_name[0].isalpha():
                pc += 1
                a = stack.pop()
                heap.store(variable_name, a)
            else:
                return core.Error.message(core.Messages.E_NOVAR, pc-1, opcode)
        elif opcode == "JUMP.EQ.0":
            number = stack.top()
            if number == 0:
                pc = label_tracker[program[pc]]
            else:
                pc += 1
        elif opcode == "JUMP.GT.0":
            number = stack.top()
            if number > 0:
                pc = label_tracker[program[pc]]
            else:
                pc += 1
        elif opcode == "JUMP.LT.0":
            number = stack.top()
            if number < 0:
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
        elif opcode == "PRINT":
            string_literal = program[pc]
            pc += 1
            if "@#" in string_literal:
                a = stack.pop()
                string_literal = string_literal.replace("@#", str(a))
            print(string_literal)
        elif opcode == "READ":
            number = int(input())
            stack.push(number)
        else:
            return core.Error.message(core.Messages.E_OPCOD, pc-1, opcode)

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

    for message in core.Messages:
        out.write(f"{message.name:7} db \"{message.value}\", 0\n")

    out.write("\n")  # insert a blank line

    for i, string_literal in enumerate(string_literals):
        out.write(f"string_literal_{i} db \"{string_literal}\", 0\n")

    out.write(textwrap.dedent("""
    ; -- Entry Point --
    section .text
    global main
    extern ExitProcess
    extern printf
    extern scanf
            
    main:
    \tPUSH rbp
    \tMOV rbp, rsp
    """))

    ip = 0
    while ip < len(program):
        opcode = program[ip]
        ip += 1

        if opcode.endswith(":"):
            out.write(f"; -- Label --\n")
            out.write(f"{opcode}\n")
        elif opcode == "PUSH":
            operand = program[ip]
            ip += 1
            
            # number or variable?
            if operand not in variable_names:
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
        elif opcode == "PRINT":
            string_literal_index = program[ip]
            ip += 1
            out.write(f"; -- {opcode} --\n")
            if "%lld" in string_literals[string_literal_index]:
                out.write(f"\tPOP rdx\n")  # value to print

            out.write(f"\tSUB rsp, 32\n")
            out.write(f"\tLEA rcx, string_literal_{string_literal_index}\n")
            out.write(f"\tXOR eax, eax\n")
            out.write(f"\tCALL printf\n")
            out.write(f"\tADD rsp, 32\n")

        elif opcode == "READ":
            out.write(f"; -- {opcode} --\n")
            out.write(f"\tSUB rsp, 32\n")
            out.write(f"\tLEA rcx, F_NUMB\n")  # format string
            out.write(f"\tLEA rdx, read_buffer\n")
            out.write(f"\tXOR eax, eax\n")
            out.write(f"\tMOV [rdx], eax\n")
            out.write(f"\tCALL scanf\n")
            out.write(f"\tADD rsp, 32\n")
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
        elif opcode == "HALT":
            out.write(f"; -- {opcode} --\n")
            out.write(f"\tJMP EXIT_LABEL\n")
        else:
            return f"{core.Error.message(core.Messages.E_OPCOD, ip-1, opcode)} in {program_filepath}", ""


    out.write(f"ERROR_LABEL:\n")
    out.write(f"; -- ERROR --\n")
    out.write(f"\tSUB rsp, 32\n")
    out.write(f"\tLEA rcx, E_DIV0\n")
    out.write(f"\tXOR eax, eax\n")
    out.write(f"\tCALL printf\n")
    out.write(f"\tADD rsp, 32\n")
    
    out.write(f"EXIT_LABEL:\n")
    out.write(f"\tXOR rax, rax\n")
    out.write(f"\tCALL ExitProcess\n")

    out.close()
    return "OK", asm_filepath

if __name__ == "__main__":
    print("Mylang tools library")
