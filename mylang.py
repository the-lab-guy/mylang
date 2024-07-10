import sys
import os
from mylangcore import Stack
import mylangtools as mlt

# read arguments
program_filepath = sys.argv[1]
print(f"source file: {program_filepath}")

print("[CMD] Parsing")
program, label_tracker = mlt.tokenise(program_filepath)
print(program)
print(label_tracker)

#########################
#   Interpret Program
#########################

print("[CMD] Interpreting")

pc = 0
stack = Stack(256)

while program[pc] != "HALT":
    opcode = program[pc]
    pc += 1

    if opcode == "PUSH":
        number = program[pc]
        pc += 1
        stack.push(number)
    elif opcode == "POP":
        stack.pop()
    elif opcode == "ADD":
        a = stack.pop()
        b = stack.pop()
        stack.push(a+b)
    elif opcode == "SUB":
        a = stack.pop()
        b = stack.pop()
        stack.push(b-a)
    elif opcode == "PRINT":
        string_literal = program[pc]
        pc += 1
        print(string_literal)
    elif opcode == "READ":
        number = int(input("?"))
        stack.push(number)
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


########################
#   Tokenise Program
########################

print("[CMD] Preprocessing")

# read file lines
program_lines = []
with open(program_filepath, "r") as program_file:
    program_lines = [line.strip() for line in program_file.readlines()]

program = []
for line in program_lines:
    parts = line.split(" ")
    opcode = parts[0]

    # check for empty line
    if opcode == "":
        continue

    # store opcode token
    program.append(opcode)

    # handle each opcode
    if opcode == "PUSH":
        # expecting a number
        number = int(parts[1])
        program.append(number)
    elif opcode == "PRINT":
        # parse string literal
        string_literal = ' '.join(parts[1:])[1:-1]
        program.append(string_literal)
    elif opcode == "JUMP.EQ.0":
        # read label
        label = parts[1]
        program.append(label)
    elif opcode == "JUMP.GT.0":
        # read label
        label = parts[1]
        program.append(label)


###############################
#   Pre-compiler Operations
###############################

string_literals = []
for ip in range(len(program)):
    if program[ip] == "PRINT":
        string_literal = program[ip+1]
        program[ip+1] = len(string_literals)
        string_literals.append(string_literal)


#######################
#   Compile Program
#######################

asm_filepath = program_filepath.split('.')[0] + ".asm"
print(f"assembly file: {asm_filepath}")
out = open(asm_filepath, "w")

out.write("""; -- header --
bits 64
default rel
""")

out.write("""; -- variables --
section .bss
read_number resq 1  ; 64-bits integer = 8 bytes
""")

out.write("""; -- constants --
section .data
read_format db "%d", 0  ; the format string for scanf
""")

for i, string_literal in enumerate(string_literals):
    out.write(f"string_literal_{i} db \"{string_literal}\", 0\n")

out.write("""; -- Entry Point --
section .text
global main
extern ExitProcess
extern printf
extern scanf
          
main:
\tPUSH rbp
\tMOV rbp, rsp
; \tSUB rsp, 32
""")

ip = 0
while ip < len(program):
    opcode = program[ip]
    ip += 1

    if opcode.endswith(":"):
        out.write(f"; -- Label --\n")
        out.write(f"{opcode}\n")
    elif opcode == "PUSH":
        number = program[ip]
        ip += 1
        
        out.write(f"; -- PUSH --\n")
        out.write(f"\tPUSH {number}\n")
    elif opcode == "POP":
        out.write(f"; -- POP --\n")
        out.write(f"\tPOP\n")
    elif opcode == "ADD":
        out.write(f"; -- ADD --\n")
        out.write(f"\tPOP rax\n")
        out.write(f"\tADD qword [rsp], rax\n")
    elif opcode == "SUB":
        out.write(f"; -- SUB --\n")
        out.write(f"\tPOP rax\n")
        out.write(f"\tSUB qword [rsp], rax\n")

    elif opcode == "PRINT":
        string_literal_index = program[ip]
        ip += 1
        out.write(f"; -- PRINT --\n")
        out.write(f"\tSUB rsp, 32\n")
        out.write(f"\tLEA rcx, string_literal_{string_literal_index}\n")
        out.write(f"\tXOR eax, eax\n")
        out.write(f"\tCALL printf\n")
        out.write(f"\tADD rsp, 32\n")

    elif opcode == "READ":
        out.write(f"; -- READ --\n")
        out.write(f"\tSUB rsp, 32\n")
        out.write(f"\tLEA rcx, read_format\n")
        out.write(f"\tLEA rdx, read_number\n")
        out.write(f"\tXOR eax, eax\n")
        out.write(f"\tMOV [rdx], eax\n")
        out.write(f"\tCALL scanf\n")
        out.write(f"\tADD rsp, 32\n")
        out.write(f"\tPUSH qword [read_number]\n")

    elif opcode == "JUMP.EQ.0":
        label = program[ip]
        ip += 1

        out.write(f"; -- JUMP.EQ.0 --\n")
        out.write(f"\tCMP qword [rsp], 0\n")
        out.write(f"\tJE {label}\n")
    elif opcode == "JUMP.GT.0":
        label = program[ip]
        ip += 1

        out.write(f"; -- JUMP.GT.0 --\n")
        out.write(f"\tCMP qword [rsp], 0\n")
        out.write(f"\tJG {label}\n")
    elif opcode == "HALT":
        out.write(f"; -- HALT --\n")
        out.write(f"\tJMP EXIT_LABEL\n")

out.write(f"EXIT_LABEL:\n")
out.write(f"\tXOR rax, rax\n")
out.write(f"\tCALL ExitProcess\n")

out.close()


print("[CMD] Assembling")
os.system(f"nasm -g -f elf64 {asm_filepath}")
print("[CMD] Linking")
os.system(f"gcc -g -o {asm_filepath[:-4] + '.exe'} {asm_filepath[:-3] + 'o'}")

print(f"[CMD] Running")
os.system(f"{asm_filepath[:-4] + '.exe'}")
