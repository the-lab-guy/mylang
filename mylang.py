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

print("[CMD] Interpreting")
status = mlt.interpret(program, label_tracker)
print(status)


print("[CMD] Preprocessing")
program = mlt.ctokenise(program_filepath)
print(program)

string_literals = mlt.precompile(program)
print(string_literals)

print("[CMD] Compiling")
status, asm_filepath = mlt.compile(program_filepath, program, string_literals)
print(status, asm_filepath)

print("[CMD] Assembling")
os.system(f"nasm -g -f elf64 {asm_filepath}")

print("[CMD] Linking")
os.system(f"gcc -g -o {asm_filepath[:-4] + '.exe'} {asm_filepath[:-4] + '.o'}")

print(f"[CMD] Running")
os.system(f"{asm_filepath[:-4] + '.exe'}")
