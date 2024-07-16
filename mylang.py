import sys
import os
import mylangtools as mlt

# read arguments
user_action = sys.argv[1]
program_filepath = sys.argv[2]
print(f"source file: {program_filepath}")

if user_action == '-r':
    # run program using interpreter
    print("[CMD] Parsing")
    error_count, program, label_tracker = mlt.tokenise(program_filepath)
    print(program)
    print(label_tracker)
    if error_count > 0:
        print(f"Errors: {error_count}")
        quit()

    print("[CMD] Interpreting")
    status = mlt.interpret(program, label_tracker)
    print(status)

elif user_action == '-c':
    # compile program to executable
    print("[CMD] Preprocessing")
    program, label_tracker = mlt.tokenise(program_filepath)
    print(program)

    string_literals, variable_names = mlt.precompile(program)
    print(string_literals, variable_names)

    print("[CMD] Compiling")
    status, asm_filepath = mlt.compile(program_filepath, program, string_literals,
                                       variable_names)
    print(status, asm_filepath)
    if status != "OK":
        quit()

    print("[CMD] Assembling")
    os.system(f"nasm -g -f elf64 {asm_filepath}")

    print("[CMD] Linking")
    os.system(f"gcc -g -o {asm_filepath[:-4] + '.exe'} {asm_filepath[:-4] + '.o'}")

    print(f"[INFO] Executable filename: {asm_filepath[:-4] + '.exe'}")

else:
    print("Invalid option:", user_action)
    print("Options:")
    print("\t-r <filename>  = run interpreted")
    print("\t-c <filename>  = compile executable")

