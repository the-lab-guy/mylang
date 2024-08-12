import sys
import os
import argparse
from pathlib import Path
import mylangtools as mlt

# read cmdline arguments
argparser = argparse.ArgumentParser(description="interpreter and compiler for the mylang programming language")
group = argparser.add_mutually_exclusive_group()
group.add_argument("-r", "--run", help="run interpreted", action="store_true")
group.add_argument("-c", "--compile", help="compile executable", action="store_true")
argparser.add_argument("file", help="file to run or compile")
args = argparser.parse_args()

# if args.run:
#     print(f"run {args.file} with interpreter")
# elif args.compile:
#     print(f"compile {args.file} into executable")

#user_action = sys.argv[1]
#program_filepath = sys.argv[2]
#print(f"source file: {program_filepath}")

source_filepath = Path(args.file)

if args.run:
    # run program using interpreter
    print("[CMD] Parsing")
    error_count, program, label_tracker = mlt.tokenise(source_filepath)
    print(f"program:{program}")
    print(f"labels:{label_tracker}")
    if error_count > 0:
        print(f"Errors: {error_count}")
        quit()

    print("[CMD] Interpreting")
    status = mlt.interpret(program, label_tracker)
    print(status)

elif args.compile:
    # compile program to executable
    print("[CMD] Preprocessing")
    error_count, program, label_tracker = mlt.tokenise(source_filepath)
    print(f"program:{program}")
    print(f"labels:{label_tracker}")
    if error_count > 0:
        print(f"Errors: {error_count}")
        quit()

    print("[CMD] Precompiling")
    string_literals, variable_names = mlt.precompile(program)
    print(f"string literals:{string_literals}")
    print(f"variables names:{variable_names}")

    print("[CMD] Compiling")
    status, asm_filepath = mlt.compile(source_filepath, program, string_literals,
                                       variable_names)
    print(status, asm_filepath)
    if status != "OK":
        quit()

    print("[CMD] Assembling")
    os.system(f"nasm -g -f elf64 {asm_filepath}")

    print("[CMD] Linking")
    os.system(f"gcc -g -o {asm_filepath.with_suffix(".exe")} {asm_filepath.with_suffix(".o")}")

    print(f"[INFO] Executable filename: {asm_filepath.with_suffix(".exe")}")

else:
    print("Invalid option:", args)
    print("Options:")
    print("\t-r <filename>  = run interpreted")
    print("\t-c <filename>  = compile executable")

