; -- header --
bits 64
default rel

; -- variables --
section .bss
read_buffer resq 1  ; 64-bits integer = 8 bytes

; -- constants --
section .data
M_OK    db "OK", 0
E_DIV0  db "Division by zero", 0
E_NOLBL db "Label name not found", 0
E_NOVAR db "Variable name not found", 0
E_OPCOD db "Unknown instruction", 0
E_MISS  db "Missing parameter", 0
E_TYPE  db "Incorrect parameter type", 0
E_ILLEG db "Illegal identifier name", 0
E_EXPR  db "Invalid expression syntax", 0
E_ELSE  db "Mismatched IF...ELSE", 0
E_THEN  db "Mismatched IF...THEN", 0
F_NUMB  db "%lld", 0
F_STR   db "%s", 0
F_FLOAT db "%f", 0

string_literal_0 db `Exp: 0 Act: %lld \n`, 0
string_literal_1 db `Wrong, expected FALSE.\n`, 0
string_literal_2 db `Right, expected FALSE.\n`, 0
string_literal_3 db `Right, expected TRUE.\n`, 0
string_literal_4 db `Wrong, expected TRUE.\n`, 0
string_literal_5 db `Exp: -1 Act: %lld \n`, 0
string_literal_6 db `Double TRUE! \n`, 0
string_literal_7 db `Bye! \n`, 0

; -- Entry Point --
section .text
global main
extern ExitProcess
extern printf
extern scanf
extern pow

main:
	PUSH rbp
	MOV rbp, rsp
; -- FALSE --
	XOR rax, rax
	PUSH rax
; -- DUP --
	POP rax
	PUSH rax
	PUSH rax
; -- IF_1 --
	POP rax
	AND rax, rax
	JZ ELSE_1
; -- PRINT --
	POP rdx
	SUB rsp, 40
	LEA rcx, string_literal_0
	XOR eax, eax
	CALL printf
	ADD rsp, 40
	JMP THEN_1
; -- Label --
ELSE_1:
; -- IF_2 --
	POP rax
	AND rax, rax
	JZ ELSE_2
; -- PRINT --
	SUB rsp, 40
	LEA rcx, string_literal_1
	XOR eax, eax
	CALL printf
	ADD rsp, 40
	JMP THEN_2
; -- Label --
ELSE_2:
; -- PRINT --
	SUB rsp, 40
	LEA rcx, string_literal_2
	XOR eax, eax
	CALL printf
	ADD rsp, 40
; -- Label --
THEN_2:
; -- TRUE --
	XOR rax, rax
	NOT rax
	PUSH rax
; -- DUP --
	POP rax
	PUSH rax
	PUSH rax
; -- IF_3 --
	POP rax
	AND rax, rax
	JZ ELSE_3
; -- IF_4 --
	POP rax
	AND rax, rax
	JZ ELSE_4
; -- PRINT --
	SUB rsp, 40
	LEA rcx, string_literal_3
	XOR eax, eax
	CALL printf
	ADD rsp, 40
	JMP THEN_4
; -- Label --
ELSE_4:
; -- PRINT --
	SUB rsp, 40
	LEA rcx, string_literal_4
	XOR eax, eax
	CALL printf
	ADD rsp, 40
; -- Label --
THEN_4:
	JMP THEN_3
; -- Label --
ELSE_3:
; -- PRINT --
	POP rdx
	SUB rsp, 40
	LEA rcx, string_literal_5
	XOR eax, eax
	CALL printf
	ADD rsp, 40
; -- Label --
THEN_3:
; -- Label --
THEN_1:
; -- TRUE --
	XOR rax, rax
	NOT rax
	PUSH rax
; -- IF_5 --
	POP rax
	AND rax, rax
	JZ THEN_5
; -- FALSE --
	XOR rax, rax
	PUSH rax
; -- IF_6 --
	POP rax
	AND rax, rax
	JZ THEN_6
; -- PRINT --
	SUB rsp, 40
	LEA rcx, string_literal_6
	XOR eax, eax
	CALL printf
	ADD rsp, 40
; -- Label --
THEN_6:
; -- PRINT --
	SUB rsp, 40
	LEA rcx, string_literal_7
	XOR eax, eax
	CALL printf
	ADD rsp, 40
; -- Label --
THEN_5:
; -- HALT --
	JMP EXIT_LABEL
ERROR_LABEL:
; -- ERROR --
	SUB rsp, 40
	LEA rcx, E_DIV0
	XOR eax, eax
	CALL printf
	ADD rsp, 40
EXIT_LABEL:
	XOR rax, rax
	CALL ExitProcess
