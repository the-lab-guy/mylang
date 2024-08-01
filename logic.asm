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
F_NUMB  db "%lld", 0
F_STR   db "%s", 0
F_FLOAT db "%f", 0

string_literal_0 db `\nLogic operations test `, 0
string_literal_1 db `\n`, 0
string_literal_2 db `\n Expected: 1 AND 1 is 1 (TRUE)\n`, 0
string_literal_3 db `\tResult: %lld \n`, 0
string_literal_4 db `\n Expected: 0 AND 1 is 0 (FALSE)\n`, 0
string_literal_5 db `\tResult: %lld \n`, 0
string_literal_6 db `\n Expected: 1 AND 0 is 0 (FALSE)\n`, 0
string_literal_7 db `\tResult: %lld \n`, 0
string_literal_8 db `\n Expected: 0 AND 0 is 0 (FALSE)\n`, 0
string_literal_9 db `\tResult: %lld \n`, 0
string_literal_10 db `\n Expected: -1 AND 1 is -1 (TRUE)\n`, 0
string_literal_11 db `\tResult: %lld \n`, 0
string_literal_12 db `==========================================`, 0
string_literal_13 db `\n Expected: 1 OR 1 is 1 (TRUE)\n`, 0
string_literal_14 db `\tResult: %lld \n`, 0
string_literal_15 db `\n Expected: 0 OR 1 is 1 (TRUE)\n`, 0
string_literal_16 db `\tResult: %lld \n`, 0
string_literal_17 db `\n Expected: 1 OR 0 is 1 (TRUE)\n`, 0
string_literal_18 db `\tResult: %lld \n`, 0
string_literal_19 db `\n Expected: 0 OR 0 is 0 (FALSE)\n`, 0
string_literal_20 db `\tResult: %lld \n`, 0
string_literal_21 db `==========================================`, 0
string_literal_22 db `\n Expected: 1 XOR 1 is 0 (FALSE)\n`, 0
string_literal_23 db `\tResult: %lld \n`, 0
string_literal_24 db `\n Expected: 0 XOR 1 is 1 (TRUE)\n`, 0
string_literal_25 db `\tResult: %lld \n`, 0
string_literal_26 db `\n Expected: 1 XOR 0 is 1 (TRUE)\n`, 0
string_literal_27 db `\tResult: %lld \n`, 0
string_literal_28 db `\n Expected: 0 XOR 0 is 0 (FALSE)\n`, 0
string_literal_29 db `\tResult: %lld \n`, 0
string_literal_30 db `==========================================`, 0
string_literal_31 db `\n Expected: NOT -1 is 0 (FALSE)\n`, 0
string_literal_32 db `\tResult: %lld \n`, 0
string_literal_33 db `\n Expected: NOT 0 is -1 (TRUE)\n`, 0
string_literal_34 db `\tResult: %lld \n`, 0

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
; -- PRINT --
	SUB rsp, 40
	LEA rcx, string_literal_0
	XOR eax, eax
	CALL printf
	ADD rsp, 40
; -- PRINT --
	SUB rsp, 40
	LEA rcx, string_literal_1
	XOR eax, eax
	CALL printf
	ADD rsp, 40
; -- PRINT --
	SUB rsp, 40
	LEA rcx, string_literal_2
	XOR eax, eax
	CALL printf
	ADD rsp, 40
; -- PUSH num --
	PUSH 1
; -- DUP --
	POP rax
	PUSH rax
	PUSH rax
; -- AND --
	POP rax
	AND qword [rsp], rax
; -- PRINT --
	POP rdx
	SUB rsp, 40
	LEA rcx, string_literal_3
	XOR eax, eax
	CALL printf
	ADD rsp, 40
; -- PRINT --
	SUB rsp, 40
	LEA rcx, string_literal_4
	XOR eax, eax
	CALL printf
	ADD rsp, 40
; -- PUSH num --
	PUSH 0
; -- PUSH num --
	PUSH 1
; -- AND --
	POP rax
	AND qword [rsp], rax
; -- PRINT --
	POP rdx
	SUB rsp, 40
	LEA rcx, string_literal_5
	XOR eax, eax
	CALL printf
	ADD rsp, 40
; -- PRINT --
	SUB rsp, 40
	LEA rcx, string_literal_6
	XOR eax, eax
	CALL printf
	ADD rsp, 40
; -- PUSH num --
	PUSH 1
; -- PUSH num --
	PUSH 0
; -- AND --
	POP rax
	AND qword [rsp], rax
; -- PRINT --
	POP rdx
	SUB rsp, 40
	LEA rcx, string_literal_7
	XOR eax, eax
	CALL printf
	ADD rsp, 40
; -- PRINT --
	SUB rsp, 40
	LEA rcx, string_literal_8
	XOR eax, eax
	CALL printf
	ADD rsp, 40
; -- PUSH num --
	PUSH 0
; -- DUP --
	POP rax
	PUSH rax
	PUSH rax
; -- AND --
	POP rax
	AND qword [rsp], rax
; -- PRINT --
	POP rdx
	SUB rsp, 40
	LEA rcx, string_literal_9
	XOR eax, eax
	CALL printf
	ADD rsp, 40
; -- PRINT --
	SUB rsp, 40
	LEA rcx, string_literal_10
	XOR eax, eax
	CALL printf
	ADD rsp, 40
; -- PUSH num --
	PUSH -1
; -- DUP --
	POP rax
	PUSH rax
	PUSH rax
; -- AND --
	POP rax
	AND qword [rsp], rax
; -- PRINT --
	POP rdx
	SUB rsp, 40
	LEA rcx, string_literal_11
	XOR eax, eax
	CALL printf
	ADD rsp, 40
; -- PRINT --
	SUB rsp, 40
	LEA rcx, string_literal_12
	XOR eax, eax
	CALL printf
	ADD rsp, 40
; -- PRINT --
	SUB rsp, 40
	LEA rcx, string_literal_13
	XOR eax, eax
	CALL printf
	ADD rsp, 40
; -- PUSH num --
	PUSH 1
; -- DUP --
	POP rax
	PUSH rax
	PUSH rax
; -- OR --
	POP rax
	OR qword [rsp], rax
; -- PRINT --
	POP rdx
	SUB rsp, 40
	LEA rcx, string_literal_14
	XOR eax, eax
	CALL printf
	ADD rsp, 40
; -- PRINT --
	SUB rsp, 40
	LEA rcx, string_literal_15
	XOR eax, eax
	CALL printf
	ADD rsp, 40
; -- PUSH num --
	PUSH 0
; -- PUSH num --
	PUSH 1
; -- OR --
	POP rax
	OR qword [rsp], rax
; -- PRINT --
	POP rdx
	SUB rsp, 40
	LEA rcx, string_literal_16
	XOR eax, eax
	CALL printf
	ADD rsp, 40
; -- PRINT --
	SUB rsp, 40
	LEA rcx, string_literal_17
	XOR eax, eax
	CALL printf
	ADD rsp, 40
; -- PUSH num --
	PUSH 1
; -- PUSH num --
	PUSH 0
; -- OR --
	POP rax
	OR qword [rsp], rax
; -- PRINT --
	POP rdx
	SUB rsp, 40
	LEA rcx, string_literal_18
	XOR eax, eax
	CALL printf
	ADD rsp, 40
; -- PRINT --
	SUB rsp, 40
	LEA rcx, string_literal_19
	XOR eax, eax
	CALL printf
	ADD rsp, 40
; -- PUSH num --
	PUSH 0
; -- DUP --
	POP rax
	PUSH rax
	PUSH rax
; -- OR --
	POP rax
	OR qword [rsp], rax
; -- PRINT --
	POP rdx
	SUB rsp, 40
	LEA rcx, string_literal_20
	XOR eax, eax
	CALL printf
	ADD rsp, 40
; -- PRINT --
	SUB rsp, 40
	LEA rcx, string_literal_21
	XOR eax, eax
	CALL printf
	ADD rsp, 40
; -- PRINT --
	SUB rsp, 40
	LEA rcx, string_literal_22
	XOR eax, eax
	CALL printf
	ADD rsp, 40
; -- PUSH num --
	PUSH 1
; -- DUP --
	POP rax
	PUSH rax
	PUSH rax
; -- XOR --
	POP rax
	XOR qword [rsp], rax
; -- PRINT --
	POP rdx
	SUB rsp, 40
	LEA rcx, string_literal_23
	XOR eax, eax
	CALL printf
	ADD rsp, 40
; -- PRINT --
	SUB rsp, 40
	LEA rcx, string_literal_24
	XOR eax, eax
	CALL printf
	ADD rsp, 40
; -- PUSH num --
	PUSH 0
; -- PUSH num --
	PUSH 1
; -- XOR --
	POP rax
	XOR qword [rsp], rax
; -- PRINT --
	POP rdx
	SUB rsp, 40
	LEA rcx, string_literal_25
	XOR eax, eax
	CALL printf
	ADD rsp, 40
; -- PRINT --
	SUB rsp, 40
	LEA rcx, string_literal_26
	XOR eax, eax
	CALL printf
	ADD rsp, 40
; -- PUSH num --
	PUSH 1
; -- PUSH num --
	PUSH 0
; -- XOR --
	POP rax
	XOR qword [rsp], rax
; -- PRINT --
	POP rdx
	SUB rsp, 40
	LEA rcx, string_literal_27
	XOR eax, eax
	CALL printf
	ADD rsp, 40
; -- PRINT --
	SUB rsp, 40
	LEA rcx, string_literal_28
	XOR eax, eax
	CALL printf
	ADD rsp, 40
; -- PUSH num --
	PUSH 0
; -- DUP --
	POP rax
	PUSH rax
	PUSH rax
; -- XOR --
	POP rax
	XOR qword [rsp], rax
; -- PRINT --
	POP rdx
	SUB rsp, 40
	LEA rcx, string_literal_29
	XOR eax, eax
	CALL printf
	ADD rsp, 40
; -- PRINT --
	SUB rsp, 40
	LEA rcx, string_literal_30
	XOR eax, eax
	CALL printf
	ADD rsp, 40
; -- PRINT --
	SUB rsp, 40
	LEA rcx, string_literal_31
	XOR eax, eax
	CALL printf
	ADD rsp, 40
; -- PUSH num --
	PUSH -1
; -- NOT --
	NOT qword [rsp]
; -- PRINT --
	POP rdx
	SUB rsp, 40
	LEA rcx, string_literal_32
	XOR eax, eax
	CALL printf
	ADD rsp, 40
; -- PRINT --
	SUB rsp, 40
	LEA rcx, string_literal_33
	XOR eax, eax
	CALL printf
	ADD rsp, 40
; -- PUSH num --
	PUSH 0
; -- NOT --
	NOT qword [rsp]
; -- PRINT --
	POP rdx
	SUB rsp, 40
	LEA rcx, string_literal_34
	XOR eax, eax
	CALL printf
	ADD rsp, 40
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
