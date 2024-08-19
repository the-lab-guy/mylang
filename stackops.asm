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

string_literal_0 db `\tStack operations test \n`, 0
string_literal_1 db `\n`, 0
string_literal_2 db `Pushing 1111 \n`, 0
string_literal_3 db `Duplicating Top \n`, 0
string_literal_4 db `\tTop: %lld \n`, 0
string_literal_5 db `Pushing 2222 \n`, 0
string_literal_6 db `Duplicating Top \n`, 0
string_literal_7 db `\tNew Top: %lld \n`, 0
string_literal_8 db `Swapping top two values \n`, 0
string_literal_9 db `Top was: %lld  -  `, 0
string_literal_10 db `Second was: %lld \n`, 0
string_literal_11 db `DROP test\n`, 0
string_literal_12 db `Pushing 1111 \n`, 0
string_literal_13 db `Pushing 2222 \n`, 0
string_literal_14 db `Dropping 2222 \n`, 0
string_literal_15 db `\tExpecting: 1111 \n`, 0
string_literal_16 db `\tActual Top: %lld \n`, 0
string_literal_17 db `ROT test\n`, 0
string_literal_18 db `Pushing 1 2 3\n`, 0
string_literal_19 db `Expecting: 1 3 2\n`, 0
string_literal_20 db `%lld `, 0
string_literal_21 db `%lld `, 0
string_literal_22 db `%lld \n`, 0

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
	PUSH 1111
; -- PRINT --
	SUB rsp, 40
	LEA rcx, string_literal_3
	XOR eax, eax
	CALL printf
	ADD rsp, 40
; -- DUP --
	POP rax
	PUSH rax
	PUSH rax
; -- PRINT --
	POP rdx
	SUB rsp, 40
	LEA rcx, string_literal_4
	XOR eax, eax
	CALL printf
	ADD rsp, 40
; -- PRINT --
	SUB rsp, 40
	LEA rcx, string_literal_5
	XOR eax, eax
	CALL printf
	ADD rsp, 40
; -- PUSH num --
	PUSH 2222
; -- PRINT --
	SUB rsp, 40
	LEA rcx, string_literal_6
	XOR eax, eax
	CALL printf
	ADD rsp, 40
; -- DUP --
	POP rax
	PUSH rax
	PUSH rax
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
; -- SWAP --
	POP rax
	POP rcx
	PUSH rax
	PUSH rcx
; -- PRINT --
	POP rdx
	SUB rsp, 40
	LEA rcx, string_literal_9
	XOR eax, eax
	CALL printf
	ADD rsp, 40
; -- PRINT --
	POP rdx
	SUB rsp, 40
	LEA rcx, string_literal_10
	XOR eax, eax
	CALL printf
	ADD rsp, 40
; -- PRINT --
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
; -- PUSH num --
	PUSH 1111
; -- PRINT --
	SUB rsp, 40
	LEA rcx, string_literal_13
	XOR eax, eax
	CALL printf
	ADD rsp, 40
; -- PUSH num --
	PUSH 2222
; -- PRINT --
	SUB rsp, 40
	LEA rcx, string_literal_14
	XOR eax, eax
	CALL printf
	ADD rsp, 40
; -- DROP --
	ADD rsp, 8
; -- PRINT --
	SUB rsp, 40
	LEA rcx, string_literal_15
	XOR eax, eax
	CALL printf
	ADD rsp, 40
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
; -- PRINT --
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
	PUSH 1
; -- PUSH num --
	PUSH 2
; -- PUSH num --
	PUSH 3
; -- ROT --
	POP rax
	POP rcx
	POP rdx
	PUSH rcx
	PUSH rax
	PUSH rdx
; -- PRINT --
	POP rdx
	SUB rsp, 40
	LEA rcx, string_literal_20
	XOR eax, eax
	CALL printf
	ADD rsp, 40
; -- PRINT --
	POP rdx
	SUB rsp, 40
	LEA rcx, string_literal_21
	XOR eax, eax
	CALL printf
	ADD rsp, 40
; -- PRINT --
	POP rdx
	SUB rsp, 40
	LEA rcx, string_literal_22
	XOR eax, eax
	CALL printf
	ADD rsp, 40
; -- HALT --
	JMP EXIT_LABEL
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
