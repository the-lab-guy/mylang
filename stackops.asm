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
F_NUMB  db "%lld", 0
F_STR   db "%s", 0

string_literal_0 db "Stack operations test ", 0
string_literal_1 db "", 0
string_literal_2 db "Pushing 1111 ", 0
string_literal_3 db "Duplicating Top ", 0
string_literal_4 db "Top: %lld ", 0
string_literal_5 db "Pushing 2222 ", 0
string_literal_6 db "Duplicating Top ", 0
string_literal_7 db "New Top: %lld ", 0
string_literal_8 db "Swapping top two values ", 0
string_literal_9 db "Top was: %lld ", 0
string_literal_10 db "Second was: %lld ", 0

; -- Entry Point --
section .text
global main
extern ExitProcess
extern printf
extern scanf

main:
	PUSH rbp
	MOV rbp, rsp
; -- PRINT --
	SUB rsp, 32
	LEA rcx, string_literal_0
	XOR eax, eax
	CALL printf
	ADD rsp, 32
; -- PRINT --
	SUB rsp, 32
	LEA rcx, string_literal_1
	XOR eax, eax
	CALL printf
	ADD rsp, 32
; -- PRINT --
	SUB rsp, 32
	LEA rcx, string_literal_2
	XOR eax, eax
	CALL printf
	ADD rsp, 32
; -- PUSH num --
	PUSH 1111
; -- PRINT --
	SUB rsp, 32
	LEA rcx, string_literal_3
	XOR eax, eax
	CALL printf
	ADD rsp, 32
; -- DUP --
	POP rax
	PUSH rax
	PUSH rax
; -- PRINT --
	POP rdx
	SUB rsp, 32
	LEA rcx, string_literal_4
	XOR eax, eax
	CALL printf
	ADD rsp, 32
; -- PRINT --
	SUB rsp, 32
	LEA rcx, string_literal_5
	XOR eax, eax
	CALL printf
	ADD rsp, 32
; -- PUSH num --
	PUSH 2222
; -- PRINT --
	SUB rsp, 32
	LEA rcx, string_literal_6
	XOR eax, eax
	CALL printf
	ADD rsp, 32
; -- DUP --
	POP rax
	PUSH rax
	PUSH rax
; -- PRINT --
	POP rdx
	SUB rsp, 32
	LEA rcx, string_literal_7
	XOR eax, eax
	CALL printf
	ADD rsp, 32
; -- PRINT --
	SUB rsp, 32
	LEA rcx, string_literal_8
	XOR eax, eax
	CALL printf
	ADD rsp, 32
; -- SWAP --
	POP rax
	POP rcx
	PUSH rax
	PUSH rcx
; -- PRINT --
	POP rdx
	SUB rsp, 32
	LEA rcx, string_literal_9
	XOR eax, eax
	CALL printf
	ADD rsp, 32
; -- PRINT --
	POP rdx
	SUB rsp, 32
	LEA rcx, string_literal_10
	XOR eax, eax
	CALL printf
	ADD rsp, 32
; -- HALT --
	JMP EXIT_LABEL
ERROR_LABEL:
; -- ERROR --
	SUB rsp, 32
	LEA rcx, E_DIV0
	XOR eax, eax
	CALL printf
	ADD rsp, 32
EXIT_LABEL:
	XOR rax, rax
	CALL ExitProcess
