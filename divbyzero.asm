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

string_literal_0 db "Divide by zero test ", 0
string_literal_1 db "DIV = %lld ", 0

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
; -- PUSH num --
	PUSH 10
; -- PUSH num --
	PUSH 0
; -- DIV --
	POP rcx
	TEST rcx, rcx
	JZ ERROR_LABEL
	POP rax
	CQO  ; sign extend rax into rdx:rax
	IDIV rcx
	PUSH rax
; -- PRINT --
	POP rdx
	SUB rsp, 32
	LEA rcx, string_literal_1
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
