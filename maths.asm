; -- header --
bits 64
default rel

; -- variables --
section .bss
read_number resq 1  ; 64-bits integer = 8 bytes
first_int resq 1  ; 64-bits integer = 8 bytes
second_int resq 1  ; 64-bits integer = 8 bytes

; -- constants --
section .data
error_div_by_zero db "Error: Division by zero", 0
read_format db "%lld", 0  ; the 64-bit format string for scanf
string_literal_0 db "Enter an integer: ", 0
string_literal_1 db "Enter another integer: ", 0
string_literal_2 db "ADD = %lld ", 0
string_literal_3 db "SUB = %lld ", 0
string_literal_4 db "MUL = %lld ", 0
string_literal_5 db "DIV = %lld ", 0
string_literal_6 db "Cannot divide by zero ", 0

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
; -- READ --
	SUB rsp, 32
	LEA rcx, read_format
	LEA rdx, read_number
	XOR eax, eax
	MOV [rdx], eax
	CALL scanf
	ADD rsp, 32
	PUSH qword [read_number]
; -- PUT --
	LEA rcx, first_int
	POP rax
	MOV [rcx], rax
; -- PRINT --
	SUB rsp, 32
	LEA rcx, string_literal_1
	XOR eax, eax
	CALL printf
	ADD rsp, 32
; -- READ --
	SUB rsp, 32
	LEA rcx, read_format
	LEA rdx, read_number
	XOR eax, eax
	MOV [rdx], eax
	CALL scanf
	ADD rsp, 32
	PUSH qword [read_number]
; -- PUT --
	LEA rcx, second_int
	POP rax
	MOV [rcx], rax
; -- GET --
	LEA rcx, first_int
	MOV rax, [rcx]
	PUSH rax
; -- GET --
	LEA rcx, second_int
	MOV rax, [rcx]
	PUSH rax
; -- ADD --
	POP rax
	ADD qword [rsp], rax
; -- PRINT --
	POP rdx
	SUB rsp, 32
	LEA rcx, string_literal_2
	XOR eax, eax
	CALL printf
	ADD rsp, 32
; -- GET --
	LEA rcx, first_int
	MOV rax, [rcx]
	PUSH rax
; -- GET --
	LEA rcx, second_int
	MOV rax, [rcx]
	PUSH rax
; -- SUB --
	POP rax
	SUB qword [rsp], rax
; -- PRINT --
	POP rdx
	SUB rsp, 32
	LEA rcx, string_literal_3
	XOR eax, eax
	CALL printf
	ADD rsp, 32
; -- GET --
	LEA rcx, first_int
	MOV rax, [rcx]
	PUSH rax
; -- GET --
	LEA rcx, second_int
	MOV rax, [rcx]
	PUSH rax
; -- MUL --
	POP rax
	POP rdx
	IMUL rdx, rax
	PUSH rdx
; -- PRINT --
	POP rdx
	SUB rsp, 32
	LEA rcx, string_literal_4
	XOR eax, eax
	CALL printf
	ADD rsp, 32
; -- GET --
	LEA rcx, first_int
	MOV rax, [rcx]
	PUSH rax
; -- GET --
	LEA rcx, second_int
	MOV rax, [rcx]
	PUSH rax
; -- JUMP.EQ.0 --
	CMP qword [rsp], 0
	JE DivByZero
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
	LEA rcx, string_literal_5
	XOR eax, eax
	CALL printf
	ADD rsp, 32
; -- HALT --
	JMP EXIT_LABEL
; -- Label --
DivByZero:
; -- PRINT --
	SUB rsp, 32
	LEA rcx, string_literal_6
	XOR eax, eax
	CALL printf
	ADD rsp, 32
; -- POP --
	POP rax
; -- HALT --
	JMP EXIT_LABEL
ERROR_LABEL:
; -- ERROR --
	SUB rsp, 32
	LEA rcx, error_div_by_zero
	XOR eax, eax
	CALL printf
	ADD rsp, 32
EXIT_LABEL:
	XOR rax, rax
	CALL ExitProcess
