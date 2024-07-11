; -- header --
bits 64
default rel

; -- variables --
section .bss
read_number resq 1  ; 64-bits integer = 8 bytes

; -- constants --
section .data
read_format db "%lld", 0  ; the 64-bit format string for scanf
string_literal_0 db "not equal", 0
string_literal_1 db "equal", 0

; -- Entry Point --
section .text
global main
extern ExitProcess
extern printf
extern scanf

main:
	PUSH rbp
	MOV rbp, rsp
; -- READ --
	SUB rsp, 32
	LEA rcx, read_format
	LEA rdx, read_number
	XOR eax, eax
	MOV [rdx], eax
	CALL scanf
	ADD rsp, 32
	PUSH qword [read_number]
; -- READ --
	SUB rsp, 32
	LEA rcx, read_format
	LEA rdx, read_number
	XOR eax, eax
	MOV [rdx], eax
	CALL scanf
	ADD rsp, 32
	PUSH qword [read_number]
; -- SUB --
	POP rax
	SUB qword [rsp], rax
; -- JUMP.EQ.0 --
	CMP qword [rsp], 0
	JE L1
; -- PRINT --
	SUB rsp, 32
	LEA rcx, string_literal_0
	XOR eax, eax
	CALL printf
	ADD rsp, 32
; -- HALT --
	JMP EXIT_LABEL
; -- Label --
L1:
; -- PRINT --
	SUB rsp, 32
	LEA rcx, string_literal_1
	XOR eax, eax
	CALL printf
	ADD rsp, 32
; -- HALT --
	JMP EXIT_LABEL
EXIT_LABEL:
	XOR rax, rax
	CALL ExitProcess
