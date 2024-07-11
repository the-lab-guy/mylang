; -- header --
bits 64
default rel

; -- variables --
section .bss
read_number resq 1  ; 64-bits integer = 8 bytes
my_int resq 1  ; 64-bits integer = 8 bytes

; -- constants --
section .data
read_format db "%lld", 0  ; the 64-bit format string for scanf
string_literal_0 db "enter an integer", 0
string_literal_1 db "plus 10 = %lld", 0

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
; -- LET --
	LEA rcx, my_int
	POP rax
	MOV [rcx], rax
; -- PUSH --
	PUSH 10
; -- GET --
	LEA rcx, my_int
	MOV rax, [rcx]
	PUSH rax
; -- ADD --
	POP rax
	ADD qword [rsp], rax
; -- LET --
	LEA rcx, my_int
	POP rax
	MOV [rcx], rax
; -- GET --
	LEA rcx, my_int
	MOV rax, [rcx]
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
EXIT_LABEL:
	XOR rax, rax
	CALL ExitProcess
