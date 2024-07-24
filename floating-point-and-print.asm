; -- header --
bits 64
default rel

; -- variables --
section .bss
read_buffer resq 1  ; 64-bits integer = 8 bytes

; -- constants --
section .data
F_FLOAT db "%f", 10, 0
pi dq 3.14159
two dq 2.0


; -- Entry Point --
section .text
global main
extern ExitProcess
extern printf
extern scanf

main:
	PUSH rbp
	MOV rbp, rsp
; -- Expression 2*2 --
	MOVSD xmm0, qword [two]
	MOVSD xmm1, qword [pi]
	MULSD xmm0, xmm1
	MOVQ rdx, xmm0
; -- PRINT --
	SUB rsp, 32
	LEA rcx, F_FLOAT
	XOR rax, rax
	CALL printf
	ADD rsp, 32
; -- EXIT --	
	XOR rax, rax
	CALL ExitProcess
