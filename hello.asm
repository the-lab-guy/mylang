; -- header --
bits 64
default rel
; -- variables --
section .bss
; -- constants --
section .data
string_literal_0 db "Hello World", 0
; -- Entry Point --
section .text
global main
extern ExitProcess
extern printf
extern scanf
          
main:
	PUSH rbp
	MOV rbp, rsp
	SUB rsp, 32
; -- PUSH --
	PUSH 10
; -- PUSH --
	PUSH 7
; -- ADD --
	POP rax
	ADD qword [rsp], rax
; -- PRINT --
	LEA rcx, string_literal_0
	XOR eax, eax
	CALL printf
; -- HALT --
	JMP EXIT_LABEL
EXIT_LABEL:
	XOR rax, rax
	CALL ExitProcess
