; -- header --
bits 64
default rel

; -- variables --
section .bss
read_number resq 1  ; 64-bits integer = 8 bytes

; -- constants --
section .data
read_format db "%lld", 0  ; the 64-bit format string for scanf
string_literal_0 db "Enter an integer: ", 0
string_literal_1 db "Error! ", 0
string_literal_2 db "Zero ", 0
string_literal_3 db "Negative ", 0
string_literal_4 db "Positive ", 0

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
; -- JUMP.EQ.0 --
	CMP qword [rsp], 0
	JE Zero
; -- JUMP.LT.0 --
	CMP qword [rsp], 0
	JL LessThan
; -- JUMP.GT.0 --
	CMP qword [rsp], 0
	JG GreaterThan
; -- PRINT --
	SUB rsp, 32
	LEA rcx, string_literal_1
	XOR eax, eax
	CALL printf
	ADD rsp, 32
; -- HALT --
	JMP EXIT_LABEL
; -- Label --
Zero:
; -- PRINT --
	SUB rsp, 32
	LEA rcx, string_literal_2
	XOR eax, eax
	CALL printf
	ADD rsp, 32
; -- HALT --
	JMP EXIT_LABEL
; -- Label --
LessThan:
; -- PRINT --
	SUB rsp, 32
	LEA rcx, string_literal_3
	XOR eax, eax
	CALL printf
	ADD rsp, 32
; -- HALT --
	JMP EXIT_LABEL
; -- Label --
GreaterThan:
; -- PRINT --
	SUB rsp, 32
	LEA rcx, string_literal_4
	XOR eax, eax
	CALL printf
	ADD rsp, 32
; -- HALT --
	JMP EXIT_LABEL
EXIT_LABEL:
	XOR rax, rax
	CALL ExitProcess
