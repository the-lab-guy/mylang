; -- header --
bits 64
default rel

; -- variables --
section .bss
read_buffer resq 1  ; 64-bits integer = 8 bytes
AV_FOR_2: resq 1  ; 64-bits integer = 8 bytes
AV_FOR_1: resq 1  ; 64-bits integer = 8 bytes

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

string_literal_0 db `%lld HEY!\n`, 0
string_literal_1 db `    %lld YAY!\n`, 0

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
; -- PUSH num --
	PUSH 1
; -- PUSH num --
	PUSH 3
; -- PUSH num --
	PUSH 1
; -- FOR_1: --
	LEA rcx, AV_FOR_1
	MOV qword[rcx], rsp
	JMP FOR_BEG_1
FOR_1:
	LEA rcx, AV_FOR_1
	MOV rdx, qword[rcx]
	MOV rax, qword[rdx+16]
	ADD rax, qword[rdx]
	MOV qword[rdx+16], rax
FOR_BEG_1:
	LEA rcx, AV_FOR_1
	MOV rdx, qword[rcx]
	MOV rax, qword[rdx+16]
	MOV rcx, qword[rdx]
	AND rcx, rcx
	JNS FOR_POS_1
	CMP rax, qword[rdx+8]
	JL NEXT_1
	JMP FOR_RUN_1
FOR_POS_1:
	CMP rax, qword[rdx+8]
	JG NEXT_1
FOR_RUN_1:
; -- INDEX --
	LEA rcx, AV_FOR_1
	MOV rdx, qword[rcx]
	MOV rax, qword[rdx+16]
	PUSH rax
; -- PRINT --
	POP rdx
	SUB rsp, 40
	LEA rcx, string_literal_0
	XOR eax, eax
	CALL printf
	ADD rsp, 40
; -- PUSH num --
	PUSH 3
; -- PUSH num --
	PUSH 1
; -- PUSH num --
	PUSH -1
; -- FOR_2: --
	LEA rcx, AV_FOR_2
	MOV qword[rcx], rsp
	JMP FOR_BEG_2
FOR_2:
	LEA rcx, AV_FOR_2
	MOV rdx, qword[rcx]
	MOV rax, qword[rdx+16]
	ADD rax, qword[rdx]
	MOV qword[rdx+16], rax
FOR_BEG_2:
	LEA rcx, AV_FOR_2
	MOV rdx, qword[rcx]
	MOV rax, qword[rdx+16]
	MOV rcx, qword[rdx]
	AND rcx, rcx
	JNS FOR_POS_2
	CMP rax, qword[rdx+8]
	JL NEXT_2
	JMP FOR_RUN_2
FOR_POS_2:
	CMP rax, qword[rdx+8]
	JG NEXT_2
FOR_RUN_2:
; -- INDEX --
	LEA rcx, AV_FOR_2
	MOV rdx, qword[rcx]
	MOV rax, qword[rdx+16]
	PUSH rax
; -- PRINT --
	POP rdx
	SUB rsp, 40
	LEA rcx, string_literal_1
	XOR eax, eax
	CALL printf
	ADD rsp, 40
; -- NEXT_2: --
	JMP FOR_2
NEXT_2:
; -- NEXT_1: --
	JMP FOR_1
NEXT_1:
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
