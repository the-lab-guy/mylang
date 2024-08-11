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
F_NUMB  db "%lld", 0
F_STR   db "%s", 0
F_FLOAT db "%f", 0

string_literal_0 db `%f \n`, 0
string_literal_1 db `%f \n`, 0
string_literal_2 db `%f \n`, 0
string_literal_3 db `%f \n`, 0

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
; -- Expression --
; (2 ^ 2)
; 2 2 ^ 
	MOV rax, __?float64?__(2.0)
	PUSH rax
	MOV rax, __?float64?__(2.0)
	PUSH rax
	POP rax
	MOVQ xmm1, rax
	POP rax
	MOVQ xmm0, rax
	SUB rsp, 40
	call pow
	ADD rsp, 40
	MOVQ rax, xmm0
	PUSH rax
; -- PRINT --
	POP rdx
	SUB rsp, 40
	LEA rcx, string_literal_0
	XOR eax, eax
	CALL printf
	ADD rsp, 40
; -- Expression --
; (-2 ^ 2)
; -2 2 ^ 
	MOV rax, __?float64?__(-2.0)
	PUSH rax
	MOV rax, __?float64?__(2.0)
	PUSH rax
	POP rax
	MOVQ xmm1, rax
	POP rax
	MOVQ xmm0, rax
	SUB rsp, 40
	call pow
	ADD rsp, 40
	MOVQ rax, xmm0
	PUSH rax
; -- PRINT --
	POP rdx
	SUB rsp, 40
	LEA rcx, string_literal_1
	XOR eax, eax
	CALL printf
	ADD rsp, 40
; -- Expression --
; (2 ^ -2)
; 2 -2 ^ 
	MOV rax, __?float64?__(2.0)
	PUSH rax
	MOV rax, __?float64?__(-2.0)
	PUSH rax
	POP rax
	MOVQ xmm1, rax
	POP rax
	MOVQ xmm0, rax
	SUB rsp, 40
	call pow
	ADD rsp, 40
	MOVQ rax, xmm0
	PUSH rax
; -- PRINT --
	POP rdx
	SUB rsp, 40
	LEA rcx, string_literal_2
	XOR eax, eax
	CALL printf
	ADD rsp, 40
; -- Expression --
; (-2 ^ -2)
; -2 -2 ^ 
	MOV rax, __?float64?__(-2.0)
	PUSH rax
	MOV rax, __?float64?__(-2.0)
	PUSH rax
	POP rax
	MOVQ xmm1, rax
	POP rax
	MOVQ xmm0, rax
	SUB rsp, 40
	call pow
	ADD rsp, 40
	MOVQ rax, xmm0
	PUSH rax
; -- PRINT --
	POP rdx
	SUB rsp, 40
	LEA rcx, string_literal_3
	XOR eax, eax
	CALL printf
	ADD rsp, 40
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
