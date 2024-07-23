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

string_literal_0 db ` %f `, 0
string_literal_1 db `\n`, 0
string_literal_2 db ` %f `, 0
string_literal_3 db `\n`, 0
string_literal_4 db ` %f `, 0
string_literal_5 db `\n`, 0

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
; ((10 ^ (3 + 2)) - ((6 * 5) / 3))
; 10 3 2 +  ^  6 5 *  3 /  - 
	MOV rax, __?float64?__(10.0)
	PUSH rax
	MOV rax, __?float64?__(3.0)
	PUSH rax
	MOV rax, __?float64?__(2.0)
	PUSH rax
	POP rax
	MOVQ xmm1, rax
	POP rax
	MOVQ xmm0, rax
	ADDSD xmm0, xmm1
	MOVQ rax, xmm0
	PUSH rax
	POP rax
	MOVQ xmm1, rax
	POP rax
	MOVQ xmm0, rax
	call pow
	MOVQ rax, xmm0
	PUSH rax
	MOV rax, __?float64?__(6.0)
	PUSH rax
	MOV rax, __?float64?__(5.0)
	PUSH rax
	POP rax
	MOVQ xmm1, rax
	POP rax
	MOVQ xmm0, rax
	MULSD xmm0, xmm1
	MOVQ rax, xmm0
	PUSH rax
	MOV rax, __?float64?__(3.0)
	PUSH rax
	POP rax
	MOVQ xmm1, rax
	POP rax
	MOVQ xmm0, rax
	DIVSD xmm0, xmm1
	MOVQ rax, xmm0
	PUSH rax
	POP rax
	MOVQ xmm1, rax
	POP rax
	MOVQ xmm0, rax
	SUBSD xmm0, xmm1
	MOVQ rax, xmm0
	PUSH rax
; -- PRINT --
	POP rdx
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
; -- Expression --
; ((2 ^ 3) / 4)
; 2 3 ^  4 / 
	MOV rax, __?float64?__(2.0)
	PUSH rax
	MOV rax, __?float64?__(3.0)
	PUSH rax
	POP rax
	MOVQ xmm1, rax
	POP rax
	MOVQ xmm0, rax
	call pow
	MOVQ rax, xmm0
	PUSH rax
	MOV rax, __?float64?__(4.0)
	PUSH rax
	POP rax
	MOVQ xmm1, rax
	POP rax
	MOVQ xmm0, rax
	DIVSD xmm0, xmm1
	MOVQ rax, xmm0
	PUSH rax
; -- PRINT --
	POP rdx
	SUB rsp, 32
	LEA rcx, string_literal_2
	XOR eax, eax
	CALL printf
	ADD rsp, 32
; -- PRINT --
	SUB rsp, 32
	LEA rcx, string_literal_3
	XOR eax, eax
	CALL printf
	ADD rsp, 32
; -- Expression --
; ((4 * ((3 + 4) / 2)) - 1)
; 4 3 4 +  2 /  *  1 - 
	MOV rax, __?float64?__(4.0)
	PUSH rax
	MOV rax, __?float64?__(3.0)
	PUSH rax
	MOV rax, __?float64?__(4.0)
	PUSH rax
	POP rax
	MOVQ xmm1, rax
	POP rax
	MOVQ xmm0, rax
	ADDSD xmm0, xmm1
	MOVQ rax, xmm0
	PUSH rax
	MOV rax, __?float64?__(2.0)
	PUSH rax
	POP rax
	MOVQ xmm1, rax
	POP rax
	MOVQ xmm0, rax
	DIVSD xmm0, xmm1
	MOVQ rax, xmm0
	PUSH rax
	POP rax
	MOVQ xmm1, rax
	POP rax
	MOVQ xmm0, rax
	MULSD xmm0, xmm1
	MOVQ rax, xmm0
	PUSH rax
	MOV rax, __?float64?__(1.0)
	PUSH rax
	POP rax
	MOVQ xmm1, rax
	POP rax
	MOVQ xmm0, rax
	SUBSD xmm0, xmm1
	MOVQ rax, xmm0
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
