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
string_literal_4 db ` %lld `, 0
string_literal_5 db `\n`, 0
string_literal_6 db `CODE: 33 + %lld =`, 0
string_literal_7 db ` %f `, 0
string_literal_8 db `\n`, 0
string_literal_9 db ` %f `, 0
string_literal_10 db `\n`, 0
string_literal_11 db ` %f `, 0
string_literal_12 db `\n`, 0

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
; ((3 * ((5 + 4) / 2)) - 1)
; 3 5 4 +  2 /  *  1 - 
	MOV rax, __?float64?__(3.0)
	PUSH rax
	MOV rax, __?float64?__(5.0)
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
; -- DUP --
	POP rax
	PUSH rax
	PUSH rax
; -- PRINT --
	POP rdx
	SUB rsp, 40
	LEA rcx, string_literal_0
	XOR eax, eax
	CALL printf
	ADD rsp, 40
; -- PRINT --
	SUB rsp, 40
	LEA rcx, string_literal_1
	XOR eax, eax
	CALL printf
	ADD rsp, 40
; -- FLOOR --
	POP rax
	PXOR xmm0, xmm0
	MOVQ xmm0, rax
	CVTTSD2SI rax, xmm0
	PUSH rax
; -- Expression --
; (((3 / 2) * 24) - (2 ^ 4))
; 3 2 /  24 *  2 4 ^  - 
	MOV rax, __?float64?__(3.0)
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
	MOV rax, __?float64?__(24.0)
	PUSH rax
	POP rax
	MOVQ xmm1, rax
	POP rax
	MOVQ xmm0, rax
	MULSD xmm0, xmm1
	MOVQ rax, xmm0
	PUSH rax
	MOV rax, __?float64?__(2.0)
	PUSH rax
	MOV rax, __?float64?__(4.0)
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
	POP rax
	MOVQ xmm1, rax
	POP rax
	MOVQ xmm0, rax
	SUBSD xmm0, xmm1
	MOVQ rax, xmm0
	PUSH rax
; -- DUP --
	POP rax
	PUSH rax
	PUSH rax
; -- PRINT --
	POP rdx
	SUB rsp, 40
	LEA rcx, string_literal_2
	XOR eax, eax
	CALL printf
	ADD rsp, 40
; -- PRINT --
	SUB rsp, 40
	LEA rcx, string_literal_3
	XOR eax, eax
	CALL printf
	ADD rsp, 40
; -- FLOOR --
	POP rax
	PXOR xmm0, xmm0
	MOVQ xmm0, rax
	CVTTSD2SI rax, xmm0
	PUSH rax
; -- ADD --
	POP rax
	ADD qword [rsp], rax
; -- PRINT --
	POP rdx
	SUB rsp, 40
	LEA rcx, string_literal_4
	XOR eax, eax
	CALL printf
	ADD rsp, 40
; -- PRINT --
	SUB rsp, 40
	LEA rcx, string_literal_5
	XOR eax, eax
	CALL printf
	ADD rsp, 40
; -- Label --
TEST_LABEL:
; -- PUSH num --
	PUSH 33
; -- PUSH expr --
; (3 * 9)
; 3 9 * 
	MOV rax, __?float64?__(3.0)
	PUSH rax
	MOV rax, __?float64?__(9.0)
	PUSH rax
	POP rax
	MOVQ xmm1, rax
	POP rax
	MOVQ xmm0, rax
	MULSD xmm0, xmm1
	MOVQ rax, xmm0
	PUSH rax
; -- FLOOR --
	POP rax
	PXOR xmm0, xmm0
	MOVQ xmm0, rax
	CVTTSD2SI rax, xmm0
	PUSH rax
; -- DUP --
	POP rax
	PUSH rax
	PUSH rax
; -- PRINT --
	POP rdx
	SUB rsp, 40
	LEA rcx, string_literal_6
	XOR eax, eax
	CALL printf
	ADD rsp, 40
; -- ADD --
	POP rax
	ADD qword [rsp], rax
; -- FLOAT --
	POP rax
	PXOR xmm0, xmm0
	CVTSI2SD xmm0, rax
	MOVQ rax, xmm0
	PUSH rax
; -- PRINT --
	POP rdx
	SUB rsp, 40
	LEA rcx, string_literal_7
	XOR eax, eax
	CALL printf
	ADD rsp, 40
; -- PRINT --
	SUB rsp, 40
	LEA rcx, string_literal_8
	XOR eax, eax
	CALL printf
	ADD rsp, 40
; -- Expression --
; (99 + 1)
; 99 1 + 
	MOV rax, __?float64?__(99.0)
	PUSH rax
	MOV rax, __?float64?__(1.0)
	PUSH rax
	POP rax
	MOVQ xmm1, rax
	POP rax
	MOVQ xmm0, rax
	ADDSD xmm0, xmm1
	MOVQ rax, xmm0
	PUSH rax
; -- Expression --
; (70 * 3)
; 70 3 * 
	MOV rax, __?float64?__(70.0)
	PUSH rax
	MOV rax, __?float64?__(3.0)
	PUSH rax
	POP rax
	MOVQ xmm1, rax
	POP rax
	MOVQ xmm0, rax
	MULSD xmm0, xmm1
	MOVQ rax, xmm0
	PUSH rax
; -- Expression --
; (34 / 7)
; 34 7 / 
	MOV rax, __?float64?__(34.0)
	PUSH rax
	MOV rax, __?float64?__(7.0)
	PUSH rax
	POP rax
	MOVQ xmm1, rax
	POP rax
	MOVQ xmm0, rax
	DIVSD xmm0, xmm1
	MOVQ rax, xmm0
	PUSH rax
; -- Expression --
; (33 - 100)
; 33 100 - 
	MOV rax, __?float64?__(33.0)
	PUSH rax
	MOV rax, __?float64?__(100.0)
	PUSH rax
	POP rax
	MOVQ xmm1, rax
	POP rax
	MOVQ xmm0, rax
	SUBSD xmm0, xmm1
	MOVQ rax, xmm0
	PUSH rax
; -- Expression --
; (2 ^ (3 + 2))
; 2 3 2 +  ^ 
	MOV rax, __?float64?__(2.0)
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
	SUB rsp, 40
	call pow
	ADD rsp, 40
	MOVQ rax, xmm0
	PUSH rax
; -- PRINT --
	POP rdx
	SUB rsp, 40
	LEA rcx, string_literal_9
	XOR eax, eax
	CALL printf
	ADD rsp, 40
; -- PRINT --
	SUB rsp, 40
	LEA rcx, string_literal_10
	XOR eax, eax
	CALL printf
	ADD rsp, 40
; -- Expression --
; (22 / 7)
; 22 7 / 
	MOV rax, __?float64?__(22.0)
	PUSH rax
	MOV rax, __?float64?__(7.0)
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
	SUB rsp, 40
	LEA rcx, string_literal_11
	XOR eax, eax
	CALL printf
	ADD rsp, 40
; -- PRINT --
	SUB rsp, 40
	LEA rcx, string_literal_12
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
