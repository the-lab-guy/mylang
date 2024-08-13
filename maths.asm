; -- header --
bits 64
default rel

; -- variables --
section .bss
read_buffer resq 1  ; 64-bits integer = 8 bytes
second_int resq 1  ; 64-bits integer = 8 bytes
first_int resq 1  ; 64-bits integer = 8 bytes

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

string_literal_0 db `Enter an integer: `, 0
string_literal_1 db `Enter another integer: `, 0
string_literal_2 db `ADD = %lld `, 0
string_literal_3 db `SUB = %lld `, 0
string_literal_4 db `MUL = %lld `, 0
string_literal_5 db `DIV = %lld `, 0
string_literal_6 db `\nNEG first int = %lld \n`, 0
string_literal_7 db `NEG second int = %lld \n`, 0
string_literal_8 db `Cannot divide by zero `, 0

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
; -- PRINT --
	SUB rsp, 40
	LEA rcx, string_literal_0
	XOR eax, eax
	CALL printf
	ADD rsp, 40
; -- READ --
	SUB rsp, 40
	LEA rcx, F_NUMB
	LEA rdx, read_buffer
	XOR eax, eax
	MOV [rdx], eax
	CALL scanf
	ADD rsp, 40
	PUSH qword [read_buffer]
; -- POP --
	LEA rcx, first_int
	POP rax
	MOV [rcx], rax
; -- PRINT --
	SUB rsp, 40
	LEA rcx, string_literal_1
	XOR eax, eax
	CALL printf
	ADD rsp, 40
; -- READ --
	SUB rsp, 40
	LEA rcx, F_NUMB
	LEA rdx, read_buffer
	XOR eax, eax
	MOV [rdx], eax
	CALL scanf
	ADD rsp, 40
	PUSH qword [read_buffer]
; -- POP --
	LEA rcx, second_int
	POP rax
	MOV [rcx], rax
; -- PUSH var --
	LEA rcx, first_int
	MOV rax, [rcx]
	PUSH rax
; -- PUSH var --
	LEA rcx, second_int
	MOV rax, [rcx]
	PUSH rax
; -- ADD --
	POP rax
	ADD qword [rsp], rax
; -- PRINT --
	POP rdx
	SUB rsp, 40
	LEA rcx, string_literal_2
	XOR eax, eax
	CALL printf
	ADD rsp, 40
; -- PUSH var --
	LEA rcx, first_int
	MOV rax, [rcx]
	PUSH rax
; -- PUSH var --
	LEA rcx, second_int
	MOV rax, [rcx]
	PUSH rax
; -- SUB --
	POP rax
	SUB qword [rsp], rax
; -- PRINT --
	POP rdx
	SUB rsp, 40
	LEA rcx, string_literal_3
	XOR eax, eax
	CALL printf
	ADD rsp, 40
; -- PUSH var --
	LEA rcx, first_int
	MOV rax, [rcx]
	PUSH rax
; -- PUSH var --
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
	SUB rsp, 40
	LEA rcx, string_literal_4
	XOR eax, eax
	CALL printf
	ADD rsp, 40
; -- PUSH var --
	LEA rcx, first_int
	MOV rax, [rcx]
	PUSH rax
; -- PUSH var --
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
	SUB rsp, 40
	LEA rcx, string_literal_5
	XOR eax, eax
	CALL printf
	ADD rsp, 40
; -- PUSH var --
	LEA rcx, second_int
	MOV rax, [rcx]
	PUSH rax
; -- NEG --
	NEG qword [rsp]
; -- PUSH var --
	LEA rcx, first_int
	MOV rax, [rcx]
	PUSH rax
; -- NEG --
	NEG qword [rsp]
; -- PRINT --
	POP rdx
	SUB rsp, 40
	LEA rcx, string_literal_6
	XOR eax, eax
	CALL printf
	ADD rsp, 40
; -- PRINT --
	POP rdx
	SUB rsp, 40
	LEA rcx, string_literal_7
	XOR eax, eax
	CALL printf
	ADD rsp, 40
; -- HALT --
	JMP EXIT_LABEL
; -- Label --
DivByZero:
; -- PRINT --
	SUB rsp, 40
	LEA rcx, string_literal_8
	XOR eax, eax
	CALL printf
	ADD rsp, 40
; -- DROP --
	ADD rsp, 8
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
