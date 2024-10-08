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

string_literal_0 db `Enter an integer: `, 0
string_literal_1 db `\n`, 0
string_literal_2 db `Error 1 `, 0
string_literal_3 db `Zero `, 0
string_literal_4 db `Negative `, 0
string_literal_5 db `Positive `, 0
string_literal_6 db `Not Zero `, 0
string_literal_7 db `Error 2 `, 0

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
; -- PRINT --
	SUB rsp, 40
	LEA rcx, string_literal_1
	XOR eax, eax
	CALL printf
	ADD rsp, 40
; -- JUMP.EQ.0 --
	CMP qword [rsp], 0
	JE Zero
; -- JUMP.NE.0 --
	CMP qword [rsp], 0
	JNE NotZero
; -- PRINT --
	SUB rsp, 40
	LEA rcx, string_literal_2
	XOR eax, eax
	CALL printf
	ADD rsp, 40
; -- HALT --
	JMP EXIT_LABEL
; -- Label --
Zero:
; -- PRINT --
	SUB rsp, 40
	LEA rcx, string_literal_3
	XOR eax, eax
	CALL printf
	ADD rsp, 40
; -- HALT --
	JMP EXIT_LABEL
; -- Label --
LessThan:
; -- PRINT --
	SUB rsp, 40
	LEA rcx, string_literal_4
	XOR eax, eax
	CALL printf
	ADD rsp, 40
; -- HALT --
	JMP EXIT_LABEL
; -- Label --
GreaterThan:
; -- PRINT --
	SUB rsp, 40
	LEA rcx, string_literal_5
	XOR eax, eax
	CALL printf
	ADD rsp, 40
; -- HALT --
	JMP EXIT_LABEL
; -- Label --
NotZero:
; -- PRINT --
	SUB rsp, 40
	LEA rcx, string_literal_6
	XOR eax, eax
	CALL printf
	ADD rsp, 40
; -- JUMP.LT.0 --
	CMP qword [rsp], 0
	JL LessThan
; -- JUMP.GT.0 --
	CMP qword [rsp], 0
	JG GreaterThan
; -- PRINT --
	SUB rsp, 40
	LEA rcx, string_literal_7
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
