# Mylang Documentation  

## Commands  
### Stack Operations
- `PUSH n`  
	+ The number 'n' is pushed (placed) on to the top of the Stack.
- `PUSH variable_name`
	+ The number stored in the named_variable's storage space is __copied__ on to the top of the Stack.
- `POP variable_name`  
	+ The number on top of the Stack is __removed__ and stored in the named variable's storage space.
- `DUP`  
	+ Duplicates the number at the top of the Stack, creating two identical entries.
- `SWAP`  
	+ Exchanges the number on top of the Stack with the number immediately below it.
- `DROP`  
	+ Removes the number at the top of the Stack.

### Input / Output Operations
- `PRINT "formatted_literal_string"`  
	+ Any text between the double-quotes will be displayed on the screen.  
	If the text contains the special character sequence `@#`, the `@#` will be replaced with the number on the top of the Stack. e.g. if 10 is on the Stack, then `PRINT "@# apples"` would display "10 apples" on the screen.  
	Including the sequence __`\n`__ in your text will output a 'newline' action.  
	Including the sequence __`\t`__ in your text will output a 'tab' action.
- `PRINT`  
	+ Displays nothing and moves to a new line on the display.
- `READ`  
	+ Pause the program and allow the user to enter data, until [Enter] is pressed. The data is left on the Stack and the program execution resumes.

### Integer Arithmetic Operations
- `ADD`  
	+ Remove two numbers from the top of the Stack, add them together and put the result on top of the Stack.
- `SUB`  
	+ Remove two numbers from the top of the Stack, subtract the first number from the second and put the result on top of the Stack.
- `MUL`  
	+ Remove two numbers from the top of the Stack, multiply them together and put the result on top of the Stack.
- `DIV`  
	+ Remove two numbers from the top of the Stack, divide the second number by the first and put the result on top of the Stack. 
- `NEG`  
	+ Negate the number on top of the Stack, thus changing a positive value into a negative one, and vice-versa. Performs a logical Two's Complement operation.

### Type Conversions
- `FLOOR`  
	+ Convert the floating-point number at the top of the Stack to an integer by rounding down towards zero. The result will replace the previous value on top of the Stack.
	+ _Note:_ If the value on the top of the Stack is not in floating-point format, the result will be undefined.

- `FLOAT`  
	+ Convert the integer number at the top of the Stack to floating-point format, leaving the result on the top of the Stack.
	+ _Note:_ If the value on the top of the Stack is not an integer, the result will be undefined.

### Logic Operations
- `AND`  
	+ Logical AND. Performs a bitwise AND operation on the two values at the top of the stack and replaces them with the result.
- `OR`  
	+ Logical OR. Performs a bitwise OR operation on the two values at the top of the stack and replaces them with the result.
- `XOR`  
	+ Logical XOR. Performs a bitwise XOR operation on the two values at the top of the stack and replaces them with the result.
- `NOT`  
	+ Logical NOT - One's Complement. Performs a bitwise NOT operation on the value at the top of the Stack and replaces it with the result.
- `TRUE`  
	+ Places the constant for logical True on the Stack. This is a number with all of the binary digits set to '1'. It is equivalent to the integer number '-1'.  
	__Note__: all non-zero numbers can be considered as representing True.  
- `FALSE`  
	+ Places the constant for logical False on the Stack. This is a number with all of the binary digits set to '0'. It is equivalent to the integer number '0'.  

### Control Flow
- `HALT`  
	+ End program execution and return control to the operating system.
- `JUMP.EQ.0 label_name`  
	+ If the value on top of the Stack is zero, then program execution continues from the instruction after label_name
- `JUMP.GT.0 label_name`  
	+ If the value on top of the Stack is positive, then program execution continues from the instruction after label_name
- `JUMP.LT.0 label_name`  
	+ If the value on top of the Stack is negative, then program execution continues from the instruction after label_name
- `JUMP.NE.0 label_name`  
	+ If the value on top of the Stack is _NOT_ zero, then program execution continues from the instruction after label_name

- `IF ... ELSE ... THEN`  
	+ These instruction combine to form a __Statement Block__. Upon _entering_ this stement block at the `IF` statement, the number at the top of the Stack is removed and tested for `TRUE` or `FALSE`. If it is `TRUE`, the code immediately following the `IF` is executed, up to the `ELSE` statement. If it was `FALSE` the code immediately following the `ELSE` is executed, up to the `THEN` statement.  
	In both cases, execution then continues with the code immediately following the `THEN` statement.  
	```
	IF
	  PRINT "Condition was True\n"
	ELSE
	  PRINT "Condition was False\n"
	THEN
	PRINT "Done\n"
	```
	_Example of an IF...ELSE...THEN statement block._  

	+ Additionally, the `ELSE` clause can be omitted when a block of code needs to be executed _only_ if the condition is `TRUE`, and alternate action is necessary.
	```
	IF
	  PRINT "Condition was True\n"
	THEN
	PRINT "Done\n"
	```
	_Example of an IF...THEN statement block._  

<br />
<br />
- `REPEAT ... UNTIL`  
	+ __TODO__
- `WHILE ... LOOP`  
	+ __TODO__
- `FOR ... NEXT`  
	+ __TODO__
- `TO ... DONE`  
	+ __TODO__

***
## Labels  
- `Label_name:` __TODO__

***
## Mathematical Expression Evaluation  
The use of simple mathematical expressions are supported in the language. The following operations are currently implemented:

- `^` raise to a power, e.g. 3^2 is equivalent to 9
- `*` multiplication, e.g. 3*2 is equivalent to 6
- `/` division, e.g. 8/2 is equivalent to 4
- `+` addition, e.g. 3+4 is equivalent to 7
- `-` subtraction, e.g. 7-3 is equivalent to 4
- `( )` parentheses to increase the precedence (or order) of operations,   
	+ e.g. 2 * (3 + 1) is equivalent to 2 * 4, or 8. Whereas, without parentheses  
	we get 2 * 3 + 1 being equivalent to 6 + 1, or 7

An expression may be entered on its own on a single line in the program, or it may be passed as a parameter to the PUSH instruction.
In both cases, the result of evaluating the expression will be placed on the top of the stack, ready for use in the program.

__NOTE:__ all operations in the operation are floating-point based. Therefore, the result returned on the Stack will always be a floating-point value. Any operations that you do with this number in your program, must be able to handle floating-point numbers.

The commands `FLOAT` and `FLOOR` are available to convert the number at the top of the Stack to a floating-point representation or an integer representation, respectively.



A valid expression _must_ consist of at least three elements, the _left_hand_term_, the _operation_, and the _right_hand_term_. Parentheses are optional.  

### Some examples of expressions  

`4 * ((3 + 4) / 2) - 1`  
Top of Stack = 13.0  

`PUSH 9 * 3`  
Top of Stack = 27.0

```
2^2
PRINT "Exp: 4.0 Act: @$ \n"
-2^2
PRINT "Exp: 4.0 Act: @$ \n"
2^-2
PRINT "Exp: 0.25 Act: @$ \n"
-2^-2
PRINT "Exp: 0.25 Act: @$ \n"
```


***

### Notes  
*Emphasized* _Emphasized_  
**Strong** __Strong__

```
10 PRINT "HELLO WORLD!"  
20 GOTO 10
```

A sentence followed by  
a newline.

#### Future Ideas
1. optimise string constants in assembly  
1. optimise numeric constants in assembly  
1. implement control flow and loop statements  
1. variables in expressions  
1. opcode for raise to power function  
1. opcodes for floating-point arithmetic  
1. optimise repeating code blocks in assembly
1. 