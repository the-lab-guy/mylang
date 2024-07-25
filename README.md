# Mylang Documentation  

## Commands  
- `PUSH n`  
	+ The number 'n' is pushed (placed) on to the top of the Stack.
- `PUSH variable_name`
	+ The number stored in the named_variable's storage space is __copied__ on to the top of the Stack.
- `POP variable_name`  
	+ The number on top of the Stack is __removed__ and stored in the named variable's storage space.
- `PRINT "formatted_literal_string"`  
	+ Any text between the double-quotes will be displayed on the screen.  
	If the text contains the special character sequence `@#`, the `@#` will be replaced with the number on the top of the Stack. e.g. if 10 is on the Stack, then `PRINT "@# apples"` would display "10 apples" on the screen.  
	Including the sequence __`\n`__ in your text will output a 'newline' action.  
	Including the sequence __`\t`__ in your text will output a 'tab' action.
- `PRINT`  
	+ Displays nothing and moves to a new line on the display.
- `READ`  
	+ Pause the program and allow the user to enter data, until [Enter] is pressed. The data is left on the Stack and the program execution resumes.
- `ADD`  
	+ Remove two numbers from the top of the Stack, add them together and put the result on top of the Stack.
- `SUB`  
	+ Remove two numbers from the top of the Stack, subtract the first number from the second and put the result on top of the Stack.
- `MUL`  
	+ Remove two numbers from the top of the Stack, multiply them together and put the result on top of the Stack.
- `DIV`  
	+ Remove two numbers from the top of the Stack, divide the second number by the first and put the result on top of the Stack. 
- `DUP`  
	+ Duplicates the number at the top of the Stack, creating two identical entries.
- `SWAP`  
	+ Exchanges the number on top of the Stack with the number immediately below it.

- `FLOOR`  
	+ Convert the floating-point number at the top of the Stack to an integer by rounding down. The result will replace the top of the Stack.
	+ _Note:_ If the value on the top of the Stack is not in floating-point format, the result will be undefined.

- `FLOAT`  
	+ Convert the integer number at the top of the Stack to floating-point format, leaving the result on the top of the Stack.
	+ _Note:_ If the value on the top of the Stack is not an integer, the result will be undefined.

- `JUMP.EQ.0 label_name`  
	+ If the value on top of the Stack is zero, then program execution continues from the instruction after label_name
- `JUMP.GT.0 label_name`  
	+ If the value on top of the Stack is positive, then program execution continues from the instruction after label_name
- `JUMP.LT.0 label_name`  
	+ If the value on top of the Stack is negative, then program execution continues from the instruction after label_name
- `JUMP.NE.0 label_name`  
	+ If the value on top of the Stack is _NOT_ zero, then program execution continues from the instruction after label_name
- `HALT`  
	+ End program execution and return control to the operating system.

## Labels  
- `Label_name:` blah


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

