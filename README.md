# About

This is a compiler for a custom programming language written entirely in 16-bit assembly ([TASM](https://en.wikipedia.org/wiki/Turbo_Assembler) ideal mode) for a school project.

It is not intended for actual use.

# Building
In order to build and run the project, you need to download a DOS emulator (I used and will provide instructions for [DOSBox](https://www.dosbox.com/)) as well as TASM.

Clone the project, and copy the contents of the folder that contains the `TASM.EXE` file into its root.<br>
In DOSBox, enter the following commands:<br>
`mount c: the\absolute\path\to\the\project`<br>
`c:`<br>
`cycles = max`<br>
`compile`<br>
(If you don't close DOSBox after doing this once, you only need to run `compile` to execute the program.)

You will now be asked which file you want to compile. Enter its path (for example, `samples\hello.al`)

If there were any errors, the program will at this point exit with an error message and a line number.

If you encounter an infinite flashing cursor on a black screen with no ability to receive any input, it means the compiler ran out of memory (we only have 64 kilobytes to work with!). Reduce the file length.

If everything completed successfully, you will be asked where to store the output file (for example, `hello.asm`).

You should be able to successfully assemble and run it using:<br>
`tasm <your_output>`<br>
`tlink <your_output>.obj`<br>
`<your_output>.exe`

# The language
There is support of the following constructs:
- Functions that can take arguments and return values
- Global and local variables
- If statements
- While loops
- Simple integer math
- Static typing (beware that there is no casting!)
- Inline assembly code

The syntax is similar to C, with python's type hints, but it requires you to announce what you are doing before actually doing it, so, for example:<br>
`my_variable = 5;` would be `set my_variable = 5;` and<br>
`my_function();` would be `call my_function();`

note that when using a function's return value, `call` should not be used, it should instead be called normally (for example: `call print(get_value());` and `set value = get_value();`

a list of all commands and examples:
- set - `set my_var = 5;`
- call - `call my_func(3, false);`
- return - `return;` in a void function or `return value;` in a non-void function.
- break (inside of a loop) - `break;`
- continue (inside of a loop) - `continue;`
- math (calculate simple arithmetic) - `math result = 3 + my_func();`. Supports `+`, `-`, `*`, `/`, `%`.
- inc (increment) - `inc my_var;`
- dec (decrement) - `dec my_var;`

To leave a comment, prefix the line with `//`, for example `// my comment.` The comment will end at the end of the line.

## Functions
In order to declare a function, type (outside of any other functions):<br>
```
func my_function_name(param1: type, param2: type, ...) -> return_type
{
    // function code
}
```
Possible types are: int, bool, char, string, and void for return_type.<br>
For example:
```
func cool_function(a: int, b: bool, c: int) -> string
{
    // this function does cool stuff!
}
```

Every project must have a function with the following signature: `func main() -> int`. It is the function that will be executed when you run the project.

## If statements
An if statement is as follows:
```
if val1 operator val2
{
    // stuff
}
else
{
    // other stuff
}
```
The else block is optional. possible operators are: `<`, `>`, `==`, `!=`, `<=`, `>=`.<br>
For example:
```
if my_func(3) < 3
{
    call do_something();
}
else
{
    call do_something_else();
}
```
Note that braces are mandatory, even if there is only one line, that parentheses around the condition are not supported, that else if does not exist (use `else { if condition {} }` instead of `else if condition {}`), and that you _can't_ chain conditions using logical operators (for example, &&).

## While loops

For a while loop:
```
while val1 operator val2
{
    // stuff
}
```
Read the section about if statements for an explanation about `val1 operator val2`.<br>
You can use `continue` and `break` inside of a loop.

## Inline assembly

In order to put inline assembly in your program:
```
asm("my assembly code");
```
For example, `asm("mov ax, 5");`

## Global variables
To declare global variables, put anywhere in your program:
```
data
{
    var my_var_name: type;
    var my_second_var_name: type;
    ...
}
```
They cannot have default values.<br>
Possible types are: `int`, `bool`, `char`, `string`.

## Local variables
To declare a local variable, type inside of a function:
```
local my_var_name: type;
```
or
```
local my_var_name: type = value;
```
Locals are local to a specific function, _not_ to a set of curly braces like in most languages. This fact was an oversight rather than an intended feature or a limitation, and may get changed in the future.

There exists a rather serious bug in which declaring a local inside of a false if statement or a while loop that gets executed any number of times other than 1 will cause your final program to crash.

# The standard library
Every file you write has access to these functions:
- func print(s: string) -> void -- prints a string to the standard output.
- func print_char(c: char) -> void -- prints a single character to the standard output.
- func print_int(i: int) -> void -- prints an integer to the standard output.
- func istr(p: string, i: itn) -> char -- get the character at index _i_ from _p_.
- func char(l: int) -> char -- constructs a char from its ascii code.
- func digit_to_char(c: int) -> char -- turns a digit into its ASCII 
representation.

# Hello world
```
func main() -> int
{
    call print("Hello, world!");
    return 0;
}
```
More examples can be found in the *samples* folder.

# Known issues
- Text in comments must have an even number of quotes and double-quotes, so `// you can't write this comment!`<br> `// 'this', however, is valid.` You will not get an error message, but no code after the comment will be compiled (until the next quote/double-quote).
- Declaring a local inside of an if statement or a while loop will break things. No error message.
- You can't call a function inside of a while loop's condition. There is an error message for this one.
- There is no error message for an unclosed function. You will get one when assembling, however.
