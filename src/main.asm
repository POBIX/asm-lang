ideal
model small
stack 100h
locals __

include "utils.asm"

include "tokens.asm"
include "errors.asm"
include "memory.asm"
include "vector.asm"
include "string.asm"
include "lexing.asm"
include "consts.asm"
include "file.asm"
include "parser.asm"
include "braces.asm"

dataseg
    CODE_SIZE equ 2048
    code db CODE_SIZE dup(0)

    INPUT_SIZE equ 32

    file_msg db "Which file do you want to compile?\n> ",0
    output_msg db "\n\nWhere do you want the output to be stored?\n> ",0
codeseg

start:
    mov ax, @data
    mov ds, ax

    call init_tokens
    call init_consts
    call init_parser
    call init_errors

    ; ask which file to compile
    push offset file_msg
    printf 0

    push INPUT_SIZE ; max characters in input
    call input

    push ax ; return value of input
    push FILE_R
    call open_file

    push ax
    push CODE_SIZE
    push offset code
    call read_file

    push ax
    call close_file

    ; push the standard library in front of our code
    push [STD_TEXT]
    push offset code
    call strappend
    mov dx, ax

    ; turn the code into a list of tokens
    push dx
    call do_lex

    ; parse do_lex's return value.
    push ax
    call do_parse
    mov di, ax

    ; store the output in file
    push offset output_msg
    printf 0

    push INPUT_SIZE ; max characters in input
    call input

    push ax
    call create_file
    mov si, ax

    push di
    call strsize

    push si
    push ax
    push di
    call write_file

    push si
    call close_file

    push 3
    call malloc

    exit ERR_NONE
end start
