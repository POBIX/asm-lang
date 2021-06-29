; an enum's size is a double word. so use db, not the actual name
enum _ERRORS_ {
    ERR_NONE,
    ERR_UNEXPECTED_TOKEN,
    ERR_FILE_NOT_FOUND = 2,
    ERR_PATH_NOT_FOUND = 3,
    ERR_UNDEFINED_SYMBOL,
    ERR_TOO_MANY_OPEN_FILES = 5,
    ERR_UNDEFINED_OPERATOR,
    ERR_TYPES_DONT_MATCH,
    ERR_UNHANDLED_TOKEN,
    ERR_VAR_CANT_BE_VOID,
    ERR_BAD_CHAR,
    ERR_WRONG_ARGS,
    ERR_PERMISSION = 12,
    ERR_OUT_OF_MEMORY = 13,
    ERR_FUNC_IN_WHILE,
    ERR_BAD_MAIN,
    ERR_LOCAL_OUTISDE_FUNCTION,
    ERR_BREAK_OUTISDE_LOOP,
    ERR_CONTINUE_OUTSIDE_LOOP,

    ERR_LAST ; used to size the messages array, not for an actual error.
}

dataseg
    err_msg db "\n\nLine %i - exited with %s\n",0
    messages dw 2 * ERR_LAST dup(null)

codeseg
macro make_error error, message
    dataseg
        __id&error db message,0
    codeseg
    mov [messages + 2 * error], offset __id&error
endm

proc init_errors
    make_error ERR_NONE "no errors."
    make_error ERR_UNEXPECTED_TOKEN "an unexpected token."
    make_error ERR_FILE_NOT_FOUND "a file not found error."
    make_error ERR_PATH_NOT_FOUND "a path not found error."
    make_error ERR_UNDEFINED_SYMBOL "an undefined symbol."
    make_error ERR_TOO_MANY_OPEN_FILES "a there are too many open files error."
    make_error ERR_UNDEFINED_OPERATOR "an undefined operator."
    make_error ERR_TYPES_DONT_MATCH "a types don't match error."
    make_error ERR_UNHANDLED_TOKEN "if you see this, there is a very serious issue in the compiler."
    make_error ERR_VAR_CANT_BE_VOID "a variable/paramater can't be of type void error."
    make_error ERR_BAD_CHAR "a too many characters in char error."
    make_error ERR_PERMISSION "a permission error."
    make_error ERR_OUT_OF_MEMORY "an out of memory error."
    make_error ERR_WRONG_ARGS "an incorrect function call."
    make_error ERR_FUNC_IN_WHILE "a function call inside of a while loop condition."
    make_error ERR_BAD_MAIN "no main: it must be a function returning int and taking no paramaters."
    make_error ERR_LOCAL_OUTISDE_FUNCTION "a local declared outside of a function."
    make_error ERR_BREAK_OUTISDE_LOOP "a break outside a loop."
    make_error ERR_CONTINUE_OUTSIDE_LOOP "a continue outside a loop."
    
    make_error ERR_LAST "If you see this, there is an extraordinarily serious issue in the compiler."

    ret
endp

; immediately exits the program with the specified code.
macro exit code
    mov ax, code
    jmp ax_exit
endm

proc ax_exit
    xor ah, ah
    push [parser_line]
    mov bx, offset messages
    add bx, ax
    add bx, ax ; multiply by two, addresses are word.
    push [word ptr bx]
    push offset err_msg
    printf 2

    mov ah, 4Ch
    int 21h

    ; no point in returning.
endp
