null equ 0
false equ 0
true equ 1

codeseg

; bool contains(word* arr, word element, int len)
proc contains
    push bp
    mov bp, sp

    push di
    push bx
    mov bx, [bp + 8]
    mov ax, [bp + 6]
    p_len equ [bp + 4]

    xor di, di

    __loop:
        cmp [word ptr bx + di], ax
        je __true

        inc di
        cmp di, p_len
        jl __loop

    __false:
        mov ax, false
        jmp __return

    __true:
        mov ax, true

    __return:
        pop bx
        pop di
        pop bp
    ret 2 * 3
endp

; void printc(char c)
proc printc
    push bp
    mov bp, sp

    push ax
    push dx

    mov dl, [bp + 4]
    ; don't print if null
    cmp dl, 0
    je __return

    mov ah, 2h
    int 21h

    __return:
        pop dx
        pop ax
        pop bp
    ret 2 * 1
endp

; void printstr(string s)
proc printstr
    push bp
    mov bp, sp

    push bx
    push ax

    mov bx, [bp + 4]
    
    xor ah, ah
    __loop:
        mov al, [byte ptr bx]
        push ax
        call printc

        inc bx
        cmp [byte ptr bx], 0
        jne __loop

    pop ax
    pop bx
    pop bp
    ret 2 * 1
endp

macro print message
    local string
    dataseg
        string db message,0
    codeseg
        push offset string
        call printstr
endm

macro println message
    print message
    print <13,10>
endm

macro callva func, amount
    push amount
    call _VA_&func
    add sp, 2 * amount
endm

; macro make_str name, text
;     nowarn pdc ; this is an intentional pass-depndent construction.
;     ifndef _make_str_&name&_
;         _make_str_&name&_ equ 1
;         dataseg
;             name db text,0
;         codeseg
;     endif
;     warn pdc
; endm

macro printf count
    callva format count
    push ax
    call printstr
endm

macro printchar c
    mov dl, c
    mov ah, 2h
    int 21h
endm
