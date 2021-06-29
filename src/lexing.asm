dataseg
    curr_line dw 1

codeseg

struc Lex
    tok db ? ; Token
    source dw ? ; string
    line dw ? ; int
ends

; Lex* get_token(string word)
proc get_token
    push bp
    mov bp, sp

    push di
    push bx
    push cx
    push si

    mov di, [bp + 4] ; word

    ; create a Lex, address stored in si
    push size Lex
    call malloc
    mov si, ax

    ; initialize it
    SI_PTR equ (Lex ptr si)
    mov [byte ptr SI_PTR.tok], TOK_NONE
    mov [SI_PTR.source], di
    mov ax, [curr_line]
    mov [SI_PTR.line], ax

    xor ah, ah
    mov cx, 1
    __loop:
        ; check if [word] is equal to the token
        push di
        mov bx, offset tokens
        add bx, cx
        add bx, cx ; adding twice beacuse it's a word
        push [word ptr bx]
        call strequ

        cmp ax, true
        je __equal

        ; try again with the next token
        inc cx
        cmp cx, TOK_EOF
        jl __loop
        ; if we reached this point, none of the tokens match. 
        ; we've initialized si.token to TOK_NONE already, so just return.
        jmp __return

    __equal:
        mov [SI_PTR.tok], cl

    __return:
        mov ax, si

        pop si
        pop cx
        pop bx
        pop di
        pop bp
    ret 2 * 1
endp

; vecword* do_lex(string source) - takes source code and converts into a vector of Lexes.
proc do_lex
    push bp
    mov bp, sp


    push bx
    push cx
    push dx
    push di
    push si

    mov bx, [bp + 4]

    ; create and initialize a vecword in si, to be fed into the split function
    push size vecword
    call malloc
    mov si, ax
    push si
    push 1024
    call vw_newcap
    SI_PTR equ (vecword ptr si)

    push bx
    push offset separators
    push si
    call split

    ; create and initialize the output, stored in di
    push size vecword
    call malloc
    mov di, ax
    push di
    push 1024
    call vw_newcap
    DI_PTR equ (vecword ptr di)

    xor ah, ah
    xor cx, cx

    jmp __loop
    __inc_line:
        ; this gets called whenever we've reached a newline.
        inc [curr_line]
        jmp __continue

    __loop:
        push si
        push cx
        call vw_index
        mov dx, ax

        ; if our current character is a newline, incrememnt the line number.
        push dx
        push 10
        call strequ_char
        cmp ax, true
        je __inc_line

        ; check whether the split[i] is a whitespace character, if it is then continue.
        push dx
        call is_whitespace
        cmp ax, true
        je __continue_near

        ; if this character is a part of a double token, we have to check whether this + the next element make a token together.
        push dx
        call strsize
        cmp ax, 1
        jne __skip

        push offset double_toks
        mov bx, dx
        mov al, [byte ptr bx]
        push ax
        call strcontains
        cmp ax, true
        jne __skip

        ; get the next token for the double token without incrementing the loop
        mov ax, cx
        inc ax
        push si
        push ax
        call vw_index

        dataseg
            __dtok db "%s%s",0
        codeseg
        push dx
        push ax
        push offset __dtok
        callva format 2

        push ax
        call get_token
        mov bx, ax
        BX_PTR equ (Lex ptr bx)

        cmp [BX_PTR.tok], TOK_COMMENT
        je __comment

        cmp [BX_PTR.tok], TOK_NONE
        je __skip
        inc cx
        jmp __push

        ; relative jump out of range
        __loop_near: jmp __loop
        __continue_near: jmp __continue
    
        __comment:
            ; as long as we don't reach a newline, increment cx.
            inc cx
            push si
            push cx
            call vw_index

            push ax
            push 10
            call strequ_char
            cmp ax, false
            je __comment

            jmp __loop

        __skip:
            ; push the split[i] token representation.
            push dx
            call get_token
            mov bx, ax
        __push:
            push di
            push bx
            call vw_push

        __continue:
            inc cx
            cmp cx, [SI_PTR.len]
            jb __loop_near

    __insert_eof:
        ; create an {EOF, null} Lex
        push size Lex
        call malloc
        mov bx, ax
        BX_PTR equ (Lex ptr bx)
        mov [BX_PTR.tok], TOK_EOF
        mov [BX_PTR.source], 0

        push di
        push bx
        call vw_push

    push si
    call free

    mov ax, di

    pop si
    pop di
    pop dx
    pop cx
    pop bx

    pop bp
    ret 2 * 1
endp
