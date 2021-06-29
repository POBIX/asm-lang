codeseg
proc _parse_lbrace far
    call history_prev
    push offset braces
    push ax
    call vb_push

    push [BX_PTR.output]
    push offset newline_str
    call vw_push

    p_next TOK_ANY

    exit ERR_UNEXPECTED_TOKEN
endp

proc _brace_func far
    mov di, [curr_func]
    DI_PTR equ (FuncInfo ptr di)

    ; push the return label
    push [BX_PTR.output]
    push [DI_PTR.return]
    call vw_push
    push [BX_PTR.output]
    push offset colon_newline_str
    call vw_push

    ; deallocate locals
    mov si, [DI_PTR.vars]
    SI_PTR equ (vecword ptr si)
    mov ax, [SI_PTR.len]
    add ax, ax ; multiply by two, since locals are each a word.

    push ax
    push offset local_dealloc_text
    callva format 1

    push [BX_PTR.output]
    push ax ; return value of format
    call vw_push

    mov si, [DI_PTR.args]
    SI_PTR equ (vecword ptr si)

    mov ax, [SI_PTR.len]
    add ax, [SI_PTR.len] ; we need to return the number of bytes pushed.
    push ax
    push offset endf_text
    callva format 1

    push [BX_PTR.output]
    push ax ; return value of format
    call vw_push

    mov [curr_func], null

    SI_PTR equ (Lex ptr si)
    ret
endp

proc _brace_data far
    push [BX_PTR.output]
    push offset code_text
    call vw_push

    ret
endp

proc _brace_if far
    push bx
    call get_current
    mov si, ax
    cmp [SI_PTR.tok], TOK_ELSE
    je __else

    push offset if_stack
    call vw_pop
    mov di, ax
    DI_PTR equ (IfInfo ptr di)

    ; append label and a colon
    push [BX_PTR.output]
    push [DI_PTR.finish]
    call vw_push
    push [BX_PTR.output]
    push offset colon_str
    call vw_push

    ret

    __else:
        add sp, 4 ; we don't need the return address or cs.
        p_next TOK_ELSE
endp

proc _brace_else far
    push offset if_stack
    call vw_pop
    mov di, ax
    push [BX_PTR.output]
    push [DI_PTR.finish]
    call vw_push
    push [BX_PTR.output]
    push offset colon_str
    call vw_push

    ret
endp

proc _brace_while
    push offset while_stack
    call vw_pop
    mov di, ax

    DI_PTR equ (WhileInfo ptr di)

    ; push the next label and if statement.
    push [BX_PTR.output]
    push [DI_PTR.next]
    call vw_push

    push [BX_PTR.output]
    push offset colon_newline_str
    call vw_push

    push [BX_PTR.output]
    push [DI_PTR.branch]
    call vw_push

    ; push the break label.
    push [BX_PTR.output]
    push [DI_PTR.break]
    call vw_push
    push [BX_PTR.output]
    push offset colon_str
    call vw_push

    ret
endp

proc _parse_rbrace far
    push offset braces
    call vb_pop

    cmp ax, TOK_IF
    je __if
    cmp ax, TOK_ELSE
    je __else
    cmp ax, TOK_FUNC
    je __func
    cmp ax, TOK_WHILE
    je __while
    cmp ax, TOK_DATA
    je __data

    jmp __return

    __if:
        call _brace_if
        jmp __return

    __else:
        call _brace_else
        jmp __return

    __func:
        call _brace_func
        jmp __return

    __while:
        call _brace_while
        jmp __return

    __data: 
        call _brace_data
        jmp __return

    __return:
        push [BX_PTR.output]
        push offset newline_str
        call vw_push

        p_next TOK_ANY

    exit ERR_UNEXPECTED_TOKEN
endp
