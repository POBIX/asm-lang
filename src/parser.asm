dataseg
    braces vecbyte ? ; vecbyte<Token>
    token_history vecbyte ? ; vecbyte<Token>

    proc_text db "proc %s\n",\
                 "\tpush bp\n",\
                 "\tmov bp, sp\n",0

    endf_text db "\tpop bp\n",\
                 "\tret %i\n",\
                 "endp\n",0

    set_text db "\tmov ax, %s\n",\
                "\tmov %s, ax",0

    call_text db "\tcall %s\n",0

    data_text db "dataseg",0
    code_text db "codeseg",0

    var_text db "\t%s dw ?",0
    byte_text db "byte",0
    db_text db "db",0
    word_text db "word",0
    dw_text db "dw",0
    dword_text db "dword",0
    dd_text db "dd",0
    int_text db "int",0
    str_text db "string",0
    char_text db "char",0
    bool_text db "bool",0
    void_text db "void",0

    variables vecword ? ; vec<VarInfo*>
    functions vecword ? ; vec<FuncInfo*>

    ax_text db "ax",0
    bx_text db "bx",0
    cx_text db "cx",0
    dx_text db "dx",0
    di_text db "di",0
    si_text db "si",0
    al_text db "al",0
    ah_text db "ah",0
    bl_text db "bl",0
    bh_text db "bh",0
    cl_text db "cl",0
    ch_text db "ch",0
    dl_text db "dl",0
    dh_text db "dh",0

    depoint_text db "[%s]",0

    label_text db "L%i",0
    label_index dw 0
    const_text db "V%i",0
    var_index dw 0

    if_stack vecword ? ; vec<IfInfo*/ElseInfo*>
    while_stack vecword ? ; vec<WhileInfo*>

    cmp_text db "\tmov ax, %s\n",\
                "\tcmp ax, %s\n",0
    jg_text db "\tjg %s\n",0
    jl_text db "\tjl %s\n",0
    je_text db "\tje %s\n",0
    jne_text db "\tjne %s\n",0
    jge_text db "\tjge %s\n",0
    jle_text db "\tjle %s\n",0
    jmp_text db "\tjmp %s\n",0

    inc_text db "\tinc %s",0
    dec_text db "\tdec %s",0

    add_text db "\tmov ax, %s\n",\
                "\tmov bx, %s\n",\
                "\tadd bx, ax\n",\
                "\tmov %s, bx",0

    sub_text db "\tmov ax, %s\n",\
                "\tmov bx, %s\n",\
                "\tsub bx, ax\n",\
                "\tmov %s, bx",0

    div_text db "\tmov bx, %s\n",\
                "\tmov ax, %s\n",\
                "\txor dx, dx\n",\
                "\tidiv bx\n",\
                "\tmov %s, ax\n",0

    mod_text db "\tmov bx, %s\n",\
                "\tmov ax, %s\n",\
                "\txor dx, dx\n",\
                "\tidiv bx\n",\
                "\tmov %s, dx\n",0

    mul_text db "\tmov ax, %s\n",\
                "\tmov bx, %s\n",\
                "\timul bx\n",\
                "\tmov %s, ax",0

    true_text db "true",0
    one_text db "1",0
    false_text db "false",0
    null_text db "null",0
    zero_text db "0",0

    curr_func dw null ; FuncInfo*

    arg_text db "[bp + %i]",0

    push_text db "\tpush %s\n",0

    local_alloc_text db 9,"sub sp, 2",10,0
    local_dealloc_text db "\tadd sp, %i\n",0
    local_text db "[word ptr bp - %i]",0

    set_dx_text db "\tmov dx, %s\n",0

    inline_str_text db "dataseg\n",\
                       "\t%s db %s,0\n",\
                       "codeseg\n",0

    offset_text db "offset %s",0

    asm_text db "\t%s\n",0

    parser_line dw 0

    main_text db "main",0
codeseg

; every member of every struct is a string unless specified otherwise
struc VarInfo
    id dw ?
    type db ?
ends

struc IfInfo
    block dw ?
    finish dw ?
ends

struc ElseInfo
    block dw ?
    finish dw ?
    elseb dw ?
ends

struc WhileInfo
    block dw ?
    next dw ?
    break dw ?
    branch dw ?
ends

struc ArgInfo
    id dw ?
    type db ?
    index dw ?
ends

struc FuncInfo
    id dw ?
    args dw ? ; vec<ArgInfo*>*
    vars dw ? ; vec<LocalInfo*>*
    return dw ?
    type db ?
ends

struc LocalInfo
    id dw ?
    type db ?
    index dw ?
ends

struc Parse
    tokens dw ? ; vec<Lex*>*
    index  dw ? ; int
    output dw ? ; vec<string>*
ends

; void init_parser()
proc init_parser
    push offset braces
    call vb_new

    push offset token_history
    push 100
    call vb_newcap

    push offset variables
    call vw_new

    push offset if_stack
    call vw_new

    push offset while_stack
    call vw_new

    push offset functions
    call vw_new
    ret
endp

; Lex* get_current(Parse* self) - returns current token
proc get_current
    push bp
    mov bp, sp

    push bx

    mov bx, [bp + 4]
    BX_PTR equ (Parse ptr bx)

    push [BX_PTR.tokens]
    push [BX_PTR.index]
    call vw_index
    mov bx, ax
    mov bx, [(Lex ptr bx).line]
    mov [parser_line], bx

    pop bx
    pop bp
    ret 2 * 1
endp

; Lex* get_previous(Parse* self) - returns previous token
proc get_previous
    push bp
    mov bp, sp

    mov bx, [bp + 4]
    BX_PTR equ (Parse ptr bx)

    dec [BX_PTR.index]

    push bx
    call get_current

    inc [BX_PTR.index]

    pop bp
    ret 2 * 1
endp

; Lex* get_next(Parse* self) - returns next token without modifying self.
proc get_next
    push bp
    mov bp, sp

    mov bx, [bp + 4]
    BX_PTR equ (Parse ptr bx)

    inc [BX_PTR.index]

    push bx
    call get_current

    dec [BX_PTR.index]

    pop bp
    ret 2 * 1
endp

; Token history_prev()
proc history_prev
    mov ax, [token_history.len]
    sub ax, 2

    push offset token_history
    push ax
    call vb_index
    xor ah, ah
    ret
endp

; si::Lex* parse_next(Parse* self, Token expect)
; progresses to next token, throws error if current token isn't the same as expect. (unless expect is TOK_ANY)
proc parse_next
    push bp
    mov bp, sp

    push ax
    push dx

    mov bx, [bp + 6]
    expect equ [byte ptr bp + 4]
    BX_PTR equ (Parse ptr bx)

    ; Load current Lex into si
    push bx
    call get_current
    mov si, ax
    SI_PTR equ (Lex ptr si)

    inc [BX_PTR.index]

    ; if expect is TOK_ANY, skip the check.
    cmp expect, TOK_ANY
    je __call

    ; throw error if si is not the same as expect.
    mov al, [SI_PTR.tok]
    cmp expect, al
    je __call

    exit ERR_UNEXPECTED_TOKEN

    __call:
        xor ah, ah
        mov al, [SI_PTR.tok]
        add al, al ; * 2

        push bx
        mov bx, offset callers
        add bx, ax
        mov dx, [bx]
        pop bx

        ; if the function was not set, we're at an unhandled token.
        cmp dx, -1
        je __default

        ; if the function is null, do nothing.
        cmp dx, null
        je __return

        push offset token_history
        xor ah, ah
        mov al, [SI_PTR.tok]
        push ax
        call vb_push

        add sp, 12 ; we're never coming back. get rid of everything :(
        jmp dx

    __return:
        pop dx
        pop ax
        pop bp
    ret 2 * 2

    __default:
        call _parse_default
        jmp __return
endp

macro p_next tok
    push bx
    push tok
    call parse_next
endm

; VarInfo* create_variable(string name, string type)
proc create_variable
    push bp
    mov bp, sp

    push bx
    push di
    push si

    mov bx, [bp + 6] ; name
    mov di, [bp + 4] ; type

    ; create a VarInfo in si.
    push size VarInfo
    call malloc
    mov si, ax

    SI_PTR equ (VarInfo ptr si)

    mov [SI_PTR.id], bx

    push di
    call parse_type
    cmp al, TYPE_VOID
    je __void_error
    mov [SI_PTR.type], al

    push offset variables
    push si
    call vw_push

    mov ax, si

    pop si
    pop di
    pop bx
    pop bp
    ret 2 * 2

    __void_error:
        exit ERR_VAR_CANT_BE_VOID
endp

; bool find_variable(string name)
proc find_variable
    push bp
    mov bp, sp

    push bx
    push di
    push si
    push cx

    mov bx, [bp + 4]
    mov di, [variables.data]
    SI_PTR equ (VarInfo ptr si)

    mov cx, [variables.len]
    cmp cx, 0
    je __false

    __loop:
        mov si, [di]
        push bx
        push [SI_PTR.id]
        call strequ
        cmp ax, true
        je __true

        add di, 2

        loop __loop

    __false:
        mov ax, null
        jmp __return

    __true:
        mov ax, si
        jmp __return

    __return:
        pop cx
        pop si
        pop di
        pop bx
        pop bp
    ret 2 * 1
endp

; bool is_register(string str)
proc is_register
    push bp
    mov bp, sp

    xor ax, ax

    ; the weird stuff with the near is becuase half of the registers are
    ; out of range from __return.

    macro reg_equ s
        push [bp + 4]
        push offset s
        call strequ
        cmp ax, true
        je __return
    endm

    macro reg_equn s
        push [bp + 4]
        push offset s
        call strequ
        cmp ax, true
        je __return_near
    endm
    reg_equ ax_text
    reg_equ bx_text
    reg_equ cx_text
    reg_equ dx_text
    reg_equ di_text
    reg_equ si_text
    jmp __next
    __return: ; it's in here because some of the registers are too far away.
        pop bp
    ret 2 * 1
    __next:
        reg_equ al_text
        reg_equ ah_text
        reg_equ bl_text
        reg_equ bh_text
        reg_equ cl_text
    jmp __nexter
    __return_near: ; this is because the last 3 registers are so far away we can't do anything about them.
        pop bp
    ret 2 * 1
    __nexter:
        reg_equn ch_text
        reg_equn dl_text
        reg_equn dh_text

    jmp __return
endp

; bool find_function(string str)
proc find_function
    push bp
    mov bp, sp

    push bx
    push cx
    push di

    mov bx, offset functions
    BX_PTR equ (vecword ptr bx)

    xor cx, cx
    __loop:
        push bx
        push cx
        call vw_index
        mov di, ax
        DI_PTR equ (FuncInfo ptr di)

        push [DI_PTR.id]
        push s
        call strequ
        cmp ax, true
        je __found

        inc cx
        cmp cx, [BX_PTR.len]
        jb __loop

    ; if we got here, no matching function was found. no need to set ax to null, since it's already false.
    jmp __return

    __found:
        mov ax, di
        jmp __return

    __return:
        pop di
        pop cx
        pop bx
        pop bp
    ret 2 * 1
endp

; string parse_symbol(string s, bx: Parse* self, cl: out Type t) - for example, takes "my_var" and transforms into "[my_var]". also resets ch.
proc parse_symbol
    push bp
    mov bp, sp

    push di
    push dx
    s equ [bp + 4]

    BX_PTR equ (Parse ptr bx)

    xor ch, ch

    push s
    call is_register
    cmp ax, true
    je __register

    push s
    call find_variable
    cmp ax, null
    jne __variable

    push s
    call is_number
    cmp ax, true
    je __int

    push s
    call find_function
    cmp ax, null
    jne __function

    push s
    call is_bool
    cmp ax, true
    je __bool

    ; this is done because of a relative jump out of range
    jmp __continue

    __register:
        mov cl, TYPE_INT
        jmp __none

    __int:
        mov cl, TYPE_INT
        jmp __none

    __none:
        mov ax, s ; don't change anything.
        jmp __return

    __variable:
        mov di, ax
        mov cl, [(VarInfo ptr di).type]

        ; turn the string into something like [name]
        push s
        push offset depoint_text
        callva format 1

        jmp __return

    __function:
        mov di, ax
        mov cl, [(FuncInfo ptr di).type]
        dec [(Parse ptr bx).index]
        call push_func_call
        mov ax, offset dx_text
        jmp __return

    __bool:
        mov cl, TYPE_BOOL
        cmp dx, true
        je __true
        jmp __false

    __true:
        ; turn it into a numeric 1
        mov ax, offset one_text
        jmp __return

    __false:
        ; turn it into a numeric 0
        mov ax, offset zero_text
        jmp __return

    __continue:
        push s
        push offset false_text
        call strequ
        cmp ax, true
        je __false

        push s
        push offset null_text
        call strequ
        cmp ax, true
        je __false

        mov di, s
        cmp [byte ptr di], '"' ; if the first character of the string is a "
        je __string
        cmp [byte ptr di], "'" ; if its a '
        je __char

        push s
        call find_local
        cmp ax, null
        jne __local_near

        push s
        call find_arg
        cmp ax, null
        jne __arg

        add sp, 4 ; clear the pushes
        exit ERR_UNDEFINED_SYMBOL

    ; relative jump out of range :(
    __local_near: jmp __local

    __char:
        mov cl, TYPE_CHAR
        jmp __none

    __string:
        mov cl, TYPE_STR
        call get_var
        mov dx, ax

        push dx
        push s
        push offset inline_str_text
        callva format 2

        push [BX_PTR.output]
        push ax ; return value of format
        call vw_push

        push dx ; return value of get_var
        push offset offset_text
        callva format 1
        ; ax will now have the return value of format.
        jmp __return

    __arg:
        mov di, ax
        DI_PTR equ (ArgInfo ptr di)

        mov cl, [DI_PTR.type]

        ; get the bp offset into ax.
        mov ax, [DI_PTR.index]
        add ax, ax ; adding twice because arguments are words

        mov di, [curr_func]
        mov di, [(FuncInfo ptr di).args]

        mov di, [(vecword ptr di).len]
        add di, di ; multiply by two
        add di, 2 ; offset bp by return address
        sub di, ax

        push di
        push offset arg_text
        callva format 1

        jmp __return

    __local:
        mov di, ax
        DI_PTR equ (LocalInfo ptr di)

        mov cl, [DI_PTR.type]

        mov ax, 2 ; initial offset
        add ax, [DI_PTR.index]
        add ax, [DI_PTR.index] ; multiply by two, since locals are words.
        push ax
        push offset local_text
        callva format 1

        jmp __return

    __return:
        pop dx
        pop di
        pop bp
    ret 2 * 1
endp

; string get_label() - returns next label name
proc get_label
    push [label_index]
    push offset label_text
    callva format 1

    inc [label_index]

    ret ; ax is going to have return value of format
endp

; string get_var() - return next variable name
proc get_var
    push [var_index]
    push offset const_text
    callva format 1

    inc [var_index]

    ret ; ax is going to have return value of format
endp

; IfInfo* create_if()
proc create_if
    push bx

    push size IfInfo
    call malloc
    mov bx, ax
    BX_PTR equ (IfInfo ptr bx)

    call get_label
    mov [BX_PTR.block], ax
    call get_label
    mov [BX_PTR.finish], ax

    push offset if_stack
    push bx
    call vw_push

    mov ax, bx
    pop bx
    ret
endp

; ElseInfo* if_to_else(IfInfo* if)
proc if_to_else
    push bp
    mov bp, sp

    push bx
    push di
    mov bx, [bp + 4]

    push size ElseInfo
    call malloc
    mov di, ax

    DI_PTR equ (ElseInfo ptr di)
    BX_PTR equ (IfInfo ptr bx)

    mov ax, [BX_PTR.block]
    mov [DI_PTR.block], ax
    mov ax, [BX_PTR.finish]
    mov [DI_PTR.elseb], ax
    call get_label
    mov [DI_PTR.finish], ax

    mov ax, di

    push bx
    call free

    pop di
    pop bx
    pop bp
    ret 2 * 1
endp

; WhileInfo* create_while()
proc create_while
    push di
    push cx

    push size WhileInfo
    call malloc
    mov di, ax
    DI_PTR equ (WhileInfo ptr di)

    call get_label
    mov [DI_PTR.block], ax
    call get_label
    mov [DI_PTR.next], ax
    call get_label
    mov [DI_PTR.break], ax
    push [DI_PTR.block]
    call parse_branch
    cmp cx, true
    je __func_in_while
    mov [DI_PTR.branch], ax

    push offset while_stack
    push di
    call vw_push

    mov ax, di

    pop cx
    pop di
    ret

    __func_in_while:
        exit ERR_FUNC_IN_WHILE
endp

; string get_branch(string lhs, Token sign, string rhs, string label)
proc get_branch
    push bp
    mov bp, sp

    push cx

    lhs equ [bp + 10]
    sign equ [byte ptr bp + 8]
    rhs equ [bp + 6]
    lbl equ [bp + 4]

    push lhs
    push rhs
    push offset cmp_text
    callva format 2
    mov cx, ax

    push lbl ; push the label to jump to for later formatting

    ; add the conditional jump
    cmp sign, TOK_EQUAL
    je __equal
    cmp sign, TOK_GREATER
    je __greater
    cmp sign, TOK_LESSER
    je __lesser
    cmp sign, TOK_NOTEQUAL
    je __notequal
    cmp sign, TOK_GREATEQ
    je __greateq
    cmp sign, TOK_LESSEQ
    je __lesseq

    exit ERR_UNDEFINED_OPERATOR

    __equal:
        push offset je_text
        jmp __continue

    __greater:
        push offset jg_text
        jmp __continue

    __lesser:
        push offset jl_text
        jmp __continue

    __notequal:
        push offset jne_text
        jmp __continue

    __greateq:
        push offset jge_text
        jmp __continue

    __lesseq:
        push offset jle_text
        jmp __continue

    __continue:
        callva format 1
        push cx
        push ax
        call strappend

    pop cx
    pop bp
    ret 2 * 4
endp

; string parse_branch(string label, out cx: is_function) - progresses the parser and pushes the branch text.
proc parse_branch
    push bp
    mov bp, sp

    push si
    push di
    push dx

    lbl equ [bp + 4]
    SI_PTR equ (Lex ptr si)
    ; store first cmp argument in cx
    p_next TOK_NONE
    push [SI_PTR.source]
    call find_function
    cmp ax, null
    jne __func1
    mov cx, false

    __continue1:
        push [SI_PTR.source]
        call parse_symbol
        mov di, ax

        freem si

        ; store operator in dx
        p_next TOK_ANY ; it's TOK_ANY because it can be one of many tokens (greater, lesser, etc.)
        mov dl, [SI_PTR.tok]
        xor dh, dh

        freem si

        ; store second cmp argument in si
        p_next TOK_NONE
        push [SI_PTR.source]
        call find_function
        cmp ax, null
        jne __func2
        mov cx, false

    __continue2:
        push [SI_PTR.source]
        call parse_symbol
        freem si
        mov si, ax

        push di
        push dx
        push si
        push lbl
        call get_branch

    pop dx
    pop di
    pop si
    pop bp
    ret 2 * 1

    __func1:
        mov cx, true
        jmp __continue1
    __func2:
        mov cx, true
        jmp __continue2
endp

; FuncInfo* create_func(string name). does not fill args and vars, only initializes them.
proc create_func
    push bp
    mov bp, sp

    push bx

    push size FuncInfo
    call malloc
    mov bx, ax

    BX_PTR equ (FuncInfo ptr bx)

    mov ax, [bp + 4]
    mov [BX_PTR.id], ax

    push size vecword
    call malloc
    mov [BX_PTR.args], ax

    push [BX_PTR.args]
    call vw_new

    push size vecword
    call malloc
    mov [BX_PTR.vars], ax

    push [BX_PTR.vars]
    call vw_new

    call get_label
    mov [BX_PTR.return], ax

    push offset functions
    push bx
    call vw_push

    mov ax, bx

    pop bx
    pop bp
    ret 2 * 1
endp

; ArgInfo* create_arg(string name, Type type, int index)
proc create_arg
    push bp
    mov bp, sp

    push bx

    push size ArgInfo
    call malloc
    mov bx, ax
    BX_PTR equ (ArgInfo ptr bx)

    mov ax, [bp + 8]
    mov [BX_PTR.id], ax

    mov ax, [bp + 6]
    mov [BX_PTR.type], al

    mov ax, [bp + 4]
    mov [BX_PTR.index], ax

    mov ax, bx

    pop bx
    pop bp
    ret 2 * 3
endp

; ArgInfo* find_arg(string name). null if not found.
proc find_arg
    push bp
    mov bp, sp

    push bx
    push di
    push cx

    mov bx, [curr_func]
    mov bx, [(FuncInfo ptr bx).args]
    BX_PTR equ (vecword ptr bx)
    DI_PTR equ (ArgInfo ptr di)

    xor cx, cx
    __loop:
        push bx
        push cx
        call vw_index
        mov di, ax

        push [DI_PTR.id]
        push [bp + 4]
        call strequ
        cmp ax, true

        je __found
        inc cx
        cmp cx, [BX_PTR.len]
        jb __loop

    __not_found:
        mov ax, null
        jmp __return

    __found:
        mov ax, di
        jmp __return

    __return:
        pop cx
        pop di
        pop bx
        pop bp
    ret 2 * 1
endp

; LocalInfo* create_local(string name, string type, int index)
proc create_local
    push bp
    mov bp, sp

    push bx
    push di

    mov bx, [curr_func]
    mov bx, [(FuncInfo ptr bx).vars]
    BX_PTR equ (vecword ptr bx)

    push size LocalInfo
    call malloc
    mov di, ax
    DI_PTR equ (LocalInfo ptr di)

    mov ax, [bp + 8]
    mov [DI_PTR.id], ax

    push [bp + 6]
    call parse_type
    cmp al, TYPE_VOID
    je __void_error
    mov [DI_PTR.type], al

    mov ax, [bp + 4]
    mov [DI_PTR.index], ax

    push bx
    push di
    call vw_push

    mov ax, di

    pop di
    pop bx
    pop bp
    ret 2 * 3

    __void_error:
        exit ERR_VAR_CANT_BE_VOID
endp

; LocalInfo* find_local(string name). returns null if not found
proc find_local
    push bp
    mov bp, sp

    push bx
    push cx
    push di

    mov bx, [curr_func]
    mov bx, [(FuncInfo ptr bx).vars]
    BX_PTR equ (vecword ptr bx)
    DI_PTR equ (LocalInfo ptr di)

    xor cx, cx
    __loop:
        push bx
        push cx
        call vw_index
        mov di, ax

        push [bp + 4]
        push [DI_PTR.id]
        call strequ
        cmp ax, true
        je __found
        inc cx
        cmp cx, [BX_PTR.len]
        jb __loop
    ; we didn't find anything. no need to set ax to null, since it's already false (0)
    __return:
        pop di
        pop cx
        pop bx
        pop bp
    ret 2 * 1

    __found:
        mov ax, di
        jmp __return
endp

; void push_func_call(bx: Parse* self)
proc push_func_call
    push ax
    push cx
    push dx
    push si
    push di

    BX_PTR equ (Parse ptr bx)

    ; store the call text in dx
    p_next TOK_NONE
    push [SI_PTR.source]
    push offset call_text
    callva format 1
    mov dx, ax
    push dx ; store dx for later use

    freem si

    ; store the args in di
    push [SI_PTR.source]
    call find_function
    mov di, ax
    mov di, [(FuncInfo ptr di).args]
    DI_PTR equ (vecword ptr di)

    freem si

    ; start parsing args
    p_next TOK_LPAREN
    freem si

    xor dx, dx

    ; can be either an argument or a close parentheses
    push bx
    call get_current
    mov si, ax
    mov al, [SI_PTR.tok]
    freem si
    cmp al, TOK_RPAREN
    je __no_args
    cmp al, TOK_NONE
    jne __exit_near
    jmp __loop

    __no_args:
        inc [BX_PTR.index]
        jmp __push

    ; it's up here because of a relative jump out of range
    __push:
        cmp dx, [DI_PTR.len]
        jne __wrong_args
        pop dx ; we pushed it just after first setting it.
        push [BX_PTR.output]
        push dx ; the call <function> line, initialized at the top of the function
        call vw_push
        jmp __return

    __wrong_args:
        exit ERR_WRONG_ARGS

    ; relative jump out of range
    __exit_near: jmp __exit
    __push_near: jmp __push

    __loop:
        p_next TOK_NONE
        push [SI_PTR.source]
        call parse_symbol
        freem si

        push ax

        cmp dx, [DI_PTR.len]
        jae __wrong_args

        ; store the current ArgInfo in si
        push di
        push dx
        call vw_index
        mov si, ax
        SI_PTR equ (ArgInfo ptr si)

        ; make sure the value's type and the argument's type match
        cmp cl, [SI_PTR.type]
        jne __type_error

        pop ax

        push ax ; return value of parse_symbol
        push offset push_text
        callva format 1

        push [BX_PTR.output]
        push ax ; return value of format
        call vw_push

        inc dx
        ; can be either comma or close parentheses
        push bx
        call get_current
        mov si, ax
        SI_PTR equ (Lex ptr si)
        inc [BX_PTR.index]
        mov al, [SI_PTR.tok]
        freem si
        cmp al, TOK_COMMA
        je __loop
        cmp al, TOK_RPAREN
        je __push_near

    __exit:
        exit ERR_UNEXPECTED_TOKEN
    __type_error:
        exit ERR_TYPES_DONT_MATCH

    __return:
        pop di
        pop si
        pop dx
        pop cx
        pop ax
        ret
endp

; Type parse_type(string str) - takes string like 'int' and returns it's actual type representation.
proc parse_type
    push bp
    mov bp, sp

    s equ [bp + 4]

    macro pt_equ t, l
        push s
        push offset t
        call strequ
        cmp ax, true
        je l
    endm

    pt_equ int_text __int
    pt_equ str_text __str
    pt_equ char_text __char
    pt_equ bool_text __bool
    pt_equ void_text __void

    __int:
        mov ax, TYPE_INT
        jmp __return
    __str:
        mov ax, TYPE_STR
        jmp __return
    __char:
        mov ax, TYPE_CHAR
        jmp __return
    __bool:
        mov ax, TYPE_BOOL
        jmp __return
    __void:
        mov ax, TYPE_VOID
        jmp __return

    __return:
        pop bp
    ret 2 * 1
endp

;; all _parse_x procedures have the following signatures:
;; void _parse_x(bx: Parse* self, si: Lex* current_token)
;; this is done mostly for efficiency and ease of use, since they are all called from a common background,
;; and pushing the arguments onto the stack is a bit of a waste.

SI_PTR equ (Lex ptr si)
BX_PTR equ (Parse ptr bx)

proc _parse_func far
    p_next TOK_NONE

    push [SI_PTR.source]
    push offset proc_text
    callva format 1

    push [BX_PTR.output]
    push ax ; return value of format
    call vw_push

    freem si

    push [SI_PTR.source]
    call create_func
    mov di, ax
    mov [curr_func], di
    DI_PTR equ (FuncInfo ptr di)

    freem si

    p_next TOK_LPAREN
    freem si

    xor cx, cx
    ; we have to check whether the function takes paramaters or not, and skip the loop if it doesn't.
    push bx
    call get_current
    mov si, ax
    cmp [SI_PTR.tok], TOK_RPAREN
    je __no_args

    __loop:
        ; get the paramater's name
        p_next TOK_NONE
        mov dx, [SI_PTR.source]
        freem si

        p_next TOK_COLON
        freem si
        ; get its type
        p_next TOK_NONE
        mov ax, [SI_PTR.source]
        freem si

        push ax
        call parse_type
        cmp al, TYPE_VOID
        je __void_error_near

        push dx
        push ax
        push cx
        call create_arg

        push [DI_PTR.args]
        push ax ; return value of create_arg
        call vw_push

        jmp __next

        ; relative jump out of range
        __void_error_near: jmp __void_error
        __no_args:
            inc [BX_PTR.index]
            jmp __return

        __next:
            p_next TOK_ANY ; can be either a comma or right parentheses
            mov al, [SI_PTR.tok]
            inc cx
            freem si

            cmp al, TOK_COMMA
            je __loop
            cmp al, TOK_RPAREN
            je __return
            exit ERR_UNEXPECTED_TOKEN

    __return:
        p_next TOK_ARROW
        freem si

        ; set the function's return type
        p_next TOK_NONE

        push [SI_PTR.source]
        call parse_type
        mov [DI_PTR.type], al ; return value of parse_type
        freem si

        p_next TOK_LBRACE
        exit ERR_UNEXPECTED_TOKEN
    
    __void_error:
        exit ERR_VAR_CANT_BE_VOID
    
endp

proc _parse_set far
    ; get the identifier of what we're setting in di
    p_next TOK_NONE
    push [SI_PTR.source]
    call parse_symbol
    mov di, ax

    freem si

    push cx ; type from parse_symbol

    p_next TOK_ASSIGN
    freem si

    ; get the value in si
    p_next TOK_NONE
    push [SI_PTR.source]
    call parse_symbol
    freem si
    mov si, ax

    pop ax ; type from first parse_symbol
    cmp ax, cx
    jne __type_error

    ; we are pushing them in reverse order due to the way set_text works.
    push si
    push di
    push offset set_text
    callva format 2

    push [BX_PTR.output]
    push ax
    call vw_push

    p_next TOK_SEMICOL
    exit ERR_UNEXPECTED_TOKEN

    __type_error:
        exit ERR_TYPES_DONT_MATCH
endp

proc _parse_semicol far
    push [BX_PTR.output]
    push offset newline_str
    call vw_push

    p_next TOK_ANY
    exit ERR_UNEXPECTED_TOKEN
endp

proc _parse_call far
    ; this function isn't inlined since it's called from other places.
    call push_func_call

    p_next TOK_SEMICOL
    exit ERR_UNEXPECTED_TOKEN
endp

proc _parse_data far
    push [BX_PTR.output]
    push offset data_text
    call vw_push
    p_next TOK_LBRACE
    exit ERR_UNEXPECTED_TOKEN
endp

proc _parse_var far
    ; store the variable's name in cx
    p_next TOK_NONE
    mov cx, [SI_PTR.source]
    freem si

    p_next TOK_COLON
    freem si

    ; store its type in dx
    p_next TOK_NONE
    mov dx, [SI_PTR.source]
    freem si

    __continue:
        push cx
        push dx
        call create_variable

        push cx
        push offset var_text
        callva format 1

        push [BX_PTR.output]
        push ax ; return value of format
        call vw_push

        p_next TOK_SEMICOL
        exit ERR_UNEXPECTED_TOKEN
endp

proc _parse_if far
    push cx

    call create_if
    mov si, ax

    SI_PTR equ (IfInfo ptr si)

    push [SI_PTR.block]
    call parse_branch

    push [BX_PTR.output]
    push ax
    call vw_push

    ; jmp to the stuff after the if
    push [SI_PTR.finish]
    push offset jmp_text
    callva format 1

    push [BX_PTR.output]
    push ax ; return value of format
    call vw_push

    ; write the block label
    push [BX_PTR.output]
    push [SI_PTR.block]
    call vw_push

    ; push a colon and newline
    push [BX_PTR.output]
    push offset colon_str
    call vw_push

    pop cx

    p_next TOK_LBRACE
    exit ERR_UNEXPECTED_TOKEN

    SI_PTR equ (Lex ptr si)
endp

proc _parse_else far
    push offset if_stack
    mov ax, [if_stack.len]
    dec ax
    push ax
    call vw_index

    push ax
    call if_to_else
    mov di, ax
    DI_PTR equ (ElseInfo ptr di)

    push [DI_PTR.finish]
    push offset jmp_text
    callva format 1

    push [BX_PTR.output]
    push ax
    call vw_push

    push [BX_PTR.output]
    push [DI_PTR.elseb]
    call vw_push
    push [BX_PTR.output]
    push offset colon_str
    call vw_push

    ; replace the IfInfo pointer with the casted ElseInfo.
    dec [if_stack.len]
    push offset if_stack
    push di
    call vw_push

    p_next TOK_LBRACE
    exit ERR_UNEXPECTED_TOKEN
endp

proc _parse_inc far
    p_next TOK_NONE
    push [SI_PTR.source]
    call parse_symbol
    freem si

    push ax
    push offset inc_text
    callva format 1

    push [BX_PTR.output]
    push ax
    call vw_push

    p_next TOK_SEMICOL
    exit ERR_UNEXPECTED_TOKEN
endp

proc _parse_dec far
    p_next TOK_NONE
    push [SI_PTR.source]
    call parse_symbol

    freem si

    push ax
    push offset dec_text
    callva format 1

    push [BX_PTR.output]
    push ax
    call vw_push

    p_next TOK_SEMICOL
    exit ERR_UNEXPECTED_TOKEN
endp

proc _parse_while far
    call create_while
    mov si, ax

    SI_PTR equ (WhileInfo ptr si)

    ; jmp to the branch before you you execute the block for the first time,
    ; so that it will be a while loop rather than a do while.
    push [SI_PTR.next]
    push offset jmp_text
    callva format 1

    push [BX_PTR.output]
    push ax
    call vw_push

    ; push the block label
    push [BX_PTR.output]
    push [SI_PTR.block]
    call vw_push
    push [BX_PTR.output]
    push offset colon_newline_str
    call vw_push

    SI_PTR equ (Lex ptr si)

    p_next TOK_LBRACE ; the condition was handled by create_while.
    exit ERR_UNEXPECTED_TOKEN
endp

proc _parse_math far
    ; get the result into di
    p_next TOK_NONE
    push [SI_PTR.source]
    call parse_symbol
    mov di, ax

    freem si

    p_next TOK_ASSIGN
    freem si

    ; get the lhs into dx
    p_next TOK_NONE
    push [SI_PTR.source]
    call parse_symbol
    mov dx, ax

    freem si

    p_next TOK_ANY
    mov al, [SI_PTR.tok]
    freem si
    cmp al, TOK_ADD
    je __add
    cmp al, TOK_SUB
    je __sub
    cmp al, TOK_MUL
    je __mul
    cmp al, TOK_DIV
    je __div
    cmp al, TOK_MOD
    je __mod
    exit ERR_UNDEFINED_OPERATOR

    __add:
        push offset add_text
        jmp __continue
    
    __sub:
        push offset sub_text
        jmp __continue

    __mul:
        push offset mul_text
        jmp __continue

    __div:
        push offset div_text
        jmp __continue
    
    __mod:
        push offset mod_text
        jmp __continue

    __continue:
        ; get rhs into ax
        p_next TOK_NONE
        push [SI_PTR.source]
        call parse_symbol

        freem si

        pop cx ; pop the operator

        push ax
        push dx
        push di
        push cx
        callva format 3

        push [BX_PTR.output]
        push ax ; return value of format
        call vw_push

    p_next TOK_SEMICOL
    exit ERR_UNEXPECTED_TOKEN
endp

proc _parse_comment far
    ; __loop:
    ;     push bx
    ;     call get_current
    ;     mov si, ax
    ;     cmp [SI_PTR.tok], TOK_COMMENT
    ;     je __return
    ;     inc [BX_PTR.index]

    ; __return:
    ;     p_next TOK_ANY
    ;     exit ERR_UNEXPECTED_TOKEN
    p_next TOK_ANY
    exit ERR_UNEXPECTED_TOKEN
endp

proc _parse_local far
    push [BX_PTR.output]
    push offset local_alloc_text
    call vw_push

    ; store its name in dx
    p_next TOK_NONE
    mov dx, [SI_PTR.source]

    freem si

    p_next TOK_COLON
    freem si

    ; store its type in cx
    p_next TOK_NONE
    mov cx, [SI_PTR.source]
    freem si

    mov di, [curr_func]
    cmp di, null
    je __local_error
    mov di, [(FuncInfo ptr di).vars]
    DI_PTR equ (vecword ptr di)

    push dx
    push cx
    push [DI_PTR.len]
    call create_local
    mov di, ax
    DI_PTR equ (LocalInfo ptr di)

    ; the next token can be either a semicolon or an equals sign.
    push bx
    call get_current
    mov si, ax

    cmp [SI_PTR.tok], TOK_SEMICOL
    je __semicolon
    cmp [SI_PTR.tok], TOK_ASSIGN
    je __assign
    exit ERR_UNEXPECTED_TOKEN

    __local_error:
        exit ERR_LOCAL_OUTISDE_FUNCTION

    __semicolon:
        p_next TOK_SEMICOL
    __assign:
        p_next TOK_ASSIGN
        freem si

        p_next TOK_NONE
        push [SI_PTR.source]
        call parse_symbol
        freem si
        cmp cl, [DI_PTR.type]
        jne __type_error
        mov di, ax

        push dx ; the name of our local, initalized at the top of the function.
        call parse_symbol

        push di
        push ax ; return value of parse_symbol
        push offset set_text
        callva format 2

        push [BX_PTR.output]
        push ax ; reutrn value of format
        call vw_push

        p_next TOK_SEMICOL
        exit ERR_UNEXPECTED_TOKEN

    __type_error:
        exit ERR_TYPES_DONT_MATCH
endp

proc _parse_return far
    push bx
    call get_current
    mov si, ax

    mov di, [curr_func]
    DI_PTR equ (FuncInfo ptr di)

    cmp [SI_PTR.tok], TOK_SEMICOL
    je __continue
    cmp [SI_PTR.tok], TOK_NONE
    je __value
    exit ERR_UNEXPECTED_TOKEN

    __value:
        inc [BX_PTR.index]
        push [SI_PTR.source]
        call parse_symbol

        cmp cl, [DI_PTR.type]
        jne __type_error

        push ax ; return value of parse_symbol
        push offset set_dx_text
        callva format 1

        push [BX_PTR.output]
        push ax ; return value of format
        call vw_push

    __continue:
        push [DI_PTR.return]
        push offset jmp_text
        callva format 1

        push [BX_PTR.output]
        push ax ; return value of format
        call vw_push

    p_next TOK_SEMICOL
    exit ERR_UNEXPECTED_TOKEN

    __type_error:
        exit ERR_TYPES_DONT_MATCH
endp

proc _parse_break far
    push offset while_stack
    mov ax, [while_stack.len]
    cmp ax, 0
    je __no_while
    dec ax
    push ax
    call vw_index
    mov di, ax

    DI_PTR equ (WhileInfo ptr di)

    push [DI_PTR.break]
    push offset jmp_text
    callva format 1

    push [BX_PTR.output]
    push ax ; return value of format
    call vw_push

    p_next TOK_SEMICOL
    exit ERR_UNEXPECTED_TOKEN

    __no_while:
        exit ERR_BREAK_OUTISDE_LOOP
endp

proc _parse_continue far
    push offset while_stack
    mov ax, [while_stack.len]
    cmp ax, 0
    je __no_while
    dec ax
    push ax
    call vw_index
    mov di, ax

    DI_PTR equ (WhileInfo ptr di)

    push [DI_PTR.next]
    push offset jmp_text
    callva format 1

    push [BX_PTR.output]
    push ax ; return value of format
    call vw_push

    p_next TOK_SEMICOL
    exit ERR_UNEXPECTED_TOKEN

    __no_while:
        exit ERR_CONTINUE_OUTSIDE_LOOP
endp

proc _parse_asm far
    p_next TOK_LPAREN
    freem si

    p_next TOK_NONE
    mov si, [SI_PTR.source]
    push si
    call trim

    push ax
    push offset asm_text
    callva format 1

    push [BX_PTR.output]
    push ax ; reutrn value of format
    call vw_push

    p_next TOK_RPAREN
    freem si

    p_next TOK_SEMICOL
    exit ERR_UNEXPECTED_TOKEN
endp

proc _parse_default
    exit ERR_UNHANDLED_TOKEN
endp

; string do_parse(vecword* lexed)
proc do_parse far
    push bp
    mov bp, sp


    mov dx, [bp + 6]

    ; create and initialize a Parse in bx
    push size Parse
    call malloc
    mov bx, ax
    BX_PTR equ (Parse ptr bx)
    mov [BX_PTR.tokens], dx
    mov [BX_PTR.index], 0

    ; set its output to a new vecword
    push size vecword
    call malloc
    push ax
    push 512
    call vw_newcap
    mov [BX_PTR.output], ax

    ; push the assembly header, required for the program to function
    push [BX_PTR.output]
    push [ASM_HEADER]
    call vw_push

    pop bp
    p_next TOK_ANY ; start parsing
    exit ERR_UNEXPECTED_TOKEN
endp

proc _parse_eof far
    ; if we got here, we're done parsing, with no errors.

    ; before doing anything, check if there is a main function with a correct signature.
    push offset main_text
    call find_function
    cmp ax, null ; check if there is any function called 'main'
    je __bad_main
    mov di, ax
    DI_PTR equ (FuncInfo ptr di)
    cmp [DI_PTR.type], TYPE_INT ; check whether it returns int
    jne __bad_main
    mov si, [DI_PTR.args]
    cmp [(vecword ptr si).len], 0 ; check whether it takes no paramaters
    jne __bad_main

    ; push the assembly footer, required for the program to function
    push [BX_PTR.output]
    push [ASM_FOOTER]
    call vw_push

    push [BX_PTR.output]
    call vw_to_str

    ; return without progressing to the next token. (there is none!!)
    ret 4

    __bad_main:
        exit ERR_BAD_MAIN
endp
