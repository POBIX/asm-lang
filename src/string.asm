dataseg
    whitespace_chars db ' ',10,13,9,0
    newline_str db 10,0
    true_str db "True",0
    false_str db "False",0
    null_str db 0
    tab_str db 9,0
    colon_str db ":",0
    colon_newline_str db ":",10,0
    space_str db " ",0
    newline_tab_str db 10,9,0

codeseg

; int strsize(string s)
proc strsize
    push bp
    mov bp, sp


    push bx
    push di
    mov bx, [bp + 4]
    xor di, di

    ; increment the pointer until we reach a null terminator.
    __loop:
        cmp [byte ptr bx + di], 0
        je __exit_loop
        inc di
        jmp __loop
    __exit_loop:
        mov ax, di

    pop di
    pop bx
    pop bp
    ret 2 * 1
endp

; bool strequ(string a, string b)
proc strequ
    push bp
    mov bp, sp

    push bx
    push di
    push cx

    mov bx, [bp + 4]
    mov di, [bp + 6]

    __loop:
        ; if a's current index is not the same as b's index, return false.
        mov al, [byte ptr bx]
        cmp al, [byte ptr di]
        jne __false
        ; if they are equal, but it's a null terminator, return true - the string ended.
        cmp al, 0
        je __true
        ; check the next index
        inc bx
        inc di
        jmp __loop

    __true:
        mov ax, true

    __exit_loop:
        pop cx
        pop di
        pop bx
        pop bp
    ret 2 * 2

    __false:
        mov ax, false
        jmp __exit_loop
endp

; bool strequ_char(string s, char c)
proc strequ_char
    push bp
    mov bp, sp

    push bx
    mov bx, [bp + 6]
    c equ [bp + 4]
    
    ; if s' size isn't 1, they can't be equal
    push bx
    call strsize
    cmp ax, 1
    jne __false

    ; actually check if they are equal
    mov al, c
    cmp [byte ptr bx], al
    je __true

    __false:
        mov ax, false
        jmp __return

    __true:
        mov ax, true 
    __return:
        pop bx
        pop bp
    ret 2 * 2
endp

; bool strequ_any(string a, string b) - check if a is any of the characters in b.
proc strequ_any
    push bp
    mov bp, sp

    push bx
    push di
    mov bx, [bp + 6]
    mov di, [bp + 4]

    ; if a's size isn't 1, return false
    push bx
    call strsize
    cmp ax, 1
    jne __false

    __loop:
        ; check if the characters are equal
        mov al, [byte ptr bx]
        cmp al, [byte ptr di]
        je __true

        ; if we aren't at the end of the string
        inc di
        cmp [byte ptr di], 0
        jne __loop

    ; if we reached the end and none of the characters matched, return false

    __false:
        mov ax, false        
        jmp __return

    __true:
        mov ax, true

    __return:
        pop di
        pop bx
        pop bp
    ret 2 * 2
endp

; bool strcontains(string str, char element)
proc strcontains
    push bp
    mov bp, sp

    push bx

    mov bx, [bp + 6]
    mov ax, [bp + 4]

    __loop:
        cmp [byte ptr bx], al
        je __true

        inc bx
        cmp [byte ptr bx], 0
        jne __loop

    __false:
        mov ax, false
        jmp __return

    __true:
        mov ax, true

    __return:
        pop bx
        pop bp
    ret 2 * 2
endp

; void vb_push_str(vecbyte* self, string str)
proc vb_push_str
    push bp
    mov bp, sp

    push ax
    push bx
    push di

    mov bx, [bp + 6] ; self
    mov di, [bp + 4] ; str

    xor ah, ah
    __loop:
        push bx
        mov al, [byte ptr di]
        push ax
        call vb_push

        inc di
        cmp [byte ptr di], 0
        jne __loop

    pop di
    pop bx
    pop ax
    pop bp
    ret 2 * 2
endp

; bool is_whitespace(string s)
proc is_whitespace
    push bp
    mov bp, sp

    push [bp + 4]
    push offset whitespace_chars
    call strequ_any
    pop bp
    ret 2 * 1
endp

; splits at any separator, keeps it in its own split
; void split(string text, char[] separators, vecword* output)
proc split
    push bp
    mov bp, sp
    sub sp, 2

    push ax
    push bx
    push di
    push si

    mov bx, [bp + 8] ; text
    seps equ [bp + 6]
    mov di, [bp + 4] ; output

    added equ [byte ptr bp - 1]
    mov added, false
    adding_str equ [byte ptr bp - 2]
    mov adding_str, false

    ; a vecbyte, its address is stored in si
    push size vecbyte
    call malloc
    mov si, ax

    SI_PTR equ (vecbyte ptr si)
    push si
    call vb_new

    jmp __loop

    ; it's up here because relative jump out of range and stuff
    __char:
        push si
        push "'"
        call vb_push

        inc bx
        xor ah, ah
        mov al, [byte ptr bx]
        push si
        push ax
        call vb_push

        inc bx
        cmp [byte ptr bx], "'"
        jne __char_error

        push si
        push "'"
        call vb_push

        jmp __add_null

        __char_error:
            exit ERR_BAD_CHAR

    __loop:
        push si
        ; push *bx as a byte
        mov al, [byte ptr bx]
        xor ah, ah
        push ax
        call vb_push
        mov added, true

    __check_null:
        inc bx
        cmp [byte ptr bx], 0
        je __end

    __check_str:
        ; check if we're dealing with a string.
        cmp [byte ptr bx], '"'
        je __string
        cmp [byte ptr bx], "'"
        je __string
        jmp __check_sep

    __string:
        cmp adding_str, true
        je __end_str

        mov adding_str, true
        jmp __loop

    __end_str:
        mov adding_str, false
        jmp __loop 

    __check_sep:
        ; if we're adding a string, skip the check
        cmp adding_str, true
        je __loop

        push seps
        ; push *bx as a byte
        mov al, [byte ptr bx]
        xor ah, ah
        push ax
        call strcontains

        cmp ax, false
        je __loop
        cmp added, false
        je __push_sep

    __add_null:
        ; terminate the string when we're done
        push si
        push 0
        call vb_push

    __push_str:
        push di
        push [SI_PTR.data]
        call vw_push

    __push_sep:
        ; we want to push the separator itself, as well as a null terminator to our vector.
        ; it can't be local since it will just get overwritten with other stuff.
        push 2
        call malloc

        ; we can't use ax for pointers, but all of the pointers are already used.
        push si
        mov si, ax

        ; our current separator is pointed to by bx.
        mov al, [byte ptr bx]
        mov [byte ptr si + 0], al
        mov [byte ptr si + 1], 0

        ; push it
        push di
        push si
        call vw_push

        pop si
        jmp __reset_si

    __reset_si:
        push si
        call vb_new
        mov added, false
        jmp __check_null

    __end:
        ; we've reached the null terminator. we just need to push the last string and return.
        ; if we only added a separator this loop, there's no need.
        cmp added, false
        je __return

        push si
        push 0
        call vb_push

        push di
        push [SI_PTR.data]
        call vw_push

    __return:
        push si
        call free

        pop si
        pop di
        pop bx
        pop ax

        pop bp
        add sp, 2
        ret 2 * 3
endp

; string vw_to_str(vecword* v)
proc vw_to_str
    push bp
    mov bp, sp
    sub sp, 2

    push bx
    push cx
    push dx
    push di
    push si

    mov bx, [bp + 4]
    BX_PTR equ (vecword ptr bx)
    _len equ [word ptr bp - 2]

    ; get the combined length of every single string in the vector
    mov _len, 0
    xor cx, cx
    __get_len:
        push bx
        push cx
        call vw_index
        push ax
        call strsize
        add _len, ax

        inc cx
        cmp cx, [BX_PTR.len]
        jb __get_len

    ; allocate the final string in di
    push _len
    call malloc
    mov di, ax

    xor cx, cx
    xor si, si
    __loop:
        push bx

        push bx
        push cx
        call vw_index
        mov bx, ax

        xor si, si
        __sub:
            mov dl, [byte ptr bx + si]
            mov [byte ptr di], dl

            inc di
            inc si
            cmp [byte ptr bx + si], 0
            jne __sub

        pop bx

        inc cx
        cmp cx, [BX_PTR.len]
        jb __loop

    ; di currently points at the last character in our string.
    mov [byte ptr di - 1], 0 ; add a null terminator
    sub di, _len ; point it at the beginning of the string

    mov ax, di

    pop si
    pop di
    pop dx
    pop cx
    pop bx
    
    pop bp
    add sp, 2
    ret 2 * 1
endp

; string int_to_str(int n)
proc int_to_str
    push bp
    mov bp, sp

    push bx
    push cx
    push dx
    n equ [word ptr bp + 4]

    xor cx, cx
    mov ax, n
    mov bx, 10
    __len:
        xor dx, dx
        inc cx
        div bx
        cmp ax, 0
        jne __len
    
    ; allocate an output string the size of our number.
    push cx
    call malloc
    mov bx, ax
    add bx, cx ; we want to go backwards through the string, so add its length to the address.

    mov [byte ptr bx], 0 ; this is the final character.

    mov ax, n
    mov cx, 10
    __loop:
        xor dx, dx
        div cx
        add dl, '0'
        mov [byte ptr bx], dl
        dec bx
        cmp ax, 0
        jne __loop

    mov ax, bx
    inc ax ; we had to decrement bx one time too much in the loop.

    pop dx
    pop cx
    pop bx
    pop bp
    ret 2 * 1
endp

; variadic string V_format(args..., string text, int num_of_args)
; same thing as c's sprintf, but it allocates memory on its own.
; num_of_args does not include the text or itself.
proc _VA_format
    push bp
    mov bp, sp


    push bx
    push dx
    push di
    push si

    mov bx, [bp + 6] ; text
    args_num equ [bp + 4]

    ; va contains the top of args. do sub si, 2 to go the the next argument.
    mov si, bp
    mov ax, 8
    add ax, args_num ; adding twice to multiply by two, since arguments are words.
    add ax, args_num
    add si, ax
    va equ [word ptr ss:si]

    ; crate a new vecbyte in di.
    push size vecbyte
    call malloc
    mov di, ax
    push di
    push 32
    call vb_newcap

    xor dh, dh
    __loop:
        cmp [byte ptr bx], '%'
        je __type
        cmp [byte ptr bx], '\'
        je __special
        __push:
            push di
            mov dl, [byte ptr bx]
            push dx
            call vb_push
            jmp __next
        
        __special:
            inc bx
            cmp [byte ptr bx], 'n'
            je __newline
            cmp [byte ptr bx], 'r'
            je __carriage
            cmp [byte ptr bx], 't'
            je __tab
            cmp [byte ptr bx], '0'
            je __null
            cmp [byte ptr bx], '\'
            je __back

            __newline:
                push di
                push 10
                call vb_push
                jmp __next
            __carriage:
                push di
                push 13
                call vb_push
                jmp __next
            __tab:
                push di
                push 9
                call vb_push
                jmp __next
            __null:
                push di
                push 0
                call vb_push
                jmp __next
            __back:
                push di
                push '\'
                call vb_push
                jmp __next
    
        __type:
            inc bx
            cmp [byte ptr bx], 'i'
            je __int
            cmp [byte ptr bx], 's'
            je __string
            cmp [byte ptr bx], 'c'
            je __char
            cmp [byte ptr bx], 'b'
            je __bool
            ; if there's nothing after the %, just push it.
            dec bx
            jmp __push

            __int:
                sub si, 2 ; go to the next argument
                ; get the argument (which should be an int) as a string
                push va
                call int_to_str
                ; push the int as a string
                push di
                push ax
                call vb_push_str
                jmp __next
            
            __string:
                sub si, 2 ; get next argument
                ; push the next argument as string
                push di
                push va
                call vb_push_str
                jmp __next
            
            ; this is a thing because the actual __loop is too far away from __next.
            __loop_near: jmp __loop
            
            __char:
                sub si, 2 ; get next argument
                push di
                push va
                call vb_push
                jmp __next
            
            __bool:
                sub si, 2 ; get next argument
                cmp va, 1
                je __true

                __false:
                    push di
                    push offset false_str
                    call vb_push_str
                    jmp __next

                __true:
                    push di
                    push offset true_str
                    call vb_push_str
                    jmp __next

        __next:
            inc bx
            cmp [byte ptr bx], 0
            jne __loop_near

    ; add a null terminator
    push di
    push 0
    call vb_push

    push di
    call free

    mov ax, [(vecbyte ptr di).data]

    pop si
    pop di
    pop dx
    pop bx

    pop bp
    ret 2 * 2
endp

macro fmt message, args:vararg
    m_count = 0
    irp item,<args>
        push item
        m_count = m_count + 1
    endm
    push offset message
    callva format m_count
endm

; bool is_digit(char c)
proc is_digit
    push bp
    mov bp, sp

    c equ [byte ptr bp + 4]

    cmp c, '0'
    jb __false

    cmp c, '9'
    ja __false

    __true:
        mov ax, true
        jmp __return

    __false:
        mov ax, false
        jmp __return
    
    __return:
        pop bp
    ret 2 * 1
endp

; string strappend(string a, string b)
proc strappend
    push bp
    mov bp, sp

    push bx
    push dx
    push si
    push di

    a equ [bp + 6]
    b equ [bp + 4]

    push a
    call strsize
    mov dx, ax
    push b
    call strsize
    inc ax ; add one byte for the null terminator
    add dx, ax

    ; store output string in di
    push dx
    call malloc
    mov di, ax

    ; add each character in a to the output string.
    mov si, a
    xor bx, bx
    __loopa:
        mov dl, [byte ptr si]
        mov [byte ptr di + bx], dl

        inc bx
        inc si
        cmp [byte ptr si], 0
        jne __loopa

    ; add each character in a to the end of the output string (after a)    
    mov si, b
    __loopb:
        mov dl, [byte ptr si]
        mov [byte ptr di + bx], dl

        inc bx
        inc si
        cmp [byte ptr si], 0
        jne __loopb

    ; add the null terminator.
    mov [byte ptr di + bx], 0

    mov ax, di

    pop di
    pop si
    pop dx
    pop bx
    pop bp
    ret 2 * 2
endp

; bool is_number(string str)
proc is_number
    push bp
    mov bp, sp

    push bx
    mov bx, [bp + 4]

    xor ah, ah
    __loop:
        mov al, [byte ptr bx]
        push ax
        call is_digit
        cmp ax, false
        je __return ; no need to set ax to false, it already is.

        inc bx
        cmp [byte ptr bx], 0
        jne __loop

    __return:
        pop bx
        pop bp
    ret 2 * 1
endp

; bool is_bool(string s, out dx: bool val). dx will not be modified if it's not a bool.
proc is_bool
    push bp
    mov bp, sp

    s equ [bp + 4]

    push s
    push offset true_text
    call strequ
    cmp ax, true
    je __true

    push s
    push offset false_text
    call strequ
    cmp ax, true
    je __false

    ; it isn't a boolean. we don't need to set ax to false, since it already is, or modify dx.
    jmp __return

    __true:
        ; no need to set ax to true, as it already is.
        mov dx, true
        jmp __return
    
    __false:
        ; no need to set ax to true, as it already is.
        mov dx, false
        jmp __return

    __return:
        pop bp
    ret 2 * 1
endp

; string trim(string s)
proc trim
    push bp
    mov bp, sp

    push bx
    push di

    mov bx, [bp + 4]

    push bx
    call strsize
    mov di, ax
    dec di

    mov [byte ptr bx + di], 0 ; get rid of last character
    inc bx ; get rid of first character

    mov ax, bx

    pop di
    pop bx
    pop bp
    ret 2 * 1
endp

; int count_lines(string s)
proc count_lines
    push bp
    mov bp, sp

    push bx

    mov bx, [bp + 4]

    xor ax, ax
    __loop:
        cmp [byte ptr bx], 10
        jne __next
        inc ax

        __next:
            inc bx
            cmp [byte ptr bx], 0
            jne __loop

    pop bx
    pop bp
    ret 2 * 1
endp

; vecword* vecchar_to_vw(vecbyte* vec)
; converts a massive vector of chars into a vector of strings. takes a null terminator as a separator.
proc vecchar_to_vw
    push bp
    mov bp, sp

    push bx
    push cx
    push si
    push di

    mov bx, [bp + 4]
    BX_PTR equ (vecbyte ptr bx)

    push size vecword
    call malloc
    mov di, ax

    push di
    push 256
    call vw_newcap

    ; push the beginning of our source vector into the output
    push di
    push [BX_PTR.data]
    call vw_push

    xor cx, cx
    __loop:
        push bx
        push cx
        call vb_index
        cmp ax, 0
        jne __next

        ; we've reached a null terminator. push the next character's address into the output.
        mov si, [BX_PTR.data]
        add si, cx
        inc si

        push di
        push si
        call vw_push

        __next:
            inc cx
            cmp cx, [BX_PTR.len]
            jb __loop

    pop di
    pop si
    pop cx
    pop bx
    pop bp
    ret 2 * 1
endp

; string input(int count)
proc input
    push bp
    mov bp, sp

    push bx
    push dx
    push di

    count equ [bp + 4]

    ; allocate string
    mov ax, count
    add ax, 3 ; two bytes for final size, one byte for enter at end.
    push count
    call malloc

    ; interrupt
    mov dx, ax
    mov di, ax
    mov ax, count
    inc ax ; for enter at the end.
    mov [byte ptr di], al
    mov ah, 0Ah
    int 21h

    ; add null terminator
    xor bh, bh
    mov bl, [byte ptr di + 1]
    mov [byte ptr 2 + di + bx], 0

    mov ax, di
    add ax, 2 ; ignore string size

    pop di
    pop dx
    pop bx
    pop bp
    ret 2 * 1
endp
