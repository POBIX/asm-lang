codeseg

macro make_vector name, sh, _bytes
    struc name
        data dw ?
        len  dw ?
        max  dw ?
    ends

    BX_PTR equ (name ptr bx)

    ; void vec_newcap(vector* output, int cap)
    proc sh&_newcap
        push bp
    mov bp, sp

        push ax
        push bx

        mov bx, [bp + 6]
        mov ax, [bp + 4]

        ; we need to store the max as words/doublewords/whatever, but call malloc with bytes
        mov [BX_PTR.max], ax
        rept _bytes - 1
            add ax, [bp + 4]
        endm
        push ax
        call malloc
        mov [word ptr BX_PTR.data], ax
        mov [BX_PTR.len], 0

        pop bx
        pop ax
        pop bp
    ret 2 * 2
    endp

    ; void vec_new(vector* output)
    proc sh&_new
        push bp
    mov bp, sp

        push [bp + 4]
        push 10
        call sh&_newcap
        pop bp
    ret 2 * 1
    endp

    ; void vec_push(vector* self, any element)
    proc sh&_push
        push bp
    mov bp, sp

        push bx
        push dx
        push di
        push cx

        mov bx, [bp + 6]
        mov dx, [bp + 4]

        mov di, [BX_PTR.len]
        inc di
        cmp di, [BX_PTR.max]
        jae __extend
        jmp __continue

        __extend:
            xor di, di
            mov cx, [BX_PTR.max]
            add cx, 15
            rept _bytes
                add di, cx
            endm
            push [BX_PTR.data]
            push di
            call realloc

            mov [BX_PTR.data], ax
            mov [BX_PTR.max], cx

        __continue:
            mov di, [word ptr BX_PTR.data]

            rept _bytes
                add di, [BX_PTR.len]
            endm

            ; actually push it
            mov [di], dx
            inc [BX_PTR.len]

        pop cx
        pop di
        pop dx
        pop bx
        pop bp
    ret 2 * 2
    endp

    ; any vec_pop(vector* self)
    proc sh&_pop
        push bp
    mov bp, sp

        push bx
        mov bx, [bp + 4]

        ; "remove" last element
        dec [BX_PTR.len]

        ; get its value (our memory contents were definitely not changed yet)
        push bx
        push [BX_PTR.len]
        call sh&_index

        pop bx
        pop bp
    ret 2 * 1
    endp
endm

make_vector vecword vw 2
make_vector vecbyte vb 1

; we have to define vec_index for each version, since we need to use differnet registers.
proc vw_index
    push bp
    mov bp, sp

    BX_PTR equ (vecword ptr bx)
    push bx
    mov bx, [bp + 6]
    _index equ [bp + 4]

    mov bx, [BX_PTR.data]
    add bx, _index
    add bx, _index
    mov ax, [word ptr bx]

    pop bx
    pop bp
    ret 2 * 2
endp

proc vb_index
    push bp
    mov bp, sp

    BX_PTR equ (vecbyte ptr bx)
    push bx
    mov bx, [bp + 6]
    _index equ [bp + 4]

    mov bx, [BX_PTR.data]
    add bx, _index
    mov al, [byte ptr bx]
    xor ah, ah

    pop bx
    pop bp
    ret 2 * 2
endp
