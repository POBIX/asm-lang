; in kilobytes
MEM_SIZE equ 58

dataseg
    vmem db 1024 * MEM_SIZE dup(null)

codeseg
struc MemHeader
    used db ?
    next dw ?
ends

; void* malloc(int bytes)
proc malloc
    push bp
    mov bp, sp

    push di
    push bx

    bytes equ [bp + 4]

    mov bx, offset vmem
    BX_PTR equ (MemHeader ptr bx)

    __loop:
        cmp [BX_PTR.used], true
        je __next

        ; this header is unused.
        ; if this header points to null, it means we're at the end of memory and can allocate without any additional checks.
        cmp [BX_PTR.next], null
        je __mem_end

        ; check if we can overwrite it.
        mov di, [BX_PTR.next]
        sub di, bx
        sub di, bytes
        cmp di, size MemHeader
        jne __next

        ; we can overwrite it.
        mov [BX_PTR.used], true
        jmp __return

        __mem_end:
            mov di, bx
            add di, size MemHeader
            add di, bytes

            mov [BX_PTR.used], true
            mov [BX_PTR.next], di
            jmp __return

        __next:
            mov bx, [BX_PTR.next]
            cmp bx, 1024 * MEM_SIZE
            jb __loop
        
        __out_of_memory:
            mov bx, offset vmem
            exit ERR_OUT_OF_MEMORY

    __return:
        mov ax, bx
        add ax, size MemHeader
        pop bx
        pop di
        pop bp
    ret 2 * 1
endp

; void free(void* addr)
proc free
    push bp
    mov bp, sp
 
    push bx

    mov bx, [bp + 4]
    sub bx, size MemHeader
    BX_PTR equ (MemHeader ptr bx)

    mov [BX_PTR.used], false

    pop bx
    pop bp
    ret 2 * 1
endp

; void memcpy(void* source, void* dest, int count (in bytes))
proc memcpy
    push bp
    mov bp, sp

    ; that's a lot of registers. it may be possible to not use ax or bx?
    push ax
    push bx
    push di
    push si

    mov si, [bp + 8]
    mov di, [bp + 6]
    count equ [bp + 4]
    xor bx, bx

    __loop:
        ; copy each byte from the source to the destination.
        mov al, [byte ptr si + bx]
        mov [byte ptr di + bx], al
        inc bx
        cmp bx, count
        jb __loop

    pop si
    pop di
    pop bx
    pop ax
    pop bp
    ret 2 * 3
endp

; void* realloc(void* data, int bytes)
proc realloc
    push bp
    mov bp, sp

    push bx

    mov bx, [bp + 6]
    bytes equ [bp + 4]

    push bytes
    call malloc

    push bx
    push ax ; return value of malloc
    push bytes
    call memcpy

    push bx
    call free

    pop bx
    pop bp
    ret 2 * 2
endp

; free batch - takes any number of arguments and frees all of them.
macro freeb args:vararg
    irp a,<args>
        push a
        call free
    endm
endm

macro freem addr
    push addr
    call free
endm
