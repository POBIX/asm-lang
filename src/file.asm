codeseg

; an enum's size is a double word. so use db, not the actual name.
enum _FILE_MODE_ {
    FILE_R,
    FILE_W,
    FILE_RW
}

; int open_file(string filename, FileMode mode)
proc open_file
    push bp
    mov bp, sp

    push bx
    push dx

    mov ah, 3Dh
    mov dx, [bp + 6]
    mov al, [byte ptr bp + 4]
    int 21h
    jnc __return

    __error:
        jmp ax_exit

    __return:
        pop dx
        pop bx
        pop bp
    ret 2 * 1
endp

; void close_file(int filehandle)
proc close_file
    push bp
    mov bp, sp

    push ax
    push bx

    mov ah, 3Eh
    mov bx, [bp + 4]
    int 21h

    pop bx
    pop ax
    pop bp
    ret 2 * 1
endp

; void read_file(int filehandle, int readbytes, string output)
proc read_file
    push bp
    mov bp, sp

    push ax
    push bx
    push cx
    push dx

    mov ah, 3Fh
    mov bx, [bp + 8]
    mov cx, [bp + 6]
    mov dx, [bp + 4]
    int 21h

    pop dx
    pop cx
    pop bx
    pop ax
    pop bp
    ret 2 * 3
endp

; void write_file(int filehandle, int bytes, string input)
proc write_file
    push bp
    mov bp, sp

    push ax
    push bx
    push cx
    push dx

    mov ah, 40h
    mov bx, [bp + 8]
    mov cx, [bp + 6]
    mov dx, [bp + 4]
    int 21h

    pop dx
    pop cx
    pop bx
    pop ax
    pop bp
    ret 2 * 3
endp

; int create_file(string filename)
proc create_file
    push bp
    mov bp, sp

    push cx
    push dx

    mov ah, 3ch
    xor cx, cx
    mov dx, [bp + 4]
    int 21h

    pop dx
    pop cx
    pop bp
    ret 2 * 1
endp
