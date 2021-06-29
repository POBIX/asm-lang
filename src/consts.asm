dataseg
    _ASM_HEADER db "ideal\n",\
                   "model small\n",\
                   "stack 100h\n",\
                   "codeseg\n\n",0

    _ASM_FOOTER db "start:\n",\
                   "\tmov ax, @data\n",\
                   "\tmov ds, ax\n",\
                   "\tcall main\n",\
                   "\tmov al, dl\n",\
                   "exit:\n",\
                   "\tmov ah, 4Ch\n",\
                   "\tint 21h\n",\
                   "end start\n",0

    ASM_HEADER dw ?
    ASM_FOOTER dw ?


    _STD_P1 db \
        'func print_char(c: char) -> void',\
        '{',\
           'asm("mov dx, [bp + 4]");',\
           'asm("mov ah, 2h");',\
           'asm("int 21h");',\
        '}',\
        'func istr(p: string, i: int) -> char',\
        '{',\
           'asm("mov bx, [bp + 6]");',\
           'asm("mov di, [bp + 4]");',\
           'asm("mov dl, [byte ptr bx + di]");',\
           'asm("xor dh, dh");',\
        '}',\
        'func char(l: int) -> char { asm("mov dx, [bp + 4]"); }'
    _STD_P2 db \ ; line too long, split into multiple parts
        "func print(s: string) -> void",\
        "{",\
            "local i: int = 0;",\
            "local c: char = istr(s, 0);",\
            "local zero: char = char(0);",\
            "while c != zero",\
            "{",\
                "call print_char(c);",\
                "inc i;",\
                "set c = istr(s, i);",\
            "}",\
        "}",\
        "func digit_to_char(c: int) -> char",\
        "{",\
            "if c < 0 { return char(0); }",\
            "if c > 9 { return char(0); }",\
            "math c = c + '0';",\
            "return char(c);",\
        "}",\
        "func print_int(i: int) -> void",\
        "{",\
            "local n: int = i;",\
            "math n = i % 10;",\
            "math i = i / 10;",\
            "if i != 0",\
            "{",\
                "call print_int(i);",\
            "}",\
            "call print_char(digit_to_char(n));",\
        "}",0

    STD_TEXT dw offset _STD_P1

codeseg

; void init_consts()
proc init_consts
    push offset _ASM_HEADER
    callva format 0
    mov [ASM_HEADER], ax

    push offset _ASM_FOOTER
    callva format 0
    mov [ASM_FOOTER], ax

    ret
endp
