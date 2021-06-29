; apparently, an enum's size is a double word. so use db, not the actual name.
enum _TOKEN_ {
    TOK_ANY = -1,
    TOK_NONE,

    TOK_FUNC,
    TOK_SEMICOL,
    TOK_SET,
    TOK_ASSIGN,
    TOK_LBRACE,
    TOK_RBRACE,
    TOK_LPAREN,
    TOK_RPAREN,
    TOK_CALL,
    TOK_DATA,
    TOK_VAR,
    TOK_COLON,
    TOK_IF,
    TOK_ELSE,
    TOK_GREATER,
    TOK_LESSER,
    TOK_EQUAL,
    TOK_NOTEQUAL,
    TOK_GREATEQ,
    TOK_LESSEQ,
    TOK_INC,
    TOK_DEC,
    TOK_WHILE,
    TOK_ADD,
    TOK_SUB,
    TOK_DIV,
    TOK_MOD,
    TOK_MUL,
    TOK_MATH,
    TOK_COMMENT,
    TOK_COMMA,
    TOK_LOCAL,
    TOK_RETURN,
    TOK_BREAK,
    TOK_ASM,
    TOK_ARROW,
    TOK_CONTINUE,

    TOK_EOF,

    TOK_LAST ; used to know how many tokens we have
}
dataseg
    tokens dw 2 * TOK_LAST dup(TOK_NONE)
    separators db " ;':={}[](),.+-*/",'"',9,10,13,0
    double_toks db "+-*/=&|<>!",0

    callers dw 2 * TOK_LAST dup(-1)

codeseg

macro make_caller t, func
    mov [callers + 2 * t], offset func
endm

macro make_token tok, string, func
    dataseg
        tok&_id db string,0
    codeseg
        mov [tokens + 2 * tok], offset tok&_id
        make_caller tok func
endm

; void init_tokens()
proc init_tokens
    push ax
    push bx

    xor ah, ah

    make_caller TOK_NONE null
    make_caller TOK_EOF _parse_eof
    make_token TOK_FUNC "func" _parse_func
    make_token TOK_SEMICOL ";" _parse_semicol
    make_token TOK_SET "set" _parse_set
    make_token TOK_ASSIGN "=" null
    make_token TOK_LBRACE "{" _parse_lbrace
    make_token TOK_RBRACE "}" _parse_rbrace
    make_token TOK_LPAREN "(" null
    make_token TOK_RPAREN ")" null
    make_token TOK_CALL "call", _parse_call
    make_token TOK_DATA "data" _parse_data
    make_token TOK_VAR "var" _parse_var
    make_token TOK_COLON ":" null
    make_token TOK_IF "if" _parse_if
    make_token TOK_ELSE "else" _parse_else
    make_token TOK_GREATER ">" null
    make_token TOK_LESSER "<" null
    make_token TOK_EQUAL "==" null
    make_token TOK_NOTEQUAL "!=" null
    make_token TOK_GREATEQ ">=" null
    make_token TOK_LESSEQ "<=" null
    make_token TOK_INC "inc" _parse_inc
    make_token TOK_DEC "dec" _parse_dec
    make_token TOK_WHILE "while" _parse_while
    make_token TOK_ADD "+" null
    make_token TOK_SUB "-" null
    make_token TOK_DIV "/" null
    make_token TOK_MOD "%" null
    make_token TOK_MUL "*" null
    make_token TOK_MATH "math" _parse_math
    make_token TOK_COMMENT "//" _parse_comment
    make_token TOK_COMMA "," null
    make_token TOK_LOCAL "local" _parse_local
    make_token TOK_RETURN "return" _parse_return
    make_token TOK_BREAK "break" _parse_break
    make_token TOK_ASM "asm" _parse_asm
    make_token TOK_ARROW "->" null
    make_token TOK_CONTINUE "continue" _parse_continue

    pop bx
    pop ax
    ret
endp

enum _TYPE_ {
    TYPE_INT,
    TYPE_STR,
    TYPE_CHAR,
    TYPE_BOOL,
    TYPE_VOID
}
