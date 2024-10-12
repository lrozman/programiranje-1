JMP main
dolzina:
    DB 10    ; število elementov v seznamu
seznam:
    DB 50    ; seznam
    DB 56
    DB 60
    DB 46
    DB 44
    DB 58
    DB 42
    DB 52
    DB 40
    DB 4
minimum:
    DB 0    ; na koncu bo tu minimum

main:
MOV A, seznam
MOV B, [seznam]
MOV [minimum], B

zanka:
CMP A, minimum
JA end
MOV B, [A]
CMP B, [minimum]
JB manjsi
INC A
JMP zanka

manjsi:
MOV [minimum], B
JMP zanka

end:
HLT
    