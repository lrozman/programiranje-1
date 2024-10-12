;menjava pomnilskih celic
JMP main
    DB 10 
    DB 56
    DB 60
    DB 46
    DB 44
    DB 58
    DB 42
    DB 52
    DB 48
    DB 54

main:
MOV A, 3 ; mesto, ki ga hočemo zamenjati
MOV B, 7; mesto, s katerim menjamo

PUSH [A]
MOV C, [B]
MOV [A], C
POP C
MOV [B], C