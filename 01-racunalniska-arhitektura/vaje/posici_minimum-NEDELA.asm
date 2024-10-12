; prvi poskus, nardi narobe, ker return ne dela
; če bi mela register D, bi blo to easy peasy

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
    DB 48
    DB 54
minimum:
    DB 0    ; na koncu bo tu minimum

main:
    ; pripravimo parametre funkcije
    MOV A, seznam
    MOV C, seznam
    ADD C, [dolzina]
    ; pokličemo funkcijo
    CALL poisci_minimum
    ; v mesto, na katerega kaže minimum, shranimo vrednost, na katero kaže B
    ; ker tega ne moremo narediti direktno, si pomagamo z registrom C
    PUSH C 
    MOV C, [B]
    MOV [minimum], C
    POP C
    HLT

poisci_minimum:

MOV B, [A]
MOV [minimum], B
MOV B, A
;PUSH B ; trenutni indeks minimuma - je že na B-ju

zanka_stetja:
INC B
CMP B, C
JAE end

primerjaj_min:
PUSH B
MOV B, [B] ;imam vrednost tega
CMP B, [minimum]
JB je_manjsi

ni_manjsi:
POP B
JMP zanka_stetja

je_manjsi:
MOV [minimum], B
; zadnji na stacku je indeks novega minimuma
POP B
PUSH B ; da mamo ta indeks na stacku
JMP zanka_stetja

end:
RET