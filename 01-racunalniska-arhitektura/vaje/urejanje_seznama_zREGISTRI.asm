; ne vem, kako bi to naredila brez obeh registrov preostalih registrov ?
; in ali je to preveč neučinkovito?

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
                        ; pripravimo argumente za funkcijo uredi
    MOV A, seznam       ; register A mora kazati na prvi element
    MOV C, seznam       ; register C mora kazati na zadnji element + 1
    ADD C, [dolzina]
    CALL uredi          ; pokličemo funkcijo za urejanje
    HLT                 ; prekinemo izvajanje 

uredi:
PUSH A
PUSH B
PUSH D ; da jih lahko vrnemo nazaj

; kopiran program od prej, indeks najmanjšega

indeks_najmanjsega:
MOV B, [A]
MOV [minimum], B
MOV B, A
MOV D, B ; shrani indeks minimuma

zanka_stetja:
INC B
CMP B, C
JAE end_vmesni

primerjaj_min:
PUSH B
MOV B, [B] ;imam vrednost tega
CMP B, [minimum]
JB je_manjsi
POP B
JMP zanka_stetja

je_manjsi:
MOV [minimum], B
; zadnji na stacku je indeks novega minimuma
POP B
MOV D, B ; da mamo ta indeks shranjen
JMP zanka_stetja

end_vmesni:
; na D-ju je indeks najmanjsega
MOV B, [A] ; damo tja prvega
MOV [D], B ; damo prvega na mesto najmanjsega
MOV D, [minimum]
MOV [A], D ;minimum je na zacetku zdej
INC A
CMP A, C
JB indeks_najmanjsega

; če sta pa ista, je seznam urejen
POP D
POP B
POP A
RET