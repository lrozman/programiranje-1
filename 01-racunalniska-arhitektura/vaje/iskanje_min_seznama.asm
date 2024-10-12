;iskanje najmanjšega stevila v seznamu

JMP main
dolzina:
    DB 10
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
;na A si bom dala pointer, kje v seznamu sem. Potem bom mela en register seprav za to, pa za primerjat a loh iz memoryja? in pač greš, če je manjše pa zapišeš

MOV A, seznam ; kle se začne seznam
MOV B, [seznam] ;vrednost na B
MOV [minimum], B ; ta prvi je najprej minimum
; zdej pa če je seznam daljši kot ena
CMP A, minimum
JB zanka

zanka:
INC A
CMP A, minimum
JA end
MOV B, [A]
CMP B, [minimum]
JB manjsi
JMP zanka

manjsi:
MOV [minimum], B
JMP zanka

end:
HLT

;mogoče poprav nakonc, da se zanka začne pred prvo in je pol vmes jump na end

;MOV C, [A]
;CMP C, 

;MOV C, [A+1]
;MOV [minimum], C


; MOV A, [dolzina]
; MOV A, dolzina   - za pregled oboje
    


