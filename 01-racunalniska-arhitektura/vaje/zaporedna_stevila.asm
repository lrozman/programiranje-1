; zaporedna števila

MOV A, 13
MOV B, 42

loop:
PUSH A
INC A
CMP A, B
JBE loop