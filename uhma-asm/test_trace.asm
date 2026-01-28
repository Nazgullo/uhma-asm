; Test file: all tokens either abstract (NUM/HEX) or repeat
; Pattern: mov REG, NUM or mov REG, HEX
mov eax, 100
mov ebx, 200
mov ecx, 300
mov edx, 400
mov eax, 0x10
mov ebx, 0x20
mov ecx, 0x30
mov edx, 0x40
add eax, 10
add ebx, 20
add ecx, 30
add edx, 40
sub eax, 5
sub ebx, 6
sub ecx, 7
sub edx, 8
xor eax, eax
xor ebx, ebx
xor ecx, ecx
xor edx, edx
mov eax, 0xAA
mov ebx, 0xBB
mov ecx, 0xCC
mov edx, 0xDD
push 100
push 200
push 300
push 400
pop eax
pop ebx
pop ecx
pop edx
cmp eax, 0
cmp ebx, 1
cmp ecx, 2
cmp edx, 3
jmp label1
jmp label2
jmp label3
jmp label4
call func1
call func2
call func3
call func4
ret
ret
ret
ret
