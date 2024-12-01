global _start

section .data
filename db "input2.txt", 0
newline db `\n`, 0
numberString db "42616 "
lenFilename equ $ -filename
STD_OUT equ 1
ASCII_NUMBERS_START equ 0x30
ASCII_NUMBERS_END equ 0x3A

section .text
_start:
 ;   mov rax, 1
 ;   mov rdi, 1
 ;   mov rsi, offset message
 ;   mov rdx, 15
 ;   syscall

    mov rax, filename
    call write

    mov rax, newline
    call write

; get file descriptor in rax
    call open

; read file
    mov rdi, rax
    mov rax, 0
; allocate buffer
    sub rsp, 256
    
    mov rax, filename
    mov rsi, rsp
    mov rdx, 256
    call readToBuffer

    mov rax, rsp
    mov r15, rax
    call write
;;;;;;; test sort

    sub rsp, (5 * 8)
    mov rax, rsp
    mov qword [rax], 3 
    mov qword [rax + 1 * 8], 4
    mov qword [rax + 2 * 8], 5
    mov qword [rax + 3 * 8], 2
    mov qword [rax + 4 * 8], 0

    mov rdi, 5
    call sort

;;;; test string to int
    mov rax, numberString
    call stringToInt

;;;; test count lines
    mov rax, r15
    call countLines
    

part1:
    push    rbp
    mov     rbp, rsp
    sub rsp, 14000 * 2
    mov r15, rsp ; r15 holds the file pointer
    mov r14, r15 ; r14 holds the floating file pointer

    mov rax, filename
    mov rsi, r15
    mov rdx, 14000 * 2
    call readToBuffer

    mov rax, r15
    call countLines
    
    mov r11, rdx ; r11 holds the list len
    shl rdx, 3
    sub rsp, rdx
    mov r13, rsp ; r13 holds list 1
    sub rsp, rdx
    mov r12, rsp ; r12 holds list 2
    
    xor rcx, rcx ; rcx is the loop counter
    mov rax, r14
part1ParseLoop:
    push r11
    
    push rcx
    call stringToInt
    pop rcx
    
    mov [r13 + rcx * 8], rdx
    add rax, 3
    
    push rcx
    call stringToInt
    add rax, 1
    pop rcx
    
    mov [r12 + rcx * 8], rdx
    pop r11
    inc rcx
    cmp rcx, r11
    jge part1ParseEnd
    jmp part1ParseLoop

part1ParseEnd:
    mov rax, r13
    mov rdi, r11
    push r13
    push r12
    push r11
    call sort
    pop r11
    pop r12
    pop r13

    mov rax, r12
    mov rdi, r11
    push r13
    push r12
    push r11
    call sort
    pop r11
    pop r12
    pop r13

    xor rcx, rcx
    xor rdx, rdx
    sub rcx, 1
    sub r11, 1
part1CountLoop:
    inc rcx
    mov rbx, [r13 + rcx * 8]
    mov rax, [r12 + rcx * 8]
    
    cmp rax, rbx
    jg raxBigger

rbxBigger:
    sub rbx, rax
    add rdx, rbx
    jmp doneSubbing
    
raxBigger:
    sub rax, rbx
    add rdx, rax
    
doneSubbing:
    cmp rcx, r11
    jl part1CountLoop

part2:
    xor rcx, rcx
    xor rdx, rdx
    xor rbx, rbx
    inc r11
    sub rcx, 1
part2Loop:
    inc rcx
    cmp rcx, r11
    jge part2EndLoop

    mov r10, qword [r13 + rcx * 8]
    mov rax, r12
    mov rdx, r11
    mov rdi, r10
    call countNumbers
    
    push rax
    mov rax, r10
    mov rdx, r8
    mul rdx
    add rbx, rax

    pop rax
    jmp part2Loop

part2EndLoop:
    
    
exit:
    mov rax, 60
    mov rdi, 0
    syscall
    
open: ;rax is filename
    mov rdi, rax
    mov rax, 2
    push rsi
    mov rsi, 0
    syscall
    pop rsi
    ret

write: ;rax is buffer
    mov rsi, rax ; set buffer ptr
    call strlen
    mov rdx, rax ; set len
    mov rax, 1 ; write opcode
    mov rdi, STD_OUT ; file descriptor
    syscall
    ret
    
readToBuffer: ;rax is filename, rsi is buffer addr, rdx is count
    call open
    mov rdi, rax
    mov rax, 0
    syscall
    ret

strlen:
    xor rdx, rdx
    
startLoop:
    add rdx, 1
    mov rcx, [rax + rdx]
    cmp cl, 0
    jne startLoop
    mov rax, rdx
    ret

sort: ;rax is buffer to sort, rdi is buffer length
    push rdx
    sub rdi, 1
    
    xor r8, r8 ; outer loop counter
    xor r9, r9 ; loop counter compare temp
startSortLoopOuter:
    xor r10, r10; swapped? 

    xor r11, r11; inner loop counter
startSortInnerLoop:
    mov r12, r11
    inc r12
    mov rcx, [rax + r11 * 8]
    mov rdx, [rax + r12 * 8]
    cmp rcx, rdx
    jle didntSwap
    
    mov [rax + r11 * 8], rdx
    mov [rax + r12 * 8], rcx
    mov r10, 1

didntSwap:
    mov r9, rdi
    sub r9, 1
    sub r9, r8
    add r9, 1
    add r11, 1
    cmp r11, r9
    jl startSortInnerLoop

    ; break if swapped
    cmp r10, 0 
    je endSortOuterLoop
    
    mov r9, rdi
    sub r9, 1
    add r9, 1
    add r8, 1
    cmp r8, r9
    jl startSortLoopOuter

endSortOuterLoop:
    pop rdx
    ret

stringToInt: ;rax is start of number, rax is new offset, rdx is return
    xor rcx,rcx
    xor rdx, rdx
    xor rbx, rbx
    
stringToIntMiddle:
    mov bl, byte [rax + rcx]
    sub rbx, ASCII_NUMBERS_START
    cmp rbx, 10 
    jge stringToIntDone
    cmp rbx, 0
    jl stringToIntDone

    push rax
    mov rax, 10

    mul rdx
    mov rdx, rax
    add rdx, rbx
    pop rax
    
    inc rcx
    jmp stringToIntMiddle
    
stringToIntDone:
    add rax, rcx
    ret

countLines: ;rax is buffer, rdx is return
    xor rcx, rcx
    xor rdx, rdx
    push rax
countLinesMiddle:
    mov cl, byte [rax]
    inc rax
    cmp cl, 0
    je endCountLines
    cmp cl, 0xa
    jne countLinesMiddle
    inc rdx
    jmp countLinesMiddle
    
endCountLines:
    pop rax
    ret
    
countNumbers: ;rax is buffer, rdx is buffer len, rdi is number, r8 is return
    push rcx
    
    xor rcx,rcx
    xor r8, r8
    sub rcx, 1
countNumbersLoop:
    inc rcx
    cmp rcx, rdx
    jge countNumbersEndLoop

    mov r9, qword [rax + rcx *8]

    cmp r9, rdi
    jne countNumbersLoop

    inc r8
    jmp countNumbersLoop
    
countNumbersEndLoop:
    pop rcx
    ret
    
