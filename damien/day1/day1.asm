global _start

section .data
filename db "input.txt", 0
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

stringToInt: ;rax is start of number, rax is return
    xor rcx,rcx
    xor rdx, rdx
    
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
    mov rax, rdx
    ret
