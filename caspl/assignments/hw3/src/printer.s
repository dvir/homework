global printer
extern resume
extern print_newline

;; /usr/include/asm/unistd_32.h
sys_write:      equ   4
stdout:         equ   1


section .data

section .text
    extern state
    extern WorldSize

printer:
        mov eax, sys_write
        mov ebx, stdout
        mov ecx, state 
        mov edx, [WorldSize]
        int 80h

        call print_newline

        xor ebx, ebx
        call resume             ; resume scheduler

        jmp printer
