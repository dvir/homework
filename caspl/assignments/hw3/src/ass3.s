global _start
global print_newline
global print_freq, state, cycles, WorldSize
extern init_co, start_co, resume
extern scheduler, printer


;; /usr/include/asm/unistd_32.h
sys_exit:       equ   1
sys_write:      equ   4
stdout:         equ   1

section .bss
    result: resd 1                  ; tmp result slot
    print_freq: resd 1              ; printing frequency 
    state:  resb 100                ; state array, consisting of 0's and 1's
    cycles: resd 1                  ; amount of cycles to execute
    WorldSize: resd 1

section .data
    error_msg: db 'Fatal error, aborting.', 10
    transitions: db "00110011"
    newline: db 10

section .text

_start:
        enter 0, 0
    
        ; arguments:
        ; [ebp+4] - argc
        ; [ebp+8] - program name
        ; [ebp+12] - initial state string
        ; [ebp+16] - amount of cycles (string)
        ; [ebp+20] - printing freq (string)
        ; [ebp+24] - transition func

        cmp dword [ebp+4], 5
        jne .wrong_parameter_count

        push dword [ebp+12]
        call create_state
        add esp, 4

        push dword [ebp+12]
        call strlen
        add esp, 4
        mov [WorldSize], eax

        cmp dword [WorldSize], 100
        jg .too_many_cells

        cmp dword [WorldSize], 0
        je .too_few_cells

        push dword [ebp+16]
        call atoi
        add esp, 4
        mov [cycles], eax

        cmp dword [cycles], 0
        jle .too_few_cycles
        
        push dword [ebp+20]
        call atoi
        add esp, 4

        cmp eax, 0
        jle .invalid_print_freq

        mov [print_freq], eax

        push dword [ebp+24]
        call set_transitions
        add esp, 4

        cmp eax, -1
        je .invalid_transitions

        xor ebx, ebx            ; scheduler is co-routine 0
        mov edx, scheduler
        call init_co            ; initialize scheduler state

        inc ebx                 ; printer i co-routine 1
        mov edx, printer
        call init_co            ; initialize printer state

        ; init cells co-routines
        mov ecx, dword [WorldSize]
        mov edx, cell
.init_cells_loop:
        inc ebx
        call init_co
        loop .init_cells_loop


        xor ebx, ebx            ; starting co-routine = scheduler
        call start_co           ; start co-routines

        jmp .end

.invalid_transitions:
.invalid_print_freq:
.too_few_cycles:
.too_few_cells:
.too_many_cells:
.wrong_parameter_count:
    ; error occurred!
    mov eax, sys_write
    mov ebx, stdout
    mov ecx, error_msg 
    mov edx, 23 
    int 80h
    jmp .end

.end:
        ;; exit
        mov eax, sys_exit
        xor ebx, ebx
        int 80h

left:
        push ebp
        mov ebp, esp 
        pusha

        mov eax, dword [WorldSize]
        dec eax

        ; arguments:
        ; [ebp+8] - num
        cmp dword [ebp+8], 0
        je .end
     
        mov eax, dword [ebp+8]
        dec eax
.end:
        mov dword [result], eax
        popa
        mov eax, dword [result]
        mov esp, ebp
        pop ebp
        ret

right:
        push ebp
        mov ebp, esp 
        pusha

        mov eax, 0

        ; arguments:
        ; [ebp+8] - num
        mov ebx, dword [WorldSize]
        dec ebx
        cmp dword [ebp+8], ebx
        je .end
     
        mov eax, dword [ebp+8]
        inc eax
.end:
        mov dword [result], eax
        popa
        mov eax, dword [result]
        mov esp, ebp
        pop ebp
        ret

cell:
        mov edx, ebx
        sub edx, 2

        ; get current state with neighbors
        mov ecx, 0

        push edx
        call left
        add esp, 4
        mov ah, byte [state + eax]
        sub ah, '0'
        add cl, ah

        shl cl, 1
        mov ah, byte [state + edx]
        sub ah, '0'
        add cl, ah
        shl cl, 1

        push edx
        call right
        add esp, 4
        mov ah, byte [state + eax]
        sub ah, '0'
        add cl, ah

        mov al, byte [transitions + ecx]

        pusha
        mov ebx, 0
        call resume
        popa

        mov byte [state + edx], al

        pusha
        mov ebx, 0
        call resume             ; resume scheduler
        popa

        jmp cell


create_state:
    push ebp
    mov ebp, esp
    pusha

    ; arguments:
    ; [ebp+8] - string pointer to initial state

    mov ecx, [ebp+8]
    mov edx, 0
.loop:    
    cmp byte [ecx], 0
    je .end

    mov al, byte [ecx]
    mov byte [state+edx], al
    
    inc edx
    inc ecx
    jmp .loop
.end:
    popa
    mov esp, ebp
    pop ebp
    ret

atoi:
    push ebp
    mov ebp, esp
    pusha
    ; [ebp+8] - string pointer to transitions func
    mov ecx, [ebp+8]
    mov eax, 0
.loop:    
    cmp byte [ecx], 0
    je .end

    movzx ebx, byte [ecx]
    sub ebx, '0'

    cmp ebx, 9
    jg .invalid

    cmp ebx, 0
    jl .invalid
    
    ; multiply previous number by 10 and add the current number
    imul eax, 10
    add eax, ebx 

    ; check for overflow
    jo .invalid

    inc ecx
    jmp .loop

.invalid:
    mov eax, -1

.end:
    mov [result], eax
    popa
    mov eax, [result]
    mov esp, ebp
    pop ebp
    ret

strlen:
    push ebp
    mov ebp, esp
    pusha
    ; [ebp+8] - string pointer to transitions func
    mov ecx, [ebp+8]
    mov eax, 0
.loop:    
    cmp byte [ecx], 0
    je .end

    inc eax

    inc ecx
    jmp .loop
.end:
    mov [result], eax
    popa
    mov eax, [result]
    mov esp, ebp
    pop ebp
    ret

char2hex:
    push ebp
    mov ebp, esp

    ; function aguments
    ; ebp+8 - character to convert to hexa

    mov eax, [ebp+8]

    ; if al >= 'a'
    cmp al, 'a'
        jge .ge_a

    ; if al >= 'A'
    cmp al, 'A'
        jge .ge_A

    ; if al >= '0'
    cmp al, '0'
        jge .ge_0

    ; invalid character
    jmp .invalid

    .ge_a: ; a <= al
        ; if al <= 'f'
        cmp al, 'f'
            jle .a_f
        ; else
            jmp .invalid 

    .ge_A: ; A <= al
        ; if al <= 'F'
        cmp al, 'F'
            jle .A_F
        ; else
            jmp .invalid 

    .ge_0: ; 0 <= al
        ; if al <= '9'
        cmp al, '9'
            jle .n0_9
        ; else
            jmp .invalid 

    .a_f: ; [a, f]
        sub al, 'a'
        add al, 10
        jmp .end 

    .A_F: ; [A, F]
        sub al, 'A'
        add al, 10
        jmp .end 

    .n0_9: ; [0, 9]
        sub al, '0'
        jmp .end 

.invalid:
    mov eax, -1

.end:
    mov esp, ebp
    pop ebp
    ret

set_transitions:
    push ebp
    mov ebp, esp
    pusha

    ; [ebp+8] - string pointer to convert to number
    mov ecx, [ebp+8]
.loop:  
    mov eax, 0

    cmp byte [ecx], 0
    je .end

    movzx ebx, byte [ecx]
    push ebx
    call char2hex
    add esp, 4

    cmp eax, -1
    je .invalid

    ; eax contains a 4-bit number represents the transition that needs to be 
    ; updated with the new action.
    ; extract the last bit
    mov edx, eax
    and dl, 1
    add dl, '0' ; make it the proper 0 or 1 character
    shr eax, 1

; update transition func
    mov byte [transitions+eax], dl

    inc ecx
    jmp .loop

.invalid:
    mov eax, -1

.end:
    mov [result], eax
    popa
    mov eax, [result]
    mov esp, ebp
    pop ebp
    ret

print_newline:
        pusha
        mov eax, sys_write
        mov ebx, stdout
        mov ecx, newline 
        mov edx, 1 
        int 80h
        popa
        
        ret
