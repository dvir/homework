global scheduler
extern resume, end_co

section .bss
    curr_cell: resd 1
    count: resd 1

section .data
    cycles_left: dw 0

section .text
    extern print_freq, state, cycles, WorldSize

scheduler:
    mov dword [count], 0
    mov eax, dword [cycles]
    mov dword [cycles_left], eax
    mov dword [curr_cell], 1

.loop:
    inc dword [count]
    mov ebx, [curr_cell]
    inc ebx
    call resume

    inc dword [curr_cell]

    mov eax, [curr_cell]
    cmp eax, dword [WorldSize]
    jle .not_cycle_end
        mov dword [curr_cell], 1
        dec dword [cycles_left]
.not_cycle_end:
    mov edx, 0
    mov eax, dword [count]
    div dword [print_freq]
    cmp edx, 0
    jne .skip_printer
    
    ; resume printer!
    mov ebx, 1
    call resume

.skip_printer:
    cmp dword [cycles_left], 0 
    jne .loop

.end:
    ; resume printer one more time before exiting
    mov ebx, 1
    call resume

    call end_co    ; stop co-routines
