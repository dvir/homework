%define MAX_STACK_SIZE 5

; linked list
%define next 0
%define data 4

section	.rodata
PROMPT_MESSAGE:
    DB  "calc: ", 0
STACKOVERFLOW:
    DB  "Error: Stack Overflow", 10, 0
NOT_ENOUGH_ARGUMENTS:
    DB  "Error: Not Enough Arguments on Stack", 10, 0

OP_COUNTER_MESSAGE:
    DB  "Operations performed: %d", 10, 0

PRINT_NUM_MESSAGE:
    DB  "%d", 10, 0

PRINT_DIGIT_MESSAGE:
    DB  "%x", 0

PRINT_NEWLINE:
    DB  10, 0

DEBUG:
    DB  "link: %d , next: %d", 10, 0

section .bss
INPUT:
    RESB    8
result:
    RESD    1

section .text
	align 16
	global main
	extern printf
    extern malloc
    extern gets

main:
    call calc ; stores operation counter in eax
    
    push eax ; push ops counter
    push OP_COUNTER_MESSAGE
    call printf
    add esp, 8

    mov ebx, 0
    mov eax, 1
    int 80h

calc:
	push ebp
	mov	ebp, esp	; Entry code - set up ebp and esp
	pushad			; Save registers

    sub esp, 16 ; save room for local variables:
    %define op_counter ebp-4 ; 1. operation counter
    %define stack_size ebp-8 ; 2. current stack size
    %define stack_base ebp-12; 3. current stack
    %define calc_mode  ebp-16; 3. calc mode

    mov dword [op_counter], 0 ; reset operation counter
    mov dword [stack_size], 0 ; reset current stack size
    mov byte  [calc_mode], 'h'

    ; allocate memory for operands stack
    push 4*MAX_STACK_SIZE
    call malloc
    add esp, 4
    mov dword [stack_base], eax

loop:
    push    PROMPT_MESSAGE
    call    printf
    add     esp, 4

    push    INPUT
    call    gets
    add     esp, 4

    inc dword [op_counter]

    ; choose operation
    cmp byte [INPUT], '+'
    je addition

    cmp byte [INPUT], 'p'
    je pop_and_print

    cmp byte [INPUT], 'd'
    je duplicate

    cmp byte [INPUT], '^'
    je exponent

    cmp byte [INPUT], 'x'
    je bitwise_xor

    cmp byte [INPUT], 'o'
    je octal

    cmp byte [INPUT], 'h'
    je hexa 

    cmp byte [INPUT], 'q'
    je .exit

    ; it's not an operation - it's a number.
    ; we should iterate on it and create a number linked list for it, and
    ; push it into the the operands stack.

    ; first, check if we have room for it in the stack.
    cmp dword [stack_size], MAX_STACK_SIZE
    je stackoverflow

    mov edx, 0 ; holds the head of the linked list
    mov ecx, INPUT
    .foreach_digit:
        movzx eax, byte [ecx]
        cmp eax, 0
        je .end_foreach_digit

        push eax
        call char2hex 
        add esp, 4

        ; got a valid hexa digit in al.
        ; create a new digit link for it and append the previous list to it
        push edx
        push eax
        call create_num
        add esp, 8

        ; eax holds the address to the new link created
        mov edx, eax ; set the current number linked list ptr to the new link

    .skip:
        inc ecx
        jmp .foreach_digit
    .end_foreach_digit:
        ; if an empty string / invalid number given, ignore it
        cmp edx, 0
        je loop
        
        ; push the new created number to the operands stack
        mov ebx, dword [stack_base]
        mov ecx, dword [stack_size]
        mov dword [ebx + 4*ecx], edx
        inc dword [stack_size]

    jmp loop 

.exit:
    ; print all numbers in the operands stack
    mov eax, 0
    mov ebx, dword [stack_base]
    .foreach_stack:
        cmp dword [stack_size], 0
        je .end_foreach_stack

        ; print the top of the stack and pop it
        mov ecx, dword [stack_size]
        mov edx, dword [ebx + 4*ecx + (-4)]

        push edx
        call print_num
        add esp, 4

        dec dword [stack_size] 
        jmp .foreach_stack
    .end_foreach_stack:

    mov eax, dword [op_counter]
    mov dword [result], eax

    add esp, 16 ; remove local variables
	popad			; Restore registers
    mov eax, dword [result] ; return operation counter
    mov	esp, ebp	; Function exit code
    pop	ebp
	ret

create_num:
    push ebp
    mov ebp, esp
    pushad

    ; function parameters
    ; ebp+8 - num byte to create
    ; ebp+12 - next address

    push 5
    call malloc
    add esp, 4

    ; eax holds the address to the 5 bytes we allocated
    mov edx, dword [ebp+12]
    mov dword [eax + next], edx
    mov bl, byte [ebp+8]
    mov byte [eax + data], bl

    mov dword [result], eax
    
    popad
    mov eax, dword [result]
    mov esp, ebp
    pop ebp
    ret

addition:
    cmp dword [stack_size], 2
    jl error_missing_args

    mov ebx, dword [stack_base]
    mov ecx, dword [stack_size]
    push dword [ebx + 4*ecx + (-4)]
    push dword [ebx + 4*ecx + (-8)] 
    call func_addition
    add esp, 8

    dec dword [stack_size]
    dec dword [stack_size]

    ; push the new created number to the operands stack
    mov ebx, dword [stack_base]
    mov ecx, dword [stack_size]
    mov dword [ebx + 4*ecx], eax
    inc dword [stack_size]
    jmp loop

func_addition:
    push ebp
    mov ebp, esp
    pushad
    
    ; function parameters
    %define n1 ebp+8 ; first number
    %define n2 ebp+12 ; second number

    ; local variables
    sub esp, 16
    ; ebp-8  - new number pointer
    ; ebp-12 - carry
    ; ebp-16 - n1
    ; ebp-20 - n2

    mov dword [ebp-8], 0
    mov dword [ebp-12], 0 ; reset carry

    mov edx, dword [ebp+8]
    mov dword [ebp-16], edx
    mov edx, dword [ebp+12]
    mov dword [ebp-20], edx
   
.loop:
    mov ebx, 0

.add_first:
    ; avoid messing with null pointers
    cmp dword [ebp-16], 0
    je .add_second

    mov edx, dword [ebp-16] ; load first number digit address
    add ebx, dword [edx + data] ; add first number

.add_second:
    ; avoid messing with null pointers
    cmp dword [ebp-20], 0
    je .add_carry

    mov edx, dword [ebp-20] ; load second number digit address
    add ebx, dword [edx + data] ; add second number

.add_carry:
    add ebx, dword [ebp-12] ; add carry

    mov dword [ebp-12], ebx
    shr dword [ebp-12], 4

    cmp dword [ebp-12], 0
    jg .has_carry
    jmp .continue

.has_carry:
    ; remove bits higher than 4
    and ebx, 15 ; 1111 in binary
    
.continue:
    push dword [ebp-8]
    push ebx 
    call create_num
    add esp, 8

    mov dword [ebp-8], eax

    ; advance numbers pointers

.advance_first:
    ; avoid messing with null pointers
    cmp dword [ebp-16], 0
    je .advance_second

    mov edx, dword [ebp-16]
    mov ebx, [edx + next]
    mov dword [ebp-16], ebx 

.advance_second:
    ; avoid messing with null pointers
    cmp dword [ebp-20], 0
    je .check_conditions

    mov edx, dword [ebp-20]
    mov ebx, [edx + next]
    mov dword [ebp-20], ebx 

.check_conditions:
    ; if either numbers or carry is not zero, continue
    cmp dword [ebp-16], 0
    jne .loop

    cmp dword [ebp-20], 0
    jne .loop

    cmp dword [ebp-12], 0
    jne .loop

    mov edx, dword [ebp-8]
    mov dword [result], edx

    add esp, 16
    popad
    mov eax, dword [result]
    mov esp, ebp
    pop ebp
    ret

pop_and_print:
    cmp dword [stack_size], 1
    jl error_missing_args

    jmp loop

duplicate:
    cmp dword [stack_size], 1
    jl error_missing_args

    cmp dword [stack_size], MAX_STACK_SIZE
    je stackoverflow

    jmp loop

exponent:
    cmp dword [stack_size], 1
    jl error_missing_args

    jmp loop

bitwise_xor:
    cmp dword [stack_size], 2
    jl error_missing_args

    jmp loop

octal:
    mov byte [calc_mode], 'o'
    jmp loop

hexa:
    mov byte [calc_mode], 'h'
    jmp loop

error_missing_args:
    push NOT_ENOUGH_ARGUMENTS
    call printf
    add esp, 4
    jmp loop

stackoverflow:
    push STACKOVERFLOW 
    call printf
    add esp, 4
    jmp loop

print_num:
    push ebp
    mov ebp, esp
    pushad

    ; ebp+8 - pointer to first num digit
    mov ebx, [ebp+8]
   
.loop:
    cmp ebx, 0
    je .end 

    movzx eax, byte [ebx + data]
    push eax
    push PRINT_DIGIT_MESSAGE
    call printf
    add esp, 8

    mov ebx, [ebx + next]
    jmp .loop

.end:
    call print_newline

    popad
    mov esp, ebp
    pop ebp
    ret

print_newline:
    push PRINT_NEWLINE
    call printf
    add esp, 4
    ret

dec2hex_char:
    push ebp
    mov ebp, esp
    pushad

    ; ebp+8 - dec number to convert to hex char ([0-15])

    mov eax, dword [ebp+8]

    ; if eax > 15
    cmp eax, 15
        jg .invalid

    ; if eax < 0
    cmp eax, 0
        jl .invalid

    ; if eax >= 10
    cmp eax, 10
        jge .n10_15

    ; if eax >= 0
    cmp eax, 0
        jge .n0_9

    .n10_15: ; [10-15]
        add eax, 55
        jmp .end

    .n0_9: ; [0-9]
        add eax, 48
        jmp .end 

.invalid:
    mov eax, -1

.end:
    mov dword [result], eax
    popad
    mov eax, dword [result]
    mov esp, ebp
    pop ebp
    ret

char2hex:
    push ebp
    mov ebp, esp

    %define char ebp+8 ; character to convert to hexa

    mov eax, [char]

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
