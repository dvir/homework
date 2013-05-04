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
    DB  "%d", 0

PRINT_HEX_DIGIT_MESSAGE:
    DB  "%x", 0

PRINT_NEWLINE:
    DB  10, 0

DEBUG:
    DB  "link: %d , next: %d", 10, 0

section .bss
INPUT:
    RESB    80
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
    %define calc_mode  ebp-16; 4. calc mode

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

    mov edx, 0 ; holds the head of the linked list
    mov ecx, INPUT
    .foreach_digit:
        movzx eax, byte [ecx]
        cmp eax, 0
        je .end_foreach_digit

        cmp byte [calc_mode], 'h'
        je .hex

    ; octal
        push eax
        call char2octal
        add esp, 4
        jmp .got_digit

    .hex:
        push eax
        call char2hex 
        add esp, 4

    .got_digit:
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
        
        ; check if we have room for it in the stack.
        cmp dword [stack_size], MAX_STACK_SIZE
        je stackoverflow

        cmp byte [calc_mode], 'o'
        je .octal
        ; else
        jmp .push

    .octal: ; convert from octal to hex
        push edx
        call octal2hex
        add esp, 4

        mov edx, eax
        
    .push:
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

        cmp byte [calc_mode], 'h'
        je .print
   
        ; octal mode
        ; convert from hex to octal before printing
        push edx
        call hex2octal
        add esp, 4
        mov edx, eax

    .print:
        push dword [calc_mode]
        push edx
        call print_num
        add esp, 8

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

    dec dword [stack_size] ; pop first operand
    dec dword [stack_size] ; pop second operand

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
    ; ebp-4  - new number pointer
    ; ebp-8 - carry
    ; ebp-12 - n1
    ; ebp-16 - n2

    mov dword [ebp-4], 0
    mov dword [ebp-8], 0 ; reset carry

    mov edx, dword [ebp+8]
    mov dword [ebp-12], edx
    mov edx, dword [ebp+12]
    mov dword [ebp-16], edx
   
.loop:
    mov ebx, 0

.add_first:
    ; avoid messing with null pointers
    cmp dword [ebp-12], 0
    je .add_second

    mov edx, dword [ebp-12] ; load first number digit address
    add ebx, dword [edx + data] ; add first number

.add_second:
    ; avoid messing with null pointers
    cmp dword [ebp-16], 0
    je .add_carry

    mov edx, dword [ebp-16] ; load second number digit address
    add ebx, dword [edx + data] ; add second number

.add_carry:
    add ebx, dword [ebp-8] ; add carry

    mov dword [ebp-8], ebx
    shr dword [ebp-8], 4

    cmp dword [ebp-8], 0
    jg .has_carry
    jmp .continue

.has_carry:
    ; remove bits higher than 4
    and ebx, 15 ; 1111 in binary
    
.continue:
    push dword [ebp-4]
    push ebx 
    call create_num
    add esp, 8

    mov dword [ebp-4], eax

    ; advance numbers pointers

.advance_first:
    ; avoid messing with null pointers
    cmp dword [ebp-12], 0
    je .advance_second

    mov edx, dword [ebp-12]
    mov ebx, [edx + next]
    mov dword [ebp-12], ebx 

.advance_second:
    ; avoid messing with null pointers
    cmp dword [ebp-16], 0
    je .check_conditions

    mov edx, dword [ebp-16]
    mov ebx, [edx + next]
    mov dword [ebp-16], ebx 

.check_conditions:
    ; if either numbers or carry is not zero, continue
    cmp dword [ebp-12], 0
    jne .loop

    cmp dword [ebp-16], 0
    jne .loop

    cmp dword [ebp-8], 0
    jne .loop

    mov edx, dword [ebp-4]
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

    mov ebx, dword [stack_base]
    mov ecx, dword [stack_size]
    mov edx, dword [ebx + 4*ecx + (-4)]

    cmp byte [calc_mode], 'h'
    je .print

    ; octal mode
    ; convert from hex to octal before printing
    push edx
    call hex2octal
    add esp, 4
    mov edx, eax

.print:
    push dword [calc_mode]
    push edx
    call print_num
    add esp, 8

    dec dword [stack_size] ; pop from operands stack

    ;push dword [stack_base]
    ;call func_pop_and_print
    ;add esp, 4

    jmp loop

func_pop_and_print:
    push ebp
    mov ebp, esp
    pushad

    popad
    mov esp, ebp
    pop ebp

duplicate:
    cmp dword [stack_size], 1
    jl error_missing_args

    cmp dword [stack_size], MAX_STACK_SIZE
    je stackoverflow

    mov ebx, dword [stack_base]
    mov ecx, dword [stack_size]

    mov eax, dword [ebx + 4*ecx + (-4)] ; get stack head value

    mov dword [ebx + 4*ecx], eax ; push to operands stack
    inc dword [stack_size]

    jmp loop

exponent:
    cmp dword [stack_size], 1
    jl error_missing_args

    mov ebx, dword [stack_base]
    mov ecx, dword [stack_size]
    push dword [ebx + 4*ecx + (-4)]
    call func_exponent
    add esp, 4
    dec dword [stack_size] ; pop from operands stack

    mov dword [ebx + 4*ecx], eax ; push to operands stack
    inc dword [stack_size]

    jmp loop

func_shl:
    push ebp
    mov ebp, esp
    pushad
    
    ; function parameters
    ; ebp+8 - number to shift

    ; local variables
    sub esp, 12
    ; ebp-4 - new number pointer
    ; ebp-8 - num pointer
    ; ebp-12 - carry

    mov dword [ebp-4], 0 ; reset new num pointer

    mov edx, dword [ebp+8] ; set num pointer
    mov dword [ebp-8], edx

    mov dword [ebp-12], 0 ; reset carry

.loop:
    cmp dword [ebp-12], 0
    je .end

    jmp loop

.end:
    mov edx, dword [ebp-4]
    mov dword [result], edx

    add esp, 12
    popad
    mov eax, dword [result]
    mov esp, ebp
    pop ebp
    ret

func_exponent:
    push ebp
    mov ebp, esp
    pushad
    
    ; function parameters
    ; ebp+8 - exp

    ; local variables
    sub esp, 8
    ; ebp-4  - new number pointer
    ; ebp-8  - exp

    mov dword [ebp-4], 0

    mov edx, dword [ebp+8]
    mov dword [ebp-8], edx
   
.loop:
    mov ebx, 0

.add_first:
    ; avoid messing with null pointers
    cmp dword [ebp-12], 0
    je .add_second

    mov edx, dword [ebp-12] ; load first number digit address
    add ebx, dword [edx + data] ; add first number

.add_second:
    ; avoid messing with null pointers
    cmp dword [ebp-16], 0
    je .add_carry

    mov edx, dword [ebp-16] ; load second number digit address
    add ebx, dword [edx + data] ; add second number

.add_carry:
    add ebx, dword [ebp-8] ; add carry

    mov dword [ebp-8], ebx
    shr dword [ebp-8], 4

    cmp dword [ebp-8], 0
    jg .has_carry
    jmp .continue

.has_carry:
    ; remove bits higher than 4
    and ebx, 15 ; 1111 in binary
    
.continue:
    push dword [ebp-4]
    push ebx 
    call create_num
    add esp, 8

    mov dword [ebp-4], eax

    ; advance numbers pointers

.advance_first:
    ; avoid messing with null pointers
    cmp dword [ebp-12], 0
    je .advance_second

    mov edx, dword [ebp-12]
    mov ebx, [edx + next]
    mov dword [ebp-12], ebx 

.advance_second:
    ; avoid messing with null pointers
    cmp dword [ebp-16], 0
    je .check_conditions

    mov edx, dword [ebp-16]
    mov ebx, [edx + next]
    mov dword [ebp-16], ebx 

.check_conditions:
    ; if either numbers or carry is not zero, continue
    cmp dword [ebp-12], 0
    jne .loop

    cmp dword [ebp-16], 0
    jne .loop

    cmp dword [ebp-8], 0
    jne .loop

    mov edx, dword [ebp-4]
    mov dword [result], edx

    add esp, 16
    popad
    mov eax, dword [result]
    mov esp, ebp
    pop ebp
    ret
bitwise_xor:
    cmp dword [stack_size], 2
    jl error_missing_args

    mov ebx, dword [stack_base]
    mov ecx, dword [stack_size]
    push dword [ebx + 4*ecx + (-4)]
    push dword [ebx + 4*ecx + (-8)] 
    call func_bitwise_xor
    add esp, 8

    dec dword [stack_size] ; pop first operand
    dec dword [stack_size] ; pop second operand

    ; push the new created number to the operands stack
    mov ebx, dword [stack_base]
    mov ecx, dword [stack_size]
    mov dword [ebx + 4*ecx], eax
    inc dword [stack_size]

    jmp loop

func_bitwise_xor:
    push ebp
    mov ebp, esp
    pushad
    
    ; function parameters
    ; ebp+8 - first number
    ; ebp+12 - second number

    ; local variables
    sub esp, 12
    ; ebp-4  - new number pointer
    ; ebp-8 - n1
    ; ebp-12 - n2

    mov dword [ebp-4], 0

    mov edx, dword [ebp+8]
    mov dword [ebp-8], edx
    mov edx, dword [ebp+12]
    mov dword [ebp-12], edx
   
.loop:
    mov ebx, 0

.get_first:
    ; avoid messing with null pointers
    cmp dword [ebp-8], 0
    je .get_second

    mov edx, dword [ebp-8] ; load first number digit address
    mov eax, dword [edx + data] ; add first number

.get_second:
    ; avoid messing with null pointers
    cmp dword [ebp-12], 0
    je .do_xor

    mov edx, dword [ebp-12] ; load second number digit address
    mov ebx, dword [edx + data] ; add second number

.do_xor:
    xor eax, ebx ; xor between the digits
    push dword [ebp-4]
    push eax 
    call create_num
    add esp, 8

    mov dword [ebp-4], eax

    ; advance numbers pointers

.advance_first:
    ; avoid messing with null pointers
    cmp dword [ebp-8], 0
    je .advance_second

    mov edx, dword [ebp-8]
    mov ebx, [edx + next]
    mov dword [ebp-8], ebx 

.advance_second:
    ; avoid messing with null pointers
    cmp dword [ebp-12], 0
    je .check_conditions

    mov edx, dword [ebp-12]
    mov ebx, [edx + next]
    mov dword [ebp-12], ebx 

.check_conditions:
    ; if either numbers or carry is not zero, continue
    cmp dword [ebp-8], 0
    jne .loop

    cmp dword [ebp-12], 0
    jne .loop

    mov edx, dword [ebp-4]
    mov dword [result], edx

    add esp, 12
    popad
    mov eax, dword [result]
    mov esp, ebp
    pop ebp
    ret

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
    mov ebx, dword [ebp+8]

    ; ebp+12 - print mode ('h' - hexa, 'o' - octal)
    mov ecx, dword [ebp+12]
   
.loop:
    cmp ebx, 0
    je .end 

    movzx eax, byte [ebx + data]
    push eax

    cmp byte [ebp+12], 'h'
    je .hexa

    push PRINT_DIGIT_MESSAGE
    jmp .call_printf

.hexa:
    push PRINT_HEX_DIGIT_MESSAGE

.call_printf:
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

char2octal:
    push ebp
    mov ebp, esp
    pushad

    ; function aguments
    ; ebp+8 - character to convert to octal

    mov eax, [ebp+8]

    ; if al >= '0'
    cmp al, '0'
        jge .ge_0

    ; invalid character
    jmp .invalid

    .ge_0: ; 0 <= al
        ; if al <= '7'
        cmp al, '7'
            jle .n0_7
        ; else
            jmp .invalid 

    .n0_7: ; [0, 9]
        sub al, '0'
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

octal2hex:
    push ebp
    mov ebp, esp
    pushad

    ; function aguments
    ; ebp+8 - num pointer to convert to hex

    sub esp, 16
    ; local variables
    ; ebp-4 - num pointer
    ; ebp-8 - new num pointer
    ; ebp-12 - count of bits in buffer
    ; ebp-16 - new num LSB pointer
    mov edx, dword [ebp+8]
    mov dword [ebp-4], edx

    mov dword [ebp-8], 0

    mov dword [ebp-12], 0 ; reset count of bits in buffer

    mov dword [ebp-16], 0 ; reset new num LSB pointer

    mov ebx, 0 ; octal buffer

.loop:
    cmp dword [ebp-4], 0 ; are we done going through the number?
    je .finish

    mov edx, dword [ebp-4]
    mov eax, dword [edx + data] ; get current octal digit
    mov ecx, dword [ebp-12]
    shl eax, cl
    add ebx, eax

    add dword [ebp-12], 3

    cmp dword [ebp-12], 4
    jl .skip ; don't handle octal if the buffer is under 4 bits

    mov eax, ebx ; place octal buffer in eax
    and eax, 15  ; keep only the first 4 bits in the octal buffer
    shr ebx, 4   ; remove first 4 bits in the octal buffer

    sub dword [ebp-12], 4

    push 0
    push eax
    call create_num
    add esp, 8

    cmp dword [ebp-8], 0
    je .set_head

    mov edx, dword [ebp-8]
    mov dword [edx + next], eax

.set_head:
    mov dword [ebp-8], eax

    cmp dword [ebp-16], 0
    jne .skip

    ; new num LSB pointer is unset, set it
    mov dword [ebp-16], eax

.skip:
    ; advance number linked list
    mov edx, dword [ebp-4]
    mov edx, dword [edx + next]
    mov dword [ebp-4], edx
    jmp .loop

.finish:
    ; check if octal buffer still has anything in it
    cmp ebx, 0
    je .end

    ; octal buffer still has one more number
    push 0 
    push ebx 
    call create_num
    add esp, 8

    cmp dword [ebp-8], 0
    je .set_head2

    mov edx, dword [ebp-8]
    mov dword [edx + next], eax

.set_head2:
    mov dword [ebp-8], eax

    cmp dword [ebp-16], 0
    jne .end

    ; new num LSB pointer is unset, set it
    mov dword [ebp-16], eax

.end:
    mov edx, dword [ebp-16]
    mov dword [result], edx
    add esp, 16 ; account for local variables

    popad
    mov eax, dword [result]
    mov esp, ebp
    pop ebp
    ret

hex2octal:
    push ebp
    mov ebp, esp
    pushad

    ; function aguments
    ; ebp+8 - num pointer to convert to octal

    sub esp, 16
    ; local variables
    ; ebp-4 - num pointer
    ; ebp-8 - new num pointer
    ; ebp-12 - count of bits in buffer
    ; ebp-16 - new num LSB pointer
    mov edx, dword [ebp+8]
    mov dword [ebp-4], edx

    mov dword [ebp-8], 0

    mov dword [ebp-12], 0 ; reset count of bits in buffer
    
    mov dword [ebp-16], 0 ; reset new num LSB pointer

    mov ebx, 0 ; hex buffer

.loop:
    cmp dword [ebp-4], 0 ; are we done going through the number?
    je .finish

    mov edx, dword [ebp-4]
    mov eax, dword [edx + data] ; get current hexa digit
    mov ecx, dword [ebp-12]
    shl eax, cl
    add ebx, eax
    add dword [ebp-12], 4

.pull_from_buffer:
    mov eax, ebx ; place hexa buffer in eax
    and eax, 7  ; keep only the first 3 bits in the hexa buffer
    shr ebx, 3   ; remove first 3 bits in the hexa buffer

    sub dword [ebp-12], 3

    push 0
    push eax
    call create_num
    add esp, 8

    cmp dword [ebp-8], 0
    je .set_head

    mov edx, dword [ebp-8]
    mov dword [edx + next], eax

.set_head:
    mov dword [ebp-8], eax

    cmp dword [ebp-16], 0
    jne .skip_set_ptr

    ; new num LSB pointer is unset, set it
    mov dword [ebp-16], eax

.skip_set_ptr:
    cmp dword [ebp-12], 3
    jge .pull_from_buffer

.skip:
    ; advance number linked list
    mov edx, dword [ebp-4]
    mov edx, dword [edx + next]
    mov dword [ebp-4], edx
    
    jmp .loop

.finish:
    ; check if hexa buffer still has anything in it
    cmp ebx, 0
    je .end

    ; hexa buffer still has one more number
    push 0 
    push ebx 
    call create_num
    add esp, 8

    cmp dword [ebp-8], 0
    je .set_head2

    mov edx, dword [ebp-8]
    mov dword [edx + next], eax

.set_head2:
    mov dword [ebp-8], eax

    cmp dword [ebp-16], 0
    jne .end

    ; new num LSB pointer is unset, set it
    mov dword [ebp-16], eax

.end:
    mov edx, dword [ebp-16]
    mov dword [result], edx
    add esp, 16 ; account for local variables

    popad
    mov eax, dword [result]
    mov esp, ebp
    pop ebp
    ret

