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

STACK_BASE:
    RESD    1 
STACK_SIZE:
    RESD    1
CALC_MODE:
    RESB    1

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

    sub esp, 4 ; save room for local variables:
    %define op_counter ebp-4 ; 1. operation counter

    mov dword [op_counter], 0 ; reset operation counter
    mov dword [STACK_SIZE], 0 ; reset current stack size
    mov byte  [CALC_MODE], 'h'

    ; allocate memory for operands stack
    push 4*MAX_STACK_SIZE
    call malloc
    add esp, 4
    mov dword [STACK_BASE], eax

.loop:
    push    PROMPT_MESSAGE
    call    printf
    add     esp, 4

    push    INPUT
    call    gets
    add     esp, 4

    inc dword [op_counter]

    ; choose operation
    cmp byte [INPUT], '+'
    jne .not_addition
    ; addition
        call addition
        jmp .loop
.not_addition:

    cmp byte [INPUT], 'p'
    jne .not_pop_and_print
    ; pop and print
        call pop_and_print
        jmp .loop
.not_pop_and_print:

    cmp byte [INPUT], 'd'
    jne .not_duplicate
    ; duplicate
        call duplicate
        jmp .loop
.not_duplicate:

    cmp byte [INPUT], '^'
    jne .not_exponent
    ; exponent
        call exponent
        jmp .loop
.not_exponent:

    cmp byte [INPUT], 'x'
    jne .not_bitwise_xor
    ; bitwise_xor
        call bitwise_xor
        jmp .loop
.not_bitwise_xor:

    cmp byte [INPUT], 'o'
    jne .not_octal
    ; octal
        call octal
        jmp .loop
.not_octal:

    cmp byte [INPUT], 'h'
    jne .not_hexa
    ; hexa
        call hexa
        jmp .loop
.not_hexa:

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

        cmp byte [CALC_MODE], 'h'
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
        je .loop
        
        ; check if we have room for it in the stack.
        cmp dword [STACK_SIZE], MAX_STACK_SIZE
        jne .not_stackoverflow
        ; stack overflow
            call stackoverflow
            jmp .loop
    .not_stackoverflow:
        cmp byte [CALC_MODE], 'o'
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
        push edx
        call func_push
        add esp, 4

    jmp .loop 

.exit:
    mov eax, dword [op_counter]
    mov dword [result], eax

    add esp, 4 ; remove local variables
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
    push ebp
    mov ebp, esp
    pushad
    
    cmp dword [STACK_SIZE], 2
    jge .not_missing_args
    ; missing args
        call error_missing_args
        jmp .end
.not_missing_args:

    call func_pop ; pop first operand
    mov ebx, eax
    call func_pop ; pop second operand
    push eax
    push ebx
    call func_addition
    add esp, 8

    ; push the new created number to the operands stack
    push eax
    call func_push
    add esp, 4
.end:
    popad
    mov esp, ebp
    pop ebp
    ret

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
    push ebp
    mov ebp, esp
    pushad

    cmp dword [STACK_SIZE], 1
    jge .not_missing_args
    ; missing args
        call error_missing_args
        jmp .end
.not_missing_args:

    call func_pop
    mov edx, eax

    cmp byte [CALC_MODE], 'h'
    je .print

    ; octal mode
    ; convert from hex to octal before printing
    push edx
    call hex2octal
    add esp, 4
    mov edx, eax

.print:
    push dword [CALC_MODE]
    push edx
    call print_num
    add esp, 8

    call print_newline

.end:
    popad
    mov esp, ebp
    pop ebp
    ret

duplicate:
    push ebp
    mov ebp, esp
    pushad

    cmp dword [STACK_SIZE], 1
    jge .not_missing_args
    ; missing args
        call error_missing_args
        jmp .end
.not_missing_args:

    cmp dword [STACK_SIZE], MAX_STACK_SIZE
    jne .not_stackoverflow
    ; stack overflow
        call stackoverflow
        jmp .end
.not_stackoverflow:

    call func_pop ; get stack head value
    
    push eax
    call func_push ; push to operands stack
    call func_push ; twice!
    add esp, 4 

.end:
    popad
    mov esp, ebp
    pop ebp
    ret

exponent:
    push ebp
    mov ebp, esp
    pushad

    cmp dword [STACK_SIZE], 1
    jge .not_missing_args
    ; missing args
        call error_missing_args
        jmp .end
.not_missing_args:

    call func_pop ; pop from operands stack
    push eax
    call func_exponent
    add esp, 4

    push eax
    call func_push ; push to operands stack
    add esp, 4

.end:
    popad
    mov esp, ebp
    pop ebp
    ret

func_shl:
    push ebp
    mov ebp, esp
    pushad
    
    ; function parameters
    ; ebp+8 - number to shift

    ; local variables
    sub esp, 16
    ; ebp-4 - new number pointer iterator
    ; ebp-8 - num pointer
    ; ebp-12 - carry
    ; ebp-16 - return num pointer

    mov dword [ebp-4], 0 ; reset new num pointer

    mov edx, dword [ebp+8] ; set num pointer
    mov dword [ebp-8], edx

    mov dword [ebp-12], 0 ; reset carry
    mov dword [ebp-16], 0 ; reset return num pointer

.loop:
    cmp dword [ebp-8], 0
    je .finish

    mov edx, dword [ebp-8]
    movzx ebx, byte [edx + data]
    mov ecx, ebx

    ; shift current digit by 1 
    shl ecx, 1
    ; add carry from previous shift
    add ecx, dword [ebp-12]
    ; trim to only the first 4 bits
    and ecx, 15
    ; create a digit for the new number with this value
    push 0
    push ecx
    call create_num
    add esp, 8
    
    ; if it's the first digit, we should just set it
    cmp dword [ebp-4], 0
    je .first_digit

    ; else, set the next of the previous to be the new digit
    mov edx, dword [ebp-4]
    mov dword [edx + next], eax
    jmp .calc_carry

.first_digit:
    mov dword [ebp-16], eax ; set return num pointer

.calc_carry:
    mov dword [ebp-4], eax ; set num pointer

    ; calculate the new carry - extract the 4th bit
    shr ebx, 3
    ; trim to only the first bit, just in case
    and ebx, 1
    ; store the carry for the next operation
    mov dword [ebp-12], ebx

    ; advance num pointer
    mov edx, dword [ebp-8]
    mov ebx, dword [edx + next]
    mov dword [ebp-8], ebx
    jmp .loop

.finish:
    ; if carry != 0
    cmp dword [ebp-12], 0
        jne .has_carry
    ; else
        jmp .end

.has_carry:
    ; number has ended but there's still a carry to push.
    ; create a new number and place it as the MSB
    push 0
    push 1 ; carry must be 1 if we got here
    call create_num
    add esp, 8

    mov edx, dword [ebp-4] ; last actual num pointer
    mov dword [edx + next], eax ; make it point to the newly created MSB
    
.end:
    mov edx, dword [ebp-16]
    mov dword [result], edx

    add esp, 16
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
    sub esp, 12
    ; ebp-4  - new number pointer
    ; ebp-8  - counter
    ; ebp-12 - number representing the jumps in counter we should do

    mov dword [ebp-4], 0
    
    push 0
    push 1
    call create_num
    add esp, 8
    mov dword [ebp-4], eax

    push 0
    push 0
    call create_num
    add esp, 8
    mov dword [ebp-8], eax

    ; create the 'jump' number - counter should increase by 1 every iteration
    push 0
    push 1
    call create_num
    add esp, 8
    mov dword [ebp-12], eax
   
.loop:
    push dword [ebp+8]
    push dword [ebp-8]
    call func_cmp
    add esp, 8

    ; if counter >= exp, stop
    cmp eax, 0 
    jge .end

    ; shift number by 1
    push dword [ebp-4]
    call func_shl
    add esp, 4
    mov dword [ebp-4], eax

    ; increase counter by 1
    push dword [ebp-12] ; counter increase value
    push dword [ebp-8] ; counter number
    call func_addition
    add esp, 8
    mov dword [ebp-8], eax
    
    jmp .loop

.end:
    mov edx, dword [ebp-4]
    mov dword [result], edx
    add esp, 12
    popad
    mov eax, dword [result]
    mov esp, ebp
    pop ebp
    ret

bitwise_xor:
    push ebp
    mov ebp, esp
    pushad
    
    cmp dword [STACK_SIZE], 2
    jge .not_missing_args
    ; missing args
        call error_missing_args
        jmp .end
.not_missing_args:

    call func_pop ; pop first operand
    mov ebx, eax
    call func_pop ; pop second operand
    push eax
    push ebx
    call func_bitwise_xor
    add esp, 8

    ; push the new created number to the operands stack
    push eax
    call func_push
    add esp, 4

.end:
    popad
    mov esp, ebp
    pop ebp
    ret

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
    push ebp
    mov ebp, esp
    pushad

    mov byte [CALC_MODE], 'o'
    
    popad
    mov esp, ebp
    pop ebp
    ret

hexa:
    push ebp
    mov ebp, esp
    pushad

    mov byte [CALC_MODE], 'h'
    
    popad
    mov esp, ebp
    pop ebp
    ret

error_missing_args:
    push ebp
    mov ebp, esp
    pushad

    push NOT_ENOUGH_ARGUMENTS
    call printf
    add esp, 4
    
    popad
    mov esp, ebp
    pop ebp
    ret

stackoverflow:
    push ebp
    mov ebp, esp
    pushad

    push STACKOVERFLOW 
    call printf
    add esp, 4
    
    popad
    mov esp, ebp
    pop ebp
    ret

print_num:
    push ebp
    mov ebp, esp
    pushad

    ; ebp+8 - pointer to first num digit
    mov ebx, dword [ebp+8]

    ; ebp+12 - print mode ('h' - hexa, 'o' - octal)
    mov ecx, dword [ebp+12]
   
    cmp ebx, 0
    je .end 

    push dword [ebp+12]
    push dword [ebx + next]
    call print_num
    add esp, 8

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

.end:
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

func_cmp:
    push ebp
    mov ebp, esp
    pushad

    ; ebp+8 - first number
    ; ebp+12 - second number

    sub esp, 8 ; local variables
               ; ebp-4 - first number pointer
               ; ebp-8 - second number pointer

    ; set first number ptr
    mov edx, dword [ebp+8]
    mov dword [ebp-4], edx

    ; set second number ptr
    mov edx, dword [ebp+12]
    mov dword [ebp-8], edx

.loop:
    cmp dword [ebp-4], 0
    je .end_first

    cmp dword [ebp-8], 0
    je .end_second

    ; none of the numbers ended yet. call recursively with pointers advanced
    mov edx, dword [ebp-8]
    push dword [edx + next] ; push second number

    mov edx, dword [ebp-4]
    push dword [edx + next] ; push first number

    call func_cmp ; recursively call func_cmp
    add esp, 8

    ; if the numbers aren't equal, just return the result, 
    ; as the result won't change while inspecting the lower bits of significance.
    cmp eax, 0
    jne .end

    ; the higher bits of significance are equal, check the current ones
    mov edx, dword [ebp-4]
    mov ebx, dword [edx + data] ; load current first number digit
    mov edx, dword [ebp-8]
    mov ecx, dword [edx + data] ; load current second number digit

    cmp ebx, ecx
    je .equal
    jg .second_less
    jl .first_less

.end_first:
    cmp dword [ebp-8], 0
    je .end_both

.first_less:
    mov eax, -1
    jmp .end

.second_less:
.end_second:
    mov eax, 1
    jmp .end

.equal:
.end_both:
    mov eax, 0
    jmp .end
    
.end:
    add esp, 8
    mov dword [result], eax
    popad
    mov eax, dword [result]
    mov esp, ebp
    pop ebp
    ret

func_pop:
    push ebp
    mov ebp, esp
    pushad

    mov ebx, dword [STACK_BASE]
    mov ecx, dword [STACK_SIZE]
    mov eax, dword [ebx + 4*ecx + (-4)]

    dec dword [STACK_SIZE] ; actually pop from the stack

    mov dword [result], eax
    popad
    mov eax, dword [result]
    mov esp, ebp
    pop ebp
    ret

func_push:
    push ebp
    mov ebp, esp
    pushad

    mov ebx, dword [STACK_BASE]
    mov ecx, dword [STACK_SIZE]
    mov edx, dword [ebp+8]
    mov dword [ebx + 4*ecx], edx
    inc dword [STACK_SIZE]

    popad
    mov esp, ebp
    pop ebp
    ret
