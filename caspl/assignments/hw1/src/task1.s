section	.rodata
LC0:
	DB	"The result is:  %u", 10, 0	; Format string

section .bss
LC1:
	RESD	1
tmp:
    RESD    1

section .text
	align 16
	global my_func
	extern printf

my_func:
	push	ebp
	mov	ebp, esp	; Entry code - set up ebp and esp
	pusha			; Save registers

	mov ecx, dword [ebp+8]	; Get argument (pointer to string)

;       Your code should be here...
	mov	dword [LC1], 0		; initialize answer

loop:
    mov al, byte [ecx]

    ; if al == \n, skip it
    cmp al, 10
        je next

    shl dword[LC1], 4 ; multiply by 16 each digit we add

    ; if al >= 'a'
    cmp al, 'a'
        jge ge_a

    ; if al >= 'A'
    cmp al, 'A'
        jge ge_A

    ; if al >= '0'
    cmp al, '0'
        jge ge_0

    ; default
    ; do nothing
    jmp next

    ge_a: ; a <= al
        ; if al <= 'f'
        cmp al, 'f'
            jle a_f
        ; else
            jmp next

    ge_A: ; A <= al
        ; if al <= 'F'
        cmp al, 'F'
            jle A_F
        ; else
            jmp next

    ge_0: ; 0 <= al
        ; if al <= '9'
        cmp al, '9'
            jle n0_9
        ; else
            jmp next

    a_f: ; [a, f]
        sub al, 'a'
        add al, 10
        add byte[LC1], al
        jmp next

    A_F: ; [A, F]
        sub al, 'A'
        add al, 10
        add byte[LC1], al
        jmp next

    n0_9: ; [0, 9]
        sub al, '0'
        add byte[LC1], al
        jmp next

    next:
    	inc	ecx      		; increment pointer
    	cmp	byte [ecx], 0   ; check if byte pointed to is zero
    	jnz	loop       		; keep looping until it is null terminated

	push	dword[LC1]		; Call printf with 2 arguments: pointer to str
	push	LC0		; and pointer to format string.
	call	printf
	add 	esp, 8		; Clean up stack after call

	popa			; Restore registers
	mov	esp, ebp	; Function exit code
	pop	ebp
	ret

