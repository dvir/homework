section	.rodata
LC0:
	DB	"The number of ones is:  %u", 10, 0	; Format string

LCERR:
    DB  "x or k, or both are off range.", 10, 0

section .bss
LC1:
	RESD	1

section .text
	align 16
	global ones_counter 
	extern printf
    extern check

ones_counter:
	push	ebp
	mov	ebp, esp	; Entry code - set up ebp and esp
	pusha			; Save registers

	mov ecx, dword [ebp+8]	; Get first argument (int x) 
	mov ebx, dword [ebp+12]	; Get second argument (int k) 
    
	mov	dword [LC1], 0		; initialize answer

    ; check the function arguments x and k.
    ; if check failed, print error and end the procedure
    push ebx
    push ecx
    call check
    add esp, 12
    cmp eax, 0
    je error

    ; if k starts with value 0
    cmp ebx, 0
    je success

loop:
    mov eax, ecx 

    ; bitwise AND with 1 - if last bit is 0, skip body of loop
    and eax, 1
    cmp eax, 0
    je .skip
    
    ; else, last bit is 1
    inc dword[LC1] ; increase ones counter

.skip:
    shr ecx, 1 ; remove last bit of ecx
    cmp ecx, 0 ; if x is 0, stop
    je success

    dec ebx ; decrease k
    cmp ebx, 0 ; if k is 0, stop
    je success

    jmp loop

error:
    ; call printf with error string
    push    LCERR
    call    printf
    add     esp, 4 ; clean up stack after call
    jmp end

success:
    ; call printf with ones counter information
	push	dword[LC1]		; Call printf with 2 arguments: pointer to str
	push	LC0		; and pointer to format string.
	call	printf
    add     esp, 8 ; clean up stack after call

end:
	popa			; Restore registers
	mov	esp, ebp	; Function exit code
	pop	ebp
	ret

