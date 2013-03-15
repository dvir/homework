    section .data                    	; data section, read-write
        an:    DD 0              	; this is a temporary var

section .text                    	; our code is always in the .text section
        global do_str            	; makes the function appear in global scope
        extern printf            	; tell linker that printf is defined elsewhere 				; (not used in the program)

do_str:                          	; functions are defined as labels
        push    ebp              	; save Base Pointer (bp) original value
        mov     ebp, esp         	; use base pointer to access stack contents
        pushad                   	; push all variables onto stack
        mov ecx, dword [ebp+8]	; get function argument
;;;;;;;;;;;;;;;; FUNCTION EFFECTIVE CODE STARTS HERE ;;;;;;;;;;;;;;;; 

	mov	dword [an], 0		; initialize answer
label_here:

    ; if ecx == '('
    cmp byte [ecx], '('
        je open_parenthesis

    ; if ecx == ')'
    cmp byte [ecx], ')'
        je close_parenthesis

    ; if ecx >= 'a'
    cmp byte [ecx], 'a'
        jge ge_a

    ; if ecx >= 'A'
    cmp byte [ecx], 'A'
        jge ge_A

    ; if ecx >= '0'
    cmp byte [ecx], '0'
        jge ge_0

    ; default
    ; do nothing
    jmp next

    open_parenthesis:
        mov byte [ecx], '['
        jmp next

    close_parenthesis:
        mov byte [ecx], ']'
        jmp next

    ge_a: ; a <= ecx
        ; if ecx <= 'z'
        cmp byte [ecx], 'z'
            jle a_z
        ; else
            jmp next

    ge_A: ; A <= ecx
        ; if ecx <= 'Z'
        cmp byte [ecx], 'Z'
            jle A_Z
        ; else
            jmp next

    ge_0: ; 0 <= ecx
        ; if ecx <= '9'
        cmp byte [ecx], '9'
            jle n0_9
        ; else
            jmp next

    a_z: ; [a, z]
        inc byte [ecx]
        jmp next

    A_Z: ; [A, Z]
        inc dword [an]
        inc byte [ecx]
        jmp next

    n0_9: ; [0, 9]
        inc byte [ecx]
        jmp next

    next:
    	inc	ecx      		; increment pointer
    	cmp	byte [ecx], 0    		; check if byte pointed to is zero
    	jnz	label_here       		; keep looping until it is null terminated

;;;;;;;;;;;;;;;; FUNCTION EFFECTIVE CODE ENDS HERE ;;;;;;;;;;;;;;;; 
         popad                    ; restore all previously used registers
         mov     eax,[an]         ; return an (returned values are in eax)
         mov     esp, ebp
         pop     dword ebp
         ret
