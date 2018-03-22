; compile with NASM: nasm.exe -f bin kbd.asm -o kbd.com

bits 16
org 0x100
beginning:

;     mov ah, 9
;     mov dx, msg1
;     int 21h
    
    
    xor     ax, ax
    mov     es, ax
    

    cli                         ; update ISR address w/ ints disabled
    mov ax, [es:8*4+2]     ; preserve ISR address
    mov [origseg], ax
    mov ax, [es:8*4]
    mov [origint], ax
    sti
    
     mov ah,25h      ;Here set your ah register for calling Interrupt vector
     mov al,8h      ;Your Interrupt Address
     mov dx,irq1isr   ;Interrupt Handler
     int 21h
    
    int 0x8
    
    mov dx, end-beginning
    mov ax, 3100h
    int 21h
    ret

irq1isr:
    pusha
    push ds
    push es
    
    push cs
    pop es
    
    push ss
    pop ds
    
    push sp
    pop si
    
    mov di, currentds
    add si, 2
    mov cx, 24
    rep movsb
    
    push cs
    pop ds
    
    pushf
    push cs
    push word kbhandlerend
    push word [origseg]
    push word [origint]
    retf
    kbhandlerend:
    
    in      al,60H 
;    push ax
    
;    mov ah, 02h
;    xor bx, bx
;    mov bl, al
;    shr bl, 4
;    add bx, numbers
;    mov dl, [bx]
;    int 21h
    
;    pop ax
;    mov ah, 02h
;    xor bx, bx
;    mov bl, al
;    and bl, 0xf
;    add bx, numbers
;    mov dl, [bx]
;    int 21h
;    mov dl, 13
;    int 21h
;    mov dl, 10
;    int 21h
    
    cmp al, 0x57
    jnz notsave
    ;mov ah ,9
    ;mov dx, msg1
    ;int 21h
    call savefunc
    notsave:
    
    cmp al, 0x58
    jz loadfunc

    
    
    
    
    pop es
    pop ds
    popa
    iret
irq1isrend: 

;save state
savefunc:
    pusha
    push ds
    
    cmp byte [saved], 0
    jnz endsave
    
    push ds
    pop es
    
    mov si, currentds
    mov di, savedds
    mov cx, 24
    rep movsb
    
    ; open savefile
    mov ah,3ch
    mov dx,filename
    int 21h
    mov [handle],ax 
    
    
    mov bx, [handle]
    
    mov ah, 40h
    mov cx, regsaveend-regsavestart
    mov dx, regsavestart
    int 21h
    
    mov bx, [handle]
    
    mov ah,40h
    mov cx,0xffff
    xor dx, dx
    push word [savedcs]
    pop ds
    int 21h
    
    ;xor si, si
    ;xor di, di
    
    ;mov ax, 0x7000
    ;mov es, ax
    ;mov cx, 0xffff
    ;rep movsb
    
    push cs
    pop ds
    
    mov ah,3eh
    mov bx,[handle]
    int 21h
    
    
    mov al, 1
    mov [saved], al
    
    endsave:
    pop ds
    popa    
    ret
    
;load saved state
loadfunc:

    cli
    mov ax,3d02h
    mov dx,filename
    int 21h
    mov [handle],ax 
    
    mov bx, [handle]
    mov ah, 3fh
    mov cx, regsaveend-regsavestart
    mov dx, regsavestart
    int 21h
    
    mov ax, ss
    mov [stashss], ax
    mov ax, sp
    mov [stashsp], ax
    
    push cs
    pop ss
    mov ax, endstack - 2
    mov sp, ax
    
       
    mov bx,[handle]
    mov ax, [savedcs]
    mov ds, ax
    
    mov ah,3fh
    
    mov cx,0xffff
    xor dx, dx
    
    
    int 21h 

    sti    
    push cs
    pop ds
    
    mov ax, [stashss]
    mov ss, ax
    mov ax, [stashsp]
    mov sp, ax
    
    


    mov di, [saveddi]
    mov si, [savedsi]
    mov bp, [savedbp]
    mov ax, [savedsp]
    add ax, 6
    mov sp, ax
    mov bx, [savedbx]
    mov dx, [saveddx]
    mov cx, [savedcx]
    
    push word [savedfl]
    popf
    
    mov ax, [savedax]
    
    
    pushf
    push word [savedcs]
    push word [savedip]
    
    push word [savedds]
    pop ds
    iret
    
    
    
enter_flat_mode:
   pusha
   cli                    ; no interrupts
   push ds                ; save real mode
 
   ;lgdt [gdtinfo]         ; load gdt register
 
   mov  eax, cr0          ; switch to pmode by
   or al,1                ; set pmode bit
   mov  cr0, eax
 
   jmp $+2                ; tell 386/486 to not crash
 
   mov  bx, 0x08          ; select descriptor 1
   mov  ds, bx            ; 8h = 1000b
 
   and al,0xFE            ; back to realmode
   mov  cr0, eax          ; by toggling bit again
 
   pop ds                 ; get back old segment
   sti
   popa
   ret
    

saved db 0

    
    
    
msg1 db "esc pressed",13,10,"$"
filename db "save.dat",0
numbers db "0123456789ABCDEF"
origint dw 0
origseg dw 0
handle dw 0

origcs dw 0


currentds dw 0
currentdi dw 0
currentsi dw 0
currentbp dw 0
currentsp dw 0
currentbx dw 0
currentdx dw 0
currentcx dw 0
currentax dw 0
currentip dw 0
currentcs dw 0
currentfl dw 0

regsavestart:
savedds dw 0
saveddi dw 0
savedsi dw 0
savedbp dw 0
savedsp dw 0
savedbx dw 0
saveddx dw 0
savedcx dw 0
savedax dw 0
savedip dw 0
savedcs dw 0
savedfl dw 0
regsaveend:

stashss dw 0
stashsp dw 0




stack:
    times   128 db 0
endstack:
end:
