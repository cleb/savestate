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
    
    mov ax,0xE000
    mov es, ax
    mov di, 0x100
    mov si, beginning
    mov cx, (end-beginning)
    rep movsb
    
    
    xor     ax, ax
    mov     es, ax
    
    cli  
    
    mov ax, irq1isr
    mov     word [es:8*4], ax
    mov ax,0xE000
    mov     [es:8*4+2],ax
    sti
    
    int 0x8
    
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

    
    
    pushf
    push cs
    push word kbhandlerend
    push word [origseg]
    push word [origint]
    retf
    kbhandlerend:
    
    pop es
    pop ds
    popa
    iret
irq1isrend: 

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
    
    push word [savedcs]
    pop ds
    xor si, si
    xor di, di
    
    mov ax, 0xD000
    mov es, ax
    mov cx, 0xffff
    rep movsb
    
    push cs
    pop ds
    
    mov al, 1
    mov [saved], al
    
    endsave:
    pop ds
    popa    
    ret
    
loadfunc:

    mov ax, [savedds]
    mov es, ax
    mov ax, 0xD000
    mov ds, ax
    xor si,si
    xor di, di
   
    mov cx, 0xffff
    rep movsb
    
    push cs
    pop ds

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
    
    
    
    push word [savedcs]
    push word [savedip]
    
    push word [savedds]
    pop ds
    retf
    

saved db 0

    
    
    
msg1 db "esc pressed",13,10,"$"
numbers db "0123456789ABCDEF"
origint dw 0
origseg dw 0

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




kbdbuf:
    times   128 db 0

end:
