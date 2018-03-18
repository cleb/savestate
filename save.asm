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
    mov ax, [es:9*4+2]     ; preserve ISR address
    mov [origseg], ax
    mov ax, [es:9*4]
    mov [origint], ax
    sti
    
    mov ax,0xe000
    mov es, ax
    mov di, 0x100
    mov si, beginning
    mov cx, (end-beginning)
    rep movsd
    
    
    xor     ax, ax
    mov     es, ax
    
    cli  
    
    mov ax, irq1isr
    mov     word [es:9*4], ax
    mov ax,0xe000
    mov     [es:9*4+2],ax
    sti
    
     

    ret

irq1isr:
    pusha
    pushf
    push ds
    
    push cs
    pop ds
    
    ; read keyboard scan code
    in      al, 0x60
 
    ; update keyboard state
    xor     bh, bh
    mov     bl, al
    and     bl, 0x7F            ; bx = scan code
    shr     al, 7               ; al = 0 if pressed, 1 if released
    xor     al, 1               ; al = 1 if pressed, 0 if released
    mov     [cs:bx+kbdbuf], al

    ; send EOI to XT keyboard
    in      al, 0x61
    mov     ah, al
    or      al, 0x80
    out     0x61, al
    mov     al, ah
    out     0x61, al

    ;send EOI to master PIC
    mov     al, 0x20
    out     0x20, al
     
    mov al, [kbdbuf+1]
    cmp al, 1
    jnz notesc
    mov ah, 9
    mov dx, msg1
    int 21h
    notesc:

    
    
    pushf
    push cs
    push word kbhandlerend
    push word [origseg]
    push word [origint]
    retf
    kbhandlerend:
    pop ds
    popf
    popa
    iret
irq1isrend:    
    
msg1 db "esc pressed",13,10,"$"
origint dw 0
origseg dw 0


kbdbuf:
    times   128 db 0

end:
