; compile with NASM: nasm.exe -f bin kbd.asm -o kbd.com

bits 16
org 0x100
beginning:    
    
    xor     ax, ax
    mov     es, ax
    
    cli                         ; update ISR address w/ ints disabled
    mov ax, [es:9*4+2]     ; preserve ISR address
    mov [origseg], ax
    mov ax, [es:9*4]
    mov [origint], ax
    
    mov ax, [es:21h*4+2]     ; preserve ISR address
    mov [orig21seg], ax
    mov ax, [es:21h*4]
    mov [orig21offset], ax
    sti
    
     mov ah,25h      ;Here set your ah register for calling Interrupt vector
     mov al,9h      ;Your Interrupt Address
     mov dx,irq1isr   ;Interrupt Handler
     int 21h
     
     mov ah,25h      ;Here set your ah register for calling Interrupt vector
     mov al,21h      ;Your Interrupt Address
     mov dx,i21hhandler   ;Interrupt Handler
     int 21h
    
    
    mov ah, 9
    mov dx, msg1
    int 21h
    
    mov dx, end-beginning
    shr dx, 4
    add dx, 17
    mov ax, 3100h
    int 21h
    ret

irq1isr:
    pusha
    push ds
    push es
    
    push cs
    pop ds
    
    mov ax, es
    mov [currentes], ax
    
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
    
    cmp al, 0x57
    jnz notsave
    call savefunc
    notsave:
    
    cmp al, 0x58
    jz loadfunc

    
    
    
    
    pop es
    pop ds
    popa
    iret
irq1isrend: 

i21hhandler:
    pusha
    mov cx, ds
    push cs
    pop ds
    mov [dsstash], cx
    cmp ah, 25h
    jnz not25h
    
    ;int 25h handler
    cmp al, 9
    jnz not9h
    
    mov [origint], dx
    mov dx, ds
    mov [origseg], dx 
    
    not9h:    
    not25h:
    popa
    
    push word [orig21seg]
    push word [orig21offset]
    push word [dsstash]
    pop ds
    retf
    
;save state
savefunc:
    pusha
    push ds
    
    cmp byte [saved], 0
    jnz endsave
    
    ;move current registers to save area
    push ds
    pop es
    
    mov si, currentds
    mov di, savedds
    mov cx, 26
    rep movsb
    
    
    ;save video mode
    mov ah, 0fh
    int 10h
    mov [videomode], al
    mov [videowidth], ah
    mov [videopage], bh
    
    
    ; open savefile
    mov ah,3ch
    mov dx,filename
    int 21h
    mov [handle],ax 
    
    ;save registers
    mov bx, [handle]
    mov ah, 40h
    mov cx, regsaveend-regsavestart
    mov dx, regsavestart
    int 21h
    
    ;save cs memory
    push ds
    mov bx, [handle]
    mov ah,40h
    mov cx,0xffff
    xor dx, dx
    push word [savedcs]
    pop ds
    int 21h
    pop ds
    
    ;save video
    mov ah, [videomode]
    cmp ah, 0dh
    jz savemode0d
    cmp ah, 10h
    jz savemode10
    jmp endvideosave
    
    savemode0d:
    push ds
    mov cx, 4
    saveplane:
    push cx
    mov ax, 5h
    mov dx, 3ceh
    OUT dx, ax         ;set up for plane masking
    mov ah, cl
    dec ah
    mov al, 04h
    OUT dx, ax             ;n is: 0102H=plane 0; 0202H=plane 1
                            ;      0402H=plane 2; 0802H=plane 3
    push ds
    mov bx, [handle]
    mov ax, 0a000h
    mov ds, ax
    mov ah,40h
    mov cx,0x4000
    xor dx, dx
    int 21h
    pop ds
    pop cx
    loop saveplane
    mov ax, 0f02h
    mov dx, 3c4h
    out dx, ax         ;restore normal plane mask
    pop ds
    jmp endvideosave
    
    savemode10:
    push ds
    mov bx, [handle]
    mov ax, 0a000h
    mov ds, ax
    mov ah,40h
    mov cx,0xffff
    xor dx, dx
    int 21h
    pop ds
    
    
    
    endvideosave:
    
    
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
    ;open file
    mov ax,3d02h
    mov dx,filename
    int 21h
    mov [handle],ax 

    ;read registers
    mov bx, [handle]
    mov ah, 3fh
    mov cx, regsaveend-regsavestart
    mov dx, regsavestart
    int 21h
    
    
    ;preserve stack segment 
    
    mov ax, ss
    mov [stashss], ax
    mov ax, sp
    mov [stashsp], ax
    
    push cs
    pop ss
    mov ax, endstack - 2
    mov sp, ax
    
    ;load cs
    mov bx,[handle]
    mov ax, [savedcs]
    mov ds, ax
    mov ah,3fh
    mov cx,0xffff
    xor dx, dx
    int 21h
    
    push cs
    pop ds
    
    mov ax, [stashss]
    mov ss, ax
    mov ax, [stashsp]
    mov sp, ax
    
    ;load video
    mov ah, [videomode]
    cmp ah, 0dh
    jz loadmode0d
    cmp ah, 10h
    jz loadmode10
    jmp endvideoload
    
    loadmode0d:
    push ds
    mov cx, 4
    loadplane:
    push cx
    push ds
    
    mov al, 1
    dec cl
    shl al, cl
    shl ax, 8
    add ax, 02h
    mov dx, 3c4h
    OUT dx, ax             ;n is: 0102H=plane 0; 0202H=plane 1
                            ;      0402H=plane 2; 0802H=plane 3
    
    mov bx, [handle]
    mov ax, 0a000h
    mov ds, ax
    mov ah,3fh
    mov cx,0x4000
    xor dx, dx
    int 21h
    
    
pop ds
    pop cx
    loop loadplane
    mov ax, 0f02h
    mov dx, 3c4h
    OUT dx, ax         ;restore normal plane mask
    pop ds
    jmp endvideoload
    
    loadmode10:
    push ds
    mov bx, [handle]
    mov ax, 0a000h
    mov ds, ax
    mov ah,3fh
    mov cx,0xffff
    xor dx, dx
    int 21h
    pop ds
    
    
    
    endvideoload:
    
    sti    
    push cs
    pop ds
    
    
    
    

    ;restore registers
    mov di, [saveddi]
    mov si, [savedsi]
    mov bp, [savedbp]
    mov ax, [savedsp]
    add ax, 6
    mov sp, ax
    mov ax, [savedes]
    mov es, ax
    mov bx, [savedbx]
    mov dx, [saveddx]
    mov cx, [savedcx]
    
    push word [savedfl]
    popf
    
    mov ax, [savedax]
    
    ;jump to saved position
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

    
    
    
msg1 db "savestate loaded. Press f11 to save, f12 to load.",13,10,"$"
filename db "save.dat",0
numbers db "0123456789ABCDEF"
origint dw 0
origseg dw 0

orig21seg dw 0
orig21offset dw 0

handle dw 0

origcs dw 0

dsstash dw 0


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
currentes dw 0

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
savedes dw 0
videomode db 0
videowidth db 0
videopage db 0
regsaveend:

stashss dw 0
stashsp dw 0




stack:
    times   128 db 0
endstack:
end:
