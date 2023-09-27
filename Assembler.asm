%include "in_out.asm"
SECTION .data
    shit db "you are here",0

    zeroOperands db "stc:11111001 clc:11111000 std:11111101 cld:11111100 syscall:0000111100000101 ret:11000011 ", 0
    
    alkjfkla db "asdff",0
    
    registers_64 db "rax: rcx: rdx: rbx: rsp: rbp: rsi: rdi: r8: r9: r10: r11: r12: r13: r14: r15: ",0
    registers_32 db "eax: ecx: edx: ebx: esp: ebp: esi: edi: r8d: r9d: r10d: r11d: r12d: r13d: r14d: r15d: ",0
    registers_16 db "ax: cx: dx: bx: sp: bp: si: di: r8w: r9w: r10w: r11w: r12w: r13w: r14w: r15w: ",0
    registers_8 db "al: cl: dl: bl: ah: ch: dh: bh: r8b: r9b: r10b: r11b: r12b: r13b: r14b: r15b: ",0
    
    registers db "rax:000 rcx:001 rdx:010 rbx:011 rsp:100 rbp:101 rsi:110 rdi:111 eax:000 ecx:001 edx:010 ebx:011 esp:100 ebp:101 esi:110 edi:111 ax:000 cx:001 dx:010 bx:011 sp:100 bp:101 si:110 di:111 al:000 cl:001 dl:010 bl:011 ah:100 ch:101 dh:110 bh:111 r8:000 r9:001 r10:010 r11:011 r12:100 r13:101 r14:110 r15:111 r8d:000 r9d:001 r10d:010 r11d:011 r12d:100 r13d:101 r14d:110 r15d:111 r8w:000 r9w:001 r10w:010 r11w:011 r12w:100 r13w:101 r14w:110 r15w:111 r8b:000 r9b:001 r10b:010 r11b:011 r12b:100 r13b:101 r14b:110 r15b:111 ",0
    reg_size db 0,
    is_new db 0,
    opcode_w db 0,
    sum dw 0,

    

    operand1_flag db 0,
    operand2_flag db 0,

    operand1_type db -1,
    operand2_type db -1,
    type db 0,
    base_flag db 0,
    index_flag db 0,
    disp_flag db 0,

    operand2_t db "al",0




    mov_imm_mmm db "100000000w",0
    mov_reg_mem db "1000101w",0
    mov_mem_reg db "1000100w",0
    mov_reg_imm db "1011w",0
    mov_mem_imm db "1100011w",0
    mov_reg_reg db "1000100w11",0
    add_reg_reg: db "0000000w11",0
    add_reg_mem: db "0000001w",0
    add_mem_reg: db "0000000w",0
    add_reg_imm: db "00000sw11000",0
    add_mem_imm: db "100000sw",0

    adc_reg_reg: db "0001000w11",0
    adc_reg_mem: db "0001001w",0
    adc_mem_reg: db "0001000w",0
    adc_reg_imm: db "100000sw11010",0
    adc_mem_imm: db "100000sw",0

    and_reg_reg: db "0010000w11",0
    and_reg_mem: db "0010001w",0
    and_mem_reg: db "0010000w",0
    and_reg_imm: db "100000sw11100",0
    and_mem_imm: db "100000sw",0

    or_reg_reg: db "0000100w11",0
    or_reg_mem: db "0000101w",0
    or_mem_reg: db "0000100w",0
    or_reg_imm: db "100000sw11001",0
    or_mem_imm: db "100000sw",0

    sub_reg_reg: db "0010100w11",0
    sub_reg_mem: db "0010101w",0
    sub_mem_reg: db "0010100w",0
    sub_reg_imm: db "100000sw11101",0
    sub_mem_imm: db "100000sw",0

    sbb_reg_reg: db "0001100w11",0
    sbb_reg_mem: db "0001101w",0
    sbb_mem_reg: db "0001100w",0
    sbb_reg_imm: db "100000sw11011",0
    sbb_mem_imm: db "100000sw",0

    xor_reg_reg: db "0001100w11",0
    xor_reg_mem: db "0001101w",0
    xor_mem_reg: db "0001100w",0
    xor_reg_imm: db "100000sw11011",0
    xor_mem_imm: db "100000sw",0

    cmp_reg_reg: db "0011100w11",0
    cmp_reg_mem: db "0011101w",0
    cmp_mem_reg: db "0011100w",0
    cmp_imm_reg: db "100000sw11111",0
    cmp_imm_mem: db "100000sw",0

    test_reg_reg: db "1000010w11",0
    test_reg_mem: db "1000010w",0
    test_mem_reg: db "1000010w",0
    test_imm_reg: db "1111011w11000",0
    test_imm_mem: db "1111011w",0

    rex: db "01001000",0

    rex_flag db 0,
    mem_size_int db 0,
    disp_size db 0,
    mod8 db "01",0
    mod32 db "10",0
    mod0 db "00",0
    bp_flag db 0,
    disp_bp db "00000000",0
    RM100 db "100",0
    sib_flag db 0,
    rex_b db "0",0
    rex_x db "0",0
    sib_base_dir_addr db "101",0
    scale1 db "00",0
    scale2 db "01",0
    scale4 db "10",0
    scale8 db "11",0
    sib_index db "000",0
    temp db "000",0
    disp32 db "00000000000000000000000000000000",0
SECTION .bss
    cmnd resb 100
    opcode resb 10
    operand1 resb 45
    operand2 resb 45
    mem_op_size resb 6
    base_reg resb 5
    index_reg resb 5
    scale_value resb 2
    disp_value resb 100

    code_finder resb 100

    binary_code resb 1000
    rm: resb 5
    regOp: resb 5
    mod: resb 3
    sib: resb 10
    sib_scale: resb 3
    ;sib_index: resb 4
    sib_base: resb 4
SECTION .text
    global _start
printHere:
    push rsi
    mov rsi, shit
    call newLine
    call printString
    call newLine
    pop rsi
    ret
print_string:
    push rsi
    call printString
    call newLine
    pop rsi
    ret
getString:
    push rax
    push rsi
getChar:
    xor rax, rax
    call getc
    cmp al, 0x0A
    je return
    mov byte [rsi], al
    inc rsi
    jmp getChar

return:
    mov al, '$'
    mov byte [rsi], al
    pop rsi
    pop rax
    ret

mainParse:
    push rsi
    push rax
    push rbx
    push rcx
    push rdx
    push r8
    xor r8,r8
    inc r8
    xor rbx, rbx
    xor rcx, rcx
    xor rdx, rdx

findOpcode:
    mov al, [rsi]
    cmp al, '$'
    je return_parse
    
    cmp al, 0x20
    je foundSpace
    mov [opcode + rbx], al
    inc rsi
    inc rbx
    jmp findOpcode

foundSpace:
    inc rsi
    mov al, [rsi]
    
    cmp al, '$'
    je return_parse
    
    cmp al, ','
    je foundComma
    
    mov [operand1 + rcx], al
    mov [operand1_flag], r8
    inc rcx
    jmp foundSpace

foundComma:
    ;call printHere
    inc rsi
    mov al, [rsi]
    cmp al, '$'
    je return_parse
    mov [operand2 + rdx], al
    mov [operand2_flag], r8
    inc rdx
    jmp foundComma

return_parse:
    pop r8
    pop rdx
    pop rcx
    pop rbx
    pop rax
    pop rsi
    ret

memory_parse:
    push rsi
    push rax
    push rbx
    push rcx
    push rdx
    push r8 ;use as flag
    push r9 ;use as temp
    xor rbx, rbx
    xor rcx, rcx
    xor rdx, rdx
    xor r8, r8

get_mem_size:
    mov al, [rsi]
    cmp al, 0x20
    je foundSpace1
    mov [mem_op_size + rbx], al
    inc rsi
    inc rbx
    jmp get_mem_size

foundSpace1:
    add rsi, 6
    xor rbx, rbx
    mov r9, rsi

bracet_parse:
    xor r10, r10
    inc r10

    check_if_there_is_base:
        mov al, [r9]
        cmp al, '0'
        je before_it_is_disp_only
    
        cmp al, '*'
        je no_base
    
        cmp al, '+'
        je there_is_base
    
        cmp al, ']'
        je there_is_base
        inc r9
        jmp check_if_there_is_base

before_it_is_disp_only:
    inc rsi
    inc rsi

it_is_disp_only:
    mov [disp_flag], r10
    mov al, [rsi]
    cmp al, ']'
    je return_mem_parse
    mov [disp_value + rdx], al
    inc rsi
    inc rdx
    jmp it_is_disp_only

no_base:
    mov al, [rsi]
    cmp al, '0'
    je before_get_disp
    cmp al, '*'
    je get_scale
    mov [index_reg + rcx], al
    inc rcx
    inc rsi
    jmp no_base

there_is_base:
    mov [base_flag], r10
    mov al, [rsi]
    cmp al, ']'
    je return_mem_parse
    cmp al, '+'
    je countinue
    mov [base_reg + rbx], al
    inc rsi
    inc rbx
    jmp there_is_base

get_scale:
    mov [index_flag], r10
    xor r10,r10
    inc rsi
    mov al, [rsi]
    mov [scale_value + r10], al
    inc rsi
    mov al, [rsi]
    cmp al, ']'
    je return_mem_parse
    jmp before_get_disp

countinue:
    inc rsi
    mov al, [rsi]
    cmp al, '0'
    je before_get_disp0
    cmp al, '*'
    je get_scale
    mov [index_reg + rcx], al
    inc rcx
    jmp countinue

before_get_disp:
    inc rsi
    inc rsi

get_disp:
    xor r10,r10
    inc r10
    mov [disp_flag], r10
    inc rsi
    mov al, [rsi]
    cmp al, ']'
    je return_mem_parse
    mov [disp_value + rdx], al
    inc rdx
    jmp get_disp

before_get_disp0:
    inc rsi
    inc rsi

get_disp0:
    xor r10,r10
    inc r10
    mov [disp_flag], r10
    mov al, [rsi]
    cmp al, ']'
    je return_mem_parse
    mov [disp_value + rdx], al
    inc rdx
    inc rsi
    jmp get_disp0

disp_check:
    mov rdi, disp_value
    call GetStrlen
    cmp rdx,1
    je disp_check2
    jmp edame_return

disp_check2:
    xor r10,r10
    cmp byte[disp_value + r10], '0'
    jne edame_return
    mov [disp_flag], r10
    jmp edame_return

return_mem_parse:
    cmp byte[disp_flag],1
    je disp_check
    jmp edame_return

edame_return:
    pop r9
    pop r8
    pop rdx
    pop rcx
    pop rbx
    pop rax
    pop rsi
    ret
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

ZeroOperandCodeFinder:
    push rsi ;our operenad
    push rdi ; ZOp db
    push rax
    push rbx
    push rcx
    push rdx
    push r8
    push r9
    push r10
    push r11
    xor r10, r10
    xor rcx, rcx ;counter
    inc rcx
    xor rbx, rbx
    mov r8, rsi
    mov r9, rdi
    mov rdi, rsi
    mov rsi, r8
    call GetStrlen ;now len of our operand is in rdx
    mov rdi, r9
    dec rdi

resetRsi:
    inc rdi
    mov rsi, r8
    xor rcx, rcx
    inc rcx

loopZero:
    mov al, [rsi]
    mov bl, [rdi]
    cmp rcx, rdx
    jg notMatch
    cmp al, bl
    jne notMatch
    cmp rcx, rdx
    je checkMatch
    inc rsi
    inc rdi
    inc rcx
    jmp loopZero

checkMatch:
    inc rdi
    mov al, [rdi]
    cmp al, ':'
    je foundMatch
    jmp notMatch

notMatch:
    mov al, [rdi]
    cmp al, 0x20
    je resetRsi
    inc rdi
    jmp notMatch

foundMatch:
    inc rdi
    mov al, [rdi]
    cmp al, 0x20
    je returnZeroFinder
    mov [r11 + r10], al
    inc r10
    jmp foundMatch

returnZeroFinder:
    ; mov rsi, r11
    ; call print_string
    pop r11
    pop r10
    pop r9
    pop r8
    pop rdx
    pop rcx
    pop rbx
    pop rax
    pop rdi
    pop rsi
    ret
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

type_checker:
    push rsi
    push rax
    push rbx
    push rcx
    push rdx
    xor r8, r8
    mov al, byte [rsi + r8]
    cmp al, 'B'
    je it_is_memory

    cmp al, 'W'
    je it_is_memory

    cmp al, 'D'
    je it_is_memory

    cmp al, 'Q'
    je it_is_memory
    ;;;;;;;;;;;;;;;;

    cmp al, 'r'
    je it_is_reg

    cmp al, 'e'
    je it_is_reg

    cmp al, 'c'
    je it_is_reg

    cmp al, 'b'
    je it_is_reg

    cmp al, 'a'
    je it_is_reg

    cmp al, 's'
    je it_is_reg

    cmp al, 'd'
    je it_is_reg

    ;;;;;;;;;;;;;;;;;;

    jmp it_is_imm

it_is_reg:
    mov [type], r8
    jmp return_type

it_is_memory:
    inc r8
    mov [type], r8
    jmp return_type

it_is_imm:
    inc r8
    inc r8
    mov [type], r8
    jmp return_type

return_type:
    pop rdx
    pop rcx
    pop rbx
    pop rax
    pop rsi
    ret
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

get_mem_size_int:
    push rsi
    push rdi
    push rax
    push rbx
    push rcx
    push rdx
    mov rsi, mem_op_size

    cmp byte[mem_op_size], 'B'
    je size_is_8

    cmp byte[mem_op_size], 'W'
    je size_is_16

    cmp byte[mem_op_size], 'D'
    je size_is_32

    cmp byte[mem_op_size], 'Q'
    je size_is_64

size_is_8:
    mov byte[mem_size_int], 8
    jmp return_get_mem_size

size_is_16:
    mov byte[mem_size_int], 16
    jmp return_get_mem_size

size_is_32:
    mov byte[mem_size_int], 32
    jmp return_get_mem_size

size_is_64:
    mov byte[mem_size_int], 64
    jmp return_get_mem_size

return_get_mem_size:
    pop rdx
    pop rcx
    pop rbx
    pop rax
    pop rdi
    pop rsi
    ret
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

find_size_in_string:
    push rsi ;our operenad
    push rdi ; ZOp db
    push rax
    push rbx
    push rcx
    push rdx
    push r8
    push r9
    xor r10, r10
    xor rcx, rcx ;counter
    inc rcx
    xor rbx, rbx
    mov r8, rsi
    mov r9, rdi
    mov rdi, rsi
    mov rsi, r8
    call GetStrlen ;now len of our operand is in rdx
    mov rdi, r9
    dec rdi

reset_find_one:
    inc rdi
    mov rsi, r8
    xor rcx, rcx
    inc rcx

loop_find:
    mov al, [rsi]
    mov bl, [rdi]
    cmp bl, 0
    je return_size
    cmp rcx, rdx
    jg notMatchFind
    cmp al, bl
    jne notMatchFind
    cmp rcx, rdx
    je checkMatchFind
    inc rsi
    inc rdi
    inc rcx
    jmp loop_find

checkMatchFind:
    inc rdi
    mov al, [rdi]
    cmp al, ':'
    je foundMatchFind
    jmp notMatchFind

notMatchFind:
    mov al, [rdi]
    cmp al, 0x20
    je reset_find_one
    inc rdi
    jmp notMatchFind

foundMatchFind:
    inc r10
    inc rdi
    mov al, [rdi]
    cmp al, 0x20
    je return_size

return_size:
    pop r9
    pop r8
    pop rdx
    pop rcx
    pop rbx
    pop rax
    pop rdi
    pop rsi
    ret

getSize:
    push rsi ;;
    push rdi
    push rax
    push rbx
    push rcx
    push rdx
    mov rdi, registers_64
    call find_size_in_string
    cmp r10, 1
    je is_64
    mov rdi, registers_32
    call find_size_in_string
    cmp r10, 1
    je is_32
    mov rdi, registers_16
    call find_size_in_string
    cmp r10, 1
    je is_16
    mov rdi, registers_8
    call find_size_in_string
    cmp r10, 1
    je is_8

is_64:
    mov byte[reg_size], 64
    jmp return_get_size

is_32:
    mov byte[reg_size], 32
    jmp return_get_size

is_16:
    mov byte[reg_size], 16
    jmp return_get_size

is_8:
    mov byte[reg_size], 8
    jmp return_get_size

return_get_size:
    pop rdx 
    pop rcx
    pop rbx
    pop rax
    pop rdi
    pop rsi
    ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

check_is_new:
    push rsi
    push rax
    push rbx
    push rcx
    xor rcx, rcx
    mov al, [rsi]
    cmp al,'r'
    je second_check
    jmp return_new_check

second_check:
    mov al,[rsi + 1]
    cmp al,48
    jl return_new_check
    cmp al,57
    jg return_new_check
    mov byte[is_new], 1
    jmp return_new_check

return_new_check:
    pop rcx
    pop rbx
    pop rax
    pop rsi
    ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

sum_of_characters:
    push rsi
    push rdi
    push rax
    push rbx
    push rcx
    push rdx
    mov rdi, rsi
    call GetStrlen ;len of opcode is in rdx
    xor rcx, rcx
    xor rax, rax
    xor rbx, rbx

loop_sum:
    cmp rcx, rdx
    je return_sum_chars
    mov byte bl, [rsi + rcx]
    add ax, bx
    inc rcx
    jmp loop_sum

return_sum_chars:
    mov [sum], rax
    pop rdx
    pop rcx
    pop rbx
    pop rax
    pop rdi
    pop rsi
    ret
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

check_if_reg_is_bp:
    push rsi
    push rdi
    cmp byte[rsi + 1], 'b'
    je next_bp_check
    jmp return_if_reg_is_bp

next_bp_check:
    cmp byte[rsi+2], 'p'
    je make_bp_flag_one
    jmp return_if_reg_is_bp

make_bp_flag_one:
    mov byte[bp_flag], 1
    jmp return_if_reg_is_bp

return_if_reg_is_bp:
    pop rdi
    pop rsi
    ret

get_disp_size:
    push rsi
    push rdi
    push rax
    push rbx
    push rcx
    push rdx
    xor rcx, rcx
    mov rdi, rsi
    call GetStrlen ;len is in rdx
    cmp rdx,1
    je check_if_its_zero

edame:
    cmp rdx, 2
    jg disp_is_32
    cmp rdx,2
    jl disp_is_8
    dec rdx ;last index

second_disp_check:
    mov al,[rsi + rcx]
    cmp al,56
    jl disp_is_8
    jmp disp_is_32

check_if_its_zero:
    xor r11,r11
    cmp byte[rsi + r11], '0'
    je its_zero
    jmp edame

its_zero:
    mov byte[disp_flag], 0
    xor rax, rax
    mov [disp_size], al
    jmp return_disp_size

disp_is_32:
    mov rax, 32
    mov [disp_size], al
    jmp return_disp_size

disp_is_8:
    mov rax, 8
    mov [disp_size], al
    jmp return_disp_size

return_disp_size:
    pop rdx
    pop rcx
    pop rbx
    pop rax
    pop rdi
    pop rsi
    ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

check_operands:
    push rsi
    push rdi
    push rax
    push rbx
    push rcx
    push rdx
    cmp byte[operand1_type], 0 ;reg
    je reg_check_op2
    ; cmp byte[operand1_type], 1 ;memory
    ; je mem_check_op2
    ; cmp byte[operand1_type], 2 ;imm
    ; je imm_op2

reg_check_op2:
    cmp byte[operand2_type], 0
    je reg_reg
    cmp byte[operand2_type], 1
    je reg_mem 
    cmp byte[operand2_type], 2
    je reg_imm
; mem_check_op2:
;     cmp byte[operand2_type], 0 ;reg
;     je mem_reg
;     cmp byte[operand2_type], 1 ;mem
;     je mem_mem
;     cmp byte[operand2_type], 2 ;imm
;     je mem_imm
reg_imm:
    mov rsi, opcode
    call sum_of_characters
    xor rax, rax
    mov ax, [sum]
    
reg_mem:
    mov rsi, opcode
    call sum_of_characters
    xor rax, rax
    mov ax, [sum]
    mov rdi, registers
    mov rsi, operand1
    mov r11, regOp
    call ZeroOperandCodeFinder
    call check_if_we_have_rex
    
    cmp byte[rex_flag], 1
    je reg_mem_rex
    
    cmp ax,338
    je is_mov_reg_mem

    cmp ax,297
    je is_add_reg_mem

    cmp ax,296
    je is_adc_reg_mem

    cmp ax,307
    je is_and_reg_mem

    cmp ax,255
    je is_or_reg_mem

    cmp ax,330
    je is_sub_reg_mem

    cmp ax,311
    je is_sbb_reg_mem

    cmp ax,345
    je is_xor_reg_mem

    cmp ax,320
    je is_cmp_reg_mem

    cmp ax,448
    je is_test_reg_mem

    is_mov_reg_mem:
        mov rsi, mov_reg_mem
        call set_w
        call handle_memory_reg_mem
        jmp return_check_operands
    
    is_add_reg_mem:
        mov rsi, add_reg_mem
        call set_w
        call handle_memory_reg_mem
        jmp return_check_operands
    
    is_adc_reg_mem:
        mov rsi, adc_reg_mem
        call set_w
        call handle_memory_reg_mem
        jmp return_check_operands

    is_and_reg_mem:
        mov rsi, and_reg_mem
        call set_w
        call handle_memory_reg_mem
        jmp return_check_operands
    
    is_or_reg_mem:
        mov rsi, or_reg_mem
        call set_w
        call handle_memory_reg_mem
        jmp return_check_operands

    is_sub_reg_mem:
        mov rsi, sub_reg_mem
        call set_w
        call handle_memory_reg_mem
        jmp return_check_operands

    is_sbb_reg_mem:
        mov rsi, sbb_reg_mem
        call set_w
        call handle_memory_reg_mem
        jmp return_check_operands
    

    is_xor_reg_mem:
        mov rsi, xor_reg_mem
        call set_w
        call handle_memory_reg_mem
        jmp return_check_operands
    
    is_cmp_reg_mem:
        mov rsi, cmp_reg_mem
        call set_w
        call handle_memory_reg_mem
        jmp return_check_operands

    is_test_reg_mem:
        mov rsi, test_reg_mem
        call set_w
        call handle_memory_reg_mem
        jmp return_check_operands
    

reg_mem_rex:
    mov rsi,operand1
    call check_is_new
    cmp byte[is_new], 1
    jne reg_mem
    mov byte[rex + 5], '1'
    jmp reg_mem


reg_reg:
    call check_if_we_have_rex
    cmp byte[rex_flag], 0
    je countinue_reg_reg
    call set_rex_reg_reg
    mov rsi, rex
    call print_string

countinue_reg_reg:
    mov rsi, operand2
    mov rdi, registers
    mov r11, regOp
    call ZeroOperandCodeFinder
    mov rdi, registers
    mov rsi, operand1
    mov rdi, registers
    mov r11, rm
    call ZeroOperandCodeFinder
    mov rsi, opcode
    call sum_of_characters
    xor rax, rax
    mov ax, [sum]

    cmp ax,338
    je is_mov_reg_reg

    cmp ax,297
    je is_add_reg_reg

    cmp ax,296
    je is_adc_reg_reg

    cmp ax,307
    je is_and_reg_reg

    cmp ax,255
    je is_or_reg_reg

    cmp ax,330
    je is_sub_reg_reg

    cmp ax,311
    je is_sbb_reg_reg

    cmp ax,345
    je is_xor_reg_reg

    cmp ax,320
    je is_cmp_reg_reg

    cmp ax,448
    je is_test_reg_reg

    is_mov_reg_reg:
        mov rsi, mov_reg_reg
        call set_w
        jmp return_check_operands


    is_add_reg_reg:
        mov rsi, add_reg_reg
        call set_w
        jmp return_check_operands

    is_adc_reg_reg:
        mov rsi, adc_reg_reg
        call set_w
        jmp return_check_operands

    is_and_reg_reg:
        mov rsi, and_reg_reg
        call set_w
        jmp return_check_operands

    is_or_reg_reg:
        mov rsi, or_reg_reg
        call set_w
        jmp return_check_operands

    is_sub_reg_reg:
        mov rsi, sub_reg_reg
        call set_w
        jmp return_check_operands

    is_sbb_reg_reg:
        mov rsi, sbb_reg_reg
        call set_w
        jmp return_check_operands

    is_xor_reg_reg:
        mov rsi, sbb_reg_reg
        call set_w
        jmp return_check_operands

    is_cmp_reg_reg:
        mov rsi, cmp_reg_reg
        call set_w
        jmp return_check_operands

    is_test_reg_reg:
        mov rsi, test_reg_reg
        call set_w
        jmp return_check_operands

return_check_operands:
    pop rdx
    pop rcx
    pop rbx
    pop rax
    pop rdi
    pop rsi
    ret

handle_memory_reg_mem:
    push rsi
    push rdi
    push rax
    push rbx
    push rcx
    push rdx
    ;call printHere
    xor rax, rax
    ; mov al,[base_flag]
    ; call writeNum
    ; call newLine
    cmp byte[base_flag], 1
    je memory_has_base
    cmp byte[index_flag], 1
    jne it_is_direct_addressing

memory_has_base:
    cmp byte[index_flag], 1
    je memory_has_index
    ;now base is rm
    mov rsi, base_reg
    mov rdi, registers
    mov r11, rm
    call ZeroOperandCodeFinder ;now rm is base_reg code
    mov rsi, base_reg
    call check_if_reg_is_bp
    cmp byte[bp_flag],1
    je mod_8_bp
    ;check mod by disp size
    cmp byte[disp_flag],1
    je base_disp
    jmp base_no_disp_no_index

base_disp:
    call check_if_we_have_rex
    cmp byte[rex_flag],1
    je change_rex_mem_1

countinue_base_disp:
    mov rsi, disp_value
    call get_disp_size
    mov al, [disp_size]
    cmp al,8
    je mod_8
    cmp al,32
    je mod_32
    mov rsi, mod8
    mov rdi, mod
    call transfer_data
    jmp return_handle_memory_reg_mem

change_rex_mem_1:
    mov rsi, base_reg
    call check_is_new
    cmp byte[is_new],0
    je countinue_base_disp
    mov byte[rex + 7], '1'
    jmp countinue_base_disp

mod_8_bp:
    ;call printHere
    ; xor rax,rax
    ; mov al, [disp_flag]
    ; call writeNum
    cmp byte[disp_flag], 1
    je mod_8
    ;call printHere
    ;mov rsi, disp_bp
    ;call print_string
    mov rsi, disp_bp
    mov rdi, disp_value
    call transfer_data
    ;call print_string
    mov rsi, mod8
    mov rdi, mod
    call transfer_data
    jmp return_handle_memory_reg_mem

mod_8:
    mov rsi, mod8
    mov rdi, mod
    call transfer_data
    jmp return_handle_memory_reg_mem

mod_32:
    mov rsi, mod32
    mov rdi, mod
    call transfer_data
    jmp return_handle_memory_reg_mem

base_no_disp_no_index:
    mov rsi, mod0
    mov rdi, mod
    call transfer_data
    jmp return_handle_memory_reg_mem

memory_has_index:
    mov rsi,RM100
    mov rdi,rm
    call transfer_data
    mov byte[sib_flag], 1
    ;hallelujah shokret 
    cmp byte[scale_value + rcx], '1'
    je scale_is_1

    cmp byte[scale_value + rcx], '2'
    je scale_is_2

    cmp byte[scale_value + rcx], '4'
    je scale_is_4

    cmp byte[scale_value + rcx], '8'
    je scale_is_8

countinue_memory_has_index:
    call check_if_we_have_rex
    cmp byte[rex_flag], 1
    je has_new_reg

next_countinue_memory_has_index:
    cmp byte[base_flag],1
    je has_base_index
    mov rsi, mod0
    mov rdi, mod
    call transfer_data
    xor rsi,rsi
    xor rdi,rdi
    xor r11,r11
    mov rsi, index_reg
    ;call print_string
    mov rdi, registers
    mov r11, sib_index
    call ZeroOperandCodeFinder
    mov rsi, sib_index
    ;call print_string
    mov rsi, sib_base_dir_addr
    mov rdi, sib_base
    call transfer_data
    cmp byte[disp_flag],1
    je disp32_darim
    mov rsi, disp32
    mov rdi,disp_value
    call transfer_data
    jmp return_handle_memory_reg_mem

disp32_darim:
    mov rdi, disp_value
    call GetStrlen
    mov rcx, 32
loop_disp32:
    mov byte[disp_value + rdx], '0'
    inc rdx
    cmp rdx,rcx
    je return_handle_memory_reg_mem
    jmp loop_disp32

has_new_reg:
    mov rsi, index_reg
    call check_is_new
    cmp byte[is_new],0
    je next_countinue_memory_has_index
    mov byte[rex + 6], '1'
    jmp next_countinue_memory_has_index

scale_is_1:
    mov rsi, scale1
    mov rdi, sib_scale
    call transfer_data
    jmp countinue_memory_has_index

scale_is_2:
    mov rsi, scale2
    mov rdi, sib_scale
    call transfer_data
    jmp countinue_memory_has_index

scale_is_4:
    mov rsi, scale4
    mov rdi, sib_scale
    call transfer_data
    jmp countinue_memory_has_index

scale_is_8:
    mov rsi, scale8
    mov rdi, sib_scale
    call transfer_data
    jmp countinue_memory_has_index

has_base_index:
    call check_if_we_have_rex
    cmp byte[rex_flag],1
    je base_index_rex

edame_has_base_index:
    mov rsi, base_reg
    mov rdi, registers
    mov r11, temp
    call ZeroOperandCodeFinder
    mov rsi, temp
    mov rdi, sib_base
    call transfer_data
    mov rsi, base_reg
    call check_if_reg_is_bp ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    mov rsi, disp_value
    call get_disp_size
    mov al, [disp_size]
    
    cmp byte[bp_flag], 1
    je has_bp

countinue_has_base_index:

    cmp al,8
    je mod_8

    cmp al,32
    je mod_32

    mov rsi, mod0
    mov rdi, mod
    call transfer_data
    jmp return_handle_memory_reg_mem

base_index_rex:
    mov rsi, base_reg
    call check_is_new
    cmp byte[is_new],0
    je edame_has_base_index
    mov byte[rex + 7], '1'

has_bp:
    cmp byte[disp_flag], 1
    je countinue_has_base_index
    jmp mod_8_bp

it_is_direct_addressing:
    mov rsi,RM100
    mov rdi,rm
    call transfer_data
    mov rsi, mod0
    mov rdi, mod
    call transfer_data
    mov rsi, sib_base_dir_addr
    mov rdi, sib_base
    call transfer_data
    mov rsi, disp_value
    call get_disp_size
    jmp return_handle_memory_reg_mem

return_handle_memory_reg_mem:
    ;call set_rex_reg_reg
    pop rdx
    pop rcx
    pop rbx
    pop rax
    pop rdi
    pop rsi
    ret


get_w_index:
    push rsi
    push rdi
    push rax
    push rbx
    push rdx
    xor rcx, rcx

loop_w_indx:
    mov al, [rsi + rcx]
    cmp al, 'w'
    je return_get_w_index
    inc rcx
    jmp loop_w_indx

return_get_w_index:
    pop rdx
    pop rbx
    pop rax
    pop rdi
    pop rsi
    ret

set_w:
    push rsi
    push rdi
    push rax
    push rbx
    push rcx
    push rdx
    mov r9, rsi
    call get_w_index ;now w index is in rcx
    mov rsi, operand2
    call getSize
    mov al, [reg_size]
    ; call newLine
    ; call writeNum
    ; call newLine
    cmp al,8
    jne set_w_one
    jmp change_in_string

set_w_one:
    xor r10, r10
    inc r10
    mov [opcode_w], r10
    jmp change_in_string

change_in_string:
    mov rsi, r9
    mov al, [opcode_w]
    add al, 48
    mov [rsi+rcx], al
    jmp return_set_w

return_set_w:
    ; mov rax,[opcode_w]
    ; call writeNum
    ; call newLine
    pop rdx
    pop rcx
    pop rbx
    pop rax
    pop rdi
    pop rsi
    ret

transfer_data:
    push rsi ;source
    push rdi ;destination
    push rax
    push rbx
    push rcx
    push rdx
    mov rbx, rdi
    xor rcx, rcx
    mov rdi, rsi
    call GetStrlen ;now len of source is in rdx
    mov rdi, rbx

loop_transfer:
    cmp rcx, rdx
    je return_transfer_data
    mov rax,[rsi + rcx]
    mov [rdi + rcx], rax
    inc rcx
    jmp loop_transfer

return_transfer_data:
    pop rdx
    pop rcx
    pop rbx
    pop rax
    pop rdi
    pop rsi
    ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

check_if_we_have_rex:
    push rsi
    push rdi
    push rax
    push rbx
    push rcx
    push rdx
    cmp byte[operand1_type], 0
    je check_reg_op1

    cmp byte[operand1_type], 1
    je check_mem_op1

check_reg_op1:
    mov rsi, operand1
    call getSize
    cmp byte[reg_size], 64
    je make_rex_flag_one

    cmp byte[operand2_flag], 1
    je check_op2
    jmp return_flag_rex

check_mem_op1:
    call get_mem_size_int
    cmp byte[mem_size_int], 64
    je make_rex_flag_one

    cmp byte[operand2_flag], 1
    je check_op2
    jmp return_flag_rex

check_op2:
    cmp byte[operand2_type], 0
    je check_reg_op2_rex

    cmp byte[operand2_type], 1
    je check_mem_op2

check_reg_op2_rex:
    mov rsi, operand2
    call getSize

    cmp byte[reg_size], 64
    je make_rex_flag_one
    jmp return_flag_rex

check_mem_op2:
    call get_mem_size_int

    cmp byte[mem_size_int], 64
    je make_rex_flag_one
    jmp return_flag_rex

make_rex_flag_one:
    mov byte[rex_flag], 1
    jmp return_flag_rex

return_flag_rex:
    pop rdx
    pop rcx
    pop rbx
    pop rax
    pop rdi
    pop rsi
    ret

set_rex_reg_reg:
    push rsi
    push rdi
    push rax
    push rbx
    push rcx
    push rdx
    mov rsi, operand1
    call check_is_new
    cmp byte[is_new], 1
    je change_rex_op1
    jmp check_rex_op2

change_rex_op1:
    mov byte[rex + 7], '1'
    jmp check_rex_op2

check_rex_op2:
    mov byte[is_new], 0
    mov rsi, operand2
    call check_is_new
    cmp byte[is_new], 1
    je change_rex_op2
    jmp return_rex_reg_reg

change_rex_op2:
    mov byte[rex + 5], '1'
    jmp return_rex_reg_reg

return_rex_reg_reg:
    pop rdx
    pop rcx
    pop rbx
    pop rax
    pop rdi
    pop rsi
    ret

init:
    push rsi
    push rdi
    push rax
    push rbx
    push rcx
    push rdx
    call mainParse
    xor rax, rax
    mov al, [operand1_flag]
    ;call writeNum
    cmp al, 1
    je init_operand1
    xor rax, rax
    mov al, [operand2_flag]
    cmp al, 1
    jmp it_is_zero_operand

init_operand1:
    ;call printHere
    mov rsi, operand1
    ;call print_string
    call type_checker
    xor rax, rax
    mov al, [type]
    ;call writeNum
    mov [operand1_type], al
    cmp al, 1
    je op_is_memory1
    xor rax, rax
    mov al, [operand2_flag]
    ;call writeNum
    cmp al, 1
    je init_operand2
    cmp al, 0
    je return_init

init_operand2:
    mov rsi, operand2
    ;call print_string
    call type_checker
    xor rax, rax
    mov al, [type]
    mov [operand2_type], al
    cmp al, 1
    je op_is_memory2
    jmp return_init

op_is_memory1:
    call memory_parse
    xor rax, rax
    mov al, [operand2_flag]
    cmp al, 1
    je init_operand2
    cmp al, 0
    je return_init

op_is_memory2:
    call memory_parse
    jmp return_init

it_is_zero_operand:
    mov rsi, opcode
    mov rdi, zeroOperands
    call ZeroOperandCodeFinder
    jmp return_init

return_init:
    pop rdx
    pop rcx
    pop rbx
    pop rax
    pop rdi
    pop rsi
    ret

_start:
    ;mov rsi, mov_reg_reg
    ;call print_string
    mov rsi, cmnd
    call getString
    ; mov rsi, cmnd
    ; mov rdi, registers
    ; mov r11,base_reg
    ; call ZeroOperandCodeFinder
    ; mov rsi, base_reg
    ; call print_string
    call print_string
    call init
    call handle_memory_reg_mem
    ;rm
    call printHere
    mov rsi, rm
    call print_string
    ;mod
    mov rsi, mod
    call print_string
    mov rsi, disp_value
    call print_string
    ;regOp
    mov rsi, regOp
    call print_string
    ;sib_scale
    call printHere
    mov rsi, sib_scale
    call print_string
    ;sib_index
    mov rsi, sib_index
    call print_string
    ;sib_base
    call printHere
    mov rsi, sib_base
    call print_string

exit:
    call newLine
    mov rax, 1
    mov rbx, 0
    int 0x80
