.text

read:
  mov $0, %rax
  syscall
  ret

write:
  mov $1, %rax
  syscall
  ret

exit:
  mov $60, %rax
  syscall

memcpy:
  testl   %edx, %edx
  jle     .L1
  leal    -1(%rdx), %r8d
  movl    %edx, %ecx
  cmpl    $6, %r8d
  jbe     .L3
  leaq    -1(%rdi), %rax
  subq    %rsi, %rax
  cmpq    $14, %rax
  jbe     .L3
  cmpl    $14, %r8d
  jbe     .L11
  shrl    $4, %ecx
  xorl    %eax, %eax
  salq    $4, %rcx
.L5:
  movdqu  (%rsi,%rax), %xmm0
  movups  %xmm0, (%rdi,%rax)
  addq    $16, %rax
  cmpq    %rax, %rcx
  jne     .L5
  movl    %edx, %eax
  andl    $-16, %eax
  movl    %eax, %r8d
  cmpl    %eax, %edx
  je      .L1
  movl    %edx, %ecx
  subl    %eax, %ecx
  leal    -1(%rcx), %r9d
  cmpl    $6, %r9d
  jbe     .L7
.L4:
  movq    (%rsi,%r8), %r9
  movq    %r9, (%rdi,%r8)
  testb   $7, %cl
  je      .L1
  andl    $-8, %ecx
  addl    %ecx, %eax
.L7:
  movslq  %eax, %rcx
  movzbl  (%rsi,%rcx), %r8d
  movb    %r8b, (%rdi,%rcx)
  leal    1(%rax), %ecx
  cmpl    %ecx, %edx
  jle     .L1
  movslq  %ecx, %rcx
  movzbl  (%rsi,%rcx), %r8d
  movb    %r8b, (%rdi,%rcx)
  leal    2(%rax), %ecx
  cmpl    %ecx, %edx
  jle     .L1
  movslq  %ecx, %rcx
  movzbl  (%rsi,%rcx), %r8d
  movb    %r8b, (%rdi,%rcx)
  leal    3(%rax), %ecx
  cmpl    %ecx, %edx
  jle     .L1
  movslq  %ecx, %rcx
  movzbl  (%rsi,%rcx), %r8d
  movb    %r8b, (%rdi,%rcx)
  leal    4(%rax), %ecx
  cmpl    %ecx, %edx
  jle     .L1
  movslq  %ecx, %rcx
  movzbl  (%rsi,%rcx), %r8d
  movb    %r8b, (%rdi,%rcx)
  leal    5(%rax), %ecx
  cmpl    %ecx, %edx
  jle     .L1
  movslq  %ecx, %rcx
  addl    $6, %eax
  movzbl  (%rsi,%rcx), %r8d
  movb    %r8b, (%rdi,%rcx)
  cmpl    %eax, %edx
  jle     .L1
  cltq
  movzbl  (%rsi,%rax), %edx
  movb    %dl, (%rdi,%rax)
  ret
.L3:
  movslq  %edx, %rdx
  xorl    %eax, %eax
.L9:
  movzbl  (%rsi,%rax), %ecx
  movb    %cl, (%rdi,%rax)
  addq    $1, %rax
  cmpq    %rdx, %rax
  jne     .L9
.L1:
  ret
.L11:
  xorl    %r8d, %r8d
  xorl    %eax, %eax
  jmp     .L4
strfind:
  testl   %edx, %edx
  jle     .L21
  movslq  %edx, %rdx
  addq    %rdi, %rdx
  jmp     .L20
.L24:
  addq    $1, %rdi
  cmpq    %rdx, %rdi
  je      .L21
.L20:
  cmpb    %sil, (%rdi)
  jne     .L24
  movl    $1, %eax
  ret
.L21:
  xorl    %eax, %eax
  ret
streq:
  testl   %edx, %edx
  jle     .L28
  movslq  %edx, %rdx
  xorl    %eax, %eax
  jmp     .L27
.L31:
  addq    $1, %rax
  cmpq    %rax, %rdx
  je      .L28
.L27:
  movzbl  (%rsi,%rax), %ecx
  cmpb    %cl, (%rdi,%rax)
  je      .L31
  xorl    %eax, %eax
  ret
.L28:
  movl    $1, %eax
  ret
strlen:
  cmpb    $0, (%rdi)
  je      .L34
  subq    $8, %rsp
  addq    $1, %rdi
  call    strlen
  addq    $8, %rsp
  addl    $1, %eax
  ret
.L34:
  xorl    %eax, %eax
  ret
strtoi:
  testl   %esi, %esi
  jle     .L42
  movslq  %esi, %rsi
  xorl    %eax, %eax
  addq    %rdi, %rsi
.L41:
  leal    (%rax,%rax,4), %edx
  movsbl  (%rdi), %eax
  addq    $1, %rdi
  leal    (%rax,%rdx,2), %eax
  cmpq    %rdi, %rsi
  jne     .L41
  ret
.L42:
  xorl    %eax, %eax
  ret
itostr:
  pushq   %rbp
  movslq  %edx, %rdx
  movl    %edi, %ecx
  addq    $15, %rdx
  andq    $-16, %rdx
  movq    %rsp, %rbp
  subq    %rdx, %rsp
  movq    %rsp, %rdx
  testl   %edi, %edi
  js      .L54
  je      .L50
  xorl    %r8d, %r8d
.L48:
  movslq  %ecx, %rax
  movl    %ecx, %edi
  imulq   $1717986919, %rax, %rax
  sarl    $31, %edi
  sarq    $34, %rax
  subl    %edi, %eax
  leal    (%rax,%rax,4), %edi
  addl    %edi, %edi
  subl    %edi, %ecx
  addl    $48, %ecx
  movb    %cl, (%rdx,%r8)
  movl    %eax, %ecx
  movq    %r8, %rax
  leaq    1(%r8), %r8
  testl   %ecx, %ecx
  jne     .L48
  movslq  %eax, %rcx
  addq    %rdx, %rcx
  movq    %rsi, %rdx
  movl    %eax, %esi
  leaq    1(%rdx,%rsi), %rdi
.L49:
  movzbl  (%rcx), %esi
  addq    $1, %rdx
  subq    $1, %rcx
  movb    %sil, -1(%rdx)
  cmpq    %rdi, %rdx
  jne     .L49
  leave
  addl    $1, %eax
  ret
.L54:
  movb    $45, (%rsp)
  negl    %ecx
  movl    $1, %r8d
  jmp     .L48
.L50:
  leave
  xorl    %eax, %eax
  ret
getc:
  subq    $24, %rsp
  movl    $1, %edx
  leaq    12(%rsp), %rsi
  call    read
  movl    $0, %edx
  testq   %rax, %rax
  movsbl  12(%rsp), %eax
  cmove   %edx, %eax
  addq    $24, %rsp
  ret
getchar:
  subq    $24, %rsp
  movl    $1, %edx
  xorl    %edi, %edi
  leaq    12(%rsp), %rsi
  call    read
  movl    $0, %edx
  testq   %rax, %rax
  movsbl  12(%rsp), %eax
  cmove   %edx, %eax
  addq    $24, %rsp
  ret
gets:
  subq    $8, %rsp
  movslq  %esi, %rdx
  movq    %rdi, %rsi
  xorl    %edi, %edi
  call    read
  addq    $8, %rsp
  ret
puts:
  subq    $8, %rsp
  movslq  %esi, %rdx
  movq    %rdi, %rsi
  movl    $1, %edi
  call    write
  addq    $8, %rsp
  ret
putc:
  subq    $24, %rsp
  movl    $1, %edx
  movb    %sil, 12(%rsp)
  leaq    12(%rsp), %rsi
  call    write
  addq    $24, %rsp
  ret
putchar:
  subq    $24, %rsp
  movl    $1, %edx
  movb    %dil, 15(%rsp)
  leaq    15(%rsp), %rsi
  movl    $1, %edi
  call    write
  addq    $24, %rsp
  ret
putd:
  pushq   %rbp
  movl    %edi, %ecx
  movq    %rsp, %rbp
  pushq   %r12
  pushq   %rbx
  subq    $144, %rsp
  testl   %edi, %edi
  js      .L83
  je      .L84
  movq    %rsp, %rdi
  subq    $112, %rsp
  leaq    -128(%rbp), %rbx
  movq    %rsp, %rdx
.L73:
  xorl    %esi, %esi
.L76:
  movslq  %ecx, %rax
  movl    %ecx, %r8d
  imulq   $1717986919, %rax, %rax
  sarl    $31, %r8d
  sarq    $34, %rax
  subl    %r8d, %eax
  leal    (%rax,%rax,4), %r8d
  addl    %r8d, %r8d
  subl    %r8d, %ecx
  movq    %rsi, %r8
  addl    $48, %ecx
  movb    %cl, (%rdx,%rsi)
  movl    %eax, %ecx
  addq    $1, %rsi
  testl   %eax, %eax
  jne     .L76
  movslq  %r8d, %rax
  leaq    -127(%rbp,%r8), %r12
  addq    %rdx, %rax
  movq    %rbx, %rdx
.L77:
  movzbl  (%rax), %ecx
  addq    $1, %rdx
  subq    $1, %rax
  movb    %cl, -1(%rdx)
  cmpq    %r12, %rdx
  jne     .L77
  movq    %rdi, %rsp
.L78:
  movzbl  (%rbx), %eax
  movl    $1, %edx
  leaq    -129(%rbp), %rsi
  movl    $1, %edi
  addq    $1, %rbx
  movb    %al, -129(%rbp)
  call    write
  cmpq    %r12, %rbx
  jne     .L78
  leaq    -16(%rbp), %rsp
  xorl    %eax, %eax
  popq    %rbx
  popq    %r12
  popq    %rbp
  ret
.L83:
  leaq    -128(%rbp), %rbx
  movl    %edi, -148(%rbp)
  movl    $1, %edx
  movl    $1, %edi
  movq    %rbx, %rsi
  movb    $45, -128(%rbp)
  call    write
  movl    -148(%rbp), %ecx
  movq    %rsp, %rdi
  subq    $112, %rsp
  movq    %rsp, %rdx
  negl    %ecx
  jmp     .L73
.L84:
  leaq    -128(%rbp), %rsi
  movl    $1, %edx
  movl    $1, %edi
  movb    $48, -128(%rbp)
  call    write
  leaq    -16(%rbp), %rsp
  xorl    %eax, %eax
  popq    %rbx
  popq    %r12
  popq    %rbp
  ret
putstr:
  pushq   %rbx
  movq    %rdi, %rbx
  cmpb    $0, (%rdi)
  je      .L87
  leaq    1(%rdi), %rdi
  call    strlen
  movq    %rbx, %rsi
  movl    $1, %edi
  leal    1(%rax), %edx
  movslq  %edx, %rdx
  call    write
  popq    %rbx
  ret
.L87:
  movq    %rbx, %rsi
  xorl    %edx, %edx
  movl    $1, %edi
  call    write
  popq    %rbx
  ret
isalpha:
  andl    $-33, %edi
  xorl    %eax, %eax
  subl    $65, %edi
  cmpb    $25, %dil
  setbe   %al
  ret
isdigit:
  subl    $48, %edi
  xorl    %eax, %eax
  cmpb    $9, %dil
  setbe   %al
  ret
isalnum:
  movl    %edi, %eax
  movl    $1, %edx
  andl    $-33, %eax
  subl    $65, %eax
  cmpb    $25, %al
  jbe     .L91
  subl    $48, %edi
  xorl    %edx, %edx
  cmpb    $9, %dil
  setbe   %dl
.L91:
  movl    %edx, %eax
  ret
isspace:
  subl    $9, %edi
  xorl    %eax, %eax
  cmpb    $23, %dil
  ja      .L94
  movq    $-8388628, %rax
  btq     %rdi, %rax
  setnc   %al
  movzbl  %al, %eax
.L94:
  ret
