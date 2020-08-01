	.arch armv8-a
	.eabi_attribute 28, 1
	.eabi_attribute 20, 1
	.eabi_attribute 21, 1
	.eabi_attribute 23, 3
	.eabi_attribute 24, 1
	.eabi_attribute 25, 1
	.eabi_attribute 26, 2
	.eabi_attribute 30, 2
	.eabi_attribute 34, 1
	.eabi_attribute 18, 4
	.file	"align.c"
	.text
	.align	2
	.global	print_address
	.arch armv8-a
	.syntax unified
	.arm
	.fpu vfp
	.type	print_address, %function
print_address:
	@ args = 0, pretend = 0, frame = 8
	@ frame_needed = 0, uses_anonymous_args = 0
	push	{r4, r5, r6, r7, r8, r9, lr}
	sub	sp, sp, #12
	mov	r6, r0
	movw	r0, #:lower16:.LC0
	movt	r0, #:upper16:.LC0
	mov	r1, r6
	bl	printf
	movw	r3, #:lower16:stdout
	movt	r3, #:upper16:stdout
	mov	r7, #4
	mov	r5, #32768
	mov	r9, #48
	ldr	r0, [r3]
	bl	fflush
.L2:
	mov	r4, #4
	mov	r8, #49
.L7:
	mov	r2, #1
	add	r1, sp, #7
	mov	r0, r2
	tst	r6, r5
	beq	.L3
	strb	r8, [sp, #7]
	lsr	r5, r5, r2
	bl	write
	subs	r4, r4, #1
	bne	.L7
.L5:
	mov	r2, #1
	movw	r1, #:lower16:.LC1
	movt	r1, #:upper16:.LC1
	mov	r0, r2
	bl	write
	subs	r7, r7, #1
	bne	.L2
	mov	r1, r6
	movw	r0, #:lower16:.LC2
	movt	r0, #:upper16:.LC2
	bl	printf
	add	sp, sp, #12
	@ sp needed
	pop	{r4, r5, r6, r7, r8, r9, pc}
.L3:
	mov	r2, #1
	add	r1, sp, #7
	mov	r0, r2
	strb	r9, [sp, #7]
	lsr	r5, r5, r2
	bl	write
	subs	r4, r4, #1
	bne	.L7
	b	.L5
	.size	print_address, .-print_address
	.section	.text.startup,"ax",%progbits
	.align	2
	.global	main
	.syntax unified
	.arm
	.fpu vfp
	.type	main, %function
main:
	@ args = 0, pretend = 0, frame = 16
	@ frame_needed = 0, uses_anonymous_args = 0
	push	{r4, r5, r6, r7, r8, r9, r10, fp, lr}
	sub	sp, sp, #20
	movw	r0, #65296
	mov	r7, #10
	movw	r3, #:lower16:.LC0
	movt	r3, #:upper16:.LC0
	str	r3, [sp]
	bl	print_address
	mov	r0, r7
	movw	r9, #:lower16:stdout
	movt	r9, #:upper16:stdout
	bl	print_address
	movw	r0, #65528
	mov	r6, #0
	mov	r8, #48
	bl	print_address
.L29:
	mov	r0, #10
	mov	r5, #4
	mov	r4, #32768
	uxth	r3, r6
	str	r3, [sp, #4]
	bl	putchar
	mov	r1, r6
	ldr	r0, [sp]
	bl	printf
	ldr	r0, [r9]
	bl	fflush
.L11:
	mov	fp, #4
	mov	r10, #49
.L16:
	ldr	r3, [sp, #4]
	mov	r2, #1
	add	r1, sp, #15
	mov	r0, r2
	tst	r3, r4
	beq	.L12
	strb	r10, [sp, #15]
	lsr	r4, r4, r2
	bl	write
	subs	fp, fp, #1
	bne	.L16
.L14:
	mov	r2, #1
	movw	r1, #:lower16:.LC1
	movt	r1, #:upper16:.LC1
	mov	r0, r2
	bl	write
	subs	r5, r5, #1
	bne	.L11
	mov	r1, r6
	movw	r0, #:lower16:.LC2
	movt	r0, #:upper16:.LC2
	uxth	r10, r7
	bl	printf
	mov	r1, r7
	ldr	r0, [sp]
	mov	r5, #4
	bl	printf
	mov	r4, #32768
	ldr	r0, [r9]
	bl	fflush
	str	r6, [sp, #4]
.L17:
	mov	r6, #4
	mov	fp, #49
.L22:
	mov	r2, #1
	add	r1, sp, #15
	mov	r0, r2
	tst	r10, r4
	beq	.L18
	strb	fp, [sp, #15]
	lsr	r4, r4, r2
	bl	write
	subs	r6, r6, #1
	bne	.L22
.L20:
	mov	r2, #1
	movw	r1, #:lower16:.LC1
	movt	r1, #:upper16:.LC1
	mov	r0, r2
	bl	write
	subs	r5, r5, #1
	bne	.L17
	mov	r1, r7
	movw	r0, #:lower16:.LC2
	movt	r0, #:upper16:.LC2
	movw	r5, #65528
	and	r5, r5, r10
	ldr	r6, [sp, #4]
	bl	printf
	mov	r1, r5
	ldr	r0, [sp]
	mov	r10, #4
	bl	printf
	mov	r4, #32768
	ldr	r0, [r9]
	bl	fflush
	str	r6, [sp, #4]
.L23:
	mov	r6, #4
	mov	fp, #49
.L28:
	mov	r2, #1
	add	r1, sp, #15
	mov	r0, r2
	tst	r5, r4
	beq	.L24
	strb	fp, [sp, #15]
	lsr	r4, r4, r2
	bl	write
	subs	r6, r6, #1
	bne	.L28
.L26:
	mov	r2, #1
	movw	r1, #:lower16:.LC1
	movt	r1, #:upper16:.LC1
	mov	r0, r2
	bl	write
	subs	r10, r10, #1
	bne	.L23
	ldr	r6, [sp, #4]
	mov	r1, r5
	movw	r0, #:lower16:.LC2
	movt	r0, #:upper16:.LC2
	add	r6, r6, #1
	bl	printf
	add	r7, r7, #1
	cmp	r6, #8
	bne	.L29
	mov	r0, r10
	add	sp, sp, #20
	@ sp needed
	pop	{r4, r5, r6, r7, r8, r9, r10, fp, pc}
.L24:
	mov	r2, #1
	add	r1, sp, #15
	mov	r0, r2
	strb	r8, [sp, #15]
	lsr	r4, r4, r2
	bl	write
	subs	r6, r6, #1
	bne	.L28
	b	.L26
.L18:
	mov	r2, #1
	add	r1, sp, #15
	mov	r0, r2
	strb	r8, [sp, #15]
	lsr	r4, r4, r2
	bl	write
	subs	r6, r6, #1
	bne	.L22
	b	.L20
.L12:
	mov	r2, #1
	add	r1, sp, #15
	mov	r0, r2
	strb	r8, [sp, #15]
	lsr	r4, r4, r2
	bl	write
	subs	fp, fp, #1
	bne	.L16
	b	.L14
	.size	main, .-main
	.section	.rodata.str1.4,"aMS",%progbits,1
	.align	2
.LC0:
	.ascii	"0x%04x \000"
.LC1:
	.ascii	" \000"
	.space	2
.LC2:
	.ascii	"%5u\012\000"
	.ident	"GCC: (Raspbian 8.3.0-6+rpi1) 8.3.0"
	.section	.note.GNU-stack,"",%progbits
