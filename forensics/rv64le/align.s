	.file	"align.c"
	.option pic
	.attribute arch, "rv64i2p1_m2p0_a2p1_f2p2_d2p2_c2p0_zicsr2p0_zifencei2p0"
	.attribute unaligned_access, 0
	.attribute stack_align, 16
	.text
	.section	.rodata.str1.8,"aMS",@progbits,1
	.align	3
.LC0:
	.string	"0x%04x "
	.align	3
.LC1:
	.string	" "
	.align	3
.LC2:
	.string	"%5u\n"
	.text
	.align	1
	.globl	print_address
	.type	print_address, @function
print_address:
.LFB11:
	.cfi_startproc
	addi	sp,sp,-96
	.cfi_def_cfa_offset 96
	sd	s8,16(sp)
	.cfi_offset 24, -80
	sext.w	s8,a0
	sd	s3,56(sp)
	mv	a1,s8
	.cfi_offset 19, -40
	mv	s3,a0
	lla	a0,.LC0
	sd	ra,88(sp)
	sd	s1,72(sp)
	sd	s2,64(sp)
	sd	s4,48(sp)
	sd	s5,40(sp)
	sd	s6,32(sp)
	sd	s7,24(sp)
	sd	s0,80(sp)
	.cfi_offset 1, -8
	.cfi_offset 9, -24
	.cfi_offset 18, -32
	.cfi_offset 20, -48
	.cfi_offset 21, -56
	.cfi_offset 22, -64
	.cfi_offset 23, -72
	.cfi_offset 8, -16
	call	printf@plt
	la	a5,stdout
	ld	a0,0(a5)
	li	s4,4
	li	s1,32768
	call	fflush@plt
	addi	s2,sp,15
	li	s6,48
	li	s5,49
	lla	s7,.LC1
.L2:
	li	s0,4
.L7:
	and	a5,s3,s1
	li	a2,1
	mv	a1,s2
	li	a0,1
	beq	a5,zero,.L3
	addiw	s0,s0,-1
	sb	s5,15(sp)
	srli	s1,s1,1
	call	write@plt
	bne	s0,zero,.L7
	li	a2,1
	mv	a1,s7
	li	a0,1
	addiw	s4,s4,-1
	call	write@plt
	bne	s4,zero,.L2
.L6:
	mv	a1,s8
	lla	a0,.LC2
	call	printf@plt
	ld	ra,88(sp)
	.cfi_remember_state
	.cfi_restore 1
	ld	s0,80(sp)
	.cfi_restore 8
	ld	s1,72(sp)
	.cfi_restore 9
	ld	s2,64(sp)
	.cfi_restore 18
	ld	s3,56(sp)
	.cfi_restore 19
	ld	s4,48(sp)
	.cfi_restore 20
	ld	s5,40(sp)
	.cfi_restore 21
	ld	s6,32(sp)
	.cfi_restore 22
	ld	s7,24(sp)
	.cfi_restore 23
	ld	s8,16(sp)
	.cfi_restore 24
	addi	sp,sp,96
	.cfi_def_cfa_offset 0
	jr	ra
.L3:
	.cfi_restore_state
	li	a2,1
	li	a0,1
	addiw	s0,s0,-1
	sb	s6,15(sp)
	srli	s1,s1,1
	call	write@plt
	bne	s0,zero,.L7
	li	a2,1
	mv	a1,s7
	li	a0,1
	addiw	s4,s4,-1
	call	write@plt
	bne	s4,zero,.L2
	j	.L6
	.cfi_endproc
.LFE11:
	.size	print_address, .-print_address
	.section	.text.startup,"ax",@progbits
	.align	1
	.globl	main
	.type	main, @function
main:
.LFB12:
	.cfi_startproc
	li	a0,65536
	addi	sp,sp,-128
	.cfi_def_cfa_offset 128
	addi	a0,a0,-240
	sd	ra,120(sp)
	sd	s0,112(sp)
	sd	s3,88(sp)
	sd	s4,80(sp)
	sd	s5,72(sp)
	sd	s6,64(sp)
	sd	s7,56(sp)
	sd	s9,40(sp)
	sd	s1,104(sp)
	sd	s2,96(sp)
	sd	s8,48(sp)
	sd	s10,32(sp)
	sd	s11,24(sp)
	.cfi_offset 1, -8
	.cfi_offset 8, -16
	.cfi_offset 19, -40
	.cfi_offset 20, -48
	.cfi_offset 21, -56
	.cfi_offset 22, -64
	.cfi_offset 23, -72
	.cfi_offset 25, -88
	.cfi_offset 9, -24
	.cfi_offset 18, -32
	.cfi_offset 24, -80
	.cfi_offset 26, -96
	.cfi_offset 27, -104
	call	print_address
	li	a0,10
	call	print_address
	li	a0,65536
	addi	a0,a0,-8
	call	print_address
	li	s7,10
	li	s6,0
	la	s9,stdout
	addi	s0,sp,15
	li	s4,48
	li	s3,49
	lla	s5,.LC1
.L29:
	li	a0,10
	call	putchar@plt
	mv	a1,s6
	lla	a0,.LC0
	call	printf@plt
	ld	a0,0(s9)
	slli	s10,s6,48
	srli	s10,s10,48
	call	fflush@plt
	li	s11,4
	li	s2,32768
.L11:
	li	s1,4
.L16:
	and	a5,s10,s2
	li	a2,1
	mv	a1,s0
	li	a0,1
	beq	a5,zero,.L12
	addiw	s1,s1,-1
	sb	s3,15(sp)
	srli	s2,s2,1
	call	write@plt
	bne	s1,zero,.L16
	li	a2,1
	mv	a1,s5
	li	a0,1
	addiw	s11,s11,-1
	call	write@plt
	bne	s11,zero,.L11
.L15:
	mv	a1,s6
	lla	a0,.LC2
	call	printf@plt
	sext.w	s11,s7
	mv	a1,s11
	lla	a0,.LC0
	call	printf@plt
	ld	a0,0(s9)
	slli	s2,s7,48
	srli	s2,s2,48
	call	fflush@plt
	li	s10,4
	li	s8,32768
.L17:
	li	s1,4
.L22:
	and	a4,s2,s8
	li	a2,1
	mv	a1,s0
	li	a0,1
	beq	a4,zero,.L18
	addiw	s1,s1,-1
	sb	s3,15(sp)
	srli	s8,s8,1
	call	write@plt
	bne	s1,zero,.L22
	li	a2,1
	mv	a1,s5
	li	a0,1
	addiw	s10,s10,-1
	call	write@plt
	bne	s10,zero,.L17
.L21:
	mv	a1,s11
	lla	a0,.LC2
	call	printf@plt
	andi	s2,s2,-8
	mv	a1,s2
	lla	a0,.LC0
	call	printf@plt
	ld	a0,0(s9)
	mv	s11,s2
	slli	s2,s2,48
	srli	s2,s2,48
	call	fflush@plt
	li	s10,4
	li	s8,32768
.L23:
	li	s1,4
.L28:
	and	a4,s2,s8
	li	a2,1
	mv	a1,s0
	li	a0,1
	beq	a4,zero,.L24
	addiw	s1,s1,-1
	sb	s3,15(sp)
	srli	s8,s8,1
	call	write@plt
	bne	s1,zero,.L28
	li	a2,1
	mv	a1,s5
	li	a0,1
	addiw	s10,s10,-1
	call	write@plt
	bne	s10,zero,.L23
.L27:
	mv	a1,s11
	lla	a0,.LC2
	call	printf@plt
	addiw	s6,s6,1
	li	a5,8
	addiw	s7,s7,1
	bne	s6,a5,.L29
	ld	ra,120(sp)
	.cfi_remember_state
	.cfi_restore 1
	ld	s0,112(sp)
	.cfi_restore 8
	ld	s1,104(sp)
	.cfi_restore 9
	ld	s2,96(sp)
	.cfi_restore 18
	ld	s3,88(sp)
	.cfi_restore 19
	ld	s4,80(sp)
	.cfi_restore 20
	ld	s5,72(sp)
	.cfi_restore 21
	ld	s6,64(sp)
	.cfi_restore 22
	ld	s7,56(sp)
	.cfi_restore 23
	ld	s8,48(sp)
	.cfi_restore 24
	ld	s9,40(sp)
	.cfi_restore 25
	ld	s10,32(sp)
	.cfi_restore 26
	ld	s11,24(sp)
	.cfi_restore 27
	li	a0,0
	addi	sp,sp,128
	.cfi_def_cfa_offset 0
	jr	ra
.L24:
	.cfi_restore_state
	li	a2,1
	li	a0,1
	addiw	s1,s1,-1
	sb	s4,15(sp)
	srli	s8,s8,1
	call	write@plt
	bne	s1,zero,.L28
	li	a2,1
	mv	a1,s5
	li	a0,1
	addiw	s10,s10,-1
	call	write@plt
	bne	s10,zero,.L23
	j	.L27
.L18:
	li	a2,1
	li	a0,1
	addiw	s1,s1,-1
	sb	s4,15(sp)
	srli	s8,s8,1
	call	write@plt
	bne	s1,zero,.L22
	li	a2,1
	mv	a1,s5
	li	a0,1
	addiw	s10,s10,-1
	call	write@plt
	bne	s10,zero,.L17
	j	.L21
.L12:
	li	a2,1
	li	a0,1
	addiw	s1,s1,-1
	sb	s4,15(sp)
	srli	s2,s2,1
	call	write@plt
	bne	s1,zero,.L16
	li	a2,1
	mv	a1,s5
	li	a0,1
	addiw	s11,s11,-1
	call	write@plt
	bne	s11,zero,.L11
	j	.L15
	.cfi_endproc
.LFE12:
	.size	main, .-main
	.ident	"GCC: (Debian 13.1.0-9) 13.1.0"
	.section	.note.GNU-stack,"",@progbits
