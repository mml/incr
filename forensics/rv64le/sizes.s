	.file	"sizes.c"
	.option pic
	.attribute arch, "rv64i2p1_m2p0_a2p1_f2p2_d2p2_c2p0_zicsr2p0_zifencei2p0"
	.attribute unaligned_access, 0
	.attribute stack_align, 16
	.text
	.section	.rodata.str1.8,"aMS",@progbits,1
	.align	3
.LC0:
	.string	"sizeof(short) = %3d\n"
	.align	3
.LC1:
	.string	"sizeof(int) = %3d\n"
	.align	3
.LC2:
	.string	"sizeof(long) = %3d\n"
	.align	3
.LC3:
	.string	"sizeof(long long) = %3d\n"
	.align	3
.LC4:
	.string	"sizeof(float) = %3d\n"
	.align	3
.LC5:
	.string	"sizeof(double) = %3d\n"
	.section	.text.startup,"ax",@progbits
	.align	1
	.globl	main
	.type	main, @function
main:
.LFB11:
	.cfi_startproc
	addi	sp,sp,-16
	.cfi_def_cfa_offset 16
	li	a1,2
	lla	a0,.LC0
	sd	ra,8(sp)
	.cfi_offset 1, -8
	call	printf@plt
	li	a1,4
	lla	a0,.LC1
	call	printf@plt
	li	a1,8
	lla	a0,.LC2
	call	printf@plt
	li	a1,8
	lla	a0,.LC3
	call	printf@plt
	li	a1,4
	lla	a0,.LC4
	call	printf@plt
	li	a1,8
	lla	a0,.LC5
	call	printf@plt
	ld	ra,8(sp)
	.cfi_restore 1
	li	a0,0
	addi	sp,sp,16
	.cfi_def_cfa_offset 0
	jr	ra
	.cfi_endproc
.LFE11:
	.size	main, .-main
	.ident	"GCC: (Debian 13.1.0-9) 13.1.0"
	.section	.note.GNU-stack,"",@progbits
