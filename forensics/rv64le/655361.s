	.file	"655361.c"
	.option pic
	.attribute arch, "rv64i2p1_m2p0_a2p1_f2p2_d2p2_c2p0_zicsr2p0_zifencei2p0"
	.attribute unaligned_access, 0
	.attribute stack_align, 16
	.text
	.align	1
	.globl	load_655361
	.type	load_655361, @function
load_655361:
.LFB0:
	.cfi_startproc
	li	a0,655360
	addi	a0,a0,1
	ret
	.cfi_endproc
.LFE0:
	.size	load_655361, .-load_655361
	.ident	"GCC: (Debian 13.1.0-9) 13.1.0"
	.section	.note.GNU-stack,"",@progbits
