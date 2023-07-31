	.file	"sub1.c"
	.option pic
	.attribute arch, "rv64i2p1_m2p0_a2p1_f2p2_d2p2_c2p0_zicsr2p0_zifencei2p0"
	.attribute unaligned_access, 0
	.attribute stack_align, 16
	.text
	.align	1
	.globl	sub1
	.type	sub1, @function
sub1:
.LFB0:
	.cfi_startproc
	addiw	a0,a0,-4
	ret
	.cfi_endproc
.LFE0:
	.size	sub1, .-sub1
	.ident	"GCC: (Debian 13.1.0-9) 13.1.0"
	.section	.note.GNU-stack,"",@progbits
