	.file	"shift.c"
	.option pic
	.attribute arch, "rv64i2p1_m2p0_a2p1_f2p2_d2p2_c2p0_zicsr2p0_zifencei2p0"
	.attribute unaligned_access, 0
	.attribute stack_align, 16
	.text
	.align	1
	.globl	shift_left_six
	.type	shift_left_six, @function
shift_left_six:
.LFB0:
	.cfi_startproc
	slliw	a0,a0,6
	ori	a0,a0,15
	sext.w	a0,a0
	ret
	.cfi_endproc
.LFE0:
	.size	shift_left_six, .-shift_left_six
	.align	1
	.globl	shift_right_six
	.type	shift_right_six, @function
shift_right_six:
.LFB1:
	.cfi_startproc
	srai	a0,a0,6
	ret
	.cfi_endproc
.LFE1:
	.size	shift_right_six, .-shift_right_six
	.ident	"GCC: (Debian 13.1.0-9) 13.1.0"
	.section	.note.GNU-stack,"",@progbits
