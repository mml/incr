	.file	"scheme_entry.c"
	.option pic
	.attribute arch, "rv64i2p1_m2p0_a2p1_f2p2_d2p2_c2p0_zicsr2p0_zifencei2p0"
	.attribute unaligned_access, 0
	.attribute stack_align, 16
	.text
	.align	1
	.globl	scheme_entry
	.type	scheme_entry, @function
scheme_entry:
.LFB0:
	.cfi_startproc
	li	a0,42
	ret
	.cfi_endproc
.LFE0:
	.size	scheme_entry, .-scheme_entry
	.align	1
	.globl	foo
	.type	foo, @function
foo:
.LFB1:
	.cfi_startproc
	li	a0,22
	ret
	.cfi_endproc
.LFE1:
	.size	foo, .-foo
	.ident	"GCC: (Debian 13.1.0-9) 13.1.0"
	.section	.note.GNU-stack,"",@progbits
