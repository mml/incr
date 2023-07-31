	.file	"fullword.c"
	.option pic
	.attribute arch, "rv64i2p1_m2p0_a2p1_f2p2_d2p2_c2p0_zicsr2p0_zifencei2p0"
	.attribute unaligned_access, 0
	.attribute stack_align, 16
	.text
	.align	1
	.globl	load_0
	.type	load_0, @function
load_0:
.LFB0:
	.cfi_startproc
	li	a0,0
	ret
	.cfi_endproc
.LFE0:
	.size	load_0, .-load_0
	.align	1
	.globl	load_1
	.type	load_1, @function
load_1:
.LFB1:
	.cfi_startproc
	li	a0,4
	ret
	.cfi_endproc
.LFE1:
	.size	load_1, .-load_1
	.align	1
	.globl	load_42
	.type	load_42, @function
load_42:
.LFB2:
	.cfi_startproc
	li	a0,168
	ret
	.cfi_endproc
.LFE2:
	.size	load_42, .-load_42
	.align	1
	.globl	load_255
	.type	load_255, @function
load_255:
.LFB3:
	.cfi_startproc
	li	a0,1020
	ret
	.cfi_endproc
.LFE3:
	.size	load_255, .-load_255
	.align	1
	.globl	load_256
	.type	load_256, @function
load_256:
.LFB4:
	.cfi_startproc
	li	a0,1024
	ret
	.cfi_endproc
.LFE4:
	.size	load_256, .-load_256
	.align	1
	.globl	load_512
	.type	load_512, @function
load_512:
.LFB5:
	.cfi_startproc
	li	a0,4096
	addi	a0,a0,-2048
	ret
	.cfi_endproc
.LFE5:
	.size	load_512, .-load_512
	.align	1
	.globl	load_65536
	.type	load_65536, @function
load_65536:
.LFB6:
	.cfi_startproc
	li	a0,262144
	ret
	.cfi_endproc
.LFE6:
	.size	load_65536, .-load_65536
	.align	1
	.globl	load_268435456
	.type	load_268435456, @function
load_268435456:
.LFB7:
	.cfi_startproc
	li	a0,1073741824
	ret
	.cfi_endproc
.LFE7:
	.size	load_268435456, .-load_268435456
	.align	1
	.globl	load_1376256
	.type	load_1376256, @function
load_1376256:
.LFB8:
	.cfi_startproc
	li	a0,5505024
	ret
	.cfi_endproc
.LFE8:
	.size	load_1376256, .-load_1376256
	.align	1
	.globl	load_4177920
	.type	load_4177920, @function
load_4177920:
.LFB9:
	.cfi_startproc
	li	a0,16711680
	ret
	.cfi_endproc
.LFE9:
	.size	load_4177920, .-load_4177920
	.align	1
	.globl	load_8355840
	.type	load_8355840, @function
load_8355840:
.LFB10:
	.cfi_startproc
	li	a0,33423360
	ret
	.cfi_endproc
.LFE10:
	.size	load_8355840, .-load_8355840
	.align	1
	.globl	load_257
	.type	load_257, @function
load_257:
.LFB11:
	.cfi_startproc
	li	a0,1028
	ret
	.cfi_endproc
.LFE11:
	.size	load_257, .-load_257
	.align	1
	.globl	load_4095
	.type	load_4095, @function
load_4095:
.LFB12:
	.cfi_startproc
	li	a0,16384
	addi	a0,a0,-4
	ret
	.cfi_endproc
.LFE12:
	.size	load_4095, .-load_4095
	.align	1
	.globl	load_65535
	.type	load_65535, @function
load_65535:
.LFB13:
	.cfi_startproc
	li	a0,262144
	addi	a0,a0,-4
	ret
	.cfi_endproc
.LFE13:
	.size	load_65535, .-load_65535
	.ident	"GCC: (Debian 13.1.0-9) 13.1.0"
	.section	.note.GNU-stack,"",@progbits
