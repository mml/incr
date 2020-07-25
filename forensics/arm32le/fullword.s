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
	.file	"fullword.c"
	.text

# load_0
	.align	2
	.global	load_0
	.arch armv8-a
	.syntax unified
	.arm
	.fpu vfp
	.type	load_0, %function
load_0:
	@ args = 0, pretend = 0, frame = 0
	@ frame_needed = 0, uses_anonymous_args = 0
	@ link register save eliminated.
	mov	r0, #0
	bx	lr
	.size	load_0, .-load_0

	.align	2
	.global	load_1
	.syntax unified
	.arm
	.fpu vfp
	.type	load_1, %function
load_1:
	@ args = 0, pretend = 0, frame = 0
	@ frame_needed = 0, uses_anonymous_args = 0
	@ link register save eliminated.
	mov	r0, #4
	bx	lr
	.size	load_1, .-load_1
	.align	2
	.global	load_42
	.syntax unified
	.arm
	.fpu vfp
	.type	load_42, %function
load_42:
	@ args = 0, pretend = 0, frame = 0
	@ frame_needed = 0, uses_anonymous_args = 0
	@ link register save eliminated.
	mov	r0, #168
	bx	lr
	.size	load_42, .-load_42
	.align	2
	.global	load_255
	.syntax unified
	.arm
	.fpu vfp
	.type	load_255, %function
load_255:
	@ args = 0, pretend = 0, frame = 0
	@ frame_needed = 0, uses_anonymous_args = 0
	@ link register save eliminated.
	mov	r0, #1020
	bx	lr
	.size	load_255, .-load_255
	.align	2
	.global	load_256
	.syntax unified
	.arm
	.fpu vfp
	.type	load_256, %function
load_256:
	@ args = 0, pretend = 0, frame = 0
	@ frame_needed = 0, uses_anonymous_args = 0
	@ link register save eliminated.
	mov	r0, #1024
	bx	lr
	.size	load_256, .-load_256
	.align	2
	.global	load_512
	.syntax unified
	.arm
	.fpu vfp
	.type	load_512, %function
load_512:
	@ args = 0, pretend = 0, frame = 0
	@ frame_needed = 0, uses_anonymous_args = 0
	@ link register save eliminated.
	mov	r0, #2048
	bx	lr
	.size	load_512, .-load_512
	.align	2
	.global	load_65536
	.syntax unified
	.arm
	.fpu vfp
	.type	load_65536, %function
load_65536:
	@ args = 0, pretend = 0, frame = 0
	@ frame_needed = 0, uses_anonymous_args = 0
	@ link register save eliminated.
	mov	r0, #262144
	bx	lr
	.size	load_65536, .-load_65536
	.align	2
	.global	load_268435456
	.syntax unified
	.arm
	.fpu vfp
	.type	load_268435456, %function
load_268435456:
	@ args = 0, pretend = 0, frame = 0
	@ frame_needed = 0, uses_anonymous_args = 0
	@ link register save eliminated.
	mov	r0, #1073741824
	bx	lr
	.size	load_268435456, .-load_268435456
	.align	2
	.global	load_1376256
	.syntax unified
	.arm
	.fpu vfp
	.type	load_1376256, %function
load_1376256:
	@ args = 0, pretend = 0, frame = 0
	@ frame_needed = 0, uses_anonymous_args = 0
	@ link register save eliminated.
	mov	r0, #5505024
	bx	lr
	.size	load_1376256, .-load_1376256
	.align	2
	.global	load_4177920
	.syntax unified
	.arm
	.fpu vfp
	.type	load_4177920, %function
load_4177920:
	@ args = 0, pretend = 0, frame = 0
	@ frame_needed = 0, uses_anonymous_args = 0
	@ link register save eliminated.
	mov	r0, #16711680
	bx	lr
	.size	load_4177920, .-load_4177920
	.align	2
	.global	load_8355840
	.syntax unified
	.arm
	.fpu vfp
	.type	load_8355840, %function
load_8355840:
	@ args = 0, pretend = 0, frame = 0
	@ frame_needed = 0, uses_anonymous_args = 0
	@ link register save eliminated.
	mov	r0, #0
	movt	r0, 510
	bx	lr
	.size	load_8355840, .-load_8355840
	.align	2
	.global	load_257
	.syntax unified
	.arm
	.fpu vfp
	.type	load_257, %function
load_257:
	@ args = 0, pretend = 0, frame = 0
	@ frame_needed = 0, uses_anonymous_args = 0
	@ link register save eliminated.
	movw	r0, #1028
	bx	lr
	.size	load_257, .-load_257
	.align	2
	.global	load_4095
	.syntax unified
	.arm
	.fpu vfp
	.type	load_4095, %function
load_4095:
	@ args = 0, pretend = 0, frame = 0
	@ frame_needed = 0, uses_anonymous_args = 0
	@ link register save eliminated.
	movw	r0, #16380
	bx	lr
	.size	load_4095, .-load_4095
	.align	2
	.global	load_65535
	.syntax unified
	.arm
	.fpu vfp
	.type	load_65535, %function
load_65535:
	@ args = 0, pretend = 0, frame = 0
	@ frame_needed = 0, uses_anonymous_args = 0
	@ link register save eliminated.
	movw	r0, #65532
	movt	r0, 3
	bx	lr
	.size	load_65535, .-load_65535
	.ident	"GCC: (Raspbian 8.3.0-6+rpi1) 8.3.0"
	.section	.note.GNU-stack,"",%progbits
