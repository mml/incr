	.arch armv6
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
	.file	"scheme_entry.c"
	.text
	.align	2
	.global	scheme_entry
	.arch armv6
	.syntax unified
	.arm
	.fpu vfp
	.type	scheme_entry, %function
scheme_entry:
	@ args = 0, pretend = 0, frame = 0
	@ frame_needed = 0, uses_anonymous_args = 0
	@ link register save eliminated.
	mov	r0, #42
	bx	lr
	.size	scheme_entry, .-scheme_entry
	.align	2
	.global	foo
	.syntax unified
	.arm
	.fpu vfp
	.type	foo, %function
foo:
	@ args = 0, pretend = 0, frame = 0
	@ frame_needed = 0, uses_anonymous_args = 0
	@ link register save eliminated.
	mov	r0, #22
	bx	lr
	.size	foo, .-foo
	.ident	"GCC: (Raspbian 8.3.0-6+rpi1) 8.3.0"
	.section	.note.GNU-stack,"",%progbits
