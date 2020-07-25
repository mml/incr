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
	.file	"shift.c"
	.text
	.align	2
	.global	shift_left_six
	.arch armv8-a
	.syntax unified
	.arm
	.fpu vfp
	.type	shift_left_six, %function
shift_left_six:
	@ args = 0, pretend = 0, frame = 0
	@ frame_needed = 0, uses_anonymous_args = 0
	@ link register save eliminated.
	lsl	r0, r0, #6
	orr	r0, r0, #15
	bx	lr
	.size	shift_left_six, .-shift_left_six
	.align	2
	.global	shift_right_six
	.syntax unified
	.arm
	.fpu vfp
	.type	shift_right_six, %function
shift_right_six:
	@ args = 0, pretend = 0, frame = 0
	@ frame_needed = 0, uses_anonymous_args = 0
	@ link register save eliminated.
	asr	r0, r0, #6
	bx	lr
	.size	shift_right_six, .-shift_right_six
	.ident	"GCC: (Raspbian 8.3.0-6+rpi1) 8.3.0"
	.section	.note.GNU-stack,"",%progbits
