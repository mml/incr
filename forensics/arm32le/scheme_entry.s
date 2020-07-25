	.arch armv8-a
	.syntax unified
	.arm
	.fpu vfp
	# .eabi_attribute 28, 1
	# .eabi_attribute 20, 1
	# .eabi_attribute 21, 1
	# .eabi_attribute 23, 3
	# .eabi_attribute 24, 1
	# .eabi_attribute 25, 1
	# .eabi_attribute 26, 2
	# .eabi_attribute 30, 2
	# .eabi_attribute 34, 1
	# .eabi_attribute 18, 4
	#.file	"scheme_entry.c"
	.text

# scheme_entry{{{
	.align	2
	.global	scheme_entry
#	.type	scheme_entry, %function
scheme_entry:
	mov	r0, #168
	bx	lr
	.size	scheme_entry, .-scheme_entry
# }}}

	.ident	"GCC: (Raspbian 8.3.0-6+rpi1) 8.3.0"
	.section	.note.GNU-stack,"",%progbits
