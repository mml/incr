To reduce the amount of platform-specific code, we could implement a few
primitive memory operations in a pass right before the final code
generation.  That is, right before emit-Labels.

Take, for example, `vector?`.  `pair?` and `string?` would be similar.
All they do is check the tag.  So why not convert them all to
`(check-tag foo (vector-tag))` or whatever.

A similar repeated operation is seen in vector-length or vector-ref.
Null out the tag, then dereference the pointer plus some offset.  So we
should add `remove-tag` and `dereference` primitives.

`null?` and `zero?` are just comparisons.

This is really inserting a final IR that is a kind of portable assembly
language, I think.  An open question is what IR is compatible with
emitting decent assembly language on both platforms.  The current
assembly is not in any way optimal, but I don't want to actively make it
worse with this change.
