# Debugging Test Failures

Error messages from test failures typically refer to compiler internals, not the test case that failed. For example, a test like `(eq? 'a 'a)` might fail with:

```
compile-program: Unsupported primcall in "'(string->symbol ...)"
```

The error references `string->symbol`, which doesn't appear in the test. This happens because compiler passes transform the code before code generation. Quoted symbols like `'a` become `(string->symbol (string #\a))`.

**To debug:** grep for the error message text under `s/` to find where in the compiler the error originates:

```
grep -r "Unsupported primcall" s/
```

This will show you which pass is failing and what primitive or feature needs to be implemented.
