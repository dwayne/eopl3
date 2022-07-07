**Q:** *What is the purpose of the call to `proc-val` on the next-to-last line
of `apply-env`?*

**A:**

It makes `apply-env` return the correct type of value.

Recall:

```
apply-env : Env x Var -> DenVal

and

DenVal = ExpVal

and

proc-val : Proc -> ExpVal
```
