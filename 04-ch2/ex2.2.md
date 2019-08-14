In general, all the representations produce the specified behavior. They only
start to breakdown for large naturals as they will all run out of memory sooner
or later.

**Unary representation:**

Because of the naive way it represents the naturals it will quickly run out of
memory for large naturals and so fail to represent such numbers. For e.g. to
represent 1,000,000 it requires a list of length 1,000,000.

**Scheme number representation:**

Since Scheme numbers use arbitrary-precision arithmetic and the internal
representation is optimized for speed and memory usage it follows that many
more naturals would be represented by this representation before the system
runs out of memory.

**Bignum representation:**

As the base changes the amount the memory required to represent the naturals
changes as well. For e.g. if base = 2 then a list of length 20 is needed to
represent 1,000,000. And if base = 64 then only a list of length 4 is needed.
