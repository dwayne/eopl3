# Preface

The most interesting question about a program as object is,

> What does it do?

Interpreters are critical because:

1. They reveal nuances of meaning.
2. They are the direct path to more efficient compilation and to other kinds of
program analyses.

Compilers transform programs into forms suitable for interpretation by
hardware or virtual machines.

Simple algebraic manipulation can be used to predict the behavior of programs
and to derive their properties.

**Question:** *What does the following mean?*

> ... we have opted for a first-order compositional continuation-passing-style
> transformation rather than a relational one.

## Organization

### Chapter 1

- emphasizes the connection between inductive data specification and recursive
programming
- introduces several notions related to the scope of variables

### Chapter 2

- introduces a data type facility
- discusses data abstraction
- provides examples of representational transformations

### Chapter 3

- describes the behavior of programming languages
- develops an interpreter for a simple, lexically scoped language with
first-class procedures and recursion
- ends by giving a thorough treatment of a language that uses indices in place
of variables

### Chapter 4

- introduces state
- explores call-by-reference, call-by-name and call-by-need parameter passing
mechanisms

### Chapter 5

- rewrites the basic interpreter in continuation-passing style
- extends the language with trampolining, exception-handling and multithreading
mechanisms

### Chapter 6

- is a companion to the previous chapter
- shows that continuation-passing style is a powerful programming tool, for it
allows any sequential control mechanism to be implemented in almost any
language

### Chapter 7

- turns the language of Chapter 3 into a typed language
- implements a type checker
- shows how the types in a program can be deduced by a unification-based type
inference algorithm

### Chapter 8

- builds typed modules
- shows how modules allow us to build and enforce abstraction boundaries
- shows how modules offer a new kind of scoping

### Chapter 9

- presents the basic concepts of object-oriented languages, centered on classes
- introduces new concepts including interfaces, abstract methods and casting

### Further Reading

- explains where each of the ideas in the book came from

### Appendix B

- describes the SLLGEN parsing system

## Dependencies

```
                             --> 5 -----> 6
                            /
1 ----> 2 ----> 3 ----> 4 --          --> 8
                            \        /
                             --> 7 --
                                     \
                                      --> 9
```

## Prerequisites

- Data structures
- Experience with a programming language
  - C, C++, Java, Python
  - Scheme, ML, Haskell

Any language that supports both first-class procedures and assignment is
adequate for working the exercises.

The abstraction facilities of functional programming languages are especially
suited to this sort of programming.
