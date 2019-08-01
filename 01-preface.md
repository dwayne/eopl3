# Preface

The book is an analytic study of programming languages. Its goal is to provide
a deep, working understanding of the essential concepts of programming
languages.

The essentials relate to the semantics of program elements. Programs called
interpreters provide the most direct, executable expression of program
semantics.

The most interesting question about a program as object is, "What does it do?"

Interpreters are critical because:

1. They reveal nuances of meaning.
2. They are the direct path to more efficient compilation and to other kinds of
program analyses.

Compilers transform programs into forms suitable for interpretation by
hardware or virtual machines. Forms of program analysis typical of compilation
include control transformation, variable binding resolution and type checking.

Simple algebraic manipulation can be used to predict the behavior of programs
and to derive their properties.

## Organization

**Chapter 1** - emphasizes the connection between inductive data specification
and recursive programming and introduces several notions related to the scope
of variables.

**Chapter 2** - introduces a data type facility and discusses data abstraction
and examples of representational transformations.

**Chapter 3** - describes the behavior of programming languages. It develops an
interpreter for a simple, lexically scoped language with first-class procedures
and recursion. It ends by giving a thorough treatment of a language that uses
indices in place of variables.

**Chapter 4** - introduces state. And they explore call-by-reference,
call-by-name and call-by-need parameter passing mechanisms.

**Chapter 5** - rewrites the basic interpreter in continuation-passing style.
The control structure that is needed to run the interpreter thereby shifts from
recursion to iteration. This exposes the control mechanism of the interpreted
language, and strengthens one's intuition for control issues in general. It
also allows us to extend the language with trampolining, exception-handling and
multithreading mechanisms.

**Chapter 6** - is a companion to the previous chapter. We see that
continuation-passing style is a powerful programming tool, for it allows any
sequential control mechanism to be implemented in almost any language.

**Chapter 7** - turns the language of Chapter 3 into a typed language. We
implement a type checker. We show how the types in a program can be deduced by
a unification-based type inference algorithm.

**Chapter 8** - builds typed modules. Modules allow us to build and enforce
abstraction boundaries, and they offer a new kind of scoping.

**Chapter 9** - presents the basic concepts of object-oriented languages,
centered on classes. It introduces new concepts including interfaces, abstract
methods and casting.
