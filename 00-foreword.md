# Foreword

The most fundamental idea in computer programming: *The interpreter for a
computer language is just another program.*

- [Kurt Gödel](https://en.wikipedia.org/wiki/Kurt_G%C3%B6del)
- [Alan Turing](https://en.wikipedia.org/wiki/Alan_Turing)
- [John von Neumann](https://en.wikipedia.org/wiki/John_von_Neumann)

Mastering the idea of an interpreter is a source of great power. It provokes a
change in the way you think about programming.

If you don't understand interpreters, you can still write programs; you can
even be a competent programmer. But you can't be a master.

Three reasons why you should learn about interpreters:

1. You will need at some point to implement interpreters. The interpreter is
the glue that lets you combine individual operations into useful patterns. It
is often the quality of the glue that most directly determines the power of the
system. It's easy to find examples of programs with good individual operations,
but lousy glue.

2. Even programs that are not themselves interpreters have important
interpreter-like pieces. One of the most powerful ways to structure a complex
program is as a collection of languages. Choosing the right kind of language
for the right purpose, and understanding the implementation tradeoffs involved:
that's what the study of interpreters is about.

3. Programming techniques that explicitly involve the structure of language are
becoming increasingly important. Our programs are becoming increasingly
complex&mdash;thinking more explicitly about languages may be our best tool for
dealing with this complexity.

Friedman and Wand don't just tell you about interpreters; they show them to
you. The core of the book is a tour de force sequence of interpreters starting
with an abstract high-level language and progressively making linguistic
features explicit until we reach a state machine.

The authors show how the same ideas can be used to analyze programs without
running them. They show how to implement type checkers and inferencers, and how
these features interact in modern object-oriented languages.

This is not an easy book. Language design is not easy. The language designer is
a further level removed from the end user than is the ordinary application
programmer. In designing a language, you consider the various applications
people might want to implement, and the ways in which they might implement them.
Which kinds of programs should be easy to write, which you can afford to make
more difficult.

Interpreters are subtle programs. Don't think you can just skim these
programs&mdash;very few people in the world can glance at a new interpreter and
predict from that how it will behave even on relatively simple programs. Study
these programs. Run them. Try to really master these programs, not just get a
vague feeling for how they work.

Creating new frameworks requires skills of the master: understanding the
principles that run across languages, appreciating which language features are
best suited for which type of application, and knowing how to craft the
interpreters that bring these languages to life. These are the skills you will
learn from this book.
