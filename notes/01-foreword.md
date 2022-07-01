# Foreword

The most fundamental idea in computer programming:

> The interpreter for a computer language is just another program.

- [Alan Turing](https://en.wikipedia.org/wiki/Alan_Turing)
- [John von Neumann](https://en.wikipedia.org/wiki/John_von_Neumann)
- [Kurt Gödel](https://en.wikipedia.org/wiki/Kurt_G%C3%B6del)

My suggested reading:

- [Logicomix](https://www.logicomix.com/en/index.html)
- [Turing's Vision](https://mitpress.mit.edu/books/turings-vision)

Mastering the idea of an interpreter is a source of great power. It provokes a
change in the way you think about programming.

> If you don't understand interpreters, you can still write programs; you can
even be a competent programmer. But you can't be a master.

Three reasons why you should learn about interpreters:

1. At some point you may need to implement interpreters, perhaps not
interpreters for full-blown general-purpose languages.

  No matter how complex and polished the individual operations are, it is often
  the quality of the glue that most directly determines the power of the system.

2. Even programs that are not themselves interpreters have important
interpreter-like pieces. One of the most powerful ways to structure a complex
program is as a collection of languages.

  Choosing the right kind of language for the right purpose, and understanding
  the implementation tradeoffs involved: that's what the study of interpreters
  is about.

3. Programming techniques that explicitly involve the structure of language are
becoming increasingly important.

  Thinking more explicitly about languages may be our best tool for dealing
  with this complexity.

The core of the book is a tour de force sequence of interpreters starting
with an abstract high-level language and progressively making linguistic
features explicit until we reach a state machine.

They show how the same ideas can be used to analyze programs without
running them.

They show how to implement type checkers and inferencers, and how these
features interact in modern object-oriented languages.

Mastery of interpreters does not come easily.

- The language designer is a further level removed from the end user than is
the ordinary application programmer.

  In designing a language, you consider the various applications people might
  want to implement, and the ways in which they might implement them.

  It all depends on how you expect your language to be used, which kinds of
  programs should be easy to write, and which you can afford to make more
  difficult.

- Interpreters are subtle programs.

  A simple change to a line of code in an interpreter can make an enormous
  difference in the behaviour of the resulting language.

Skills you will learn from the book:

- The skill to understand the principles that run across languages.
- The skill to appreciate which language features are best suited for which
type of application.
- The skill to craft the interpreters that bring these languages to life.
