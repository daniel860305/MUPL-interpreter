# MUPL-interpreter
This is a project from the course "Programming Languages, Part B".

MUPL-interpreter represents a made-up-programming-language inside DrRacket.

# Implementation
Structure types in Racket are used to define different MUPL expressions.

The eval-under-env is the main function that handles different MUPL expressions along with the evaluation environment.

Some MUPL macros (**ifaunit, mlet', ifeq**) are also defined to expand the MUPL language.

A built-in MUPL function **mupl-map** is defined to enhance the richness of the MUPL language.

# Unit test
"interpreter-test.rkt" is a file that does the unit testing work on "interpreter.rkt".

Racket provides a convenient unit testing API to simplify the testing.

In the file you can also have a glimpse of how the MUPL language is used in the test cases.