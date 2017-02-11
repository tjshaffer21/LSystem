A simple implementation of the Lindenmayer system algorithm.

An L-System is defined as G=(V,w (omega), P). Where:

V is a set of symbols that are either variables or terminals.
w (omega) is the axiom or the initial state of the L-System.
P is the set of rules that define how variables are replaced.

main.lisp is the main exectution point for the LSystem implementation. It can be
used in terminal by loading it into a list interpreter. It accepts two
commandline arguments: -rules and -translate both of which are text files
containing information to run the program.

Example:

ccl -l main.lisp -- -rules=rules.txt -translate=translate.txt

Dependecies:

* iterate
* cl-utilities
* cl-opengl
* cl-glfw3
* asdf and quickload

rules.txt:

[Rules (one per line)] (ex. F=G)

[Iteration Number]
[Axiom]

Examples:

Algae
-----

A=AB
B=A

7
A

translate.txt:

[size] (ex. 1000,1000)
[rules (one per line) (ex. F:80:white)



TODO
======
+ Error handling
+ Zoom in and out
+ Handle multiple rules and translations
+ Modify rules and translations during run time.