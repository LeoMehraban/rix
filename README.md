# the rix programming language

rix (doesn't stand for anything) is a programming language I'm currently working on creating. There are probably a lot of bugs at the moment.

## installing

to use rix right now, you need to have installed [factor](https://factorcode.org/). Then, clone this repo into the work folder in your factor directory and load rix.factor. once you do that, you can run the repl word to open the rix repl

## so what is rix?

rix is a very simple programming language inspired by LISP and Factor

Here's some examples:

`prn "Hello, World!"`: can you guess what this program does?

`x: 5`: sets the variable x to five

`x`: gets the value of the variable x

`'x`: returns the symbol x

`foo: fn [x y] [+ x y]`: defines foo to be a function that takes the params x and y and returns their sums

`pdesc ;prn`: prints the description of the prn function. notice the semicolon syntax. that gets the value of the variable prn without evaluating it. if you just wrote `pdesc prn`, rix would complain that you aren't providing enough parameters to the prn function

try out getting the description of other functions you've seen here so far, to get a better feel for what they do
