# the rix programming language

rix (doesn't stand for anything) is a programming language I'm currently working on creating. There are probably a lot of bugs at the moment.

## installing

to use rix right now, you need to have installed [factor](https://factorcode.org/). Then, clone this repo into the work folder in your factor directory and load rix.factor. once you do that, you can run the repl word to open the rix repl

## so what is rix?

rix is a very simple programming language inspired by LISP and Factor

Here's some examples:

`prn "Hello, World!"`: can you guess what this program does?

`x: 5;`: sets the variable x to five

`x`: gets the value of the variable x

`'x`: returns the symbol x

`foo: fn [x y] [+ x y];`: defines foo to be a function that takes the params x and y and returns their sums

`pdesc $prn`: prints the description of the prn function. notice the dollarsign syntax. that gets the value of the variable prn without evaluating it. if you just wrote `pdesc prn`, rix would complain that you aren't providing enough parameters to the prn function

all functions included by default are listed below. if you don't understand some things, it's helpful to look through test.rix and tests.rix (particularly tests.rix)

## builtin functions, macros, and inline functions

| name | signature      | description |
| --- | ------------------| ------------|
| + | fn [x y] | adds two numbers |
| incl | mac [name] | includes the module specified by 'name' |
| number.<= | fn [x y] | compares two numbers |
| \|=\| | fn [x y] | checks for loose equality (when two rix values have the same internal value) |
| len | fn [list] | gets the length of a list |
| typ? | fn [type val] | tests if a value is of a certain type |
| prn | fn [val] | prints anything, inserting a newline afterwards |
| pprn | fn [val] | prints anything, leaving strings in their literal representation and inserting a newline afterwards |
| at | fn [key hash] | gets the value at key stored within 'hash' |
| tpop | fn [] | pops from the tokenstack. should mainly be used with macros, as using with functions or inline functions may provide unexpected results |
| mac | mac [params body] | creates a function with params and a body that returns either a list or a single value that is pushed onto the token array |
| genr | fn [sym params] | creates a new generic with the given name and params (and assigns it to that name within the global scope) |
| suquot | fn [val] | makes a value a splice-unquote |
| or | fn [x y] | preforms 'or' on two bools |
| if | fn [cond body1 body2] | based on the boolean cond, picks either body1 or body2 and evaluates it |
| assert= | fn [one two] | throws an error if 'one' and 'two' aren't equal |
| typ | fn [val type] | sets the type of the value to the given string type. note that sometimes this will cause errors because the types are incompatible (such as if you set a number to type 'symbol') |
| desc | fn [desc value] | sets the description of a value |
| nth | fn [list n] | gets the nth element of a list |
| > | genr '> [x y] | No description |
| tapp | fn [val] | concatinates a list onto the tokenstack |
| uquot | fn [val] | makes a value an unquote |
| varimac | mac [params body] | creates a function with params and a body that returns either a list or a single value that is pushed onto the token array. the last named param is a list of every token between the penultimate param and a ; token |
| str>lst | fn [string] | converts a string to a list |
| - | fn [x y] | subtracts two numbers |
| gdesc | fn [value] | gets the description of a value and returns it |
| comb | genr 'comb [x y] | No description |
| number.< | fn [x y] | compares two numbers |
| varinl | mac [params body] | creates a function with params and a body whose results may depend on the surrounding enviroment. the last named param is a list of every value between the penultimate param and a ; token |
| set | inl [name value] | defines a variable |
| <= | genr '<= [x y] | No description |
| exp | mac [dec] | takes a declaration and sets it for exporting. note that inline functions should not be exported |
| nxt | fn [] | evaluates tokens until a value is returned |
| decons | fn [list] | returns a list where the first element is the first element of the input list, and the last element is the remainder of the list |
| >= | genr '>= [x y] | No description |
| and | fn [x y] | preforms 'and' on two bools |
| imps? | fn [gen val] | tests if the type of this value implements a generic |
| fn | mac [params body] | creates a function with params and a body |
| ifdo | fn [cond body] | if the boolean cond is true, evaluates the body |
| quot | fn [val] | quotes a value |
| < | genr '< [x y] | No description |
| prn* | genr 'prn* [val] | No description |
| * | fn [x y] | multiplies two numbers |
| scop | fn [code hash] | evaluates 'code' within a new scope defined with the keys and values in 'hash' |
| map | fn [list func] | applies 'func' to each element in 'list', returning a new list made up of all the return values |
| glob | mac [dec] | takes a declaration and evaluates it within the global namespace |
| pdesc | fn [value] | gets the description of a value and prints it |
| number.> | fn [x y] | compares two numbers |
| letfn | mac [params body] | creates a function with params and a body and calls that function immediatly |
| not | fn [x] | preforms 'not' on a bool |
| pwrt | fn [val] | prints anything, leaving strings in their literal representation |
| try | fn [list catch] | evaluates the list. if there's an error, calls the function with the list and the error (in that order). errors automatically throw themselves when evaluated, so handle them carefully |
| err | fn [str] | throws an error with the given string as a message |
| number.>= | fn [x y] | compares two numbers |
| lst>str | fn [list] | converts a list to a string |
| vari | mac [params body] | creates a function with params and a body. the last named param is a list of every value between the penultimate param and a ; token |
| new-err | fn [str] | returns an error with the given string as a message |
| / | fn [x y] | divides two numbers |
| prnted | fn [list] | runs 'list', returning a string containing everything that was printed during its running |
| wrt | fn [val] | prints anything |
| setat | fn [key value hash] | sets 'value' at 'key' within 'hash' |
| tpush | fn [val] | pushes to the tokenstack |
| = | fn [x y] | checks for equality |
| inl | mac [params body] | creates a function with params and a body whose results may depend on the surrounding enviroment |

## some more stuff

'generics' (not the typical generic types) are functions that call different sections of code depending on the type of the first parameter. you declare a new generic with the `genr` function.

'modules' are files of code that can be included with the `incl` macro. it searches your disk, first checking in the current directory for a file `modulename.rix`, and then in the `~/.rix` folder (if it exists) for the same file

you can override the directories it checks with the RIX-PATH enviroment variable. Only declarations prefixed with the `exp` macro (see test.rix for an example) will be exported.

you can access stuff declared in modules with the `symbol@modulename` syntax. of course you can also assign the value of `symbol@modulename` to a variable as well, so you can access it without all that extra syntax