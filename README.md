# the rix programming language

rix (doesn't stand for anything) is a programming language I'm currently working on creating. There are probably a lot of bugs at the moment.

## installing
to build rix, you need to have installed [factor](https://factorcode.org/). Then, clone this repo into the work folder in your factor directory and load rix.factor. then, run `"rix" deploy`, and wait for an executable to be generated

if you have a mac (maybe also a linux computer but I haven't checked), you can use the pre-built binary

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

## some more stuff

'generics' (not the typical generic types) are functions that call different sections of code depending on the type of the first parameter. you declare a new generic with the `genr` function.

'modules' are files of code that can be included with the `incl` macro. it searches your disk, first checking in the current directory for a file `modulename.rix`, and then in the `~/.rix` folder (if it exists) for the same file

you can override the directories it checks with the RIX-PATH enviroment variable. Only declarations prefixed with the `exp` macro (see test.rix for an example) will be exported.

you can access stuff declared in modules with the `symbol@modulename` syntax, or you can include the module with `imp` instead of `incl`, which will add everything to the global namespace so you can access it like any normal func

## builtin functions, macros, and inline functions

| name | signature      | description |
| --- | ------------------| ------------|
| pprn | fn [val] | prints anything, leaving strings in their literal representation and inserting a newline afterwards |
| at | fn [key hash] | gets the value at key stored within 'hash' |
| mac | mac [params body] | creates a function with params and a body that returns either a list or a single value that is pushed onto the token array |
| first | fn [list] | returns the first element of a list |
| consvarinl | fn [params body] | constructs a variadic inline function; see the description of 'consfn' for the difference between this and 'varinl' |
| typ? | fn [type val] | tests if a value is of a certain type |
| nxt | fn [] | evaluates tokens until a value is returned |
| ^^ | fn [base power] | raises 'base' to the power 'power' |
| rng | fn [from to] | returns a list with numbers that go from 'from' to 'to', including both 'from' and 'to' |
| wrt | fn [val] | prints anything |
| letfn | mac [params body] | creates a function with params and a body and calls that function immediatly |
| not | fn [x] | preforms 'not' on a bool |
| number.> | fn [x y] | compares two numbers |
| number.>= | fn [x y] | compares two numbers |
| lst>str | fn [list] | converts a list to a string |
| nth | fn [list n] | gets the nth element of a list |
| uquot | fn [val] | makes a value an unquote |
| pwrt | fn [val] | prints anything, leaving strings in their literal representation |
| < | genr '< [x y] | No description |
| prn* | genr 'prn* [val] | No description |
| desc | fn [desc value] | sets the description of a value |
| str>lst | fn [string] | converts a string to a list |
| pdesc | fn [value] | gets the description of a value and prints it |
| * | fn [x y] | multiplies two numbers |
| scop | fn [code hash] | evaluates 'code' within a new scope defined with the keys and values in 'hash' |
| err | fn [str] | throws an error with the given string as a message |
| num>str | fn [n] | turns a number into a base10 string |
| imps? | fn [gen val] | tests if the type of this value implements a generic |
| prnted | fn [list] | runs 'list', returning a string containing everything that was printed during its running |
| len | fn [list] | gets the length of a list |
| fbody | fn [func] | gets the body of a function. primatives return [prim] |
| <= | genr '<= [x y] | No description |
| esc | fn [char] | converts from a single character to an associated escaped character. for example, turns 'n' into a newline |
| >= | genr '>= [x y] | No description |
| consvarimac | fn [params body] | constructs a variadic macro; see the description of 'consfn' for the difference between this and 'varimac' |
| assert= | fn [one two] | throws an error if 'one' and 'two' aren't equal |
| try | fn [list catch] | evaluates the list. if there's an error, calls the function with the list and the error (in that order). errors automatically throw themselves when evaluated, so handle them carefully |
| number.< | fn [x y] | compares two numbers |
| varinl | mac [params body] | creates a function with params and a body whose results may depend on the surrounding enviroment. the last named param is a list of every value between the penultimate param and a ; token |
| / | fn [x y] | divides two numbers |
| glob | mac [dec] | takes a declaration and evaluates it within the global namespace |
| mod | fn [x y] | gets the modulus of two numbers |
| if | fn [cond body1 body2] | based on the boolean cond, picks either body1 or body2 and evaluates it |
| tapp | fn [val] | concatinates a list onto the tokenstack |
| quot | fn [val] | quotes a value |
| ifdo | fn [cond body] | if the boolean cond is true, evaluates the body |
| let | fn [hash code] | identical to scop, but with the parameters swapped |
| genr | inl [sym params] | creates a new generic with the given name and params (and assigns it to that name within the global scope) |
| prn | fn [val] | prints anything, inserting a newline afterwards |
| fparams | fn [func] | gets the params of a function |
| evl | fn [val] | evaluates 'val' |
| set | inl [name value] | defines a variable |
| rest | fn [list] | returns all of the list but the first element |
| vari | mac [params body] | creates a function with params and a body. the last named param is a list of every value between the penultimate param and a ; token |
| new-err | fn [str] | returns an error with the given string as a message |
| - | fn [x y] | subtracts two numbers |
| setat | fn [key value hash] | sets 'value' at 'key' within 'hash' |
| rdstr | fn [string code] | runs 'code' in an enviroment with the given string as an input stream, meaning that within 'code', (for example) every 'rdln' reads a line from the stream |
| comb | genr 'comb [x y] | No description |
| map | fn [list func] | applies 'func' to each element in 'list', returning a new list made up of all the return values |
| suquot | fn [val] | makes a value a splice-unquote |
| tpop | fn [] | pops from the tokenstack. should mainly be used with macros, as using with functions or inline functions may provide unexpected results |
| rdln | fn [] | reads a line from the input stream |
| or | fn [x y] | preforms 'or' on two bools |
| consvari | fn [params body] | constructs a variadic function; see the description of 'consfn' for the difference between this and 'vari' |
| exp | mac [dec] | takes a declaration and sets it for exporting. note that inline functions should not be exported |
| = | fn [x y] | checks for equality |
| str>num | fn [str] | parses a string as a number. returns fal if that's not possible |
| and | fn [x y] | preforms 'and' on two bools |
| inl | mac [params body] | creates a function with params and a body whose results may depend on the surrounding enviroment |
| tpush | fn [val] | pushes to the tokenstack |
| number.<= | fn [x y] | compares two numbers |
| incl | mac [name] | includes the module specified by 'name' |
| + | fn [x y] | adds two numbers |
| cstack | fn [] | returns the callstack |
| consfn | fn [params body] | constructs a function with params and a body. consfn is a function itself, meaning it evaluates its input parameters, so you can create a function with a runtime-computed definition |
| imp | mac [name] | imports the module specified by 'name', defining everything exported in the global namespace, so you don't have to use name@module syntax to access the things within the module |
| sqrt | fn [x] | gets the square root of a number |
| varimac | mac [params body] | creates a function with params and a body that returns either a list or a single value that is pushed onto the token array. the last named param is a list of every token between the penultimate param and a ; token |
| rdch | fn [] | reads a single character from the input stream |
| gdesc | fn [value] | gets the description of a value and returns it |
| typreq | fn [list func] | sets a list of required types for this function. 'any' means any type can be inputted, and 'callable' means that functions, inlines, macros, variadics etc. can all be used |
| consmac | fn [params body] | constructs a macro; see the description of 'consfn' for the difference between this and 'mac' |
| > | genr '> [x y] | No description |
| genexp | inl [sym params] | creates a new generic with the given name and params, exports it, and assigns it to that name within the global scope |
| typ | fn [val type] | sets the type of the value to the given string type. note that sometimes this will cause errors because the types are incompatible (such as if you set a number to type 'symbol') |
| fn | mac [params body] | creates a function with params and a body |
| consinl | fn [params body] | constructs an inline function; see the description of 'consfn' for the difference between this and 'inl' |