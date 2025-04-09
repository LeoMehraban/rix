# the rix programming language

rix (doesn't stand for anything) is a programming language I'm currently working on creating. There are probably a lot of bugs at the moment.

## installing
to build rix, you need to have installed [factor](https://factorcode.org/). Then, clone this repo into the work folder in your factor directory and load rix.factor. then, run `"rix" deploy`, and wait for an executable to be generated

if you have a mac (maybe also a linux computer but I haven't checked), you can use the pre-built binary

if you want to have the (very basic) standard library installed, then run ./install.sh. Maybe this works on Windows, but honestly I don't know

## so what is rix?

rix is a very simple programming language inspired by LISP and Factor

Here's some examples:

`prn "Hello, World!"`: can you guess what this program does?

`x: 5;`: sets the variable x to five

`x`: gets the value of the variable x

`'x`: returns the symbol x

`foo: fn [x y] [+ x y];`: defines foo to be a function that takes the params x and y and returns their sums

`pdesc $prn`: prints the description of the prn function. notice the dollarsign syntax. that gets the value of the variable prn without evaluating it.

if you just wrote `pdesc prn`, rix would complain that you aren't providing enough parameters to the prn function

all functions included by default are listed below. if you don't understand some things, it's helpful to look through tests.rix and std/\*.rix

## some more stuff

'generics' (not the typical generic types) are functions that call different sections of code depending on the type of the first parameter. you declare a new generic with the `genr` function.

'modules' are files of code that can be included with the `incl` macro. it searches your disk, first checking in the current directory for a file `modulename.rix`, and then in the `~/.rix` folder (if it exists) for the same file

you can override the directories it checks with the RIX-PATH enviroment variable. Only declarations prefixed with the `exp` macro (see test.rix for an example) will be exported.

modules can also be organized into folders on the file system.

'.' works like a normal file system '/', and '|' works like a normal file system '../'. For example, '|foo.bar' will search for dir/../foo/bar.rix. The module name would be bar

everything exported into a module is stored in the global env, but if this causes ambiguity and conflicting names, you can access stuff declared in modules with the `symbol@modulename` syntax

## builtin functions, macros, and inline functions

| name | signature      | description |
| --- | ------------------| ------------|
| glob | mac [dec] | takes a declaration and evaluates it within the global namespace |
| rest | fn [list] | returns all of the list but the first element |
| set | inl [name value] | defines a variable |
| nop | nop | No description |
| < | genr '< [x y] | No description |
| tdelc | fn [cont] | pops the next token in the continuation 'cont', returning the continuation |
| map | fn [list func] | applies 'func' to each element in 'list', returning a new list made up of all the return values |
| consinl | fn [params body] | constructs an inline function; see the description of 'consfn' for the difference between this and 'inl' |
| underlying | fn [val] | gets the underlying value of a builtin rix type as a hashtable |
| quot | fn [val] | quotes a value |
| comb | genr 'comb [x y] | No description |
| tpopc | fn [cont] | pops the next token in the continuation 'cont'. this is pretty much the only rix function with a side effect, as it modifies 'cont' without returning it |
| rng | fn [from to] | returns a list with numbers that go from 'from' to 'to', including both 'from' and 'to' |
| up | fn [cont sym] | returns the result of retriving 'sym' in the parent enviroment of the one within cont |
| and | fn [x y] | preforms 'and' on two bools |
| nxt | fn [] | evaluates tokens until a value is returned |
| exp | mac [dec] | takes a declaration and sets it for exporting. note that inline functions should not be exported |
| <= | genr '<= [x y] | No description |
| ostream | fn [stream code] | runs 'code' with the output stream remapped to 'stream' |
| if | fn [cond body1 body2] | based on the boolean cond, picks either body1 or body2 and evaluates it |
| rdln | fn [] | reads a line from the input stream |
| or | fn [x y] | preforms 'or' on two bools |
| runbef | fn [cont code] | sets the continuation to run some code before the code it would otherwise run |
| genr | inl [sym params] | creates a new generic with the given name and params (and assigns it to that name within the global scope) |
| rdch | fn [] | reads a single character from the input stream |
| number>string | fn [n] | turns a number into a base10 string |
| fapp | fn [path encoding] | returns a file-appending output stream. supported values of 'encoding' are "ascii", "utf8" and "utf16" |
| pprn | fn [val] | prints anything, leaving strings in their literal representation and inserting a newline afterwards |
| wrtstr | fn [string] | prints a string |
| istream | fn [stream code] | runs 'code' with the input stream remapped to 'stream' |
| len | fn [list] | gets the length of a list |
| string>list | fn [string] | converts a string to a list |
| > | genr '> [x y] | No description |
| + | fn [x y] | adds two numbers |
| incl | mac [name] | includes the module specified by 'name'. if you import two modules with conflicting names, you can be specific by using 'name@modulename' syntax |
| typreq | fn [list func] | sets a list of required types for this function. 'typ any' means any type can be inputted, and 'typ callable' means that functions, inlines, macros, variadics etc. can all be used |
| tpushc | fn [cont val] | pushes a value to the continuation 'cont' |
| unlock | fn [locked] | 'unlocks' a locked value, returning it |
| gdesc | fn [value] | gets the description of a value and returns it |
| cstack | fn [] | returns the callstack |
| getenv | fn [] | gets the current enviroment as a hashtable |
| msg | fn [err] | gets the message of an error, if the error has one |
| downret | fn [cont val] | resumes the continuation 'cont' with an enviroment set to an empty child of the of the one within cont, returning to it with 'val' as a result |
| new-err | fn [str] | returns an error with the given string as a message |
| os | fn [] | gets the current operating system |
| fbody | fn [func] | gets the body of a function. primatives return [prim] |
| res | fn [val] | makes a symbol a resolve expression ($sym syntax) |
| pwrt | fn [val] | prints anything, leaving strings in their literal representation |
| values | fn [hash] | gets the keys in a hash as a list |
| try | fn [list catch] | evaluates the list. if there's an error, calls the catch function with the list and the error (in that order). errors automatically throw themselves when evaluated, so handle them carefully |
| fin | fn [path encoding] | returns a file-reading input stream. supported values of 'encoding' are "ascii", "utf8" and "utf16" |
| prn | fn [val] | prints anything, inserting a newline afterwards |
| maphash | fn [hash func] | applies 'func' to each key-value pair in 'hash', returning a new hash made up of all the return values. (return values are in the format [key value]) |
| consvarimac | fn [params body] | constructs a variadic macro; see the description of 'consfn' for the difference between this and 'varimac' |
| callcc | fn [func] | creates a continuation at the point after the end of this function call, and runs the inputed function, passing the continuation as a value |
| rixparse | fn [code] | parses a rix string, returning a list of tokens |
| err | fn [str] | throws an error with the given string as a message |
| prnted | fn [list] | runs 'list', returning a string containing everything that was printed during its running |
| = | fn [x y] | checks for equality |
| inl | mac [params body] | creates a function with params and a body whose results may depend on the surrounding enviroment |
| rdn | fn [n] | reads a single character from the input stream |
| fparams | fn [func] | gets the params of a function |
| evl | fn [val] | evaluates 'val' |
| consvari | fn [params body] | constructs a variadic function; see the description of 'consfn' for the difference between this and 'vari' |
| ifdo | fn [cond body] | if the boolean cond is true, evaluates the body |
| envstack | fn [] | gets the current enviroment as a list, where the last element is a hash containing the current enviroment, and the previous elements are the parents of that enviroment as hashtables |
| args | fn [] | gets command line arguments. the first element is the name of the rix file when run with 'rix file.rix' |
| imps? | fn [gen val] | tests if the type of this value implements a generic |
| consvarinl | fn [params body] | constructs a variadic inline function; see the description of 'consfn' for the difference between this and 'varinl' |
| not | fn [x] | preforms 'not' on a bool |
| rdstr | fn [string code] | runs 'code' in an enviroment with the given string as an input stream, meaning that within 'code', (for example) every 'rdln' reads a line from the stream |
| mod | fn [x y] | gets the modulus of two numbers |
| pdesc | fn [value] | gets the description of a value and prints it |
| fout | fn [path encoding] | returns a file-writing (overrides current file contents) output stream. supported values of 'encoding' are "ascii", "utf8" and "utf16" |
| ^^ | fn [base power] | raises 'base' to the power 'power' |
| prn* | genr 'prn* [val] | No description |
| floor | fn [n] | floors a number |
| number.< | fn [x y] | compares two numbers |
| varinl | mac [params body] | creates a function with params and a body whose results may depend on the surrounding enviroment. the last named param is a list of every value between the penultimate param and a ; token |
| sqrt | fn [x] | gets the square root of a number |
| genexp | inl [sym params] | creates a new generic with the given name and params, exports it, and assigns it to that name within the global scope |
| keys | fn [hash] | gets the keys in a hash as a list |
| lock | fn [val] | 'locks' a value. it will remain locked, and unable to be evaluated, until 'unlock' is called |
| norm | fn [cont val] | evaluates 'val' within the continuation 'cont', returning the result of the evaluation. this has the side effect of modifying 'cont' |
| - | fn [x y] | subtracts two numbers |
| compress-hash | fn [hash func] | applies 'func' to each key-value pair in 'hash', returning a new list made up of all the return values |
| modf | fn [sym val] | modifies the value at a given name if it exists, rather than setting a new value in the current namespace |
| >= | genr '>= [x y] | No description |
| varimac | mac [params body] | creates a function with locked params (which must be accessed via 'unlock') and a body that returns either a list or a single value that is pushed onto the token array. the last named param is a list of every token between the penultimate param and a ; token |
| gettyp | fn [val] | gets the type of a value |
| typ | mac [sym] | turns a symbol into a type. honestly I didn't need to make a seperate 'type' type, but I thought I did, changed a bunch of stuff, and it's too tiresome to change it back |
| consfn | fn [params body] | constructs a function with params and a body. consfn is a function itself, meaning it evaluates its input parameters, so you can create a function with a runtime-computed definition |
| timps? | fn [gen val] | tests if this type implements a generic |
| consmac | fn [params body] | constructs a macro; see the description of 'consfn' for the difference between this and 'mac' |
| list>string | fn [list] | converts a list to a string |
| mac | mac [params body] | creates a function with locked params (which must be accessed via 'unlock') and a body that returns either a list or a single value that is pushed onto the token array |
| tpop | fn [] | pops from the tokenstack. should mainly be used with macros, as using with functions or inline functions may provide unexpected results |
| newcont | fn [tokens] | returns a new continuation with a child enviroment of the current enviroment |
| at | fn [key hash] | gets the value at key stored within 'hash' |
| string>number | fn [str] | parses a string as a number. returns fal if that's not possible |
| tapp | fn [val] | concatinates a list onto the tokenstack |
| nth | fn [list n] | gets the nth element of a list |
| cast | fn [val type] | sets the type of the value to the given type by calling the function 'type1>type2' |
| ucast | fn [val typ] | preforms an unsafe cast of one type to another. should mainly be used in constructors |
| uquot | fn [val] | makes a value an unquote |
| desc | fn [desc value] | sets the description of a value |
| retc | fn [cont] | resumes the continuation 'cont', returning to it with the current continuation as a result |
| assert= | fn [one two] | throws an error if 'one' and 'two' aren't equal |
| tpush | fn [val] | pushes to the tokenstack |
| setat | fn [key value hash] | sets 'value' at 'key' within 'hash' |
| wrt | fn [val] | prints anything |
| / | fn [x y] | divides two numbers |
| vari | mac [params body] | creates a function with params and a body. the last named param is a list of every value between the penultimate param and a ; token |
| runc | fn [cont code] | runs 'code' within the continuation 'cont' (returning 'cont'), returning to the current continuation afterwards |
| typ? | fn [type val] | tests if a value is of a certain type |
| number.> | fn [x y] | compares two numbers |
| number.<= | fn [x y] | compares two numbers |
| getval | fn [cont sym] | returns the result of retriving 'sym' in the enviroment within cont |
| first | fn [list] | returns the first element of a list |
| upret | fn [cont val] | resumes the continuation 'cont' with an enviroment set to the parent of the of the one within cont, returning to it with 'val' as a result |
| * | fn [x y] | multiplies two numbers |
| esc | fn [char] | converts from a single character to an associated escaped character. for example, turns 'n' into a newline |
| splice | fn [val] | makes a value a splice |
| let | fn [hash code] | evaluates 'code' within a new scope defined with the keys and values in 'hash' |
| tpeekc | fn [cont] | looks at the next token in the continuation 'cont' |
| tcountc | fn [cont] | returns the amount of tokens in 'cont' |
| fn | mac [params body] | creates a function with params and a body |
| number.>= | fn [x y] | compares two numbers |
| letfn | mac [params body] | creates a function with params and a body and calls that function immediatly |
| ret | fn [cont val] | resumes the continuation 'cont', returning to it with 'val' as a result |
