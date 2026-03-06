# the rix programming language

rix (doesn't stand for anything) is a programming language I'm currently working on creating. There are probably a lot of bugs at the moment.

## installing
to build rix, you need to have installed [factor](https://factorcode.org/). Then, clone this repo into the work folder in your factor directory and load rix.factor. then, run `"rix" deploy`, and wait for an executable to be generated

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

all functions included by default are listed below. if you don't understand some things, it's helpful to look through tutorial.rix, tests.rix and std/\*.rix

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
| vec | typreq [(typ list)] fn [list] | creates a vector from a list. vectors are like lists, but they don't get copied when you add to them |
| glob | typreq [(typ dec)] mac [dec] | takes a declaration and evaluates it within the global namespace |
| rest | typreq [(typ list)] fn [list] | returns all of the list but the first element |
| set | typreq [(typ symbol) (typ any)] inl [name value] | defines a variable |
| nop | nop | No description |
| < | typreq [(typ number) (typ number)] fn [x y] | compares two numbers |
| tdelc | typreq [(typ continuation)] fn [cont] | pops the next token in the continuation 'cont', returning the continuation |
| map | typreq [(typ list) (typ callable)] fn [list func] | applies 'func' to each element in 'list', returning a new list made up of all the return values |
| consinl | typreq [(typ list) (typ list)] fn [params body] | constructs an inline function; see the description of 'consfn' for the difference between this and 'inl' |
| underlying | fn [val] | gets the underlying value of a builtin rix type as a hashtable |
| list>vec | vec | alias for 'vec' |
| quot | quot | quotes a value |
| comb | genr 'comb [x y] | No description |
| tpopc | typreq [(typ continuation)] fn [cont] | pops the next token in the continuation 'cont'. this is pretty much the only rix function with a side effect, as it modifies 'cont' without returning it |
| rng | typreq [(typ number) (typ number)] fn [from to] | returns a list with numbers that go from 'from' to 'to', including both 'from' and 'to' |
| up | typreq [(typ continuation) (typ symbol)] fn [cont sym] | returns the result of retriving 'sym' in the parent enviroment of the one within cont |
| and | fn [x y] | preforms 'and' on two bools |
| nxt | fn [] | evaluates tokens until a value is returned |
| exp | typreq [(typ dec)] mac [dec] | takes a declaration and sets it for exporting. note that inline functions should not be exported |
| <= | typreq [(typ number) (typ number)] fn [x y] | compares two numbers |
| str-first | typreq [(typ string)] fn [string] | gets the first character of a string as a unicode codepoint |
| ostream | typreq [(typ stream) (typ list)] fn [stream code] | runs 'code' with the output stream remapped to 'stream' |
| if | if | based on the boolean cond, picks either body1 or body2 and evaluates it |
| rdln | fn [] | reads a line from the input stream |
| append | typreq [(typ vec) (typ any)] fn [vec value] | adds an item to the end of a vector |
| or | fn [x y] | preforms 'or' on two bools |
| genr | typreq [(typ symbol) (typ list)] inl [sym params] | creates a new generic with the given name and params (and assigns it to that name within the global scope) |
| runbef | typreq [(typ continuation) (typ list)] fn [cont code] | sets the continuation to run some code before the code it would otherwise run |
| rdch | fn [] | reads a single character from the input stream |
| number>string | typreq [(typ number)] fn [n] | turns a number into a base10 string |
| fapp | typreq [(typ string) (typ string)] fn [path encoding] | returns a file-appending output stream. supported values of 'encoding' are "ascii", "utf8" and "utf16" |
| pprn | fn [val] | prints anything, leaving strings in their literal representation and inserting a newline afterwards |
| tpeek | fn [] | returns the top of the tokenstack. should mainly be used with macros, as using with functions or inlines may provide unexpected results |
| wrtstr | typreq [(typ string)] fn [string] | prints a string |
| istream | typreq [(typ stream) (typ list)] fn [stream code] | runs 'code' with the input stream remapped to 'stream' |
| len | typreq [(typ list)] fn [list] | gets the length of a list |
| string>list | typreq [(typ string)] fn [string] | converts a string to a list |
| > | typreq [(typ number) (typ number)] fn [x y] | compares two numbers |
| + | + | adds two numbers |
| incl | typreq [(typ symbol)] mac [name] | includes the module specified by 'name'. if you import two modules with conflicting names, you can be specific by using 'name@modulename' syntax |
| typreq | typreq [(typ list) (typ callable)] fn [list func] | sets a list of required types for this function. 'typ any' means any type can be inputted, and 'typ callable' means that functions, inlines, macros, variadics etc. can all be used |
| tpushc | typreq [(typ continuation) (typ any)] fn [cont val] | pushes a value to the continuation 'cont' |
| unlock | typreq [(typ lock)] fn [locked] | 'unlocks' a locked value, returning it |
| clo | typreq [(typ hashstruct) (typ callable)] fn [hash func] | includes the hash 'hash' within the env of the closure 'func', or if 'func' is an inline function, then turns it into a closure with the env 'hash' |
| gdesc | fn [value] | gets the description of a value and returns it |
| cstack | fn [] | returns the callstack |
| getenv | fn [] | gets the current enviroment as a hashtable |
| msg | typreq [(typ error)] fn [err] | gets the message of an error, if the error has one |
| downret | typreq [(typ continuation) (typ any)] fn [cont val] | resumes the continuation 'cont' with an enviroment set to an empty child of the of the one within cont, returning to it with 'val' as a result |
| new-err | typreq [(typ string)] fn [str] | returns an error with the given string as a message |
| os | fn [] | gets the current operating system |
| fbody | typreq [(typ callable)] fn [func] | gets the body of a function. primatives return [prim] |
| res | typreq [(typ symbol)] fn [val] | makes a symbol a resolve expression ($sym syntax) |
| pwrt | fn [val] | prints anything, leaving strings in their literal representation |
| values | typreq [(typ hash)] fn [hash] | gets the keys in a hash as a list |
| try | typreq [(typ list) (typ callable)] fn [list catch] | evaluates the list. if there's an error, calls the catch function with the list and the error (in that order). errors automatically throw themselves when evaluated, so handle them carefully |
| fin | typreq [(typ string) (typ string)] fn [path encoding] | returns a file-reading input stream. supported values of 'encoding' are "ascii", "utf8" and "utf16" |
| prn | fn [val] | prints anything, inserting a newline afterwards |
| maphash | typreq [(typ hash) (typ callable)] fn [hash func] | applies 'func' to each key-value pair in 'hash', returning a new hash made up of all the return values. (return values are in the format [key value]) |
| consvarimac | typreq [(typ list) (typ list)] fn [params body] | constructs a variadic macro; see the description of 'consfn' for the difference between this and 'varimac' |
| callcc | typreq [(typ callable)] fn [func] | creates a continuation at the point after the end of this function call, and runs the inputed function, passing the continuation as a value |
| rixparse | typreq [(typ string)] fn [code] | parses a rix string, returning a list of tokens |
| err | fn [str] | throws an error with the given string as a message |
| prnted | typreq [(typ list)] fn [list] | runs 'list', returning a string containing everything that was printed during its running |
| = | fn [x y] | checks for equality |
| inl | typreq [(typ list) (typ any)] mac [params body] | creates a function with params and a body whose results may depend on the surrounding enviroment |
| setparams | typreq [(typ callable) (typ list)] fn [func list] | sets the param names of a callable without changing its type |
| rdn | typreq [(typ number)] fn [n] | reads a single character from the input stream |
| fparams | typreq [(typ callable)] fn [func] | gets the params of a function |
| evl | fn [val] | evaluates 'val' |
| consvari | typreq [(typ list) (typ list)] fn [params body] | constructs a variadic function; see the description of 'consfn' for the difference between this and 'vari' |
| ifdo | fn [cond body] | if the boolean cond is true, evaluates the body |
| envstack | fn [] | gets the current enviroment as a list, where the last element is a hash containing the current enviroment, and the previous elements are the parents of that enviroment as hashtables |
| args | fn [] | gets command line arguments. the first element is the name of the rix file when run with 'rix file.rix' |
| imps? | typreq [(typ generic) (typ any)] fn [gen val] | tests if the type of this value implements a generic |
| consvarinl | typreq [(typ list) (typ list)] fn [params body] | constructs a variadic inline function; see the description of 'consfn' for the difference between this and 'varinl' |
| not | fn [x] | preforms 'not' on a bool |
| rdstr | typreq [(typ string) (typ list)] fn [string code] | runs 'code' in an enviroment with the given string as an input stream, meaning that within 'code', (for example) every 'rdln' reads a line from the stream |
| clone | fn [val] | clones a mutable value, allowing you to modify it without changing the original |
| mod | typreq [(typ number) (typ number)] fn [x y] | gets the modulus of two numbers |
| pdesc | fn [value] | gets the description of a value and prints it |
| fout | typreq [(typ string) (typ string)] fn [path encoding] | returns a file-writing (overrides current file contents) output stream. supported values of 'encoding' are "ascii", "utf8" and "utf16" |
| ^^ | typreq [(typ number) (typ number)] fn [base power] | raises 'base' to the power 'power' |
| prn* | genr 'prn* [val] | No description |
| floor | typreq [(typ num)] fn [n] | floors a number |
| varinl | typreq [(typ list) (typ any)] mac [params body] | creates a function with params and a body whose results may depend on the surrounding enviroment. the last named param is a list of every value between the penultimate param and a ; token |
| sqrt | typreq [(typ number)] fn [x] | gets the square root of a number |
| pop | typreq [(typ vec)] fn [vec] | removes a value from the end of a vector, returning the removed value |
| genexp | typreq [(typ symbol) (typ list)] inl [sym params] | creates a new generic with the given name and params, exports it, and assigns it to that name within the global scope |
| keys | typreq [(typ hash)] fn [hash] | gets the keys in a hash as a list |
| vec>list | typreq [(typ vec)] fn [vec] | turns a vector into a list |
| remove-nth | typreq [(typ vec) (typ number)] fn [vec n] | removes the nth value from a vector, returning the vector |
| insert-nth | typreq [(typ vec) (typ number) (typ any)] fn [vec n value] | inserts a value at a specific index in a vector |
| lock | fn [val] | 'locks' a value. it will remain locked, and unable to be evaluated, until 'unlock' is called |
| - | - | subtracts two numbers |
| norm | typreq [(typ continuation) (typ any)] fn [cont val] | evaluates 'val' within the continuation 'cont', returning the result of the evaluation. this has the side effect of modifying 'cont' |
| compress-hash | typreq [(typ hashstruct) (typ callable)] fn [hash func] | applies 'func' to each key-value pair in 'hash', returning a new list made up of all the return values |
| modf | typreq [(typ symbol) (typ any)] fn [sym val] | modifies the value at a given name if it exists, rather than setting a new value in the current namespace |
| >= | typreq [(typ number) (typ number)] fn [x y] | compares two numbers |
| varimac | typreq [(typ list) (typ any)] mac [params body] | creates a function with locked params (which must be accessed via 'unlock') and a body that returns either a list or a single value that is pushed onto the token array. the last named param is a list of every token between the penultimate param and a ; token |
| gettyp | fn [val] | gets the type of a value |
| typ | typreq [(typ symbol)] mac [sym] | turns a symbol into a type. honestly I didn't need to make a seperate 'type' type, but I thought I did, changed a bunch of stuff, and it's too tiresome to change it back |
| newhash | fn [] | creates a new hashtable at runtime |
| consfn | consfn | constructs a function with params and a body. consfn is a function itself, meaning it evaluates its input parameters, so you can create a function with a runtime-computed definition |
| timps? | typreq [(typ generic) (typ typ)] fn [gen val] | tests if this type implements a generic |
| update | typreq [(typ box) (typ any)] fn [box value] | updates the value a box is pointing to |
| list>string | typreq [(typ list)] fn [list] | converts a list to a string |
| consmac | consmac | constructs a macro; see the description of 'consfn' for the difference between this and 'mac' |
| prepend | typreq [(typ vec) (typ any)] fn [vec value] | adds an item to the beginning of a vector |
| mac | mac | creates a function with locked params (which must be accessed via 'unlock') and a body that returns either a list or a single value that is pushed onto the token array |
| setbody | typreq [(typ callable) (typ list)] fn [func list] | sets the body of a callable without changing its type |
| newcont | typreq [(typ list)] fn [tokens] | returns a new continuation with a child enviroment of the current enviroment |
| tpop | fn [] | pops from the tokenstack. should mainly be used with macros, as using with functions or inline functions may provide unexpected results |
| at | typreq [(typ any) (typ hashstruct)] fn [key hash] | gets the value at key stored within 'hash' |
| string>number | typreq [(typ string)] fn [str] | parses a string as a number. returns fal if that's not possible |
| str-rest | typreq [(typ string)] fn [string] | gets the all characters in a string but the first one, and returns them as a string |
| tapp | typreq [(typ list)] fn [val] | concatinates a list onto the tokenstack |
| nth | typreq [(typ list) (typ number)] fn [list n] | gets the nth element of a list |
| unbox | typreq [(typ box)] fn [box] | unboxes a value from a box |
| ucast | typreq [(typ any) (typ type)] fn [val typ] | preforms an unsafe cast of one type to another. should mainly be used in constructors |
| cast | typreq [(typ any) (typ type)] fn [val type] | sets the type of the value to the given type by calling the function 'type1>type2' |
| desc | typreq [(typ string) (typ any)] fn [desc value] | sets the description of a value |
| retc | typreq [(typ continuation)] fn [cont] | resumes the continuation 'cont', returning to it with the current continuation as a result |
| uquot | uquot | makes a value an unquote |
| tpush | fn [val] | pushes to the tokenstack |
| setat | typreq [(typ any) (typ any) (typ hashstruct)] fn [key value hash] | sets 'value' at 'key' within 'hash', mutating hash |
| wrt | fn [val] | prints anything |
| assert= | fn [one two] | throws an error if 'one' and 'two' aren't equal |
| / | / | divides two numbers |
| vari | typreq [(typ list) (typ any)] mac [params body] | creates a function with params and a body. the last named param is a list of every value between the penultimate param and a ; token |
| runc | typreq [(typ continuation) (typ list)] fn [cont code] | runs 'code' within the continuation 'cont' (returning 'cont'), returning to the current continuation afterwards |
| typ? | typreq [(typ type) (typ any)] fn [type val] | tests if a value is of a certain type |
| box | fn [value] | creates a box from a value. boxes are mutable references to other values |
| str-len | typreq [(typ string)] fn [string] | gets the length of a string |
| getval | typreq [(typ continuation) (typ symbol)] fn [cont sym] | returns the result of retriving 'sym' in the enviroment within cont |
| first | typreq [(typ list)] fn [list] | returns the first element of a list |
| upret | typreq [(typ continuation) (typ any)] fn [cont val] | resumes the continuation 'cont' with an enviroment set to the parent of the of the one within cont, returning to it with 'val' as a result |
| * | * | multiplies two numbers |
| esc | typreq [(typ number)] fn [char] | converts from a single character to an associated escaped character. for example, turns 'n' into a newline |
| splice | fn [val] | makes a value a splice |
| let | let | evaluates 'code' within a new scope defined with the keys and values in 'hash' |
| tpeekc | typreq [(typ continuation)] fn [cont] | looks at the next token in the continuation 'cont' |
| tcountc | typreq [(typ continuation)] fn [cont] | returns the amount of tokens in 'cont' |
| fn | fn | creates a function with params and a body |
| cloex | typreq [(typ list) (typ callable)] fn [symbols func] | modifies this function or macro to exclude these symbols from its stored environment |
| letfn | typreq [(typ list) (typ list)] mac [params body] | creates a function with params and a body and calls that function immediatly |
| ret | typreq [(typ continuation) (typ any)] fn [cont val] | resumes the continuation 'cont', returning to it with 'val' as a result |

