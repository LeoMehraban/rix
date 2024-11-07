! Copyright (C) 2024 Your name.
! See https://factorcode.org/license.txt for BSD license
USING: kernel generic parser assocs literals namespaces arrays environment lexer prettyprint splitting classes.tuple colors hashtables continuations sequences.deep prettyprint.custom prettyprint.sections  words classes.predicate quotations accessors vectors classes.parser math sequences combinators combinators.smart unicode strings io io.styles io.files io.encodings.utf8 io.streams.string math.parser strings.parser ;
IN: rix
DEFER: lex-val
DEFER: lex-until-newline

ERROR: lexer-error msg ;
ERROR: eval-error msg ;
SYMBOL: genv

TUPLE: rix-lexer str pos ;
<<
DEFER: include-file
DEFER: eval-until-one
DEFER: evaluator-parent
GENERIC: rix-eval ( evaluator self -- evaluator result/f )
TUPLE: rix-env parent bindings ;
TUPLE: rix-value value type description ;
TUPLE: dec symbol expr ;
TUPLE: inl param-names quot ;
TUPLE: module-call symbol module ;
TUPLE: func env param-names quot ;
TUPLE: genr name params ;
: <val> ( value type -- rix-value ) "No description" rix-value boa ;
: scan-rix-type ( -- type ) scan-new-class dup [ name>> 4 tail [ swap type>> = ] curry ] keep \ rix-value rot define-predicate-class ;
SYNTAX: RIX-TYPE: 
    scan-rix-type \ rix-eval create-method \ ; parse-until >quotation define ;
SYNTAX: SYM: scan-token "symbol" <val> suffix! ;
SYNTAX: GENR: scan-token dup [ "symbol" <val> ] bi@ ";" parse-tokens [ "symbol" <val> ] map "list" <val> genr boa "generic" <val> 2array suffix! ;
: <builtin> ( param-names quot -- func ) f H{ } clone rix-env boa -rot [ [ evaluator-parent evaluator-parent ] dip ] compose func boa "func" <val> ;
: <builtin-inl> ( param-names quot -- func ) [ [ evaluator-parent ] dip ] compose inl boa "inl" <val> ;
: <macro> ( param-names quot -- func  ) inl boa "macro" <val>  ;
: <builtin-macro> ( param-names quot -- func ) [ evaluator-parent ] compose <macro> ;
GENERIC: pprint-rix-value ( value -- )
M: rix-value rix-eval ;
M: rix-value pprint* "RIX:" text [ pprint-rix-value ] with-string-writer text ;
CONSTANT: rix-version "0.01"
: desc ( val desc -- val ) >>description ;
>>

: when% ( quot -- ? ) [ f ] if ; inline
: dwhen% ( quot -- ? ) [ drop f ] if ; inline
: when*% ( quot -- ? ) [ f ] if* ; inline


: current ( lexer -- char )  [ pos>> ] [ str>> ] bi nth ;
: writech ( char -- ) 1string write ;
: lexer-next ( lexer -- lexer char/f ) dup [ current ] [ [ 1 + ] change-pos ] bi [ pos>> ] [ str>> length ] bi < [ drop f ] unless ;
: valid-length? ( lexer -- lexer ? ) dup [ str>> length ] [ pos>> 1 + ] bi >= ;
: ?lexer-next ( lexer -- lexer char ? ) dup [ current ] [ [ 1 + ] change-pos ] bi [ pos>> ] [ str>> length ] bi < ;
: match-and-advance ( lexer char -- lexer matched? ) over valid-length? nip [ over current = [ [ lexer-next drop ] when ] keep ] dwhen% ;
: reset-if ( lexer quot: ( ..a lexer -- ..b lexer parsed? ) -- ..b lexer parsed? ) over clone [ call swap ] dip [ ? ] keepdd ; inline
: skip-whitespace ( lexer -- lexer ) valid-length? [ [ dup current blank? [ lexer-next ] when% ] loop ] when ;
: skip-non-newline-whitespace ( lexer -- lexer ) valid-length? [ [ dup current [ blank? ] [ CHAR: \n = not ] bi and [ lexer-next ] when% ] loop ] when ;
: valid-char? ( char -- ? ) [ { [ CHAR: ( eq? ] [ CHAR: ) eq? ] [ CHAR: [ eq? ] [ CHAR: ] eq? ] [ CHAR: ; eq? ] [ CHAR: @ eq? ]  } cleave ] output>array [  ] any? not ;
: lex-word ( lexer -- lexer word/f ) t [ [ [ dup current [ valid-char? ] [ blank? not ] bi and ] when% ] [ ?lexer-next [ writech ] dip ] while ] with-string-writer dup empty? [ drop f ] when ;
: lex-number ( lexer -- lexer number/f ) [ lex-word [ dec> [ "number" <val> ] when*% ] when*% ] reset-if ;
: lex-module-call ( lexer -- lexer module-call/f ) [ lex-word [ swap CHAR: @ match-and-advance [ lex-word [ swapd [ "symbol" <val> ] bi@ module-call boa "module-call" <val> ] [ nip f ] if* ] [ nip f ] if ] when*% ] reset-if ;
: lex-resolve ( lexer -- lexer resolve/f ) CHAR: ; match-and-advance [ [ lex-module-call [ value>> "module-resolve" <val> ]  [ lex-word [ "resolve" <val> ] when*% ] if* ] reset-if ] when%  ;
: lex-dec ( lexer -- lexer dec/f ) [ lex-word [ unclip-last CHAR: : = [ swap lex-until-newline swapd dec boa "dec" <val> ] dwhen% ] when*% ] reset-if ;
: lex-list ( lexer -- lexer list/f ) 91 match-and-advance [ V{  } clone swap [ 93 match-and-advance not ] [ lex-val swapd suffix! swap ] while swap "list" <val> ] when% ;
: lex-symbol ( lexer -- lexer symbol/f ) lex-word [ dup [ "tru" = ] [ "fal" = ] bi or [ "tru" = "bool" <val> ]  [ dup "nil" = [ drop f "nil" <val> ] [ "symbol" <val> ] if ] if ] when*% ;
: lex-quote ( lexer -- lexer quote/f ) CHAR: ' match-and-advance [ lex-val "quote" <val> ] when% ;
: lex-unquote ( lexer -- lexer quote/f ) CHAR: , match-and-advance [ lex-val "unquote" <val> ] when% ;
: lex-splice-unquote ( lexer -- lexer quote/f ) CHAR: @ match-and-advance [ lex-val "splice-unquote" <val> ] when% ;
: char-or-escape ( lexer -- lexer char )
    92 match-and-advance [ lexer-next ] dip [ escape ] when ;
: lex-parens ( lexer -- lexer parens/f ) 40 match-and-advance [ V{  } clone swap [ 41 match-and-advance not ] [ lex-val swapd suffix! swap ] while swap "parens" <val> ] when% ;
: lex-string ( lexer -- lexer string/f )
    34 match-and-advance [
        [
            [ 34 match-and-advance not ]
            [ char-or-escape writech ] while
        ] with-string-writer "string" <val>
    ] when% ;
: lex-comment ( lexer -- lexer ) CHAR: # match-and-advance [ [ dup current CHAR: \n = not [ lexer-next ] when% ] loop ] when ;
: lex-comment? ( lexer -- lexer ? ) CHAR: # match-and-advance [ [ dup current CHAR: \n = not [ lexer-next ] when% ] loop t ] when% ;
: skip-useless ( lexer -- lexer ) [ skip-whitespace lex-comment? ] loop ;
: lex-val ( lexer -- lexer val ) 
    skip-useless
    lex-quote
    [ lex-unquote ] unless*
    [ lex-splice-unquote ] unless*
    [ lex-number ] unless*
    [ lex-dec ] unless*
    [ lex-string ] unless*
    [ lex-list ] unless*
    [ lex-module-call ] unless*
    [ lex-resolve ] unless*
    [ lex-parens ] unless*
    [ lex-symbol ] unless*
    [ skip-useless ] dip
    [ "Invalid expression" lexer-error ] unless*
 ;
: lex-until-newline ( lexer -- lexer expr ) 
    V{ } clone swap [
        skip-useless
        lex-quote
        [ lex-unquote ] unless*
        [ lex-splice-unquote ] unless*
        [ lex-number ] unless*
        [ lex-dec ] unless*
        [ lex-string ] unless*
        [ lex-list ] unless*
        [ lex-module-call ] unless*
        [ lex-resolve ] unless*
        [ lex-parens ] unless*
        [ lex-symbol ] unless*
        [ skip-non-newline-whitespace ] dip
        [ valid-length? [ dup current CHAR: ; = not ] [ t ] if [ "Invalid expression" lexer-error ] when f ] unless*
        [ swapd suffix! swap ] when*
        dup valid-length? not [ [ CHAR: \n match-and-advance nip ] [ CHAR: ; match-and-advance nip ] bi or ] dip or not
    ] loop
    swap
 ;

: can-continue? ( lexer -- lexer ? ) skip-whitespace lex-comment valid-length? ;
: lex-str ( str -- tokens ) 0 rix-lexer boa V{  } clone swap [ lex-val swapd suffix! swap can-continue? ] loop drop ;


: <env> ( parent -- env ) clone H{  } clone rix-env boa ;
! value parent
: env-get ( value env -- value/f ) 2dup bindings>> at [ 2nip ] [ parent>> [ env-get ] [ drop f ] if* ] if* ;
: env-set ( symbol value env -- env ) [ [ set-at ] keep ] 2with change-bindings ;
TUPLE: evaluator tokens global-namespace to-export current-namespace results extra-tokens ;
: get-value ( evaluator string -- value ) "symbol" <val> swap current-namespace>> env-get value>> ;
: get-value* ( evaluator string -- value ) "symbol" <val> swap current-namespace>> env-get ;
: unclip! ( sequence -- rest first ) [ first ] [ 0 swap remove-nth! ] bi swap  ;
: prepend! ( seq1 seq2 -- seq1 ) [ reverse! ] bi@ [ append! ] keep [ reverse! ] bi@ drop ;
: prefix! ( seq elt -- seq ) 1array prepend! ;
: (pop-token) ( eval -- eval token ) [ unclip! swap ] change-tokens swap ;
: pop-token ( eval -- eval token ) dup tokens>> length 0 > [ (pop-token) ] [ [ dup pop dup length 0 > [ unclip -rot suffix! ] [ drop dup pop unclip -rot suffix! ] if ] change-extra-tokens swap ] if ;
: push-token ( eval token -- eval ) [ 1array prepend! ] curry change-tokens ;
: push-tokens ( eval tokens -- eval ) [ prepend! ] curry change-tokens ;
: push-results ( eval result -- eval ) [ [ suffix! ] curry change-results ] when* ;
: pop-results ( eval -- eval result ) [ [ pop ] keep ] change-results swap ;

: <evaluator> ( tokens global -- evaluator ) H{  } clone over V{  } dup [ clone ] bi@ evaluator boa ;

: (get-env-depth) ( env n  -- parent n ) [ parent>> ] [ 1 + ] bi* over [ (get-env-depth) ] when ;
: get-env-depth ( env -- depth ) 0 (get-env-depth) nip ;

M: evaluator pprint* "EVAL" text [ current-namespace>> get-env-depth >dec "{" append text ] keep tokens>> [ [ pprint-rix-value ] with-string-writer text ] each "}" text ;

M: rix-env pprint* "env" text [ [ [ parent>> not [ "g" write ] when ] [ genv get-global eq? not [ "i" write ] when ] bi ] with-string-writer text ] keep bindings>> pprint* ;

TUPLE: flagged val ;

MIXIN: rix-sequence
M: rix-sequence length value>> length ;
M: rix-sequence nth value>> nth ;
M: rix-sequence set-nth value>> set-nth ;
M: rix-sequence set-length value>> set-length ;
M: rix-sequence lengthen value>> set-length ;
M: rix-sequence like [ value>> like ] [ type>> ] bi <val> ;
M: rix-sequence clone [ value>> clone ] [ type>> clone ] bi <val>  ;
INSTANCE: rix-sequence sequence


: eval-next ( evaluator -- evaluator result ) 
    pop-token rix-eval ;

: can-cont? ( evaluator -- ? ) [ tokens>> length ] [ extra-tokens>> concat length ] bi + 0 >  ;

: evaluator-<env> ( evaluator -- evaluator ) dup current-namespace>> clone <env> >>current-namespace ;

: evaluator-parent ( evaluator -- evaluator ) dup current-namespace>> parent>> [ "Tried to get the parent of a global enviorment" eval-error f ] unless* >>current-namespace ;

: eval-until-one ( evaluator -- evaluator result ) f [ over can-cont? [ [ eval-next ] unless* dup not ] when% ] loop ;

: eval-one ( evaluator token -- evaluator result ) push-token eval-until-one ;

: flatten-flagged ( array -- array ) [ dup sequence? [ dup [ clone V{ } >>value [ dup flagged? [ val>> dup sequence? [ append ] [ suffix ] if ] [ suffix ] if ] reduce ] keep like ] when ] deep-map ;

: unquote-scan ( evaluator val -- evaluator val )
    value>> dup sequence? [ dupd [ dup sequence? not
                     [ dup type>> "unquote" =
                       [ value>> eval-one nip ] [ dup type>> "splice-unquote" = [ value>> rix-eval [ eval-next ] unless* flagged boa ] when nip ] if ] [ nip ] if ] with deep-map flatten-flagged ] when "quote" <val> ;

: eval-all ( evaluator -- evaluator ) [ dup tokens>> length 0 > ] [ eval-next push-results ] while ;

: eval-full ( evaluator -- evaluator ) [ dup can-cont? ] [ eval-next push-results ] while  ;

: eval-all-tokens ( tokens env -- evaluator ) <evaluator> eval-all ;

: extra-tokens>tokens ( eval -- eval ) dup extra-tokens>> pop >>tokens ;

: eval-tokens ( evaluator tokens -- evaluator result )
    over results>> clone [ [ dup tokens>> [ suffix! ] curry change-extra-tokens V{ } clone >>tokens ] dip
                                                        push-tokens eval-all extra-tokens>tokens ] dip [ [ last ] dip ] curry change-results swap ;

: eval-set-global ( evaluator symbol value -- evaluator ) swap pick global-namespace>> env-set drop ;

: not-reached-target? ( target evaluator -- ?  ) [ results>> length > ] 1check [ can-cont? [ "Expected expression but got nothing" eval-error f ] unless* ] [ drop f ] if ;

: eval-to-target ( evaluator target -- evaluator )
    over results>> length + swap
    [ 2dup not-reached-target? ]
    [ eval-until-one push-results ] while nip ;

: target-to-results ( evaluator target -- evaluator results )
    V{ } clone [ [ length > ] 2check [ pick can-cont? [ "Expected expression but got nothing" eval-error f ] unless* ] when% ]
    [ [ eval-until-one ] 2dip rot suffix! ] while nip ;

: with-env ( evaluator quot -- evaluator ) [ evaluator-<env> ] dip call evaluator-parent ; inline

: make-macro ( evaluator -- evaluator macro ) dup [ "params" get-value ]  [ "body" get-value* ] bi [ dup sequence? not [ 1vector ] when f "scopeup" <val> suffix eval-tokens
                                                                                                     dup sequence? [ push-tokens ]  [ push-token ] if ] curry <macro> "quote" <val>  ;

: make-inl ( evaluator -- evaluator func ) dup [ "params" get-value ]  [ "body" get-value* ] bi [ dup sequence? not [ 1vector ] when f "scopeup" <val> suffix push-tokens f ] curry inl boa "inl" <val> "quote" <val> ;

: env-deep-clone ( env -- env ) clone dup parent>> [ [ env-deep-clone ] change-parent ] when [ [ [ clone ] bi@ ] assoc-map ] change-bindings ;

: make-func ( evaluator -- evaluator func )
    dup
    [ current-namespace>> parent>> env-deep-clone ] [ "params" get-value ]  [ "body" get-value* ] tri [ dup sequence? not [ 1vector ] when f "scopeup" <val> dup [ suffix ] bi@ push-tokens f ] curry func boa "func" <val> "quote" <val> ;

: find-global-env ( env -- global ) [ parent>> ] 1check [ nip find-global-env ] when* ;

: with-global-env ( eval quot -- eval ) [ [ dup find-global-env ] change-current-namespace ] dip rot [ call ] dip >>current-namespace ; inline

: get-paths ( -- paths ) "RIX-PATH" os-env [ ":" split ] [ { "~/.rix" } ] if* "." prefix ;

: find-file-path ( module-name -- file-path ) ".rix" append "/" prepend get-paths swap [ append ] curry map [ file-exists? ] find nip [ "Unknown module" eval-error f ] unless*  ;

: default-env ( -- env )
    f
    H{
        GENR: comb x y ;
        GENR: < x y ;
        GENR: > x y ;
        GENR: <= x y ;
        GENR: >= x y ;
        GENR: prn* val ;
        { SYM: + $[ { SYM: x SYM: y } [ dup [ "x" get-value ] [ "y" get-value ] bi + "number" <val> ] <builtin> "adds two numbers" desc ] } 
        { SYM: - $[ { SYM: x SYM: y } [ dup [ "x" get-value ] [ "y" get-value ] bi - "number" <val> ] <builtin> "subtracts two numbers" desc ] }
        { SYM: * $[ { SYM: x SYM: y } [ dup [ "x" get-value ] [ "y" get-value ] bi * "number" <val> ] <builtin> "multiplies two numbers" desc ] }
        { SYM: / $[ { SYM: x SYM: y } [ dup [ "x" get-value ] [ "y" get-value ] bi / "number" <val> ] <builtin> "divides two numbers" desc ] }
        { SYM: number.< $[ { SYM: x SYM: y } [ dup [ "x" get-value ] [ "y" get-value ] bi < "bool" <val> ] <builtin> "compares two numbers" desc ] }
        { SYM: number.> $[ { SYM: x SYM: y } [ dup [ "x" get-value ] [ "y" get-value ] bi > "bool" <val> ] <builtin> "compares two numbers" desc ] }
        { SYM: number.<= $[ { SYM: x SYM: y } [ dup [ "x" get-value ] [ "y" get-value ] bi <= "bool" <val> ] <builtin> "compares two numbers" desc ] }
        { SYM: number.>= $[ { SYM: x SYM: y } [ dup [ "x" get-value ] [ "y" get-value ] bi >= "bool" <val> ] <builtin> "compares two numbers" desc ] }
        { SYM: = $[ { SYM: x SYM: y } [ dup [ "x" get-value* ] [ "y" get-value* ] bi = "bool" <val> ] <builtin> "checks for equality" desc ] }
        { SYM: decons $[ { SYM: list } [ dup "list" get-value unclip swap "list" <val> 2array >vector "list" <val> ] <builtin>
                         "returns a list where the first element is the first element of the input list, and the last element is the remainder of the list" desc ] }
        { SYM: |=| $[ { SYM: x SYM: y } [ dup [ "x" get-value ] [ "y" get-value ] bi = "bool" <val> ] <builtin> "checks for loose equality (when two rix values have the same internal value)" desc ] }
        { SYM: and $[ { SYM: x SYM: y } [ dup [ "x" get-value ] [ "y" get-value ] bi and "bool" <val> ] <builtin> "preforms 'and' on two bools" desc ] }
        { SYM: or $[ { SYM: x SYM: y } [ dup [ "x" get-value ] [ "y" get-value ] bi or "bool" <val> ] <builtin> "preforms 'or' on two bools" desc ] }
        { SYM: nth $[ { SYM: list SYM: n } [ dup [ "n" get-value ] [ "list" get-value ] bi nth ] <builtin> "gets the nth element of a list" desc ] }
        { SYM: len $[ { SYM: list } [ dup "list" get-value length "number" <val> ] <builtin> "gets the length of a list" desc ] }
        { SYM: map $[ { SYM: list SYM: func } [ dup [ "list" get-value ] [ "func" get-value* ] bi [ swap 2array push-tokens eval-until-one ] curry map "list" <val> ] <builtin>
                      "applies 'func' to each element in 'list', returning a new list made up of all the return values" desc ] }
        { SYM: lst>str $[ { SYM: list } [ dup "list" get-value [ value>> ] "" map-as "string" <val> ] <builtin> "converts a list to a string" desc ] }
        { SYM: not $[ { SYM: x } [ dup "x" get-value not "bool" <val> ] <builtin> "preforms 'not' on a bool" desc ] }
        { SYM: inl $[ { SYM: params SYM: body } [ make-inl push-token ] <builtin-macro> "creates a function with params and a body whose results may depend on the surrounding enviroment" desc ] } 
        { SYM: fn $[ { SYM: params SYM: body } [ make-func push-token ] <builtin-macro> "creates a function with params and a body" desc  ] }
        { SYM: mac $[ { SYM: params SYM: body } ! 
                      [ make-macro push-token ] <builtin-macro>
                      "creates a function with params and a body that returns either a list or a single value that is pushed onto the token array" desc ] }
        { SYM: tpop $[ { } [ pop-token ] <builtin> "pops from the tokenstack. should mainly be used with macros, as using with functions or closures may provide unexpected results" desc ] }
        { SYM: tpush $[ { SYM: val } [ dup "val" get-value* push-token f ] <builtin> "pushes to the tokenstack" desc ] }
        { SYM: tapp $[ { SYM: val } [ dup "val" get-value* push-tokens f ] <builtin> "concatinates a list onto the tokenstack" desc ] }
        { SYM: letfn $[ { SYM: params SYM: body } [ make-func value>> push-token ] <builtin-macro> "creates a function with params and a body and calls that function immediatly" desc ] }
        { SYM: prn $[ { SYM: val } [ dup "val" get-value* [ dup type>> "string" = [ value>> "symbol" <val> ] when pprint-rix-value "\n" write ] keep ] <builtin> "prints anything, inserting a newline afterwards" desc ] }
        { SYM: wrt $[ { SYM: val } [ dup "val" get-value* [ dup type>> "string" = [ value>> "symbol" <val> ] when pprint-rix-value ] keep  ] <builtin> "prints anything" desc ] }
        { SYM: pprn $[ { SYM: val } [ dup "val" get-value* [ pprint-rix-value "\n" write ] keep ] <builtin> "prints anything, leaving strings in their literal representation and inserting a newline afterwards" desc ] }
        { SYM: pwrt $[ { SYM: val } [ dup "val" get-value* [ pprint-rix-value ] keep ] <builtin> "prints anything, leaving strings in their literal representation" desc ] }
        { SYM: str>lst $[ { SYM: string } [ dup "string" get-value [ "number" <val> ] V{ } map-as "list" <val> ] <builtin> "converts a string to a list" desc ]  }
        { SYM: incl $[ { SYM: name } [ dup "name" get-value [ include-file ] keep "symbol" <val> swap eval-set-global ] <builtin-macro> "includes the module specified by 'name'" desc ] }
        { SYM: if $[ { SYM: cond SYM: body1 SYM: body2 }
                     [ dup [ "cond" get-value ] [ "body1" get-value* ] [ "body2" get-value* ] tri ? dup type>> "list" = [ value>> push-tokens f ] when ] <builtin>
                    "based on the boolean cond, picks either body1 or body2 and evaluates it" desc ] } 
        { SYM: ifdo $[ { SYM: cond SYM: body }
                       [ dup [ "body" get-value* ] [ "cond" get-value ] bi [ dup type>> "list" = [ value>> push-tokens ] [ push-token ] if f ] [ drop f "nil" <val> ] if ] <builtin>
                       "if the boolean cond is true, evaluates the body" desc ] }
        { SYM: typ $[ { SYM: val SYM: type } [ dup [ "val" get-value ] [ "type" get-value ] bi <val> ] <builtin>
                      "sets the type of the value to the given string type. note that sometimes this will cause errors because the types are incompatible (such as if you set a number to type 'symbol')" desc ] }
        { SYM: scop $[ { SYM: code SYM: scope } [ evaluator-<env> dup [ "scope" get-value ] [ "code" get-value ] bi append f "scopeup" <val> suffix eval-tokens ] <builtin>
                       "evaluates the list 'scope' within its own scope, and then calls 'code' within that same scope" desc ] }
        { SYM: set $[ { SYM: name SYM: value } [ dup [ "name" get-value* ] [ "value" get-value* ] bi pick current-namespace>> parent>> [ swapd env-set drop ] keepd ] <builtin-inl> "defines a variable" desc ] }
        { SYM: genr $[ { SYM: sym SYM: params } [ dup [ "sym" get-value* ] [ "params" get-value* ] bi genr boa "generic" <val> [ eval-set-global ] keep ] <builtin>
                       "creates a new generic with the given name and params (and assigns it to that name within the global scope)" desc ]  }
        { SYM: gdesc $[ { SYM: value } [ dup "value" get-value* description>> "string" <val> ] <builtin> "gets the description of a value and returns it" desc ] }
        { SYM: pdesc $[ { SYM: value } [ dup "value" get-value* [ description>> print ] keep ] <builtin> "gets the description of a value and prints it" desc ] }
        { SYM: desc $[ { SYM: desc SYM: value } [ dup [ "value" get-value* ] [ "desc" get-value ] bi desc ] <builtin> "sets the description of a value" desc ] }
        { SYM: nxt $[ { } [ eval-until-one ] <builtin> "evaluates tokens until a value is returned" desc ] }
        { SYM: glob $[ { SYM: dec } [ [ "dec" get-value* ] keep [ swap rix-eval swap ] with-global-env swap push-token ] <builtin-macro> "takes a declaration and evaluates it within the global namespace" desc ] }
        { SYM: exp $[ { SYM: dec } [ dup "dec" get-value [ expr>> eval-tokens ] [ symbol>> "symbol" <val> ] bi rot [ to-export>> set-at ] 3keep spin [ eval-set-global ] keep push-results ]
                         <builtin-macro> "takes a declaration and sets it for exporting. note that inline functions should not be exported" desc ]  }
        { SYM: quot $[ { SYM: val } [ dup "val" get-value* "quote" <val> ] <builtin> "quotes a value" desc ] }
        { SYM: uquot $[ { SYM: val } [ dup "val" get-value* "unquote" <val> ] <builtin> "makes a value an unquote" desc ] }
        { SYM: suquot $[ { SYM: val } [ dup "val" get-value* "splice-unquote" <val> ] <builtin> "makes a value a splice-unquote" desc ] }
        { SYM: err $[ { SYM: str } [ dup "str" get-value \ eval-error boa "error" <val> push-token f ]  <builtin> "throws an error with the given string as a message" desc ] }
        { SYM: new-err $[ { SYM: str } [ dup "str" get-value \ eval-error boa "error" <val> ]  <builtin> "returns an error with the given string as a message" desc ]  }
        { SYM: try $[ { SYM: list SYM: catch } [ dup [ "catch" get-value* ] [ "list" get-value* ] bi [ nip eval-tokens ] [ "error" <val> "quote" <val> 3array >vector eval-tokens ] recover ] <builtin>
                      "evaluates the list. if there's an error, calls the function with the list and the error (in that order). errors automatically throw themselves when evaluated, so handle them carefully" desc ] }
        { SYM: prnted $[ { SYM: list } [ dup "list" get-value [ eval-tokens drop ] with-string-writer "string" <val> ]  <builtin> "runs 'list', returning a string containing everything that was printed during its running" desc ] }
        { SYM: assert= $[ { SYM: one SYM: two } [ dup [ "one" get-value* ] [ "two" get-value* ] bi [ = ] 2check [ drop ]
                                                   [ [ [ pprint-rix-value ] with-string-writer ] bi@ " != " prepend append "Assert failed: " prepend eval-error ] if ]
                            <builtin> "throws an error if 'one' and 'two' aren't equal" desc ] }
    } clone rix-env boa clone
    ;

: global-env ( -- env )
    genv [ default-env ] initialize
    genv get-global
    ;

: fresh-env ( -- env ) default-env drop global-env ;

: clear-genv ( -- ) f genv set-global ;

: get-rix-impl ( val genr -- fun/f ) [ type>> ] dip append "symbol" <val> global-env bindings>> at ;
: eval-rix-fun ( params fn -- result ) swap 2array >vector global-env <evaluator> eval-all results>> last ;

M: rix-value pprint-rix-value dup ".prn*" get-rix-impl [ eval-rix-fun drop ] [ value>> pprint ] if* ;

: map-acc ( x seq quot -- x seq' ) collector [ each ] dip ; inline

: eval-str ( str -- result ) [ lex-str fresh-env <evaluator> eval-full results>> dup empty? [ drop f "nil" <val> ] [ last ] if ] [ clear-genv ] finally ;

: include-file ( module-name -- module ) find-file-path utf8 file-contents global-env [ lex-str default-env <evaluator> eval-full to-export>> "module" <val> ] dip genv set-global ;

: eval-str-to-str ( str -- str ) eval-str [ pprint-rix-value ] with-string-writer  ;

: repl ( -- )
    [
        "Welcome to the rix repl! (" write rix-version ")" append print
        "Press Control+D to quit" print V{ } clone fresh-env <evaluator>
        [
            "rix> " write readln
            [
                [ lex-str >>tokens eval-all dup results>> dup empty? not [ last pprint-rix-value "\n" write t ] [ drop t ] if ] when*% ] [ [ "error" <val> pprint-rix-value "\n" write ] with-string-writer COLOR: red
                                                                                                                                           foreground associate format ] recover
        ] loop drop
    ]  [ clear-genv ] finally ;

: compress-env ( env -- env ) dup parent>> [ compress-env swap [ bindings>> ] bi@ assoc-union f swap rix-env boa ] when* ;
: env-set-parent ( env parent -- env ) [ swap [ >>parent ] when* ] curry change-parent ;
: load-env ( env1 env2 -- env ) compress-env env-set-parent ;

: global-env>markdown ( -- )
    "| name | signature      | description |\n| --- | ------------------| ------------|" print default-env bindings>> [ [ "| " write [ pprint-rix-value " | " write ] bi@ ] keep description>> write " |" print ] assoc-each  ;

: tpopn ( eval n -- eval toks ) [ 1 - over pop-token nip ] collector [ [ dup 0 <= not ] swap while ] dip nip ;

! hello@world
RIX-TYPE: rix-module-call value>> [ module>> over current-namespace>> [ env-get ] curry ?transmute [ value>> "unknown module: " prepend eval-error ] unless value>> ] keep symbol>> swap [ at ] curry ?transmute
[ value>> "unknown symbol: " prepend eval-error ] unless push-token f ;
M: rix-module-call pprint-rix-value value>> [ symbol>> value>> write ] [ "@" write module>> value>> write ] bi ;

! ;hello@world
RIX-TYPE: rix-module-resolve value>> [ module>> over current-namespace>> [ env-get ] curry ?transmute [ value>> "unknown module: " prepend eval-error ] unless value>> ] keep symbol>> swap [ at ] curry ?transmute
[ value>> "unknown symbol: " prepend eval-error ] unless ;
M: rix-module-resolve pprint-rix-value ";" write value>> [ symbol>> value>> write ] [ "@" write module>> value>> write ] bi ;

! 1798
RIX-TYPE: rix-number ;

RIX-TYPE: rix-scopeup drop evaluator-parent f ;
M: rix-scopeup pprint-rix-value drop "^" write ;

RIX-TYPE: rix-scopedown drop evaluator-<env> f  ;
M: rix-scopedown pprint-rix-value drop "|" write ;

! "Hello, World"
RIX-TYPE: rix-string ;

RIX-TYPE: rix-module ;
M: rix-module pprint-rix-value value>> name>> "mod " write write ;

! err "example error"
RIX-TYPE: rix-error value>> throw ;
M: rix-error pprint-rix-value value>> "ERROR " write "msg" swap [ ?offset-of-slot ] 1check [ msg>> ] when pprint ;

! 'something
RIX-TYPE: rix-quote unquote-scan value>> ;
M: rix-quote pprint-rix-value "'" write value>> pprint-rix-value ;

! [ things 1 2 4 5 ]
RIX-TYPE: rix-list ;
M: rix-list pprint-rix-value "[" write value>> dup empty? not [ unclip pprint-rix-value ] when [ " " write pprint-rix-value ] each "]" write  ;
INSTANCE: rix-list rix-sequence

! ;symbol
RIX-TYPE: rix-resolve value>> "symbol" <val> over current-namespace>> [ env-get ] curry ?transmute [ value>> "unknown symbol: " prepend eval-error ] unless ;
M: rix-resolve pprint-rix-value ";" write value>> write ;

! (evaluated-immediatly 1 1)
RIX-TYPE: rix-parens value>> eval-tokens ;
M: rix-parens pprint-rix-value "(" write value>> dup empty? not [ unclip pprint-rix-value ] when [ " " write pprint-rix-value ] each ")" write ;
INSTANCE: rix-parens rix-sequence

! symbol
RIX-TYPE: rix-symbol over current-namespace>> [ env-get ] curry ?transmute [ value>> "unknown symbol: " prepend eval-error ] unless push-token f ;
M: rix-symbol pprint-rix-value value>> write ;

! tru
RIX-TYPE: rix-bool ;
M: rix-bool pprint-rix-value value>> "tru" "fal" ? write ;

! nil
RIX-TYPE: rix-nil ;
M: rix-nil pprint-rix-value drop "nil" write ;

! foo: bar
RIX-TYPE: rix-dec value>> [ symbol>> "symbol" <val> ] [ expr>> ] bi swapd eval-tokens rot [ [ rot env-set ] 2curry change-current-namespace ] keepd ;
M: rix-dec pprint-rix-value value>> [ symbol>> write ":" write ] [ expr>> [ " " write pprint-rix-value ] each ] bi  ;

! inl [x y] [+ x y]
RIX-TYPE: rix-inl
  [ evaluator-<env> ] dip
  [ value>> param-names>> length target-to-results swap ] keep
  [ value>> param-names>> [ tuck current-namespace>> ] dip swap [ rot env-set ] 2reduce >>current-namespace ] keep value>> quot>> call( evaluator -- evaluator result/f  )
  ;
M: rix-inl pprint-rix-value value>> "inl " write param-names>> "list" <val> pprint-rix-value ;

! fn [x y] [+ x y]
! eval self
RIX-TYPE: rix-func
  [ value>> param-names>> length target-to-results swap ] keep
  [ value>> env>> [ [ <env> ] dip load-env ] curry change-current-namespace ] keep
  [ value>> param-names>> [ tuck current-namespace>> ] dip swap [ rot env-set ] 2reduce >>current-namespace ] keep
  value>> quot>> call( evaluator -- evaluator result/f )
  ;
M: rix-func pprint-rix-value value>> "fn " write param-names>> "list" <val> pprint-rix-value ;

! mac [x y] [+ x y]
RIX-TYPE: rix-macro
  [ evaluator-<env> ] dip
  [ value>> param-names>> length tpopn swap ] keep
  [ value>> param-names>> [ tuck current-namespace>> ] dip swap [ rot env-set ] 2reduce >>current-namespace ] keep value>> quot>> call( evaluator -- evaluator ) f ;
M: rix-macro pprint-rix-value value>> "mac " write param-names>> "list" <val> pprint-rix-value ;

! ,something
RIX-TYPE: rix-unquote ;
M: rix-unquote pprint-rix-value "," write value>> pprint-rix-value ;

! @something
RIX-TYPE: rix-splice-unquote ;
M: rix-splice-unquote pprint-rix-value "@" write value>> pprint-rix-value ;

! genr 'foo [x y]
RIX-TYPE: rix-generic
   value>> [ params>> length [ eval-to-target ] keep [ cut* swap ] curry change-results ] keep
   pick first [ name>> value>> "." prepend ] [ type>> ] bi* prepend "symbol" <val> swapd prefix push-tokens f ;
M: rix-generic pprint-rix-value "genr '" write [ value>> name>> value>> write ] [ " " write value>> params>> pprint-rix-value ] bi ;
