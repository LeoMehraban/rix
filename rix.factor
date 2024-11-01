! Copyright (C) 2024 Your name.
! See https://factorcode.org/license.txt for BSD license
USING: kernel generic parser assocs literals namespaces arrays lexer prettyprint classes.tuple colors hashtables continuations sequences.deep prettyprint.custom prettyprint.sections  words classes.predicate quotations accessors vectors classes.parser math sequences combinators combinators.smart unicode strings io io.styles io.streams.string math.parser strings.parser ;
IN: rix
DEFER: lex-val
DEFER: lex-until-newline

ERROR: lexer-error msg ;
ERROR: eval-error msg ;
SYMBOL: genv

TUPLE: rix-lexer str pos ;
<<
DEFER: eval-until-one
GENERIC: rix-eval ( evaluator self -- evaluator result/f )
TUPLE: rix-value value type description ;
TUPLE: dec symbol expr ;
TUPLE: func param-names quot ;

TUPLE: closure env param-names quot ;
: <val> ( value type -- rix-value ) "No description" rix-value boa ;
: scan-rix-type ( -- type ) scan-new-class dup [ name>> 4 tail [ swap type>> = ] curry ] keep \ rix-value rot define-predicate-class ;
SYNTAX: RIX-TYPE: 
    scan-rix-type \ rix-eval create-method \ ; parse-until >quotation define ;
SYNTAX: SYM: scan-token "symbol" <val> suffix! ;
SYNTAX: GENR: scan-token dup [ "symbol" <val> ] [ "generic" <val> ] bi* 2array suffix! ;
: <func> ( param-names quot -- func  ) func boa "func" <val>  ;
: <macro> ( param-names quot -- func  ) func boa "macro" <val>  ;
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
: valid-char? ( char -- ? ) [ { [ CHAR: ( eq? ] [ CHAR: ) eq? ] [ CHAR: [ eq? ] [ CHAR: ] eq? ] [ CHAR: ; eq? ] } cleave ] output>array [  ] any? not ;
: lex-word ( lexer -- lexer word/f ) t [ [ [ dup current [ valid-char? ] [ blank? not ] bi and ] when% ] [ ?lexer-next [ writech ] dip ] while ] with-string-writer dup empty? [ drop f ] when ;
: lex-number ( lexer -- lexer number/f ) [ lex-word [ dec> [ "number" <val> ] when*% ] when*% ] reset-if ;
: lex-resolve ( lexer -- lexer resolve/f ) CHAR: ; match-and-advance [ [ lex-word [ "resolve" <val> ] when*% ] reset-if ] when%  ;
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

TUPLE: env parent bindings ;
: <env> ( parent -- env ) clone H{  } clone env boa ;
! value parent
: env-get ( value env -- value/f ) 2dup bindings>> at [ 2nip ] [ parent>> [ env-get ] [ drop f ] if* ] if* ;
: env-set ( symbol value env -- env ) [ [ set-at ] keep ] 2with change-bindings ;
TUPLE: evaluator tokens global-namespace current-namespace results extra-tokens ;
: get-value ( evaluator string -- value ) "symbol" <val> swap current-namespace>> env-get value>> ;
: get-value* ( evaluator string -- value ) "symbol" <val> swap current-namespace>> env-get ;
: unclip! ( sequence -- rest first ) [ first ] [ 0 swap remove-nth! ] bi swap  ;
: prepend! ( seq1 seq2 -- seq1 ) [ reverse! ] bi@ [ append! ] keep [ reverse! ] bi@ drop ;
: (pop-token) ( eval -- eval token ) [ unclip! swap ] change-tokens swap ;
: pop-token ( eval -- eval token ) dup tokens>> length 0 > [ (pop-token) ] [ [ unclip! swap ] change-extra-tokens swap ] if ;
: push-token ( eval token -- eval ) [ 1array prepend! ] curry change-tokens ;
: push-tokens ( eval tokens -- eval ) [ prepend! ] curry change-tokens ;
: push-results ( eval result -- eval ) [ [ suffix! ] curry change-results ] when* ;
: pop-results ( eval -- eval result ) [ [ pop ] keep ] change-results swap ;

: <evaluator> ( tokens global -- evaluator ) dup V{  } dup [ clone ] bi@ evaluator boa ;

M: evaluator pprint* tokens>> "EVAL{" text [ [ pprint-rix-value ] with-string-writer text ] each "}" text ;

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

: can-cont? ( evaluator -- ? ) [ tokens>> length ] [ extra-tokens>> length ] bi + 0 >  ;

: eval-until-one ( evaluator -- evaluator result ) f [ over can-cont? [ [ eval-next ] unless* dup not ] when% ] loop  ;

: eval-one ( evaluator token -- evaluator result ) push-token eval-until-one ;

: flatten-flagged ( array -- array ) [ dup sequence? [ dup [ clone V{ } >>value [ dup flagged? [ val>> dup sequence? [ append ] [ suffix ] if ] [ suffix ] if ] reduce ] keep like ] when ] deep-map ;

: unquote-scan ( evaluator val -- evaluator val )
    value>> dup sequence? [ dupd [ dup sequence? not
                     [ dup type>> "unquote" =
                       [ value>> eval-one nip ] [ dup type>> "splice-unquote" = [ value>> rix-eval [ eval-next ] unless* flagged boa ] when nip ] if ] [ nip ] if ] with deep-map flatten-flagged ] when "quote" <val> ;

: eval-all ( evaluator -- evaluator ) [ dup tokens>> length 0 > ] [ eval-next push-results ] while ;

: eval-all-tokens ( tokens env -- evaluator ) <evaluator> eval-all ;

: eval-tokens ( evaluator tokens -- evaluator result )
    [ dup tokens>> [ append! ] curry change-extra-tokens V{ } clone >>tokens ] dip push-tokens eval-all dup [ extra-tokens>> >>tokens V{ } clone >>extra-tokens ] [ results>> pop ] bi ;

: evaluator-<env> ( evaluator -- evaluator ) dup current-namespace>> clone <env> >>current-namespace ;

: evaluator-parent ( evaluator -- evaluator ) dup current-namespace>> parent>> >>current-namespace ;

: eval-set-global ( evaluator symbol value -- evaluator ) swap pick global-namespace>> env-set drop ;

: not-reached-target? ( target evaluator -- ?  ) [ results>> length > ] 1check [ can-cont? [ "Expected expression but got nothing" eval-error f ] unless* ] [ drop f ] if ;

: eval-to-target ( evaluator target -- evaluator )
    over results>> length + swap
    [ 2dup not-reached-target? ]
    [ eval-until-one push-results ] while nip ;

: with-env ( evaluator quot -- evaluator ) [ evaluator-<env> ] dip call evaluator-parent ; inline

: make-func ( evaluator -- evaluator func ) dup [ "params" get-value ]  [ "body" get-value* ] bi [ dup sequence? not [ 1vector ] when eval-tokens ] curry <func> "quote" <val> ;

: env-deep-clone ( env -- env ) clone dup parent>> [ parent>> env-deep-clone ] when [ [ [ clone ] bi@ ] assoc-map ] change-bindings ;


: make-closure ( evaluator -- evaluator closure )
    dup [ current-namespace>> parent>> env-deep-clone ] [ "params" get-value ]  [ "body" get-value* ] tri [ dup sequence? not [ 1vector ] when eval-tokens ] curry closure boa "closure" <val> "quote" <val> ;

: fn-from-rix ( params str -- fn ) dupd [ "" [ value>> append " " append ] reduce "[" prepend "] " append ] dip "[" prepend "]" append append "fn " prepend lex-str
    [ swap dup [ <evaluator> eval-all results>> dup empty? [ drop f "nil" <val> ] [ last ] if ] dip current-namespace>> values swap prefix
    swap <evaluator> eval-all results>> dup empty? [ drop f "nil" <val> ] [ last ] if ] curry
    <func> ;


: default-env ( -- env )
    f
    H{
        GENR: +
        GENR: -
        GENR: *
        GENR: /
        GENR: <
        GENR: >
        GENR: <=
        GENR: =>
        GENR: nth
        GENR: not
        GENR: len
        GENR: prn*
        { SYM: number.+ $[ { SYM: x SYM: y } [ dup [ "x" get-value ] [ "y" get-value ] bi + "number" <val> ] <func> "adds two numbers" desc ] } 
        { SYM: number.- $[ { SYM: x SYM: y } [ dup [ "x" get-value ] [ "y" get-value ] bi - "number" <val> ] <func> "subtracts two numbers" desc ] }
        { SYM: number.* $[ { SYM: x SYM: y } [ dup [ "x" get-value ] [ "y" get-value ] bi * "number" <val> ] <func> "multiplies two numbers" desc ] }
        { SYM: number./ $[ { SYM: x SYM: y } [ dup [ "x" get-value ] [ "y" get-value ] bi / "number" <val> ] <func> "divides two numbers" desc ] }
        { SYM: number.< $[ { SYM: x SYM: y } [ dup [ "x" get-value ] [ "y" get-value ] bi < "bool" <val> ] <func> "compares two numbers" desc ] }
        { SYM: number.> $[ { SYM: x SYM: y } [ dup [ "x" get-value ] [ "y" get-value ] bi > "bool" <val> ] <func> "compares two numbers" desc ] }
        { SYM: number.<= $[ { SYM: x SYM: y } [ dup [ "x" get-value ] [ "y" get-value ] bi <= "bool" <val> ] <func> "compares two numbers" desc ] }
        { SYM: number.=> $[ { SYM: x SYM: y } [ dup [ "x" get-value ] [ "y" get-value ] bi >= "bool" <val> ] <func> "compares two numbers" desc ] }
        { SYM: = $[ { SYM: x SYM: y } [ dup [ "x" get-value* ] [ "y" get-value* ] bi = "bool" <val> ] <func> "checks for equality" desc ] }
        { SYM: decons $[ { SYM: list } [ dup "list" get-value unclip swap "list" <val> 2array >vector "list" <val> ] <func>
                         "returns a list where the first element is the first element of the input list, and the last element is the remainder of the list" desc ] }
        { SYM: |=| $[ { SYM: x SYM: y } [ dup [ "x" get-value ] [ "y" get-value ] bi = "bool" <val> ] <func> "checks for loose equality (when two rix values have the same internal value)" desc ] }
        { SYM: and $[ { SYM: x SYM: y } [ dup [ "x" get-value ] [ "y" get-value ] bi and "bool" <val> ] <func> "preforms 'and' on two bools" desc ] }
        { SYM: or $[ { SYM: x SYM: y } [ dup [ "x" get-value ] [ "y" get-value ] bi or "bool" <val> ] <func> "preforms 'or' on two bools" desc ] }
        { SYM: list.nth $[ { SYM: list SYM: n } [ dup [ "n" get-value ] [ "list" get-value ] bi nth ] <func> "gets the nth element of a list" desc ] }
        { SYM: list.len $[ { SYM: list } [ dup "list" get-value length "number" <val> ] <func> "gets the length of a list" desc ] }
        { SYM: map $[ { SYM: list SYM: func } [ dup [ "list" get-value ] [ "func" get-value* ] bi [ swap 2array push-tokens eval-until-one ] curry map "list" <val> ] <func>
                      "applies 'func' to each element in 'list', returning a new list made up of all the return values" desc ] }
        { SYM: bool.not $[ { SYM: x } [ dup "x" get-value not "bool" <val> ] <func> "preforms 'not' on a bool" desc ] }
        { SYM: fn $[ { SYM: params SYM: body } [ make-func push-token ] <macro> "creates a function with params and a body" desc ] } 
        { SYM: clo $[ { SYM: params SYM: body } [ make-closure push-token ] <macro> "creates a function with params and a body that saves the state of the enviorment it was created in" desc  ] }
        { SYM: mac $[ { SYM: params SYM: body } ! 
                      [ make-func [ value>> [ [ dup rix-sequence? [ push-tokens ] [ push-token ] if ] compose ] change-quot "macro" <val> ] change-value push-token ] <macro>
                      "creates a function with params and a body that returns either a list or a single value that is pushed onto the token array" desc ] }
        { SYM: tpop $[ { } [ pop-token ] <func> "pops from the tokenstack" desc ] }
        { SYM: tpush $[ { SYM: val } [ dup "val" get-value* push-token f ] <func> "pushes to the tokenstack" desc ] }
        { SYM: tapp $[ { SYM: val } [ dup "val" get-value* push-tokens f ] <func> "concatinates a list onto the tokenstack" desc ] }
        { SYM: letfn $[ { SYM: params SYM: body } [ make-func value>> push-token ] <macro> "creates a function with params and a body and calls that function immediatly" desc ] }
        { SYM: prn $[ { SYM: val } [ dup "val" get-value* [ dup type>> "string" = [ value>> "symbol" <val> ] when pprint-rix-value "\n" write ] keep ] <func> "prints anything, inserting a newline afterwards" desc ] }
        { SYM: wrt $[ { SYM: val } [ dup "val" get-value* [ dup type>> "string" = [ value>> "symbol" <val> ] when pprint-rix-value ] keep  ] <func> "prints anything" desc ] }
        { SYM: pprn $[ { SYM: val } [ dup "val" get-value* [ pprint-rix-value "\n" write ] keep ] <func> "prints anything, leaving strings in their literal representation and inserting a newline afterwards" desc ] }
        { SYM: pwrt $[ { SYM: val } [ dup "val" get-value* [ pprint-rix-value ] keep ] <func> "prints anything, leaving strings in their literal representation" desc ] }
        { SYM: str>lst $[ { SYM: string } [ dup "string" get-value [ "number" <val> ] V{ } map-as "list" <val> ] <func> "converts a string to a list" desc ]  }
        { SYM: if $[ { SYM: cond SYM: body1 SYM: body2 }
                     [ dup [ "cond" get-value ] [ "body1" get-value* ] [ "body2" get-value* ] tri ? dup type>> "list" = [ value>> push-tokens f ] when ] <func>
                    "based on the boolean cond, picks either body1 or body2 and evaluates it" desc ] } 
        { SYM: ifdo $[ { SYM: cond SYM: body }
                       [ dup [ "body" get-value* ] [ "cond" get-value ] bi [ dup type>> "list" = [ value>> push-tokens ] [ push-token ] if f ] [ drop f "nil" <val> ] if ] <func>
                       "if the boolean cond is true, evaluates the body" desc ] }
        { SYM: typ $[ { SYM: val SYM: type } [ dup [ "val" get-value ] [ "type" get-value ] bi <val> ] <func>
                      "sets the type of the value to the given string type. note that sometimes this will cause errors because the types are incompatible (such as if you set a number to type 'symbol')" desc ] }
        { SYM: scop $[ { SYM: code SYM: scope } [ [ dup [ "scope" get-value ] [ "code" get-value ] bi append eval-tokens swap ] with-env swap ] <func>
                       "evaluates the list 'scope' within its own scope, and then calls 'code' within that same scope" desc ] }
        { SYM: set $[ { SYM: name SYM: value } [ dup [ "name" get-value* ] [ "value" get-value* ] bi pick current-namespace>> parent>> [ swapd env-set drop ] keepd ] <func> "defines a variable" desc ] }
        { SYM: genr $[ { SYM: sym } [ dup "sym" get-value* dup value>> clone "generic" <val> [ eval-set-global ] keep ] <func> "creates a new generic with the given name (and assigns it to that name within the global scope)" desc ]  }
        { SYM: gdesc $[ { SYM: value } [ dup "value" get-value* description>> "string" <val> ] <func> "gets the description of a value and returns it" desc ] }
        { SYM: pdesc $[ { SYM: value } [ dup "value" get-value* [ description>> print ] keep ] <func> "gets the description of a value and prints it" desc ] }
        { SYM: desc $[ { SYM: desc SYM: value } [ dup [ "value" get-value* ] [ "desc" get-value ] bi desc ] <func> "sets the description of a value" desc ] }
        { SYM: nxt $[ { } [ eval-until-one ] <func> "evaluates tokens until a value is returned" desc ] }
        { SYM: quot $[ { SYM: val } [ dup "val" get-value* "quote" <val> ] <func> "quotes a value" desc ] }
        { SYM: uquot $[ { SYM: val } [ dup "val" get-value* "unquote" <val> ] <func> "makes a value an unquote" desc ] }
        { SYM: suquot $[ { SYM: val } [ dup "val" get-value* "splice-unquote" <val> ] <func> "makes a value a splice-unquote" desc ] }
        { SYM: err $[ { SYM: str } [ dup "str" get-value \ eval-error boa "error" <val> push-token f ]  <func> "throws an error with the given string as a message" desc ] }
        { SYM: new-err $[ { SYM: str } [ dup "str" get-value \ eval-error boa "error" <val> ]  <func> "returns an error with the given string as a message" desc ]  }
        { SYM: try $[ { SYM: list SYM: catch } [ dup [ "catch" get-value* ] [ "list" get-value* ] bi [ nip eval-tokens ] [ "error" <val> "quote" <val> 3array >vector eval-tokens ] recover ] <func>
                      "evaluates the list. if there's an error, calls the function with the list and the error (in that order). errors automatically throw themselves when evaluated, so handle them carefully" desc ] }
    } clone env boa clone
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

: eval-str ( str -- result ) [ lex-str fresh-env <evaluator> eval-all results>> dup empty? [ drop f "nil" <val> ] [ last ] if ] [ clear-genv ] finally ;

: eval-str-to-str ( str -- str ) eval-str [ pprint-rix-value ] with-string-writer  ;

: repl ( -- )
    [
        "Welcome to the rix repl! (" write rix-version ")" append print
        "Press Control+D to quit" print V{ } clone fresh-env <evaluator>
        [
            "rix> " write readln
            [
                [ lex-str >>tokens eval-all dup results>> dup empty? not [ last pprint-rix-value "\n" write t ] [ drop f ] if ] when*% ] [ [ "error" <val> pprint-rix-value "\n" write ] with-string-writer COLOR: red
                                                                                                                                           foreground associate format ] recover
        ] loop drop
    ]  [ clear-genv ] finally ;

! 1798
RIX-TYPE: rix-number ;

! "Hello, World"
RIX-TYPE: rix-string ;

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
RIX-TYPE: rix-parens value>> push-tokens 1 eval-to-target pop-results ;
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
RIX-TYPE: rix-dec over [ value>> [ symbol>> "symbol" <val> ] [ expr>> ] bi ] dip current-namespace>> eval-all-tokens results>> dup length 0 > [ last ] [ drop f ] if rot [ [ swapd env-set ] 2with change-current-namespace ] keepd ;
M: rix-dec pprint-rix-value value>> [ symbol>> write ":" write ] [ expr>> [ " " write pprint-rix-value ] each ] bi  ;

! fn [x y] [+ x y]
RIX-TYPE: rix-func
  swap [ swap
  [ value>> param-names>> length [ eval-to-target ] keep [ cut* swap ] curry change-results ] keep
  [ value>> param-names>> [ tuck current-namespace>> ] dip swap [ rot env-set ] 2reduce >>current-namespace ] keep value>> quot>> call( evaluator -- evaluator result/f  )
  swap
  ] with-env
  swap ;
M: rix-func pprint-rix-value value>> "fn " write param-names>> "list" <val> pprint-rix-value ;

! clo [x y] [+ x y]
RIX-TYPE: rix-closure
  [ value>> param-names>> length [ eval-to-target ] keep [ cut* swap ] curry change-results ] keep
  dupd [ current-namespace>> ] 2dip
  [ [ value>> env>> clone <env> >>current-namespace ] [ value>> param-names>> [ tuck current-namespace>> roll ] dip rot [ rot env-set ] 2reduce swapd >>current-namespace ] bi ] keep value>> quot>> call( evaluator -- evaluator result/f  )
  spin >>current-namespace swap
  ;
M: rix-closure pprint-rix-value value>> "clo " write param-names>> "list" <val> pprint-rix-value ;

! mac [x y] [+ x y]
RIX-TYPE: rix-macro
  swap [ swap
  [ [ value>> param-names>> length cut ] curry change-tokens ] keep
  [ value>> param-names>> [ tuck current-namespace>> ] dip swap [ rot env-set ] 2reduce >>current-namespace ] keep value>> quot>> call( evaluator -- evaluator ) ] with-env f ;
M: rix-macro pprint-rix-value value>> "mac " write param-names>> "list" <val> pprint-rix-value ;

! ,something
RIX-TYPE: rix-unquote ;
M: rix-unquote pprint-rix-value "," write value>> pprint-rix-value ;

! @something
RIX-TYPE: rix-splice-unquote ;
M: rix-splice-unquote pprint-rix-value "@" write value>> pprint-rix-value ;

! genr foo
RIX-TYPE: rix-generic [ dup [ tokens>> clone ] [ results>> clone ] [ eval-until-one nip ] tri [ [ >>tokens ] [ >>results ] bi* ] dip type>> ] dip value>> "." prepend append "symbol" <val> push-token f ;
M: rix-generic pprint-rix-value "genr '" write value>> write ;
