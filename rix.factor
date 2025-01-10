! Copyright (C) 2024 Your name.
! See https://factorcode.org/license.txt for BSD license
USING: kernel generic parser make assocs command-line ranges literals tools.continuations namespaces serialize arrays environment lexer system alien.libraries prettyprint splitting classes.tuple colors hashtables continuations sequences.deep prettyprint.custom prettyprint.sections  words classes.predicate quotations accessors vectors classes.parser math math.functions sequences combinators classes combinators.smart unicode strings io io.styles io.files io.encodings.utf8 io.streams.string math.parser strings.parser alien.libraries.finder alien.c-types alien io.encodings.ascii io.encodings.utf16 ;
IN: rix


ERROR: lexer-error msg ;
ERROR: eval-error msg ;
<PRIVATE

DEFER: lex-val
DEFER: lex-until-semi
DEFER: callstack-pop
<<
GENERIC: pprint-rix-value ( value -- )
GENERIC: corrisponding-c-types ( value -- types )
TUPLE: rix-env parent bindings ;
TUPLE: callstack-element name params func ;
TUPLE: rix-value value type description ;
M: rix-value corrisponding-c-types drop [ ] ;
TUPLE: dec symbol expr ;
TUPLE: inl types param-names quot body ;
TUPLE: module-call symbol module ;
TUPLE: func types env param-names quot body ;
TUPLE: genr name params ;
: <val> ( value type -- rix-value ) "No description" rix-value boa ;
SYNTAX: SYM: scan-token "symbol" <val> suffix! ;
>>

TUPLE: rix-lexer str pos ;
<<
DEFER: include-file
DEFER: eval-until-one
DEFER: evaluator-parent
GENERIC: rix-eval ( evaluator self -- evaluator result/f )
: scan-rix-type ( -- type ) scan-new-class dup [ name>> 4 tail [ swap type>> = ] curry ] keep \ rix-value rot define-predicate-class ;
SYNTAX: RIX-TYPE: 
    scan-rix-type \ rix-eval create-method \ ; parse-until >quotation define ;
SYNTAX: GENR: scan-token dup [ "symbol" <val> ] bi@ ";" parse-tokens [ "symbol" <val> ] map "list" <val> genr boa "generic" <val> 2array suffix! ;
: <builtin> ( types param-names quot -- func ) f H{ } clone rix-env boa -rot [ [ evaluator-parent callstack-pop evaluator-parent ] dip ] compose [ drop ] prepose V{ SYM: prim } "list" <val> func boa "func" <val> ;
: <builtin-inl> ( types param-names quot -- inl ) [ [ evaluator-parent callstack-pop ] dip ] compose [ drop ] prepose V{ SYM: prim } "list" <val> inl boa "inl" <val> ;
: <macro> ( types env param-names quot body -- macro ) func boa "macro" <val> ;
: <builtin-macro> ( types param-names quot -- macro ) f H{ } clone rix-env boa -rot [ evaluator-parent callstack-pop evaluator-parent ] compose [ drop ] prepose V{ SYM: prim } "list" <val> func boa "macro" <val> ;
M: rix-value rix-eval ;
M: rix-value pprint* [ "RIX:" text [ pprint-rix-value ] with-string-writer text ] [ "ERROR:" text nip class-of name>> text ] recover ;
CONSTANT: rix-version "0.03.1"
: desc ( val desc -- val ) >>description ;
>>

: when% ( quot -- ? ) [ f ] if ; inline
: dwhen% ( quot -- ? ) [ drop f ] if ; inline
: when*% ( quot -- ? ) [ f ] if* ; inline

: seq-to-pairs ( seq -- pairs ) dup length 0 > [ unclip V{  } clone swap suffix! [ over last vector?
                                                                                   [ over last length 2 <= [ suffix! ] [ [ unclip-last ] dip suffix! suffix! ] if ] [ [ unclip-last ] dip 2array >vector suffix! ] if ] reduce ] when ;
: hash-to-seq ( hash -- seq ) V{ } clone [ [ suffix! ] bi@ ] assoc-reduce ;

: current ( lexer -- char )  [ pos>> ] [ str>> ] bi nth ;
: writech ( char -- ) 1string write ;
: lexer-next ( lexer -- lexer char/f ) dup [ current ] [ [ 1 + ] change-pos ] bi [ pos>> ] [ str>> length ] bi < [ drop f ] unless ;
: valid-length? ( lexer -- lexer ? ) dup [ str>> length ] [ pos>> 1 + ] bi >= ;
: ?lexer-next ( lexer -- lexer char ? ) dup [ current ] [ [ 1 + ] change-pos ] bi [ pos>> ] [ str>> length ] bi < ;
: match-and-advance ( lexer char -- lexer matched? ) over valid-length? nip [ over current = [ [ lexer-next drop ] when ] keep ] dwhen% ;
: reset-if ( lexer quot: ( ..a lexer -- ..b lexer parsed? ) -- ..b lexer parsed? ) over clone [ call swap ] dip [ ? ] keepdd ; inline
: skip-whitespace ( lexer -- lexer ) valid-length? [ [ dup current blank? [ lexer-next ] when% ] loop ] when ;
: skip-non-newline-whitespace ( lexer -- lexer ) valid-length? [ [ dup current [ blank? ] [ CHAR: \n = not ] bi and [ lexer-next ] when% ] loop ] when ;
: valid-char? ( char -- ? ) [ { [ CHAR: ( eq? ] [ CHAR: ) eq? ] [ CHAR: { eq? ] [ CHAR: } eq? ]  [ CHAR: [ eq? ] [ CHAR: ] eq? ] [ CHAR: ; eq? ] [ CHAR: @ eq? ] [ CHAR: $ eq? ] } cleave ] output>array [  ] any? not ;
: lex-word ( lexer -- lexer word/f ) t [ [ [ dup current [ valid-char? ] [ blank? not ] bi and ] when% ] [ ?lexer-next [ writech ] dip ] while ] with-string-writer dup empty? [ drop f ] when ;
: lex-number ( lexer -- lexer number/f ) [ lex-word [ dec> [ "number" <val> ] when*% ] when*% ] reset-if ;
: lex-module-call ( lexer -- lexer module-call/f ) [ lex-word [ swap CHAR: @ match-and-advance [ lex-word [ swapd [ "symbol" <val> ] bi@ module-call boa "module-call" <val> ] [ nip f ] if* ] [ nip f ] if ] when*% ] reset-if ;
: lex-resolve ( lexer -- lexer resolve/f ) CHAR: $ match-and-advance [ [ lex-module-call [ value>> "module-resolve" <val> ]  [ lex-word [ "resolve" <val> ] when*% ] if* ] reset-if ] when%  ;
: lex-semi ( lexer -- lexer semi/f ) CHAR: ; match-and-advance [ t "semicolon" <val> ] when% ;
: lex-dec ( lexer -- lexer dec/f ) [ lex-word [ unclip-last CHAR: : = [ swap lex-until-semi swapd dec boa "dec" <val> ] dwhen% ] when*% ] reset-if ;
: lex-list ( lexer -- lexer list/f ) 91 match-and-advance [ V{  } clone swap [ 93 match-and-advance not ] [ lex-val swapd suffix! swap ] while swap "list" <val> ] when% ;
: lex-hash ( lexer -- lexer hash/f ) CHAR: { match-and-advance [ V{  } clone swap [ CHAR: } match-and-advance not ] [ lex-val swapd suffix! swap ] while swap seq-to-pairs parse-hashtable "hash" <val> ] when% ;
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
    [ lex-semi ] unless*
    [ lex-hash ] unless*
    [ skip-useless ] dip
    [ "Invalid expression" lexer-error ] unless*
 ;
: lex-until-semi ( lexer -- lexer expr ) 
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
        [ lex-hash ] unless*
        [ skip-useless ] dip
        [ valid-length? [ dup current CHAR: ; = not ] [ t ] if [ "Invalid expression" lexer-error ] when f ] unless*
        [ swapd suffix! swap ] when*
        dup valid-length? not [ CHAR: ; match-and-advance nip ] dip or not
    ] loop
    swap
 ;

: can-continue? ( lexer -- lexer ? ) skip-whitespace lex-comment valid-length? ;
: lex-str ( str -- tokens ) 0 rix-lexer boa V{  } clone swap [ lex-val swapd suffix! swap can-continue? ] loop drop ;


: <env> ( parent -- env ) clone H{  } clone rix-env boa ;
! value parent
: env-get ( value env -- value/f ) 2dup bindings>> at [ 2nip ] [ parent>> [ env-get ] [ drop f ] if* ] if* ;
: env-set ( symbol value env -- env ) [ clone ] dip [ [ set-at ] keep ] 2with change-bindings ;
TUPLE: evaluator tokens to-export current-namespace results extra-tokens callstack last-name ;
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
: <evaluator> ( tokens global -- evaluator ) H{  } clone swap V{  } dup dup [ clone ] tri@ "<anon>" evaluator boa ;
: callstack-push ( evaluator name params func -- ) callstack-element boa [ over push ] curry change-callstack drop ;
: callstack-pop ( evaluator -- evaluator ) [ dup pop drop ] change-callstack ;
: print-callstack ( evaluator -- evaluator ) "callstack: " print dup callstack>> reverse [ "(" write
                                                                                             [ name>> write ]
                                                                                             [ params>> [ " " write pprint-rix-value ] each ]
                                                                                             bi ")" print ] each ;
: EVAL{ ( -- ) ;
: (get-env-depth) ( env n  -- parent n ) [ parent>> ] [ 1 + ] bi* over [ (get-env-depth) ] when ;
: get-env-depth ( env -- depth ) 0 (get-env-depth) nip ;
! "EVAL{" text tokens>> [ [ pprint-rix-value ] with-string-writer text ] each "}" text
M: evaluator pprint-delims drop \ EVAL{ \ } ;
M: evaluator >pprint-sequence tokens>> ;
M: evaluator pprint* pprint-object ;

M: rix-env pprint* "env" text bindings>> pprint* ;

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
    pop-token [ rix-eval ] keep type>> "symbol" = [ over "<anon>" >>last-name drop ] unless ;

: can-cont? ( evaluator -- ? ) [ tokens>> length ] [ extra-tokens>> concat length ] bi + 0 >  ;

: evaluator-<env> ( evaluator -- evaluator ) dup current-namespace>> clone <env> >>current-namespace ;

: evaluator-parent ( evaluator -- evaluator ) dup current-namespace>> parent>> [ "Tried to get the parent of a global enviorment" eval-error f ] unless* >>current-namespace ;

: eval-until-one ( evaluator -- evaluator result ) f [ over can-cont? [ [ eval-next ] unless* dup not ] when% ] loop ;

: is-rix-callable? ( type -- ? )
    {
        "func"
        "inl"
        "mac"
        "vari"
        "varimac"
        "varinl"
    } [ = ] with any? ;

: type-matches? ( value type -- ? ) [ type>> ] dip {
        { "any" [ drop t ] }
        { "callable" [ is-rix-callable? ] }
        [ = ]
    } case ;

: typecheck ( eval params types -- eval )
    [ [ 2length = ] 2check [ [ type-matches? ] 2map [ ] all? ] [ 2drop f ] if ] 2check
    [ 2drop ] [ [ [ type>> ] map ] dip [ unparse ] bi@ [ swap % " != " % % ] "" make eval-error ] if ;

: eval-one ( evaluator token -- evaluator result ) push-token eval-until-one ;

: flatten-flagged ( array -- array ) [ dup sequence? [ dup [ clone V{ } clone >>value [ dup flagged? [ val>> dup sequence? [ append ] [ suffix ] if ] [ suffix ] if ] reduce ] keep like ] when ] deep-map ;


DEFER: unquote-scan

: hash-unquote-scan ( evaluator hash -- evaluator hash ) value>> hash-to-seq "list" <val> "quote" <val> unquote-scan value>> seq-to-pairs parse-hashtable "hash" <val> ;

: (unquote-scan) ( evaluator val -- val )
    {
        { [ dup type>> "unquote" = ] [ value>> eval-one nip ] }
        { [ dup type>> "splice-unquote" = ] [ value>> eval-one flagged boa nip ] }
        { [ dup type>> "hash" = ] [ hash-unquote-scan nip ] }
        [ nip ]
    } cond
    ;

: unquote-scan ( evaluator val -- evaluator val )
    value>> dup sequence?
    [
        dupd [ (unquote-scan) ] with deep-map flatten-flagged
    ]
    [ dup type>> "hash" = [ hash-unquote-scan ] when ] if
    "quote" <val>
    ;

: eval-all ( evaluator -- evaluator ) [ dup tokens>> length 0 > ] [ eval-next push-results ] while ;

: eval-full ( evaluator -- evaluator ) [ [ dup can-cont? ] [ eval-next push-results ] while ] [ [ print-callstack ] dip rethrow ] recover ;

: eval-all-tokens ( tokens env -- evaluator ) <evaluator> eval-all ;

: extra-tokens>tokens ( eval -- eval ) dup extra-tokens>> pop >>tokens ;

: eval-tokens ( evaluator tokens -- evaluator result )
    over results>> clone [ [ dup tokens>> [ suffix! ] curry change-extra-tokens V{ } clone >>tokens ] dip
                           push-tokens eval-all extra-tokens>tokens ] dip [ [ last ] dip ] curry change-results swap ;
: eval-until-semi ( evaluator -- evaluator results ) V{ } clone [ over can-cont? [ swap pop-token [ type>> "semicolon" = not ] 1check [ eval-one swap [ suffix! ] dip t ] [ drop f ] if swapd ] when% ] loop  ;

: not-reached-target? ( target evaluator -- ?  ) [ results>> length > ] 1check [ can-cont? [ "Expected expression but got nothing" eval-error f ] unless* ] [ drop f ] if ;

: eval-to-target ( evaluator target -- evaluator )
    over results>> length + swap
    [ 2dup not-reached-target? ]
    [ eval-until-one push-results ] while nip ;

: target-to-results ( evaluator target -- evaluator results )
    V{ } clone [ [ length > ] 2check [ pick can-cont? [ "Expected expression but got nothing" eval-error f ] unless* ] when% ]
    [ [ eval-until-one ] 2dip rot suffix! ] while nip ;

: with-env ( evaluator quot -- evaluator ) [ evaluator-<env> ] dip call evaluator-parent ; inline

: env-deep-clone ( env -- env ) clone [ clone [ [ clone ] bi@ ] assoc-map ] change-bindings dup parent>> [ env-deep-clone >>parent ] when* ;

M: func clone { [ types>> ] [ env>> env-deep-clone ] [ param-names>> ] [ quot>> ] [ body>> clone ] } cleave func boa ;

: make-macro ( evaluator -- evaluator macro )
     dup
     [ current-namespace>> parent>> env-deep-clone ] [ "params" get-value [ length "any" <array> ] keep ] [ "body" get-value* ] tri [ swap ] 2dip
     [ dup sequence? not [ 1vector ] when f "return" <val> suffix eval-tokens dup sequence? [ push-tokens ] [ push-token ] if ] swap <macro> "quote" <val> ;

: make-inl ( evaluator -- evaluator func )
    dup [ "params" get-value ] [ "body" get-value* ] bi [ [ length "any" <array> ] keep ] dip [ dup sequence? not [ 1vector ] when f "return" <val> suffix push-tokens f ] swap inl boa "inl" <val> "quote" <val> ;

: evaluator-callstack-push ( name evaluator params func -- evaluator params func ) [ [ swap ] 2dip callstack-push ] 3keep ;

: with-env-up ( eval quot: ( ..a eval -- ..b eval ) -- ..b eval ) over current-namespace>> [ [ dup parent>> [ nip ] when* ] change-current-namespace ] 2dip [ call ] dip >>current-namespace ; inline
! eval types env params quot body
: make-func ( evaluator -- evaluator func )
    dup
    [ current-namespace>> parent>> env-deep-clone ] [ "params" get-value [ length "any" <array> ] keep ] [ "body" get-value* ] tri [ swap ] 2dip
    [ dup sequence? not [ 1vector ] when f "return" <val> f "scopeup" <val> [ suffix ] bi@ push-tokens f ] swap func boa "func" <val> "quote" <val> ;

: change-global-env ( env quot -- env ) swap [ parent>> ] 1check [ [ swap change-global-env ] change-parent ] [ swap call( env -- env ) ] if ;

: find-global-env ( env -- global ) [ parent>> ] 1check [ nip find-global-env ] when* ;

: with-global-env ( eval quot -- eval ) [ [ dup find-global-env ] change-current-namespace ] dip rot [ call ] dip >>current-namespace ; inline

: eval-set-global ( evaluator symbol value -- evaluator ) [ [ spin env-set ] 2curry change-global-env ] 2curry change-current-namespace ;

: get-paths ( -- paths ) "RIX-PATH" os-env [ ":" split ] [ { "~/.rix" } ] if* "." prefix ;

: find-file-path ( module-name -- file-path ) ".rix" append "/" prepend get-paths swap [ append ] curry map [ file-exists? ] find nip [ "Unknown module" eval-error f ] unless*  ;

: global-env ( -- env )
    f
    H{
        GENR: comb x y ;
        GENR: < x y ;
        GENR: > x y ;
        GENR: <= x y ;
        GENR: >= x y ;
        GENR: prn* val ;
        { SYM: + [ { "number" "number" } { SYM: x SYM: y } [ dup [ "x" get-value ] [ "y" get-value ] bi + "number" <val> ] <builtin> "adds two numbers" desc ] }
        { SYM: - [ { "number" "number" } { SYM: x SYM: y } [ dup [ "x" get-value ] [ "y" get-value ] bi - "number" <val> ] <builtin> "subtracts two numbers" desc ] }
        { SYM: * [ { "number" "number" } { SYM: x SYM: y } [ dup [ "x" get-value ] [ "y" get-value ] bi * "number" <val> ] <builtin> "multiplies two numbers" desc ] }
        { SYM: / [ { "number" "number" } { SYM: x SYM: y } [ dup [ "x" get-value ] [ "y" get-value ] bi / "number" <val> ] <builtin> "divides two numbers" desc ] }
        { SYM: number.< [ { "number" "number" } { SYM: x SYM: y } [ dup [ "x" get-value ] [ "y" get-value ] bi < "bool" <val> ] <builtin> "compares two numbers" desc ] }
        { SYM: number.> [ { "number" "number" } { SYM: x SYM: y } [ dup [ "x" get-value ] [ "y" get-value ] bi > "bool" <val> ] <builtin> "compares two numbers" desc ] }
        { SYM: number.<= [ { "number" "number" } { SYM: x SYM: y } [ dup [ "x" get-value ] [ "y" get-value ] bi <= "bool" <val> ] <builtin> "compares two numbers" desc ] }
        { SYM: number.>= [ { "number" "number" } { SYM: x SYM: y } [ dup [ "x" get-value ] [ "y" get-value ] bi >= "bool" <val> ] <builtin> "compares two numbers" desc ] }
        { SYM: = [ { "any" "any" } { SYM: x SYM: y } [ dup [ "x" get-value* ] [ "y" get-value* ] bi = "bool" <val> ] <builtin> "checks for equality" desc ] }
        { SYM: rest [ { "list" } { SYM: list } [ dup "list" get-value* 1 tail ] <builtin>
                       "returns all of the list but the first element" desc ] }
        { SYM: rdln [ { } { } [ readln [ "string" <val> ] [ f "bool" <val> ] if* ] <builtin> "reads a line from the input stream" desc ] }
        { SYM: rdch [ { } { } [ 1 read first "number" <val> ] <builtin> "reads a single character from the input stream" desc ] }
        { SYM: rdstr [ { "string" "list" } { SYM: string SYM: code } [ dup [ "code" get-value ] [ "string" get-value ] bi [ swap [ swap eval-tokens swap ] with-env-up swap ] with-string-reader ] <builtin>
                        "runs 'code' in an enviroment with the given string as an input stream, meaning that within 'code', (for example) every 'rdln' reads a line from the stream" desc ] }
        { SYM: str>num [ { "string" } { SYM: str } [ dup "str" get-value dec> [ "number" <val> ] [ f "bool" <val> ] if* ]  <builtin> "parses a string as a number. returns fal if that's not possible" desc ] }
        { SYM: num>str [ { "number" } { SYM: n } [ dup "n" get-value >dec "string" <val> ] <builtin> "turns a number into a base10 string" desc ] }
        { SYM: esc
          [ { "number" }
             { SYM: char } [ dup "char" get-value escape [ "number" <val> ] [ f "bool" <val> ] if* ] <builtin> "converts from a single character to an associated escaped character. for example, turns 'n' into a newline" desc ] }
        { SYM: first [ { "list" } { SYM: list } [ dup "list" get-value* first ] <builtin>
                         "returns the first element of a list" desc ] }
        ! { SYM: |=| $[ { "any" "any" } { SYM: x SYM: y } [ dup [ "x" get-value ] [ "y" get-value ] bi = "bool" <val> ] <builtin> "checks for loose equality (when two rix values have the same internal value)" desc ] }
        { SYM: and [ { "any" "any" } { SYM: x SYM: y } [ dup [ "x" get-value ] [ "y" get-value ] bi and "bool" <val> ] <builtin> "preforms 'and' on two bools" desc ] }
        { SYM: or [ { "any" "any" } { SYM: x SYM: y } [ dup [ "x" get-value ] [ "y" get-value ] bi or "bool" <val> ] <builtin> "preforms 'or' on two bools" desc ] }
        { SYM: nth [ { "list" "number" } { SYM: list SYM: n } [ dup [ "n" get-value ] [ "list" get-value ] bi nth ] <builtin> "gets the nth element of a list" desc ] }
        { SYM: len [ { "list" } { SYM: list } [ dup "list" get-value length "number" <val> ] <builtin> "gets the length of a list" desc ] }
        { SYM: map [ { "list" "callable" } { SYM: list SYM: func } [ dup [ "list" get-value ] [ "func" get-value* ] bi [ swap 2array eval-tokens ] curry map "list" <val> ] <builtin>
                      "applies 'func' to each element in 'list', returning a new list made up of all the return values" desc ] }
        { SYM: fbody [ { "callable" } { SYM: func } [ dup "func" get-value body>> ] <builtin> "gets the body of a function. primatives return [prim]" desc ] }
        { SYM: fparams [ { "callable" } { SYM: func } [ dup "func" get-value param-names>> "list" <val> ] <builtin> "gets the params of a function" desc ] }
        { SYM: lst>str [ { "list" } { SYM: list } [ dup "list" get-value [ value>> ] "" map-as "string" <val> ] <builtin> "converts a list to a string" desc ] }
        { SYM: at [ { "any" "any" } { SYM: key SYM: hash } [ dup [ "key" get-value* ] [ "hash" get-value ] bi at [ f "bool" <val> ] unless* ] <builtin> "gets the value at key stored within 'hash'" desc ] }
        { SYM: not [ { "any" } { SYM: x } [ dup "x" get-value not "bool" <val> ] <builtin> "preforms 'not' on a bool" desc ] }
        { SYM: inl [ { "list" "any" } { SYM: params SYM: body } [ make-inl push-token ] <builtin-macro> "creates a function with params and a body whose results may depend on the surrounding enviroment" desc ] } 
        { SYM: fn [ { "list" "any" } { SYM: params SYM: body } [ make-func push-token ] <builtin-macro> "creates a function with params and a body" desc ] }
        { SYM: mac [ { "list" "any" } { SYM: params SYM: body } ! 
                      [ make-macro push-token ] <builtin-macro>
                      "creates a function with params and a body that returns either a list or a single value that is pushed onto the token array" desc ] }
        { SYM: varinl [ { "list" "any" } { SYM: params SYM: body } [ make-inl [ "inline-variadic" >>type ] change-value push-token ] <builtin-macro>
                         "creates a function with params and a body whose results may depend on the surrounding enviroment. the last named param is a list of every value between the penultimate param and a ; token" desc ] } 
        { SYM: vari [ { "list" "any" } { SYM: params SYM: body } [ make-func [ "variadic" >>type ] change-value push-token ] <builtin-macro>
                       "creates a function with params and a body. the last named param is a list of every value between the penultimate param and a ; token" desc  ] }
        { SYM: varimac [ { "list" "any" } { SYM: params SYM: body }
                          [ make-macro [ "variadic-macro" >>type ] change-value push-token ] <builtin-macro>
                      "creates a function with params and a body that returns either a list or a single value that is pushed onto the token array. the last named param is a list of every token between the penultimate param and a ; token"
                                                                                                                                                                                                                                  desc ] }
        { SYM: typ? [ { "string" "any" } { SYM: type SYM: val } [ dup [ "type" get-value ] [ "val" get-value* ] bi type>> = "bool" <val> ] <builtin> "tests if a value is of a certain type" desc ] }
        { SYM: imps? [ { "generic" "any" } { SYM: gen SYM: val } [ dup [ "gen" get-value name>> ] [ "val" get-value* ] bi type>> "." append prepend over current-namespace>> env-get [ t ] when% "bool" <val> ]
                        <builtin> "tests if the type of this value implements a generic" desc ] }
        { SYM: evl [ { "any" } { SYM: val } [ dup "val" get-value* eval-one ] <builtin> "evaluates 'val'" desc ] }
        { SYM: setat [ { "any" "any" "any" } { SYM: key SYM: value SYM: hash } [ dup [ "value" get-value* ] [ "key" get-value* ] [ "hash" get-value clone ] tri [ set-at ] keep "hash" <val> ] <builtin>
                        "sets 'value' at 'key' within 'hash'" desc ] }
        { SYM: tpop [ { } { } [ pop-token ] <builtin> "pops from the tokenstack. should mainly be used with macros, as using with functions or inline functions may provide unexpected results" desc ] }
        { SYM: tpush [ { "any" } { SYM: val } [ dup "val" get-value* push-token f ] <builtin> "pushes to the tokenstack" desc ] }
        { SYM: tapp [ { "list" } { SYM: val } [ dup "val" get-value* push-tokens f ] <builtin> "concatinates a list onto the tokenstack" desc ] }
        { SYM: args [ { } { } [ command-line get ] <builtin> "gets command line arguments. the first element is the name of the rix file when run with 'rix file.rix'" desc ] }
        { SYM: fin [ { "string" "string" } { SYM: path SYM: encoding } [ dup [ "path" get-value ] [ "encoding" get-value { { "ascii" [ ascii ] } { "utf8" [ utf8 ] } { "utf16" [ utf16 ] } } case ] bi <file-reader> "stream" <val> ]
                      <builtin> "returns a file-reading input stream. supported values of 'encoding' are \"ascii\", \"utf8\" and \"utf16\"" desc ] }
        { SYM: fout [ { "string" "string" } { SYM: path SYM: encoding } [ dup [ "path" get-value ] [ "encoding" get-value { { "ascii" ascii } { "utf8" utf8 } { "utf16" utf16 } } case ] bi <file-writer> "stream" <val> ]
                       <builtin> "returns a file-writing (overrides current file contents) output stream. supported values of 'encoding' are \"ascii\", \"utf8\" and \"utf16\"" desc ] }
        { SYM: fapp [ { "string" "string" } { SYM: path SYM: encoding } [ dup [ "path" get-value ] [ "encoding" get-value { { "ascii" ascii } { "utf8" utf8 } { "utf16" utf16 } } case ] bi <file-appender> "stream" <val> ]
                      <builtin> "returns a file-appending output stream. supported values of 'encoding' are \"ascii\", \"utf8\" and \"utf16\"" desc ] }
        { SYM: consfn [ { "list" "list" } { SYM: params SYM: body } [ make-func value>> ] <builtin>
                         "constructs a function with params and a body. consfn is a function itself, meaning it evaluates its input parameters, so you can create a function with a runtime-computed definition" desc ] }
        { SYM: consinl [ { "list" "list" } { SYM: params SYM: body } [ make-inl value>> ] <builtin>
                          "constructs an inline function; see the description of 'consfn' for the difference between this and 'inl'" desc ] }
        { SYM: consmac [ { "list" "list" } { SYM: params SYM: body } [ make-macro value>> ] <builtin>
                          "constructs a macro; see the description of 'consfn' for the difference between this and 'mac'" desc ] }
        { SYM: consvarinl [ { "list" "list" } { SYM: params SYM: body } [ make-inl [ "inline-variadic" >>type ] change-value ] <builtin>
                         "constructs a variadic inline function; see the description of 'consfn' for the difference between this and 'varinl'" desc ] } 
        { SYM: consvari [ { "list" "list" } { SYM: params SYM: body } [ make-func [ "variadic" >>type ] change-value ] <builtin>
                       "constructs a variadic function; see the description of 'consfn' for the difference between this and 'vari'" desc  ] }
        { SYM: consvarimac [ { "list" "list" } { SYM: params SYM: body }
                          [ make-macro [ "variadic-macro" >>type ] change-value ] <builtin>
                          "constructs a variadic macro; see the description of 'consfn' for the difference between this and 'varimac'" desc ] }
        { SYM: istream [ { "stream" "list" } { SYM: stream SYM: code } [ dup [ "stream" get-value ] [ "code" get-value ] bi [ swap [ swap eval-tokens swap ] with-env-up ] curry with-input-stream swap ] <builtin>
                          "runs 'code' with the input stream remapped to 'stream'" desc ] }
        { SYM: ostream [ { "stream" "list" } { SYM: stream SYM: code } [ dup [ "stream" get-value ] [ "code" get-value ] bi [ swap [ swap eval-tokens swap ] with-env-up ] curry with-output-stream swap ] <builtin>
                          "runs 'code' with the output stream remapped to 'stream'" desc ] }
        { SYM: letfn [ { "list" "list" } { SYM: params SYM: body } [ make-func value>> push-token ] <builtin-macro>
                        "creates a function with params and a body and calls that function immediatly" desc ] }
        { SYM: ^^ [ { "number" "number" } { SYM: base SYM: power } [ dup [ "base" get-value ] [ "power" get-value ] bi ^ "number" <val> ] <builtin> "raises 'base' to the power 'power'" desc ] }
        { SYM: mod [ { "number" "number" } { SYM: x SYM: y } [ dup [ "x" get-value ] [ "y" get-value ] bi mod "number" <val> ] <builtin> "gets the modulus of two numbers" desc ] }
        { SYM: sqrt [ { "number" } { SYM: x } [ dup "x" get-value sqrt "number" <val> ] <builtin> "gets the square root of a number" desc ] }
        { SYM: prn [ { "any" } { SYM: val } [ dup "val" get-value* [ dup type>> "string" = [ value>> "symbol" <val> ] when pprint-rix-value "\n" write ] keep ] <builtin> "prints anything, inserting a newline afterwards" desc ] }
        { SYM: wrt [ { "any" } { SYM: val } [ dup "val" get-value* [ dup type>> "string" = [ value>> "symbol" <val> ] when pprint-rix-value ] keep  ] <builtin> "prints anything" desc ] }
        { SYM: pprn [ { "any" } { SYM: val } [ dup "val" get-value* [ pprint-rix-value "\n" write ] keep ] <builtin> "prints anything, leaving strings in their literal representation and inserting a newline afterwards" desc ] }
        { SYM: pwrt [ { "any" } { SYM: val } [ dup "val" get-value* [ pprint-rix-value ] keep ] <builtin> "prints anything, leaving strings in their literal representation" desc ] }
        { SYM: str>lst [ { "string" } { SYM: string } [ dup "string" get-value [ "number" <val> ] V{ } clone map-as "list" <val> ] <builtin> "converts a string to a list" desc ]  }
        { SYM: incl [ { "symbol" } { SYM: name } [ dup "name" get-value [ include-file ] keep "symbol" <val> swap eval-set-global ] <builtin-macro> "includes the module specified by 'name'" desc ] }
        { SYM: imp [ { "symbol" } { SYM: name } [ dup "name" get-value include-file value>> [ eval-set-global ] assoc-map ] <builtin-macro>
                      "imports the module specified by 'name', defining everything exported in the global namespace, so you don't have to use name@module syntax to access the things within the module" desc ] }
        { SYM: if [ { "any" "any" "any" } { SYM: cond SYM: body1 SYM: body2 }
                     [ dup [ "cond" get-value ] [ "body1" get-value* ] [ "body2" get-value* ] tri ? dup type>> "list" = [ value>> push-tokens f ] when ] <builtin>
                     "based on the boolean cond, picks either body1 or body2 and evaluates it" desc ] }
        { SYM: typreq [ { "list" "callable" } { SYM: list SYM: func } [ dup [ "func" get-value* ] [ "list" get-value ] bi [ value>> ] map [ >>types ] curry change-value ] <builtin>
                       "sets a list of required types for this function. 'any' means any type can be inputted, and 'callable' means that functions, inlines, macros, variadics etc. can all be used" desc ] }
        { SYM: ifdo [ { "any" "any" } { SYM: cond SYM: body }
                       [ dup [ "body" get-value* ] [ "cond" get-value ] bi [ dup type>> "list" = [ value>> push-tokens ] [ push-token ] if f ] [ drop f "nil" <val> ] if ] <builtin>
                       "if the boolean cond is true, evaluates the body" desc ] }
        { SYM: typ [ { "any" "string" } { SYM: val SYM: type } [ dup [ "val" get-value ] [ "type" get-value ] bi <val> ] <builtin>
                      "sets the type of the value to the given string type. note that sometimes this will cause errors because the types are incompatible (such as if you set a number to type 'symbol')" desc ] }
        { SYM: scop [ { "list" "any" } { SYM: code SYM: hash } [ dup [ "code" get-value ] [ "hash" get-value ] bi swapd [ [ <env> ] dip >>bindings ] curry change-current-namespace swap f "scopeup" <val> suffix eval-tokens ] <builtin>
                       "evaluates 'code' within a new scope defined with the keys and values in 'hash'" desc ] }
        { SYM: set [ { "symbol" "any" } { SYM: name SYM: value } [ dup [ "name" get-value* ] [ "value" get-value* ] bi pick current-namespace>> parent>> [ swapd env-set drop ] keepd ] <builtin-inl> "defines a variable" desc ] }
        { SYM: genr [ { "symbol" "list" } { SYM: sym SYM: params } [ dup [ "sym" get-value* ] [ "params" get-value* ] bi [ genr boa "generic" <val> ] keepd swap [ eval-set-global ] keep ] <builtin-inl>
                       "creates a new generic with the given name and params (and assigns it to that name within the global scope)" desc ]  }
        { SYM: gdesc [ { "any" } { SYM: value } [ dup "value" get-value* description>> "string" <val> ] <builtin> "gets the description of a value and returns it" desc ] }
        { SYM: pdesc [ { "any" } { SYM: value } [ dup "value" get-value* [ description>> print ] keep ] <builtin> "gets the description of a value and prints it" desc ] }
        { SYM: desc [ { "string" "any" } { SYM: desc SYM: value } [ dup [ "value" get-value* ] [ "desc" get-value ] bi desc ] <builtin> "sets the description of a value" desc ] }
        { SYM: nxt [ { } { } [ eval-until-one ] <builtin> "evaluates tokens until a value is returned" desc ] }
        { SYM: glob [ { "dec" } { SYM: dec } [ [ "dec" get-value* ] keep [ swap rix-eval swap ] with-global-env swap push-token ] <builtin-macro> "takes a declaration and evaluates it within the global namespace" desc ] }
        { SYM: exp [ { "dec" } { SYM: dec } [ dup "dec" get-value [ expr>> eval-tokens ] [ symbol>> "symbol" <val> ] bi rot [ to-export>> set-at ] 3keep spin [ eval-set-global ] keep push-results ]
                      <builtin-macro> "takes a declaration and sets it for exporting. note that inline functions should not be exported" desc ] }
        { SYM: genexp [ { "symbol" "list" } { SYM: sym SYM: params }
                         [ dup [ "sym" get-value* ] [ "params" get-value* ] bi [ genr boa "generic" <val> ] keepd rot [ to-export>> set-at ] 3keep spin [ eval-set-global ] keep push-results ]
                         <builtin-inl> "creates a new generic with the given name and params, exports it, and assigns it to that name within the global scope" desc ] }
        { SYM: cstack [ { } { } [ dup callstack>> [ [ params>> ] [ name>> ] bi prepend "parens" <val> ] map "list" <val> ] <builtin> "returns the callstack" desc ] }
        { SYM: quot [ { "any" } { SYM: val } [ dup "val" get-value* "quote" <val> ] <builtin> "quotes a value" desc ] }
        { SYM: uquot [ { "any" } { SYM: val } [ dup "val" get-value* "unquote" <val> ] <builtin> "makes a value an unquote" desc ] }
        { SYM: suquot [ { "any" } { SYM: val } [ dup "val" get-value* "splice-unquote" <val> ] <builtin> "makes a value a splice-unquote" desc ] }
        { SYM: err [ { "string" } { SYM: str } [ dup "str" get-value \ eval-error boa "error" <val> push-token f ]  <builtin> "throws an error with the given string as a message" desc ] }
        { SYM: new-err [ { "string" } { SYM: str } [ dup "str" get-value \ eval-error boa "error" <val> ]  <builtin> "returns an error with the given string as a message" desc ]  }
        { SYM: try [ { "list" "callable" } { SYM: list SYM: catch } [ dup [ "catch" get-value* ] [ "list" get-value* ] bi [ nip eval-tokens ] [ "error" <val> "quote" <val> 3array >vector eval-tokens ] recover ] <builtin>
                      "evaluates the list. if there's an error, calls the function with the list and the error (in that order). errors automatically throw themselves when evaluated, so handle them carefully" desc ] }
        { SYM: rng [ { "number" "number" } { SYM: from SYM: to }
                      [ dup [ "from" get-value ] [ "to" get-value ] bi [a..b] >vector [ "number" <val> ] map "list" <val> ] <builtin> "returns a list with numbers that go from 'from' to 'to', including both 'from' and 'to'" desc ]  }
        { SYM: prnted [ { "list" } { SYM: list } [ dup "list" get-value [ swap [ swap eval-tokens drop ] with-env-up ] with-string-writer "string" <val> ]
                         <builtin> "runs 'list', returning a string containing everything that was printed during its running" desc ] }
        { SYM: assert= [ { "any" "any" } { SYM: one SYM: two } [ dup [ "one" get-value* ] [ "two" get-value* ] bi [ = ] 2check [ drop ]
                                                   [ [ [ pprint-rix-value ] with-string-writer ] bi@ " != " prepend append "Assert failed: " prepend eval-error ] if ]
                          <builtin> "throws an error if 'one' and 'two' aren't equal" desc ] }
        { SYM: os [ { } { } [ os name>> "string" <val> ] <builtin> "gets the current operating system" desc ] }
        { SYM: let [ { "hash" "list" } { SYM: hash SYM: code } [
                          dup [ "code" get-value ] [ "hash" get-value ] bi swapd [ [ <env> ] dip >>bindings ] curry change-current-namespace swap f "scopeup" <val> suffix eval-tokens ] <builtin>
          "identical to scop, but with the parameters swapped" desc ] }
    } clone [ [ clone ] [ dup quotation? [ call( -- val ) ] when ] bi* ] assoc-map rix-env boa clone
    ;


M: rix-value pprint-rix-value dup value>> hashtable? [ dup type>> "hash" = not [ [ type>> write ] [ value>> "hash" <val> pprint-rix-value ] bi ] [ value>> pprint ] if ] [ value>> pprint ] if ;

: map-acc ( x seq quot -- x seq' ) collector [ each ] dip ; inline

: eval-str ( str -- result ) lex-str global-env <evaluator> eval-full results>> dup empty? [ drop f "nil" <val> ] [ last ] if ;

: include-file ( module-name -- module ) find-file-path utf8 file-contents lex-str global-env <evaluator> eval-full to-export>> "module" <val> ;

: compress-env ( env -- env ) dup parent>> [ compress-env swap [ bindings>> ] bi@ assoc-union f swap rix-env boa ] when* ;
: env-set-parent ( env parent -- env ) swap [ 2dup = [ drop <env> ] [ swap [ >>parent ] when* ] if ] curry change-parent ;
: load-env ( env1 env2 -- env ) compress-env env-set-parent ;

: global-env>markdown ( -- )
    "| name | signature      | description |\n| --- | ------------------| ------------|" print global-env bindings>> [ [ "| " write [ pprint-rix-value " | " write ] bi@ ] keep description>> write " |" print ] assoc-each  ;

: tpopn ( eval n -- eval toks ) [ 1 - over pop-token nip ] collector [ [ dup 0 <= not ] swap while ] dip nip ;
: tpop-until-semi ( eval -- eval toks ) [ pop-token [ type>> "semicolon" = not ] 1check [ drop f f ] unless* swap ] collector [ loop ] dip ;
PRIVATE>
: eval-str-to-str ( str -- str ) eval-str [ pprint-rix-value ] with-string-writer  ;

: repl ( -- )
        "Welcome to the rix repl! (" write rix-version ")" append print
        "Press Control+D to quit" print V{ } clone global-env <evaluator>
        [
            "rix> " write flush readln
            [
                [ lex-str >>tokens eval-full V{ } clone >>callstack dup results>> dup empty? not [ last pprint-rix-value "\n" write t ] [ drop t ] if ] when*% ] [
                [ "error" <val> pprint-rix-value "\n" write ] with-string-writer
                COLOR: red
                foreground associate format
            ] recover
        ] loop drop ;

: run-rix ( -- ) command-line get-global dup length 0 > [ [ 4 cut* ".rix" = [ ".rix" append utf8 file-contents [ eval-str-to-str ] [ . ] recover drop ] [ "file name must end in .rix" eval-error ] if ] each ] [ drop repl ] if ;


! hello@world
RIX-TYPE: rix-module-call value>>
[ symbol>> >>last-name ] keep
[ module>> over current-namespace>> [ env-get ] curry ?transmute [ value>> "unknown module: " prepend eval-error ] unless value>> ] keep symbol>> swap [ at ] curry ?transmute
[ value>> "unknown symbol: " prepend eval-error ] unless push-token f ;
M: rix-module-call pprint-rix-value value>> [ symbol>> value>> write ] [ "@" write module>> value>> write ] bi ;

! $hello@world
RIX-TYPE: rix-module-resolve value>>
[ symbol>> >>last-name ] keep
[ module>> over current-namespace>> [ env-get ] curry ?transmute [ value>> "unknown module: " prepend eval-error ] unless value>> ] keep symbol>> swap [ at ] curry ?transmute
[ value>> "unknown symbol: " prepend eval-error ] unless ;
M: rix-module-resolve pprint-rix-value "$" write value>> [ symbol>> value>> write ] [ "@" write module>> value>> write ] bi ;

RIX-TYPE: rix-stream ;
M: rix-stream pprint-rix-value drop "(stream)" write ;

! 1798
RIX-TYPE: rix-number ;

RIX-TYPE: rix-semicolon drop f ;
M: rix-semicolon pprint-rix-value drop ";" write ;

RIX-TYPE: rix-scopeup drop evaluator-parent f ;
M: rix-scopeup pprint-rix-value drop "^" write ;

RIX-TYPE: rix-return drop evaluator-parent callstack-pop f ;
M: rix-return pprint-rix-value drop "ret" write ;

RIX-TYPE: rix-scopedown drop evaluator-<env> f  ;
M: rix-scopedown pprint-rix-value drop "|" write ;

! "Hello, World"
RIX-TYPE: rix-string ;

RIX-TYPE: rix-hash ;
M: rix-hash pprint-rix-value value>> V{ } [ [ suffix ] bi@ ] assoc-reduce "{" write dup empty? not [ unclip pprint-rix-value ] when [ " " write pprint-rix-value ] each "}" write ;

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

! $symbol

RIX-TYPE: rix-resolve value>> [ >>last-name ] keep "symbol" <val> over current-namespace>> [ env-get ] curry ?transmute [ value>> "unknown symbol: " prepend eval-error ] unless ;
M: rix-resolve pprint-rix-value "$" write value>> write ;

! (evaluated-immediatly 1 1)
RIX-TYPE: rix-parens value>> eval-tokens ;
M: rix-parens pprint-rix-value "(" write value>> dup empty? not [ unclip pprint-rix-value ] when [ " " write pprint-rix-value ] each ")" write ;
INSTANCE: rix-parens rix-sequence

! symbol
RIX-TYPE: rix-symbol [ value>> >>last-name ] keep over current-namespace>> [ env-get ] curry ?transmute [ value>> "unknown symbol: " prepend eval-error ] unless push-token f ;
M: rix-symbol pprint-rix-value value>> write ;

! tru
RIX-TYPE: rix-bool ;
M: rix-bool pprint-rix-value value>> "tru" "fal" ? write ;

! nil
RIX-TYPE: rix-nil ;
M: rix-nil pprint-rix-value drop "nil" write ;

! foo: bar;
RIX-TYPE: rix-dec value>> [ symbol>> "symbol" <val> ] [ expr>> ] bi swapd eval-tokens rot [ [ rot env-set ] 2curry change-current-namespace ] keepd ;
M: rix-dec pprint-rix-value value>> [ symbol>> write ":" write ] [ expr>> [ " " write pprint-rix-value ] each ] bi  ;

! inl [x y] [+ x y]
RIX-TYPE: rix-inl
  [ evaluator-<env> [ last-name>> ] keep ] dip
  [ value>> param-names>> length target-to-results ] keep evaluator-callstack-push
  [ value>> types>> typecheck ] 2keep swapd
  [ value>> param-names>> [ tuck current-namespace>> ] dip swap [ rot env-set ] 2reduce >>current-namespace ] keep value>> [ body>> ] [ quot>> ] bi call( evaluator body -- evaluator result/f  )
  ;
M: rix-inl pprint-rix-value value>> "inl " write param-names>> "list" <val> pprint-rix-value ;

! fn [x y] [+ x y]
! eval self
RIX-TYPE: rix-func
  [ [ last-name>> ] keep ] dip
  [ value>> param-names>> length target-to-results ] keep evaluator-callstack-push
  [ value>> types>> typecheck ] 2keep swapd
  [ value>> env>> [ [ <env> ] dip load-env ] curry change-current-namespace ] keep
  [ value>> param-names>> [ tuck current-namespace>> ] dip swap [ rot env-set ] 2reduce >>current-namespace ] keep
  value>> [ body>> ] [ quot>> ] bi call( evaluator body -- evaluator result/f  )
  ;
M: rix-func pprint-rix-value value>> "fn " write param-names>> "list" <val> pprint-rix-value ;

! mac [x y] [+ x y]
RIX-TYPE: rix-macro
  [ [ last-name>> ] keep ] dip
  [ value>> param-names>> length tpopn ] keep evaluator-callstack-push
  [ value>> types>> typecheck ] 2keep swapd
  [ value>> env>> [ [ <env> ] dip load-env ] curry change-current-namespace ] keep
  [ value>> param-names>> [ tuck current-namespace>> ] dip swap [ rot env-set ] 2reduce >>current-namespace ] keep
  value>> [ body>> ] [ quot>> ] bi call( evaluator body -- evaluator ) f ;
M: rix-macro pprint-rix-value value>> "mac " write param-names>> "list" <val> pprint-rix-value ;

! vari [x ys] '[,x @ys]
RIX-TYPE: rix-variadic
  [ [ last-name>> ] keep ] dip
  [ value>> param-names>> length 1 - target-to-results [ eval-until-semi "list" <val> ] dip swap suffix ] keep evaluator-callstack-push
  [ value>> types>> typecheck ] 2keep swapd
  [ value>> env>> [ [ <env> ] dip load-env ] curry change-current-namespace ] keep
  [ value>> param-names>> [ tuck current-namespace>> ] dip swap [ rot env-set ] 2reduce >>current-namespace ] keep
  value>> [ body>> ] [ quot>> ] bi call( evaluator body -- evaluator result/f  )
;
M: rix-variadic pprint-rix-value value>> "vari " write param-names>> unclip-last clone [ "..." append ] change-value suffix "list" <val> pprint-rix-value ;

! varimac [x ys] '[+ ,x ,(nth ys 0)]
RIX-TYPE: rix-variadic-macro
  [ [ last-name>> ] keep ] dip
  [ value>> param-names>> length 1 - tpopn [ tpop-until-semi "list" <val> ] dip swap suffix ] keep evaluator-callstack-push
  [ value>> types>> typecheck ] 2keep swapd
  [ value>> env>> [ [ <env> ] dip load-env ] curry change-current-namespace ] keep
  [ value>> param-names>> [ tuck current-namespace>> ] dip swap [ rot env-set ] 2reduce >>current-namespace ] keep
  value>> [ body>> ] [ quot>> ] bi call( evaluator body -- evaluator ) f ;
M: rix-variadic-macro pprint-rix-value value>> "varimac " write param-names>> "list" <val> pprint-rix-value ;

! varinl  [x ys] '[,x @ys]
RIX-TYPE: rix-inline-variadic
  [ evaluator-<env> [ last-name>> ] keep ] dip
  [ value>> param-names>> length target-to-results [ eval-until-semi "list" <val> ] dip swap suffix ] keep evaluator-callstack-push
  [ value>> types>> typecheck ] 2keep swapd
  [ value>> param-names>> [ tuck current-namespace>> ] dip swap [ rot env-set ] 2reduce >>current-namespace ] keep value>> [ body>> ] [ quot>> ] bi call( evaluator body -- evaluator result/f )
  ;
M: rix-inline-variadic pprint-rix-value value>> "varinl " write param-names>> "list" <val> pprint-rix-value ;

! ,something
RIX-TYPE: rix-unquote ;
M: rix-unquote pprint-rix-value "," write value>> pprint-rix-value ;

! @something
RIX-TYPE: rix-splice-unquote ;
M: rix-splice-unquote pprint-rix-value "@" write value>> pprint-rix-value ;

! genr 'foo [x y]
RIX-TYPE: rix-generic
   value>> [ eval-until-one swap ] dip
   pick [ name>> value>> "." prepend ] [ type>> ] bi* prepend "symbol" <val> rot 2array >vector push-tokens f ;
M: rix-generic pprint-rix-value "genr '" write [ value>> name>> value>> write ] [ " " write value>> params>> pprint-rix-value ] bi ;

MAIN: run-rix
