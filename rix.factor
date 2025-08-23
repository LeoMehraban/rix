! Copyright (C) 2024 Your name.
! See https://factorcode.org/license.txt for BSD license
USING: kernel generic parser make assocs command-line ranges literals tools.continuations namespaces serialize arrays environment lexer system prettyprint splitting classes.tuple colors hashtables continuations sequences.deep prettyprint.custom prettyprint.sections  words classes.predicate quotations accessors vectors classes.parser math math.functions sequences combinators classes combinators.smart unicode strings io io.styles io.files io.encodings.utf8 io.streams.string math.parser combinators.short-circuit strings.parser summary io.encodings.ascii io.encodings.utf16 io.pathnames io.directories rix.lexer rix.common tools.time sorting kernel.private delegate delegate.protocols ;
IN: rix

DEFER: rix-eval
! TODO: make it so module names don't conflict with existing names
<PRIVATE

DEFER: callstack-pop
SYMBOL: rix-callcount
SYMBOL: rix-profile
<<
GENERIC: pprint-rix-value ( value -- )
GENERIC: underlying-rix ( value -- rix-hash )
GENERIC: evaluates-to-something? ( evaluator value -- evaluator ? )
TUPLE: callstack-element name params func ;
CONSTANT: do-profiling? t

>>

<<
DEFER: include-file
DEFER: eval-until-one
DEFER: evaluator-parent
TUPLE: rix-env cache seq ;
: scan-rix-type ( -- type ) scan-new-class dup [ name>> 4 tail [ swap type>> = ] curry ] keep \ rix-value rot define-predicate-class ;
SYNTAX: RIX-TYPE: scan-rix-type drop ;
SYNTAX: GENR: scan-token dup [ "symbol" <val> ] bi@ ";" parse-tokens [ "symbol" <val> ] map "list" <val> genr boa "generic" <val> 2array suffix! ;
: <builtin> ( types param-names quot -- func ) 
    H{ } clone H{ } clone 1vector rix-env boa -rot [ [ evaluator-parent callstack-pop ] dip ] compose [ drop ] prepose V{ SYM: prim } "list" <val> func boa "func" <val> ;
: <builtin-inl> ( types param-names quot -- inl ) [ [ evaluator-parent callstack-pop ] dip ] compose [ drop ] prepose V{ SYM: prim } "list" <val> inl boa "inl" <val> ;
: <macro> ( types env param-names quot body -- macro ) func boa "macro" <val> ;
: <builtin-macro> ( types param-names quot -- macro )
    H{ } clone H{ } clone 1vector rix-env boa -rot [ evaluator-parent callstack-pop ] compose [ drop ] prepose V{ SYM: prim } "list" <val> func boa "macro" <val> ;

: <func> ( types param-names quot -- func ) [ drop ] prepend H{ } clone H{ } clone 1vector rix-env boa -rot V{ SYM: prim } "list" <val> func boa "func" <val> ;

M: rix-value pprint* [ "RIX:" text [ pprint-rix-value ] with-string-writer text ] [ "ERROR:" text nip class-of name>> text ] recover ;
CONSTANT: rix-version "0.07.0"
: desc ( val desc -- val ) >>description ;
>>
SYMBOL: rix-breakpoints

INSTANCE: rix-env sequence
M: rix-env length seq>> length ;
M: rix-env nth seq>> nth ;
M: rix-env set-nth 
    2dup length = ! elt n seq 
    [ pick dup assoc? [ assoc-empty? ] dwhen% [ H{ } clone >>cache ] unless [ [ set-nth ] keep ] change-seq drop ] 
    [ [ [ set-nth ] keep ] change-seq H{ } clone >>cache drop ] if ;
M: rix-env set-length seq>> set-length ;
M: rix-env lengthen seq>> lengthen ;
M: rix-env like drop H{ } clone swap >vector rix-env boa ;
M: rix-env equal? over rix-env? [ [ seq>> ] bi@ equal? ] [ 2drop f ] if ;

! value parent
: <env> ( parent -- env ) H{ } clone suffix ; inline
: env-get ( value env -- value/f ) [ cache>> ] keep [ <reversed> f -rot [ at nip dup ] with find 2drop ] curry cache ;
: env-set ( symbol value env -- env ) [ cache>> overd delete-at ] keep [ last set-at ] keep ; inline
TUPLE: evaluator tokens to-export current-namespace extra-tokens callstack last-name last-result nesting-count ;
M: evaluator clone 
    { 
        [ tokens>> clone ] [ to-export>> clone ] [ current-namespace>> clone ] [ extra-tokens>> clone ] [ callstack>> clone ] [ last-name>> clone ] [ last-result>> clone ] [ nesting-count>> ] 
    } cleave evaluator boa ;
! time name
: rix-benchmark ( ..a evaluator quot: ( ..a -- ..b ) -- ..b ) 
    do-profiling? [ swap callstack>> last name>> [ benchmark ] dip rix-profile [ H{ } clone ] initialize rix-profile [ [ [ [ + ] when* ] change-at ] keep ] change ] [ nip call ] if ; inline
: rix-callcount-inc ( last-name -- ) rix-callcount [ H{ } clone ] initialize rix-callcount [ [ [ [ 1 + ] [ 1 ] if* ] change-at ] keep ] change ;
: env>seq ( env -- seq ) seq>> [ "hash" <val> ] map reverse ;
M: evaluator underlying-rix [ {
            [ tokens>> [ "list" <val> ] [ f "nil" <val> ] if* SYM: tokens ,, ]
            [ to-export>> [ "hash" <val> ] [ f "nil" <val> ] if* SYM: to-export ,, ]
            [ current-namespace>> [ env>seq "list" <val> ] [ f "nil" <val> ] if* SYM: current-env ,, ] 
            [ extra-tokens>> [ [ "list" <val> ] map "list" <val> ] [ f "nil" <val> ] if*  SYM: extra-tokens ,, ] 
            [ callstack>> [ [ [ params>> ] [ name>> "symbol" <val> ] bi prefix "parens" <val> ] map "list" <val> ] [ f "nil" <val> ] if* SYM: callstack ,, ]
            [ last-name>> [ f "nil" <val> ] unless* "symbol" <val> SYM: last-name ,, ]
            [ last-result>> [ f "nil" <val> ] unless* SYM: last-result ,, ]
            [ nesting-count>> "number" <val> SYM: nesting-count ,, ]
        } cleave ] H{ } make ;
: valid-evaluation-function? ( val -- ? ) [ dup type>> "func" = [ value>> param-names>> length 2 = ] dwhen% ] when*% ;

M: rix-value evaluates-to-something?
    type>> ".eval" append "symbol" <val> over current-namespace>> env-get valid-evaluation-function?
    ;
: get-value ( evaluator string -- value ) "symbol" <val> swap current-namespace>> env-get value>> ;
: mac-get-value ( evaluator string -- value ) get-value value>> ;
: get-value* ( evaluator string -- value ) "symbol" <val> swap current-namespace>> env-get ;
: mac-get-value* ( evaluator string -- value ) get-value ;
: unclip! ( sequence -- rest first ) [ first ] [ 0 swap remove-nth! ] bi swap  ;
: prepend! ( seq1 seq2 -- seq1 ) [ reverse! ] bi@ [ append! ] keep [ reverse! ] bi@ drop ;
: prefix! ( seq elt -- seq ) 1array prepend! ;
: (pop-token) ( eval -- eval token ) [ unclip! swap ] change-tokens swap ;
: pop-token ( eval -- eval token ) dup tokens>> length 0 > [ (pop-token) ] [ [ dup pop dup length 0 > [ unclip -rot suffix! ] [ drop dup pop unclip -rot suffix! ] if ] change-extra-tokens swap ] if ;
: push-token ( eval token -- eval ) [ 1array prepend! ] curry change-tokens ;
: push-tokens ( eval tokens -- eval ) [ prepend! ] curry change-tokens ;
: peek-token ( eval -- eval token ) dup tokens>> length 0 > [ dup tokens>> first ] 
    [ dup extra-tokens>> last dup length 0 > [ first ] [ drop [ [ pop drop ] keep ] change-extra-tokens peek-token ] if ] if ;
: set-last-result ( eval result -- eval ) [ >>last-result ] when* ;

: <evaluator> ( tokens global -- evaluator ) H{  } clone swap V{  } dup [ clone ] bi@ "<anon>" f 0 evaluator boa ;
: callstack-push ( evaluator name params func -- ) callstack-element boa [ over push ] curry change-callstack drop ;
: callstack-pop ( evaluator -- evaluator ) [ dup length 0 = [ dup pop drop ] unless ] change-callstack ;
: print-callstack ( evaluator -- evaluator ) "callstack: " print dup callstack>> reverse [ "(" write
                                                                                             [ name>> write ]
                                                                                             [ params>> [ " " write pprint-rix-value ] each ]
                                                                                             bi ")" print ] each ;
: EVAL{ ( -- ) ;
: get-env-depth ( env -- depth ) length ;
! "EVAL{" text tokens>> [ [ pprint-rix-value ] with-string-writer text ] each "}" text
M: evaluator pprint-delims drop \ EVAL{ \ } ;
M: evaluator >pprint-sequence tokens>> ;
M: evaluator pprint* dup [ current-namespace>> get-env-depth pprint* ] [ nesting-count>> pprint* ] bi pprint-object ;
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

M: rix-decl underlying-rix [ symbol>> "symbol" <val> SYM: symbol associate ] [ expr>> "list" <val> SYM: expr associate ] bi assoc-union "hash" <val> ;
M: module-call underlying-rix [ symbol>> SYM: symbol associate ] [ module>> SYM: module associate ] bi assoc-union ;
M: func underlying-rix [
        {
            [ types>> "list" <val> SYM: types ,, ]
            [ env>> env>seq "list" <val> SYM: env ,, ]
            [ param-names>> "list" <val> SYM: param-names ,, ]
            ! [ quot>> SYM: quot ,, ]
            [ body>> "list" <val> SYM: body ,, ]
        } cleave
    ] H{ } make ;
M: inl underlying-rix [
        {
            [ types>> "list" <val> SYM: types ,, ]
            [ param-names>> "list" <val> SYM: param-names ,, ]
            ! [ quot>> SYM: quot ,, ]
            [ body>> "list" <val> SYM: body ,, ]
        } cleave
    ] H{ } make ;
M: genr underlying-rix [ name>> SYM: name associate ] [ params>> "list" <val> SYM: params associate ] bi assoc-union ;

: terminator? ( token -- ? ) type>> [ "return" = ] [ "endexpr" = ] bi or  ;

: should-rix-breakpoint? ( evaluator -- ? ) peek-token nip value>> rix-breakpoints [ { } ] initialize rix-breakpoints get-global index ;

: eval-next ( evaluator -- evaluator result ) 
    pop-token [ rix-eval ] keep type>> "symbol" = [ over "<anon>" >>last-name drop ] unless ;

: can-cont? ( evaluator -- ? ) [ tokens>> length ] [ extra-tokens>> concat length ] bi + 0 >  ;

: evaluator-<env> ( evaluator -- evaluator ) dup current-namespace>> clone <env> >>current-namespace ;

: evaluator-parent ( evaluator -- evaluator ) dup current-namespace>> but-last [ "Tried to get the parent of a global enviorment" eval-error f ] unless* H{ } clone >>cache >>current-namespace ;

: relivent-terminator? ( nesting-count evaluator -- nesting-count ? ) 
    [ nesting-count>> ] [ peek-token nip terminator? ] bi 
    [ [ 2dup = [ 2drop t ] [ > [ "bug with rix: nesting" eval-error ] when% ] if ] keepd swap ] [ drop f ] if ;

: collect-if ( quot: ( ... -- ... val/f ) -- quot: ( ... -- ... ) accumulator ) [ ] selector [ compose ] dip ; inline

: target-to-results ( evaluator target -- evaluator results ) 
    V{ } clone -rot [ [ nesting-count>> ] keep ] dip 
    [ 
        [ [ can-cont? ] [ 0 > ] bi* and ] 2keep rot 
    ]
    [ 
        [ 
            dup should-rix-breakpoint? [ break ] when 
            [ relivent-terminator? ] keep swap 
            [ pop-token swap [ swapd suffix! swap ] dip f ] 
            [ 
                eval-next [ set-last-result ] keep
                [ 
                    [ nesting-count>> 2dup = [ 2drop t ] [ > [ "bug with rix: nesting" eval-error ] when% ] if ] 2keep rot 
                ] dip and 
            ] if
        ] dip swap [ [ 1 - ] dip ] when*%
    ] collect-if 
    [ while nipd 0 > [ "Expected expression but got nothing" eval-error ] when swap push-tokens ] dip ;

: eval-until-one ( evaluator -- evaluator result ) 1 target-to-results first ;

: is-rix-callable? ( type -- ? )
    {
        "func"
        "inl"
        "macro"
        "variadic"
        "variadic-macro"
        "inline-variadic"
    } [ = ] with any? ;


: eval-one ( evaluator token -- evaluator result ) push-token eval-until-one ;

DEFER: hash-unquote-scan
DEFER: unquote-scan

: (unquote-scan) ( evaluator val -- evaluator )
    {
        { [ dup type>> "unquote" = ] [ value>> eval-one , ] }
        { [ dup type>> "splice" = ] [ value>> eval-one % ] }
        { [ dup type>> "hash" = ] [ hash-unquote-scan , ] }
        { [ dup sequence? ] [ "quote" <val> unquote-scan , ] }
        [ , ]
    } cond
    ;

: (unquote-scan)* ( evaluator val -- evaluator val )
    {
        { [ dup type>> "unquote" = ] [ value>> eval-one ] }
        { [ dup type>> "hash" = ] [ hash-unquote-scan ] }
        { [ dup sequence? ] [ "quote" <val> unquote-scan ] }
        [ ]
    } cond
    ;

: unquote-scan ( evaluator val -- evaluator val )
    dup type>> "quote" = [ "bug with rix: cannot call unquote-scan on a non-quote value" eval-error ] unless 
    dup value>> sequence? 
    [ [ value>> [ (unquote-scan) ] each ] over value>> make ] [ value>> (unquote-scan)* ] if
    ;

: hash-unquote-scan ( evaluator hash -- evaluator hash ) value>> [ rot [ -rot overd [ (unquote-scan)* nip ] 2bi@ ] keep -rot ] assoc-map "hash" <val> ;

: eval-all ( evaluator -- evaluator ) [ dup tokens>> length 0 > ] [ eval-next set-last-result ] while ;

: eval-full ( evaluator -- evaluator ) [ [ dup can-cont? ] [ eval-next set-last-result ] while ] [ [ print-callstack ] dip dup "error" <val> pprint-rix-value "\n" write rethrow ] recover ;

: eval-all-tokens ( tokens env -- evaluator ) <evaluator> eval-all ;

: extra-tokens>tokens ( eval -- eval ) dup extra-tokens>> dup length 0 > [ pop ] [ drop V{ } clone ] if >>tokens ;

: eval-expr ( evaluator -- evaluator result ) 
    pop-token drop [ nesting-count>> ] keep 
    [ [ dup can-cont? [ relivent-terminator? not ] dwhen% ] keep swap ] 
    [ dup should-rix-breakpoint? [ break ] when eval-next set-last-result ] while 
    [ last-result>> ] [ eval-next drop [ 1 + ] change-nesting-count ] bi swap nipd ;

: eval-tokens ( evaluator tokens -- evaluator result ) f "begnexpr" <val> prefix f "endexpr" <val> suffix push-tokens eval-expr ;

: eval-tokens-results ( evaluator tokens -- evaluator results ) 
    [ dup tokens>> [ swap [ push ] keep ] curry change-extra-tokens V{ } clone >>tokens ] dip push-tokens  
    [ dup tokens>> length 0 > ] [ dup should-rix-breakpoint? [ break ] when eval-next [ set-last-result ] keep ] collector [ while extra-tokens>tokens ] dip sift ;
: eval-until-semi ( evaluator -- evaluator results ) 
    [ dup [ can-cont? ] [ tokens>> first type>> "semicolon" = not ] bi and ] [ dup should-rix-breakpoint? [ break ] when eval-next [ set-last-result ] keep ] collect-if [ while ] dip
    [ dup tokens>> first type>> "semicolon" = [ "missing a semicolon" eval-error ] unless ] dip ;
: evaluator-save ( evaluator -- evaluator ) clone ;

: type= ( type1 type2 -- ? )
    {
        { "any" [ drop t ] }
        { "callable" [ is-rix-callable? ] }
        [ = ]
    } case ;

: type-matches? ( eval value type -- eval ? )
    [ [ type>> ] dip type= ] 2check
    [ 2drop t ]
    [
        dup "hashstruct" = [ drop value>> hashtable? ] [ "?" append "symbol" <val> swap [ over current-namespace>> env-get ] dip over
        [ over type>> "func" = [ [ swapd evaluates-to-something? swapd ] keep swap [ "quote" <val> ] when 2array eval-tokens value>> ] [ 2drop f ] if ] [ 2drop f ] if ] if
    ] if ;

: typecheck ( eval params types -- eval )
    [ value>> ] map [ [ 2length = ] 2check [ [ type-matches? ] 2map [ ] all? ] [ 2drop f ] if ] 2check
    [ 2drop ] [ [ [ type>> ] map ] dip [ unparse ] bi@ [ swap % " != " % % ] "" make eval-error ] if ;

: with-env ( evaluator quot -- evaluator ) [ evaluator-<env> ] dip call evaluator-parent ; inline

: env-deep-clone ( env -- env ) [ clone ] map clone ;

M: func clone { [ types>> ] [ env>> env-deep-clone ] [ param-names>> ] [ quot>> ] [ body>> clone ] } cleave func boa ;

: make-macro ( evaluator is-mac? -- evaluator macro )
    over
    [ current-namespace>> but-last H{ } clone >>cache ] 
    [ pick [ "params" mac-get-value ] [ "params" get-value ] if [ length TYP: any <array> ] keep ] 
    [ [ rot ] 2dip rot [ "body" mac-get-value* ] [ "body" get-value* ] if ] tri [ swap ] 2dip
    [ dup sequence? not [ 1vector ] when f "return" <val> suffix f "begnexpr" <val> prefix push-tokens eval-expr dup sequence? [ push-tokens ] [ push-token ] if ] swap <macro> "quote" <val> ;

: make-inl ( evaluator is-mac? -- evaluator func )
    over
    [ over [ "params" mac-get-value ] [ "params" get-value ] if ] 
    [ rot [ "body" mac-get-value* ] [ "body" get-value ] if ] bi [ [ length TYP: any <array> ] keep ] dip
    [ dup sequence? not [ 1vector ] when f "return" <val> suffix f "begnexpr" <val> prefix push-tokens f ] swap inl boa "inl" <val> "quote" <val> ;

: evaluator-callstack-push ( name evaluator params func -- evaluator params func ) [ [ swap ] 2dip callstack-push ] 3keep ;

: compress-env ( env -- env ) [ <reversed> H{ } clone [ swap assoc-union ] reduce 1vector ] change-seq ;

: env-set-parent ( env parent -- env ) over empty? not [ [ unclip-last swap ] dip append swap suffix ] [ nip ] if ;
: env-parent-union ( env -- env ) dup length 1 > [ 2 cut* first2 swap assoc-union suffix ] when ;
: load-env ( env1 env2 -- env ) compress-env env-set-parent env-parent-union ;

: with-env-up ( eval quot: ( ..a eval -- ..b eval ) -- ..b eval ) 
    over current-namespace>> [ [ dup length 1 > [ but-last H{ } clone >>cache ] [ "Tried to get the parent of a global environment" eval-error ] if ] change-current-namespace ] 2dip 
    [ call ] dip last [ suffix ] curry change-current-namespace ; inline
! eval types env params quot body
! eval body params type parent
: make-func ( evaluator is-macro? -- evaluator func )
    over
    [ current-namespace>> but-last env-deep-clone H{ } clone >>cache ] 
    [ pick [ "params" mac-get-value ] [ "params" get-value ] if [ length TYP: any <array> ] keep ] 
    [ [ rot ] 2dip rot [ "body" mac-get-value* ] [ "body" get-value* ] if ] tri [ swap ] 2dip
    [ dup sequence? not [ 1vector ] when f "return" <val> suffix f "begnexpr" <val> prefix push-tokens f ] swap func boa "func" <val> "quote" <val> ;

: change-global-env ( env quot -- env ) 0 -rot change-nth ; inline

: find-global-env ( env -- global ) [ first 1vector ] keep like ;

: find-specific-env ( env pred: ( ... env -- ... ? ) -- env ) 
    [ find nip 1vector ] keepd like ; inline

: with-global-env ( eval quot -- eval ) [ [ dup find-global-env ] change-current-namespace ] dip rot [ call ] dip >>current-namespace ; inline

: with-specific-env ( eval pred quot -- eval ) [ [ [ find-specific-env ] keepd swap ] curry change-current-namespace ] dip rot [ call ] dip >>current-namespace ; inline

: eval-set-global ( evaluator symbol value -- evaluator ) spin [ current-namespace>> first set-at ] keep ;

: eval-tokens-with-env-up ( evaluator tokens -- evaluator result ) swap [ swap eval-tokens swap ] with-env-up swap ;

: call-rix-function ( evaluator func args -- evaluator result ) pick [ over evaluates-to-something? nip [ "quote" <val> ] when ] curry map swap prefix eval-tokens ;

: call-rix-function-with-env-up ( evaluator func args -- evaluator result ) pick [ over evaluates-to-something? nip [ "quote" <val> ] when ] curry map swap prefix eval-tokens-with-env-up  ; 

: closure? ( val -- ? ) type>> { "func" "variadic" "macro" "variadic-macro" } [ = ] with any? ;

MEMO: rix-at ( key hashtable -- value/f ) { hashtable } declare at ; inline

: global-env ( -- env )
    H{
        GENR: comb x y ;
        GENR: prn* val ;
        { SYM: + [ t "+" <val> "adds two numbers" desc ] }
        { SYM: - [ t "-" <val> "subtracts two numbers" desc ] }
        { SYM: * [ t "*" <val> "multiplies two numbers" desc ] }
        { SYM: / [ t "/" <val> "divides two numbers" desc ] }
        { SYM: < [ { TYP: number TYP: number } { SYM: x SYM: y } [ dup [ "x" get-value ] [ "y" get-value ] bi < "bool" <val> ] <builtin> "compares two numbers" desc ] }
        { SYM: > [ { TYP: number TYP: number } { SYM: x SYM: y } [ dup [ "x" get-value ] [ "y" get-value ] bi > "bool" <val> ] <builtin> "compares two numbers" desc ] }
        { SYM: <= [ { TYP: number TYP: number } { SYM: x SYM: y } [ dup [ "x" get-value ] [ "y" get-value ] bi <= "bool" <val> ] <builtin> "compares two numbers" desc ] }
        { SYM: >= [ { TYP: number TYP: number } { SYM: x SYM: y } [ dup [ "x" get-value ] [ "y" get-value ] bi >= "bool" <val> ] <builtin> "compares two numbers" desc ] }
        { SYM: = [ { TYP: any TYP: any } { SYM: x SYM: y } [ dup [ "x" get-value* ] [ "y" get-value* ] bi = "bool" <val> ] <builtin> "checks for equality" desc ] }
        { SYM: nop [ f "nop" <val> ] }
        { SYM: rest [ { TYP: list } { SYM: list } [ dup "list" get-value* 1 tail ] <builtin>
                       "returns all of the list but the first element" desc ] }
        { SYM: rdln [ { } { } [ readln [ "string" <val> ] [ f "nil" <val> ] if* ] <builtin> "reads a line from the input stream" desc ] }
        { SYM: rdch [ { } { } [ 1 read first "number" <val> ] <builtin> "reads a single character from the input stream" desc ] }
        { SYM: rdn [ { TYP: number } { SYM: n } [ dup "n" get-value read first "number" <val> ] <builtin> "reads a single character from the input stream" desc ] }
        { SYM: rdstr [ { TYP: string TYP: list } { SYM: string SYM: code } [ dup [ "code" get-value ] [ "string" get-value ] bi [ eval-tokens ] with-string-reader ] <builtin>
                        "runs 'code' in an enviroment with the given string as an input stream, meaning that within 'code', (for example) every 'rdln' reads a line from the stream" desc ] }
        { SYM: string>number [ { TYP: string } { SYM: str } [ dup "str" get-value dec> [ "number" <val> ] [ f "bool" <val> ] if* ]  <builtin> "parses a string as a number. returns fal if that's not possible" desc ] }
        { SYM: number>string [ { TYP: number } { SYM: n } [ dup "n" get-value >dec "string" <val> ] <builtin> "turns a number into a base10 string" desc ] }
        { SYM: esc
          [ { TYP: number }
             { SYM: char } [ dup "char" get-value escape [ "number" <val> ] [ f "bool" <val> ] if* ] <builtin> "converts from a single character to an associated escaped character. for example, turns 'n' into a newline" desc ] }
        { SYM: first [ { TYP: list } { SYM: list } [ dup "list" get-value* first ] <builtin>
                       "returns the first element of a list" desc ] }
        ! { SYM: abs [ { TYP: number } { SYM: n } [ dup "n" get-value abs "number" <val> ] <builtin> "gets the absolute value of a number" desc ] }
        { SYM: and [ { TYP: any TYP: any } { SYM: x SYM: y } [ dup [ "x" get-value ] [ "y" get-value ] bi and "bool" <val> ] <builtin> "preforms 'and' on two bools" desc ] }
        { SYM: or [ { TYP: any TYP: any } { SYM: x SYM: y } [ dup [ "x" get-value ] [ "y" get-value ] bi or "bool" <val> ] <builtin> "preforms 'or' on two bools" desc ] }
        { SYM: nth [ { TYP: list TYP: number } { SYM: list SYM: n } [ dup [ "n" get-value ] [ "list" get-value ] bi nth ] <builtin> "gets the nth element of a list" desc ] }
        { SYM: len [ { TYP: list } { SYM: list } [ dup "list" get-value length "number" <val> ] <builtin> "gets the length of a list" desc ] }
        { SYM: map [ { TYP: list TYP: callable } { SYM: list SYM: func } [ dup [ "list" get-value ] [ "func" get-value* ] bi [ swap 1array call-rix-function-with-env-up ] curry map "list" <val> ] <builtin>
                     "applies 'func' to each element in 'list', returning a new list made up of all the return values" desc ] }
        { SYM: compress-hash [ { TYP: hashstruct TYP: callable } { SYM: hash SYM: func } [ dup [ "hash" get-value ] [ "func" get-value* ] bi { } [ roll [ -rot 2array call-rix-function-with-env-up ] dip swap suffix ]
                                                                                    curry assoc-reduce "list" <val> ] <builtin>
                               "applies 'func' to each key-value pair in 'hash', returning a new list made up of all the return values" desc ] }
        { SYM: lock [ { TYP: any } { SYM: val } [ dup "val" get-value* "lock" <val> ] <builtin> "'locks' a value. it will remain locked, and unable to be evaluated, until 'unlock' is called" desc ] }
        { SYM: unlock [ { TYP: lock } { SYM: locked } [ dup "locked" get-value ] <builtin> "'unlocks' a locked value, returning it" desc ] }
        { SYM: maphash [ { TYP: hash TYP: callable } { SYM: hash SYM: func } [ dup [ "hash" get-value ] [ "func" get-value* ] bi [ -rot 2array call-rix-function-with-env-up ] curry assoc-map "hash" <val> ] <builtin>
                         "applies 'func' to each key-value pair in 'hash', returning a new hash made up of all the return values. (return values are in the format [key value])" desc ] }
        { SYM: fbody [ { TYP: callable  } { SYM: func } [ dup "func" get-value body>> ] <builtin> "gets the body of a function. primatives return [prim]" desc ] }
        { SYM: fparams [ { TYP: callable } { SYM: func } [ dup "func" get-value param-names>> "list" <val> ] <builtin> "gets the params of a function" desc ] }
        { SYM: list>string [ { TYP: list } { SYM: list } [ dup "list" get-value [ value>> ] "" map-as "string" <val> ] <builtin> "converts a list to a string" desc ] }
        { SYM: at [ { TYP: any TYP: hashstruct } { SYM: key SYM: hash } 
            [ dup [ "key" get-value* ] [ "hash" get-value ] bi rix-at [ f "bool" <val> ] unless* ] <builtin> "gets the value at key stored within 'hash'" desc ] }
        { SYM: not [ { TYP: any } { SYM: x } [ dup "x" get-value not "bool" <val> ] <builtin> "preforms 'not' on a bool" desc ] }
        { SYM: inl [ { TYP: list TYP: any } { SYM: params SYM: body } [ t make-inl push-token ] <builtin-macro> "creates a function with params and a body whose results may depend on the surrounding enviroment" desc ] } 
        { SYM: fn [ t "fn" <val> "creates a function with params and a body" desc ] }
        { SYM: mac [ t "mac" <val> 
            "creates a function with locked params (which must be accessed via 'unlock') and a body that returns either a list or a single value that is pushed onto the token array" desc ] }
        { SYM: varinl [ { TYP: list TYP: any } { SYM: params SYM: body } [ t make-inl clone [ "inline-variadic" >>type ] change-value push-token ] <builtin-macro>
                         "creates a function with params and a body whose results may depend on the surrounding enviroment. the last named param is a list of every value between the penultimate param and a ; token" desc ] } 
        { SYM: vari [ { TYP: list TYP: any } { SYM: params SYM: body } [ t make-func clone [ "variadic" >>type ] change-value push-token ] <builtin-macro>
                       "creates a function with params and a body. the last named param is a list of every value between the penultimate param and a ; token" desc  ] }
        { SYM: varimac [ { TYP: list TYP: any } { SYM: params SYM: body }
                          [ t make-macro clone [ "variadic-macro" >>type ] change-value push-token ] <builtin-macro>
                      "creates a function with locked params (which must be accessed via 'unlock') and a body that returns either a list or a single value that is pushed onto the token array. the last named param is a list of every token between the penultimate param and a ; token"
                                                                                                                                                                                                                                  desc ] }
        { SYM: typ? [ { TYP: type TYP: any } { SYM: type SYM: val } [ dup [ "val" get-value* ] [ "type" get-value ] bi type-matches? "bool" <val> ] <builtin> "tests if a value is of a certain type" desc ] }
        { SYM: imps? [ { TYP: generic TYP: any } { SYM: gen SYM: val } [ dup [ "gen" get-value name>> clone ] [ "val" get-value* ] bi type>> [ "." append prepend ] curry
                                                                         change-value over current-namespace>> env-get [ t ] when% "bool" <val> ]
                       <builtin> "tests if the type of this value implements a generic" desc ] }
        { SYM: setbody 
            [ 
                { TYP: callable TYP: list } { SYM: func SYM: list } 
                [ 
                    dup [ "func" get-value* clone ] [ "list" get-value* ] bi [ [ clone ] dip >>body ] curry change-value 
                ] <builtin> "sets the body of a callable without changing its type" desc
            ] 
        }
        { SYM: str-first [ { TYP: string } { SYM: string } [ dup "string" get-value* first "number" <val> ] <builtin> "gets the first character of a string as a unicode codepoint" desc ] }
        { SYM: str-len [ { TYP: string } { SYM: string } [ dup "string" get-value* length "number" <val> ] <builtin> "gets the length of a string" desc ] }
        { SYM: str-rest 
        [ { TYP: string } { SYM: string } [ dup "string" get-value* rest "string" <val> ] <builtin> "gets the all characters in a string but the first one, and returns them as a string" desc ] }
        { SYM: setparams
            [ 
                { TYP: callable TYP: list } { SYM: func SYM: list } 
                [ 
                    dup [ "func" get-value* clone ] [ "list" get-value ] bi [ [ clone ] dip >>param-names ] curry change-value 
                ] <builtin> "sets the param names of a callable without changing its type" desc
            ] 
        }
        { SYM: timps? [ { TYP: generic TYP: typ } { SYM: gen SYM: val }
                        [ dup [ "gen" get-value name>> clone ] [ "val" get-value* ] bi value>> [ "." append prepend ] curry change-value over current-namespace>> env-get [ t ] when% "bool" <val> ]
                        <builtin> "tests if this type implements a generic" desc ] }
        { SYM: evl [ { TYP: any } { SYM: val } [ dup "val" get-value* eval-one ] <builtin> "evaluates 'val'" desc ] }
        { SYM: setat [ { TYP: any TYP: any TYP: hashstruct } { SYM: key SYM: value SYM: hash } 
            [ dup [ "value" get-value* ] [ "key" get-value* ] [ "hash" get-value clone ] tri [ set-at ] keep "hash" <val> ] <builtin>
                       "sets 'value' at 'key' within 'hash'" desc ] }
        { SYM: tpop [ { } { } [ pop-token ] <builtin> "pops from the tokenstack. should mainly be used with macros, as using with functions or inline functions may provide unexpected results" desc ] }
        { SYM: tpush [ { TYP: any } { SYM: val } [ dup "val" get-value* push-token f ] <builtin> "pushes to the tokenstack" desc ] }
        { SYM: tapp [ { TYP: list } { SYM: val } [ dup "val" get-value* push-tokens f ] <builtin> "concatinates a list onto the tokenstack" desc ] }
        { SYM: args [ { } { } [ command-line get ] <builtin> "gets command line arguments. the first element is the name of the rix file when run with 'rix file.rix'" desc ] }
        { SYM: fin [ { TYP: string TYP: string } { SYM: path SYM: encoding } [ dup [ "path" get-value ] [ "encoding" get-value { { "ascii" [ ascii ] } { "utf8" [ utf8 ] } { "utf16" [ utf16 ] } } case ] bi <file-reader> "stream" <val> ]
                      <builtin> "returns a file-reading input stream. supported values of 'encoding' are \"ascii\", \"utf8\" and \"utf16\"" desc ] }
        { SYM: fout [ { TYP: string TYP: string } { SYM: path SYM: encoding } [ dup [ "path" get-value ] [ "encoding" get-value { { "ascii" ascii } { "utf8" utf8 } { "utf16" utf16 } } case ] bi <file-writer> "stream" <val> ]
                       <builtin> "returns a file-writing (overrides current file contents) output stream. supported values of 'encoding' are \"ascii\", \"utf8\" and \"utf16\"" desc ] }
        { SYM: fapp [ { TYP: string TYP: string } { SYM: path SYM: encoding } [ dup [ "path" get-value ] [ "encoding" get-value { { "ascii" ascii } { "utf8" utf8 } { "utf16" utf16 } } case ] bi <file-appender> "stream" <val> ]
                      <builtin> "returns a file-appending output stream. supported values of 'encoding' are \"ascii\", \"utf8\" and \"utf16\"" desc ] }
        { SYM: consfn [ t "consfn" <val> "constructs a function with params and a body. consfn is a function itself, meaning it evaluates its input parameters, so you can create a function with a runtime-computed definition" desc ] }
        { SYM: consinl [ { TYP: list TYP: list } { SYM: params SYM: body } [ f make-inl value>> ] <builtin>
                          "constructs an inline function; see the description of 'consfn' for the difference between this and 'inl'" desc ] }
        { SYM: consmac [ t "consmac" <val> "constructs a macro; see the description of 'consfn' for the difference between this and 'mac'" desc ] }
        { SYM: consvarinl [ { TYP: list TYP: list } { SYM: params SYM: body } [ f make-inl [ "inline-variadic" >>type ] change-value ] <builtin>
                         "constructs a variadic inline function; see the description of 'consfn' for the difference between this and 'varinl'" desc ] } 
        { SYM: consvari [ { TYP: list TYP: list } { SYM: params SYM: body } [ f make-func [ "variadic" >>type ] change-value ] <builtin>
                       "constructs a variadic function; see the description of 'consfn' for the difference between this and 'vari'" desc  ] }
        { SYM: consvarimac [ { TYP: list TYP: list } { SYM: params SYM: body }
                          [ f make-macro [ "variadic-macro" >>type ] change-value ] <builtin>
                          "constructs a variadic macro; see the description of 'consfn' for the difference between this and 'varimac'" desc ] }
        { SYM: istream [ { TYP: stream TYP: list } { SYM: stream SYM: code } [ dup [ "stream" get-value ] [ "code" get-value ] bi [ eval-tokens ] curry with-input-stream ] <builtin>
                          "runs 'code' with the input stream remapped to 'stream'" desc ] }
        { SYM: ostream [ { TYP: stream TYP: list } { SYM: stream SYM: code } [ dup [ "stream" get-value ] [ "code" get-value ] bi [ eval-tokens ] curry with-output-stream ] <builtin>
                          "runs 'code' with the output stream remapped to 'stream'" desc ] }
        { SYM: letfn [ { TYP: list TYP: list } { SYM: params SYM: body } [ t make-func value>> push-token ] <builtin-macro>
                        "creates a function with params and a body and calls that function immediatly" desc ] }
        { SYM: ^^ [ { TYP: number TYP: number } { SYM: base SYM: power } [ dup [ "base" get-value ] [ "power" get-value ] bi ^ "number" <val> ] <builtin> "raises 'base' to the power 'power'" desc ] }
        { SYM: mod [ { TYP: number TYP: number } { SYM: x SYM: y } [ dup [ "x" get-value ] [ "y" get-value ] bi mod "number" <val> ] <builtin> "gets the modulus of two numbers" desc ] }
        { SYM: sqrt [ { TYP: number } { SYM: x } [ dup "x" get-value sqrt "number" <val> ] <builtin> "gets the square root of a number" desc ] }
        { SYM: prn [ { TYP: any } { SYM: val } [ dup "val" get-value* [ dup type>> "string" = [ value>> "symbol" <val> ] when pprint-rix-value "\n" write ] keep ] <builtin> "prints anything, inserting a newline afterwards" desc ] }
        { SYM: wrt [ { TYP: any } { SYM: val } [ dup "val" get-value* [ dup type>> "string" = [ value>> "symbol" <val> ] when pprint-rix-value ] keep  ] <builtin> "prints anything" desc ] }
        { SYM: pprn [ { TYP: any } { SYM: val } [ dup "val" get-value* [ pprint-rix-value "\n" write ] keep ] <builtin> "prints anything, leaving strings in their literal representation and inserting a newline afterwards" desc ] }
        { SYM: pwrt [ { TYP: any } { SYM: val } [ dup "val" get-value* [ pprint-rix-value ] keep ] <builtin> "prints anything, leaving strings in their literal representation" desc ] }
        { SYM: string>list [ { TYP: string } { SYM: string } [ dup "string" get-value [ "number" <val> ] V{ } clone map-as "list" <val> ] <builtin> "converts a string to a list" desc ]  }
        ! eval module
        { SYM: incl [ { TYP: symbol } { SYM: name } [ dup "name" mac-get-value [ include-file [ value>> [ eval-set-global ] assoc-each ] keep ] keep "." split last "symbol" <val> swap eval-set-global ] <builtin-macro>
                      "includes the module specified by 'name'. if you import two modules with conflicting names, you can be specific by using 'name@modulename' syntax" desc ] }
        { SYM: typreq
          [
              { TYP: list TYP: callable } { SYM: list SYM: func } [ dup [ "func" get-value* ] [ "list" get-value ] bi overd swap [ swap eval-tokens-results swap ] with-env-up swap nip [ >>types ] curry change-value ]
              <builtin> "sets a list of required types for this function. 'typ any' means any type can be inputted, and 'typ callable' means that functions, inlines, macros, variadics etc. can all be used" desc
          ]
        }
        { SYM: ifdo [ { TYP: any TYP: any } { SYM: cond SYM: body }
                       [ dup [ "body" get-value* ] [ "cond" get-value ] bi [ dup type>> "list" = [ value>> push-tokens ] [ push-token ] if f ] [ drop f "nil" <val> ] if ] <builtin>
                       "if the boolean cond is true, evaluates the body" desc ] }
        { SYM: typ [ { TYP: symbol } { SYM: sym } [ dup "sym" mac-get-value "type" <val> push-token ] <builtin-macro>
                     "turns a symbol into a type. honestly I didn't need to make a seperate 'type' type, but I thought I did, changed a bunch of stuff, and it's too tiresome to change it back" desc ] }
        { SYM: cast [ { TYP: any TYP: type } { SYM: val SYM: type } [
                          dup [ "val" get-value* dup type>> ] [ "type" get-value ] bi ">" prepend append "symbol" <val> swap [ swapd evaluates-to-something? swapd ] keep swap [ "quote" <val> ] when 2array push-tokens f
                      ] <builtin> "sets the type of the value to the given type by calling the function 'type1>type2'" desc ] }
        { SYM: ucast [ { TYP: any TYP: type } { SYM: val SYM: typ } [ dup [ "val" get-value ] [ "typ" get-value ] bi <val> ] <builtin> "preforms an unsafe cast of one type to another. should mainly be used in constructors" desc ] }
        { SYM: set 
            [ 
                { TYP: symbol TYP: any } { SYM: name SYM: value } 
                [ dup [ "name" get-value* ] [ "value" get-value* ] bi pick current-namespace>> but-last [ swapd env-set drop ] keepd ] <builtin-inl> "defines a variable" desc 
            ] 
        }
        ! eval sym genr
        { SYM: genr [ { TYP: symbol TYP: list } { SYM: sym SYM: params } [ dup [ "sym" get-value* ] [ "params" get-value* ] bi [ genr boa "generic" <val> ] keepd swap [ eval-set-global ] keep ] <builtin-inl>
                       "creates a new generic with the given name and params (and assigns it to that name within the global scope)" desc ]  }
        { SYM: gdesc [ { TYP: any } { SYM: value } [ dup "value" get-value* description>> "string" <val> ] <builtin> "gets the description of a value and returns it" desc ] }
        { SYM: pdesc [ { TYP: any } { SYM: value } [ dup "value" get-value* [ description>> print ] keep ] <builtin> "gets the description of a value and prints it" desc ] }
        { SYM: desc [ { TYP: string TYP: any } { SYM: desc SYM: value } [ dup [ "value" get-value* ] [ "desc" get-value ] bi desc ] <builtin> "sets the description of a value" desc ] }
        { SYM: nxt [ { } { } [ eval-until-one ] <builtin> "evaluates tokens until a value is returned" desc ] }
        { SYM: glob [ { TYP: dec } { SYM: dec } [ [ "dec" mac-get-value* ] keep [ swap rix-eval swap ] with-global-env swap push-token ] <builtin-macro> "takes a declaration and evaluates it within the global namespace" desc ] }
        ! eval name res
        { SYM: exp [ { TYP: dec } { SYM: dec } [ dup "dec" mac-get-value [ expr>> eval-tokens-with-env-up ] 
        [ symbol>> "symbol" <val> ] bi rot [ to-export>> set-at ] 3keep spin [ eval-set-global ] keep set-last-result ]
                      <builtin-macro> "takes a declaration and sets it for exporting. note that inline functions should not be exported" desc ] }
        { SYM: genexp [ { TYP: symbol TYP: list } { SYM: sym SYM: params }
                        ! eval sym genr
                         [ dup [ "sym" get-value* ] [ "params" get-value* ] bi [ genr boa "generic" <val> ] keepd rot [ to-export>> set-at ] 3keep spin [ eval-set-global ] keep ]
                         <builtin-inl> "creates a new generic with the given name and params, exports it, and assigns it to that name within the global scope" desc ] }
        { SYM: cstack [ { } { } [ dup callstack>> [ [ params>> ] [ name>> ] bi prepend "parens" <val> ] map "list" <val> ] <builtin> "returns the callstack" desc ] }
        { SYM: quot [ t "quot" <val> "quotes a value" desc ] }
        { SYM: res [ { TYP: symbol } { SYM: val } [ dup "val" get-value "resolve" <val> ] <builtin> "makes a symbol a resolve expression ($sym syntax)" desc ] }
        { SYM: uquot [ t "uquot" <val> "makes a value an unquote" desc ] }
        { SYM: splice [ { TYP: any } { SYM: val } [ dup "val" get-value* "splice" <val> ] <builtin> "makes a value a splice" desc ] }
        { SYM: err [ { TYP: any } { SYM: str } [ dup "str" get-value \ eval-error boa "error" <val> push-token f ]  <builtin> "throws an error with the given string as a message" desc ] }
        { SYM: new-err [ { TYP: string } { SYM: str } [ dup "str" get-value \ eval-error boa "error" <val> ]  <builtin> "returns an error with the given string as a message" desc ]  }
        { SYM: try [ { TYP: list TYP: callable } { SYM: list SYM: catch } [ dup [ "catch" get-value* ] [ "list" get-value* ] bi
                                                                            [ nip [ evaluator-save ] dip eval-tokens-with-env-up ] [ "error" <val> 2array >vector call-rix-function-with-env-up ] recover ] <builtin>
                      "evaluates the list. if there's an error, calls the catch function with the list and the error (in that order). errors automatically throw themselves when evaluated, so handle them carefully" desc ] }
        { SYM: rng [ { TYP: number TYP: number } { SYM: from SYM: to }
                      [ dup [ "from" get-value ] [ "to" get-value ] bi [a..b] >vector [ "number" <val> ] map "list" <val> ] <builtin> "returns a list with numbers that go from 'from' to 'to', including both 'from' and 'to'" desc ]  }
        { SYM: prnted [ { TYP: list } { SYM: list } [ dup "list" get-value [ eval-tokens-with-env-up drop ] with-string-writer "string" <val> ]
                         <builtin> "runs 'list', returning a string containing everything that was printed during its running" desc ] }
        { SYM: assert= [ { TYP: any TYP: any } { SYM: one SYM: two } [ dup [ "one" get-value* ] [ "two" get-value* ] bi [ = ] 2check [ drop ]
                                                   [ [ [ pprint-rix-value ] with-string-writer ] bi@ " != " prepend append "Assert failed: " prepend eval-error ] if ]
                         <builtin> "throws an error if 'one' and 'two' aren't equal" desc ] }
        { SYM: if [ t "if" <val> "based on the boolean cond, picks either body1 or body2 and evaluates it" desc ] }
        { SYM: os [ { } { } [ os name>> "string" <val> ] <builtin> "gets the current operating system" desc ] }
        { SYM: let [ t "let" <val> "evaluates 'code' within a new scope defined with the keys and values in 'hash'" desc ] }
        { SYM: up [ { TYP: continuation TYP: symbol } { SYM: cont SYM: sym } [ dup [ "sym" get-value* ] [ "cont" get-value ] bi [ [ current-namespace>> env-get ] keep ] with-env-up drop ] <builtin>
                    "returns the result of retriving 'sym' in the parent enviroment of the one within cont" desc ] }
        { SYM: getval [ { TYP: continuation TYP: symbol } { SYM: cont SYM: sym } [ dup [ "sym" get-value* ] [ "cont" get-value ] bi current-namespace>> env-get ] <builtin>
                     "returns the result of retriving 'sym' in the enviroment within cont" desc ] }
        { SYM: upret [ { TYP: continuation TYP: any } { SYM: cont SYM: val } [
                           [ "cont" get-value evaluator-parent evaluator-<env> V{ } clone >>callstack ] [ "val" get-value* ] bi dup type>> "nil" = [ drop ] [ push-token ] if f ] <func>
                       "resumes the continuation 'cont' with an enviroment set to the parent of the of the one within cont, returning to it with 'val' as a result" desc ] }
        { SYM: downret [ { TYP: continuation TYP: any } { SYM: cont SYM: val } [ [ "cont" get-value evaluator-<env> V{ } clone >>callstack ] [ "val" get-value* ] bi dup type>> "nil" = [ drop ] [ push-token ] if f ] <func>
                       "resumes the continuation 'cont' with an enviroment set to an empty child of the of the one within cont, returning to it with 'val' as a result" desc ] }
        { SYM: ret [ { TYP: continuation TYP: any } { SYM: cont SYM: val } 
                      [ [ "cont" get-value V{ } clone >>callstack ] [ "val" get-value* ] bi dup type>> "nil" = [ drop ] [ set-last-result ] if f ] <func>
                     "resumes the continuation 'cont', returning to it with 'val' as a result" desc ] }
        { SYM: callcc [ { TYP: callable } { SYM: func } 
        ! TODO: check if natually returning from functions with callcc actually works, or if it pushes *ret* *endexpr*
        [ dup [ "func" get-value* ] [ evaluator-save evaluator-parent evaluator-save f "endexpr" <val> push-token "continuation" <val> ] bi 2array push-tokens f ] <builtin>
                        "creates a continuation at the point after the end of this function call, and runs the inputed function, passing the continuation as a value" desc ] }
        { SYM: runc [ { TYP: continuation TYP: list } { SYM: cont SYM: code } [ dup [ "cont" get-value evaluator-save ] [ "code" get-value ] bi eval-tokens drop "continuation" <val> ] <builtin>
                      "runs 'code' within the continuation 'cont' (returning 'cont'), returning to the current continuation afterwards" desc ] }
        { SYM: norm [ { TYP: continuation TYP: any } { SYM: cont SYM: val } [ dup [ "cont" get-value ] [ "code" get-value* ] bi eval-one nip ] <builtin>
                      "evaluates 'val' within the continuation 'cont', returning the result of the evaluation. this has the side effect of modifying 'cont'" desc ] }
        { SYM: runbef [ { TYP: continuation TYP: list } { SYM: cont SYM: code } [ dup [ "cont" get-value evaluator-save ] [ "code" get-value ] bi push-tokens "continuation" <val> ] <builtin>
                        "sets the continuation to run some code before the code it would otherwise run" desc ] }
        { SYM: tpeekc [ { TYP: continuation } { SYM: cont } [ dup "cont" get-value evaluator-save pop-token nip ] <builtin> "looks at the next token in the continuation 'cont'" desc ] }
        { SYM: tpushc [ { TYP: continuation TYP: any } { SYM: cont SYM: val }
                        [ dup [ "cont" get-value evaluator-save ] [ "val" get-value* ] bi push-token "continuation" <val> ] <builtin> "pushes a value to the continuation 'cont'" desc ] }
        { SYM: tpopc [ { TYP: continuation } { SYM: cont } [ dup "cont" get-value pop-token nip ] <builtin>
                       "pops the next token in the continuation 'cont'. this is pretty much the only rix function with a side effect, as it modifies 'cont' without returning it"
                       desc ] }
        { SYM: tdelc [ { TYP: continuation } { SYM: cont } [ dup "cont" get-value evaluator-save pop-token drop "continuation" <val> ] <builtin>
                       "pops the next token in the continuation 'cont', returning the continuation"
                       desc ] }
        { SYM: retc [ { TYP: continuation } { SYM: cont } [ [ "cont" get-value ] keep "continuation" <val> set-last-result f ]
                      <func> "resumes the continuation 'cont', returning to it with the current continuation as a result" desc ] }
        { SYM: getenv [ { } { } [ dup current-namespace>> compress-env last "hash" <val> ] <builtin> "gets the current enviroment as a hashtable" desc ] }
        { SYM: tcountc [ { TYP: continuation } { SYM: cont } [ dup "cont" get-value [ tokens>> length ] [ extra-tokens>> concat length ] bi + "number" <val> ] <builtin> "returns the amount of tokens in 'cont'" desc ] }
        { SYM: newcont [ { TYP: list } { SYM: tokens } [ dup [ "tokens" get-value ] [ current-namespace>> <env> ] bi <evaluator> "continuation" <val> ] <builtin>
                         "returns a new continuation with a child enviroment of the current enviroment" desc ] }
        { SYM: wrtstr [ { TYP: string } { SYM: string } [ dup "string" get-value dup write "string" <val> ] <builtin> "prints a string" desc ] }
        { SYM: floor [ { TYP: num } { SYM: n } [ dup "n" get-value floor "number" <val> ] <builtin> "floors a number" desc ] }
        { SYM: modf [ { TYP: symbol TYP: any } { SYM: sym SYM: val } 
            [ dup [ "val" get-value* ] [ "sym" get-value* ] bi [ dup [ of ] curry [ swapd [ current-namespace>> last swapd set-at ] keep ] 2with with-specific-env ] keep ] <builtin> 
            "modifies the value at a given name if it exists, rather than setting a new value in the current namespace" desc ] }
        { SYM: gettyp [ { TYP: any } { SYM: val } [ dup "val" get-value* type>> "type" <val> ] <builtin> "gets the type of a value" desc ] }
        { SYM: keys [ { TYP: hash } { SYM: hash } [ dup "hash" get-value keys "list" <val> ] <builtin> "gets the keys in a hash as a list" desc ] }
        { SYM: values [ { TYP: hash } { SYM: hash } [ dup "hash" get-value values "list" <val> ] <builtin> "gets the keys in a hash as a list" desc ] }
        { SYM: msg [ { TYP: error } { SYM: err } [ dup "err" get-value msg>> "string" <val> ] <builtin> "gets the message of an error, if the error has one" desc ] }
        { SYM: underlying [ { TYP: any } { SYM: val } [ dup "val" get-value underlying-rix "hash" <val> ] <builtin> "gets the underlying value of a builtin rix type as a hashtable" desc ] }
        {
            SYM: envstack [ { } { } [ dup current-namespace>> env>seq "list" <val> ] <builtin>
                            "gets the current enviroment as a list, where the last element is a hash containing the current enviroment, and the previous elements are the parents of that enviroment as hashtables" desc ]
        }
        { SYM: cloex 
            [ 
                { TYP: list TYP: callable } { SYM: symbols SYM: func } 
                [
                    dup [ "symbols" get-value ] [ "func" get-value* clone ] bi 
                    [ [ [ [ drop dupd [ = ] curry any? not ] assoc-filter ] map nip ] change-env ] change-value 
                ] 
                <builtin> "modifies this function or macro to exclude these symbols from its stored environment" desc 
            ] 
        }
        { SYM: clo 
            [ 
                { TYP: hashstruct TYP: callable } { SYM: hash SYM: func } 
                [
                    dup [ "func" get-value* clone ] [ "hash" get-value ] bi over closure? 
                    [ [ [ suffix ] curry change-env ] curry change-value ] 
                    [ 
                        swap 
                        [ 
                            value>> 
                            { ! TODO: lsp weird offset here 
                                [ types>> swap 1vector H{ } clone swap rix-env boa ] 
                                [ param-names>> ] 
                                [ drop [ dup sequence? not [ 1vector ] when f "return" <val> suffix f "begnexpr" <val> prefix push-tokens f ] ]
                                [ body>> ]
                            } cleave func boa 
                        ] [ type>> "inl" = ] bi
                        [ "func" <val> ] [ "variadic" <val> ] if
                    ] if
                ]
                <builtin> "includes the hash 'hash' within the env of the closure 'func', or if 'func' is an inline function, then turns it into a closure with the env 'hash'" desc
            ] 
        }
        { SYM: rixparse [ { TYP: string } { SYM: code } [ dup "code" get-value lex-str "list" <val> ] <builtin> "parses a rix string, returning a list of tokens" desc ] }
       ! types env param-names quot body  
    } clone [ [ clone ] [ dup quotation? [ call( -- val ) ] when ] bi* ] assoc-map 1vector H{ } clone swap rix-env boa
    ;

M: rix-value pprint-rix-value dup value>> hashtable? [ dup type>> "hash" = not [ [ type>> write ] [ value>> "hash" <val> pprint-rix-value ] bi ] [ value>> pprint ] if ] [ value>> pprint ] if ;

: map-acc ( x seq quot -- x seq' ) collector [ each ] dip ; inline

: eval-str ( str -- result ) lex-str global-env <evaluator> eval-full last-result>> [ f "nil" <val> ] unless* ;

: include-in-closures ( env vals -- vals ) [ dup closure? [ [ [ load-env ] with change-env ] with change-value ] [ nip ] if ] with map-values ;

: include-file ( module-name -- module ) 
    find-file-path utf8 file-contents lex-str global-env <evaluator> eval-full 
    [ current-namespace>> ] [ to-export>> ] bi include-in-closures "module" <val> ;

: global-env>markdown ( -- )
    "| name | signature      | description |\n| --- | ------------------| ------------|" print global-env last [ [ "| " write [ pprint-rix-value " | " write ] bi@ ] keep description>> write " |" print ] assoc-each  ;

: tpopn ( eval n -- eval toks ) [ 1 - over pop-token nip ] collector [ [ dup 0 <= not ] swap while ] dip nip ;
: tpop-until-semi ( eval -- eval toks ) [ pop-token [ type>> "semicolon" = not ] 1check [ drop f f ] unless* swap ] collector [ loop ] dip ;

: is-bracket? ( char -- ? ) { CHAR: ( CHAR: { CHAR: [ CHAR: " } index ;  
: bracket-in-list ( str -- str ) dup length 0 > [ dup first is-bracket? [ dup CHAR: \s swap index [ "(" ")" surround ] when ] unless ] when ;

: pprint-in-list ( val -- ) [ pprint-rix-value ] with-string-writer bracket-in-list write ;
PRIVATE>
: eval-str-to-str ( str -- str ) eval-str [ pprint-rix-value ] with-string-writer  ;

: print-rix-callcount ( -- ) 
    rix-callcount get-global inv-sort-values [ [ ": " append write ] dip >dec print ] assoc-each ;

: print-rix-profile ( -- ) 
    rix-profile get-global inv-sort-values dup values sum [ / 10000 * [ ": " append write ] dip 100 /mod >fixnum [ >dec ] bi@ CHAR: . prefix append CHAR: % suffix print ] curry assoc-each ;

: repl ( -- )
        "Welcome to the rix repl! (" write rix-version ")" append print
        "Press Control+D to quit" print V{ } clone global-env <evaluator>
        [
            "rix> " write flush readln
            [
                [ lex-str >>tokens eval-full V{ } clone >>callstack dup last-result>> [ pprint-rix-value "\n" write t ] [ t ] if* ] when*% ] [
                [ "error" <val> pprint-rix-value "\n" write ] with-string-writer
                COLOR: red
                foreground associate format
            ] recover
        ] loop drop ;

: add-rix-breakpoint ( symbol -- ) 
    rix-breakpoints [ { } ] initialize rix-breakpoints [ swap suffix ] with change-global ;

: clear-rix-breakpoints ( -- ) { } rix-breakpoints set-global ;

: run-rix-file ( file-path -- ) 4 cut* ".rix" = [ ".rix" append dup parent-directory [ file-name utf8 file-contents eval-str-to-str ] with-directory drop ]
                                               [ "file name must end in .rix" eval-error ] if ;
: run-rix ( -- )
    command-line get-global dup length 0 > [ [ [ run-rix-file ] [ nip . ] recover ] each ] [ drop repl ] if ;
! { "if" "let" "fn" "consfn" "mac" "consmac" "quot" "uquot" "+" "-" "*" "/" }
<PRIVATE
PREDICATE: rix-keyword < rix-value type>> keywords index ;
M: rix-keyword pprint-rix-value type>> write ;

CONSTANT: if-function 
    $[ { TYP: any TYP: any TYP: any } { SYM: cond SYM: body1 SYM: body2 }
            [ dup [ "cond" get-value ] [ "body1" get-value* ] [ "body2" get-value* ] tri ? dup type>> "list" = [ value>> push-tokens f ] when ] <builtin>
       ]
CONSTANT: let-function 
    $[ { TYP: hashstruct TYP: list } { SYM: hash SYM: code } [
                         dup [ "code" get-value ] [ "hash" get-value ] bi 
                         rot [ swap [ suffix ] curry change-current-namespace swap f "scopeup" <val> suffix f "begnexpr" <val> prefix f "endexpr" <val> suffix
                         push-tokens f swap ] with-env-up swap ] <builtin> 
                     ]
CONSTANT: fn-function $[ { TYP: list TYP: any } { SYM: params SYM: body } [ t make-func push-token ] <builtin-macro> ]

CONSTANT: consfn-function $[ { TYP: list TYP: list } { SYM: params SYM: body } [ f make-func value>> ] <builtin> ]

CONSTANT: mac-function $[ { TYP: list TYP: any } { SYM: params SYM: body } [ t make-macro push-token ] <builtin-macro> ]

CONSTANT: consmac-function $[ { TYP: list TYP: any } { SYM: params SYM: body } [ f make-macro value>> ] <builtin> ]

CONSTANT: quot-function $[ { TYP: any } { SYM: val } [ dup "val" get-value* "quote" <val> ] <builtin> ] 

CONSTANT: uquot-function $[ { TYP: any } { SYM: val } [ dup "val" get-value* "unquote" <val> ] <builtin> ] 

CONSTANT: +-function $[ { TYP: number TYP: number } { SYM: x SYM: y } [ dup [ "x" get-value ] [ "y" get-value ] bi + "number" <val> ] <builtin> ]

CONSTANT: --function $[ { TYP: number TYP: number } { SYM: x SYM: y } [ dup [ "x" get-value ] [ "y" get-value ] bi - "number" <val> ] <builtin> ]

CONSTANT: *-function $[ { TYP: number TYP: number } { SYM: x SYM: y } [ dup [ "x" get-value ] [ "y" get-value ] bi * "number" <val> ] <builtin> ]

CONSTANT: /-function $[ { TYP: number TYP: number } { SYM: x SYM: y } [ dup [ "x" get-value ] [ "y" get-value ] bi / "number" <val> ] <builtin> ]

PRIVATE>
: rix-eval ( eval tok -- eval result/f ) 
    {
        { [ dup type>> "module-call" = ] [ 
            value>>
            [ symbol>> >>last-name ] keep
            [ module>> over current-namespace>> [ env-get ] curry ?transmute [ value>> "unknown module: " prepend eval-error ] unless value>> ] keep symbol>> swap [ at ] curry ?transmute
            [ value>> "unknown symbol: " prepend eval-error ] unless push-token f 
        ] }
        { [ dup type>> "module-resolve" = ] [
            value>>
            [ symbol>> >>last-name ] keep
            [ module>> over current-namespace>> [ env-get ] curry ?transmute [ value>> "unknown module: " prepend eval-error ] unless value>> ] keep symbol>> swap [ at ] curry ?transmute
            [ value>> "unknown symbol: " prepend eval-error ] unless 
        ] }
        { [ dup type>> "nop" = ] [
            drop f
        ] }
        { [ dup type>> "semicolon" = ] [
            drop f
        ] }
        { [ dup type>> "scopeup" = ] [
            drop evaluator-parent f 
        ] }
        { [ dup type>> "return" = ] [
            drop evaluator-parent callstack-pop [ 1 - ] change-nesting-count dup last-result>> 
        ] }
        { [ dup type>> "endexpr" = ] [
            drop [ 1 - ] change-nesting-count dup last-result>> 
        ] }
        { [ dup type>> "error" = ] [
            value>> throw 
        ] }
        { [ dup type>> "if" = ] [
            drop if-function push-token "if" >>last-name f 
        ] }
        { [ dup type>> "let" = ] [
            drop let-function push-token "let" >>last-name f 
        ] }
        { [ dup type>> "fn" = ] [
            drop fn-function push-token "fn" >>last-name f 
        ] }
        { [ dup type>> "consfn" = ] [
            drop consfn-function push-token "consfn" >>last-name f 
        ] }
        { [ dup type>> "mac" = ] [
            drop mac-function push-token "mac" >>last-name f 
        ] }
        { [ dup type>> "consmac" = ] [
            drop consmac-function push-token "consmac" >>last-name f 
        ] }
        { [ dup type>> "quot" = ] [
            drop quot-function push-token "quot" >>last-name f 
        ] }
        { [ dup type>> "uquot" = ] [
            drop uquot-function push-token "uquot" >>last-name f 
        ] }
        { [ dup type>> "+" = ] [
            drop +-function push-token "+" >>last-name f 
        ] }
        { [ dup type>> "-" = ] [
            drop --function push-token "-" >>last-name f 
        ] }
        { [ dup type>> "*" = ] [
            drop *-function push-token "*" >>last-name f
        ] }
        { [ dup type>> "/" = ] [
            drop /-function push-token "/" >>last-name f 
        ] }
        { [ dup type>> "quote" = ] [
            unquote-scan 
        ] }
        { [ dup type>> "resolve" = ] [
            value>> [ >>last-name ] keep "symbol" <val> over current-namespace>> [ env-get ] curry ?transmute [ value>> "unknown symbol: " prepend eval-error ] unless 
        ] }
        { [ dup type>> "parens" = ] [
            value>> f "endexpr" <val> suffix f "begnexpr" <val> prefix push-tokens f 
        ] }
        { [ dup type>> "symbol" = ] [ 
            [ value>> >>last-name ] keep over current-namespace>> [ env-get ] curry ?transmute [ value>> "unknown symbol: " prepend eval-error ] unless push-token f 
        ] }
        { [ dup type>> "begnexpr" = ] [
            drop [ 1 + ] change-nesting-count f 
        ] }
        { [ dup type>> "scopedown" = ] [
            drop evaluator-<env> f 
        ] }
        { [ dup type>> "dec" = ] [
            value>> [ symbol>> "symbol" <val> ] [ expr>> ] bi swapd eval-tokens rot [ [ rot env-set ] 2curry change-current-namespace ] keepd 
        ] }
        { [ dup type>> "inl" = ] [
             [ evaluator-<env> [ last-name>> dup rix-callcount-inc ] keep ] dip
             [ value>> param-names>> length target-to-results ] keep evaluator-callstack-push
             [ value>> types>> typecheck ] 2keep swapd
             [ value>> param-names>> [ tuck current-namespace>> ] dip swap [ rot env-set ] 2reduce >>current-namespace ] keep 
             value>> [ body>> ] [ quot>> ] bi [ call( evaluator body -- evaluator result/f ) ] curry overd rix-benchmark
         ] }
        { [ dup type>> "func" = ] [
             [ [ last-name>> dup rix-callcount-inc ] keep ] dip
             [ value>> param-names>> length target-to-results ] keep evaluator-callstack-push
             [ value>> types>> typecheck ] 2keep swapd
             [ value>> env>> [ [ <env> ] dip load-env ] curry change-current-namespace ] keep
             [ value>> param-names>> [ tuck current-namespace>> ] dip swap [ rot env-set ] 2reduce >>current-namespace ] keep
             value>> [ body>> ] [ quot>> ] bi [ call( evaluator body -- evaluator result/f  ) ] curry overd rix-benchmark
         ] }
        { [ dup type>> "macro" = ] [
            [ [ last-name>> dup rix-callcount-inc ] keep ] dip
            [ value>> param-names>> length tpopn ] keep evaluator-callstack-push
            [ value>> types>> typecheck ] 2keep swapd
            [ [ "lock" <val> ] map ] 2dip
            [ value>> env>> [ [ <env> ] dip load-env ] curry change-current-namespace ] keep
            [ value>> param-names>> [ tuck current-namespace>> ] dip swap [ rot env-set ] 2reduce >>current-namespace ] keep
            value>> [ body>> ] [ quot>> ] bi [ call( evaluator body -- evaluator ) f ] curry overd rix-benchmark 
        ] }
        { [ dup type>> "variadic" = ] [
            [ [ last-name>> dup rix-callcount-inc ] keep ] dip
            [ value>> param-names>> length 1 - target-to-results [ eval-until-semi "list" <val> ] dip swap suffix ] keep evaluator-callstack-push
            [ value>> types>> typecheck ] 2keep swapd
            [ value>> env>> [ [ <env> ] dip load-env ] curry change-current-namespace ] keep
            [ value>> param-names>> [ tuck current-namespace>> ] dip swap [ rot env-set ] 2reduce >>current-namespace ] keep
            value>> [ body>> ] [ quot>> ] bi [ call( evaluator body -- evaluator result/f  ) ] curry overd rix-benchmark
        ] }
        { [ dup type>> "variadic-macro" = ] [
            [ [ last-name>> dup rix-callcount-inc ] keep ] dip
            [ value>> param-names>> length 1 - tpopn [ tpop-until-semi "list" <val> ] dip swap suffix ] keep evaluator-callstack-push
            [ value>> types>> typecheck ] 2keep swapd
            [ [ "lock" <val> ] map ] 2dip
            [ value>> env>> [ [ <env> ] dip load-env ] curry change-current-namespace ] keep
            [ value>> param-names>> [ tuck current-namespace>> ] dip swap [ rot env-set ] 2reduce >>current-namespace ] keep
            value>> [ body>> ] [ quot>> ] bi [ call( evaluator body -- evaluator ) f ] curry overd rix-benchmark
        ] }
        { [ dup type>> "inline-variadic" = ] [
            [ evaluator-<env> [ last-name>> dup rix-callcount-inc ] keep ] dip
            [ value>> param-names>> length target-to-results [ eval-until-semi "list" <val> ] dip swap suffix ] keep evaluator-callstack-push
            [ value>> types>> typecheck ] 2keep swapd
            [ value>> param-names>> [ tuck current-namespace>> ] dip swap [ rot env-set ] 2reduce >>current-namespace ] keep value>> [ body>> ] [ quot>> ] bi 
            [ call( evaluator body -- evaluator result/f  ) ] curry overd rix-benchmark
        ] }
        { [ dup type>> "generic" = ] [
             value>> [ eval-until-one swap ] dip
             pick [ name>> value>> "." prepend ] [ type>> ] bi* prepend "symbol" <val> rot 2array >vector push-tokens f 
        ] }
        { [ dup type>> { "number" "string" "continuation" "stream" "lock" "hash" "list" "bool" "nil" "type" } index ] [ ] }
        [ 
            [ evaluates-to-something? ] 1check
            [ 
                dup type>> ".eval" append "symbol" <val> pick current-namespace>> env-get swap "lock" <val> [ over clone "continuation" <val> ] dip 3array eval-tokens dup type>> "nop" = [ drop f ] when 
            ] when 
        ]
    } cond
    ;

! hello@world
RIX-TYPE: rix-module-call
M: rix-module-call pprint-rix-value value>> [ symbol>> value>> write ] [ "@" write module>> value>> write ] bi ;
M: rix-module-call evaluates-to-something? drop t ;

RIX-TYPE: rix-continuation
M: rix-continuation pprint-rix-value drop "*continuation*" write ;

! $hello@world
RIX-TYPE: rix-module-resolve
M: rix-module-resolve pprint-rix-value "$" write value>> [ symbol>> value>> write ] [ "@" write module>> value>> write ] bi ;
M: rix-module-resolve evaluates-to-something? drop t ;

RIX-TYPE: rix-stream
M: rix-stream pprint-rix-value drop "*stream*" write ;

! 1798
RIX-TYPE: rix-number

RIX-TYPE: rix-lock
M: rix-lock pprint-rix-value "lock " write value>> pprint-rix-value ;

RIX-TYPE: rix-nop
M: rix-nop pprint-rix-value drop "nop" write ;
M: rix-nop evaluates-to-something? drop t ;

RIX-TYPE: rix-semicolon
M: rix-semicolon pprint-rix-value drop ";" write ;
M: rix-semicolon evaluates-to-something? drop t ;

RIX-TYPE: rix-scopeup
M: rix-scopeup pprint-rix-value drop "*^*" write ;
M: rix-scopeup evaluates-to-something? drop t ;

RIX-TYPE: rix-return
M: rix-return pprint-rix-value drop "*ret*" write ;
M: rix-return evaluates-to-something? drop t ;

RIX-TYPE: rix-endexpr
M: rix-endexpr pprint-rix-value drop "*endexpr*" write ;
M: rix-endexpr evaluates-to-something? drop t ;

RIX-TYPE: rix-scopedown
M: rix-scopedown pprint-rix-value drop "*|*" write ;
M: rix-scopedown evaluates-to-something? drop t ;

! "Hello, World"
RIX-TYPE: rix-string

RIX-TYPE: rix-hash
M: rix-hash pprint-rix-value value>>
    V{ } [ [ suffix ] bi@ ] assoc-reduce "{" write dup empty? not
                                           [ unclip pprint-in-list ] when
                                           [ " " write pprint-in-list ] each "}" write ;

RIX-TYPE: rix-module

! err "example error"
RIX-TYPE: rix-error
M: rix-error pprint-rix-value value>> "ERROR " write dup class-of \ summary ?lookup-method [ summary write ] [ pprint ] if ;
M: rix-error evaluates-to-something? drop t ;

! 'something
RIX-TYPE: rix-quote
M: rix-quote pprint-rix-value "'" write value>> pprint-rix-value ;
M: rix-quote evaluates-to-something? drop t ;

! [ things 1 2 4 5 ]
RIX-TYPE: rix-list
M: rix-list pprint-rix-value "[" write value>> dup empty? not [ unclip pprint-in-list ] when [ " " write pprint-in-list ] each "]" write  ;
INSTANCE: rix-list rix-sequence

! $symbol
RIX-TYPE: rix-resolve
M: rix-resolve pprint-rix-value "$" write value>> write ;
M: rix-resolve evaluates-to-something? drop t ;

! (evaluated-immediatly 1 1)
RIX-TYPE: rix-parens
M: rix-parens pprint-rix-value "(" write value>> dup empty? not [ unclip pprint-rix-value ] when [ " " write pprint-rix-value ] each ")" write ;
M: rix-parens evaluates-to-something? drop t ;
INSTANCE: rix-parens rix-sequence

! symbol
RIX-TYPE: rix-symbol
M: rix-symbol pprint-rix-value value>> write ;
M: rix-symbol evaluates-to-something? drop t ;

RIX-TYPE: rix-begnexpr
M: rix-begnexpr pprint-rix-value drop "*begnexpr*" write ;

! tru
RIX-TYPE: rix-bool
M: rix-bool pprint-rix-value value>> "tru" "fal" ? write ;

! nil
RIX-TYPE: rix-nil
M: rix-nil pprint-rix-value drop "nil" write ;

! foo: bar;
RIX-TYPE: rix-dec
M: rix-dec pprint-rix-value value>> [ symbol>> write ":" write ] [ expr>> [ " " write pprint-rix-value ] each ] bi  ;
M: rix-dec evaluates-to-something? drop t ;

! inl [x y] [+ x y]
RIX-TYPE: rix-inl
M: rix-inl pprint-rix-value value>> "inl " write [ param-names>> "list" <val> pprint-rix-value ] 
    [ body>> dup V{ SYM: prim } "list" <val> = not [ " " write dup sequence? [ 1array ] unless "list" <val> pprint-rix-value ] [ drop ] if ] bi  ;
M: rix-inl evaluates-to-something? drop t ;


! fn [x y] [+ x y]
! eval self

RIX-TYPE: rix-func
M: rix-func pprint-rix-value value>> "fn " write [ param-names>> "list" <val> pprint-rix-value ] 
    [ body>> dup V{ SYM: prim } "list" <val> = not [ " " write dup sequence? [ 1array ] unless "list" <val> pprint-rix-value ] [ drop ] if ] bi ;
M: rix-func evaluates-to-something? drop t ;


! mac [x y] [+ (unlock x) (unlock y)]
RIX-TYPE: rix-macro
M: rix-macro pprint-rix-value value>> "mac " write 
    [ param-names>> "list" <val> pprint-rix-value ] 
    [ body>> dup V{ SYM: prim } "list" <val> = not [ " " write dup sequence? [ 1array ] unless "list" <val> pprint-rix-value ] [ drop ] if ] bi  ;
M: rix-macro evaluates-to-something? drop t ;

! vari [x ys] '[,x @ys]
RIX-TYPE: rix-variadic
M: rix-variadic pprint-rix-value value>> "vari " write [ param-names>> "list" <val> pprint-rix-value ] 
    [ body>> dup V{ SYM: prim } "list" <val> = not [ " " write dup sequence? [ 1array ] unless "list" <val> pprint-rix-value ] [ drop ] if ] bi ;
M: rix-variadic evaluates-to-something? drop t ;

! varimac [x ys] '[+ ,x ,(nth ys 0)]
RIX-TYPE: rix-variadic-macro
M: rix-variadic-macro pprint-rix-value 
    value>> "varimac " write [ param-names>> "list" <val> pprint-rix-value  ] 
    [ body>> dup V{ SYM: prim } "list" <val> = not [ " " write dup sequence? [ 1array ] unless "list" <val> pprint-rix-value ] [ drop ] if ] bi  ;
M: rix-variadic-macro evaluates-to-something? drop t ;


! varinl  [x ys] '[,x @ys]
RIX-TYPE: rix-inline-variadic
M: rix-inline-variadic pprint-rix-value value>> "varinl " write [ param-names>> "list" <val> pprint-rix-value ] 
    [ body>> dup V{ SYM: prim } "list" <val> = not [ " " write dup sequence? [ 1array ] unless "list" <val> pprint-rix-value ] [ drop ] if ] bi  ;
M: rix-inline-variadic evaluates-to-something? drop t ;

! ,something
RIX-TYPE: rix-unquote
M: rix-unquote pprint-rix-value "," write value>> pprint-rix-value ;

RIX-TYPE: rix-type ! sorta funny
M: rix-type pprint-rix-value "typ " write value>> write ;

! @something
RIX-TYPE: rix-splice
M: rix-splice pprint-rix-value "@" write value>> pprint-rix-value ;

! genr 'foo [x y]
RIX-TYPE: rix-generic
M: rix-generic pprint-rix-value "genr '" write [ value>> name>> value>> write ] [ " " write value>> params>> pprint-rix-value ] bi ;
M: rix-generic evaluates-to-something? drop t ;

MAIN: run-rix
