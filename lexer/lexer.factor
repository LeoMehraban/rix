IN: rix.lexer
USING: kernel generic parser make assocs command-line ranges literals tools.continuations namespaces serialize arrays environment lexer system alien.libraries prettyprint splitting classes.tuple colors hashtables continuations sequences.deep prettyprint.custom prettyprint.sections  words classes.predicate quotations accessors vectors classes.parser math math.functions sequences combinators classes combinators.smart unicode strings io summary io.styles io.files io.encodings.utf8 io.streams.string math.parser strings.parser io.encodings.ascii io.encodings.utf16 io.pathnames io.directories rix.common ;

ERROR: lexer-error lexer msg ;
TUPLE: rix-lexer str pos ;
! ...before after...
M: lexer-error summary 
    [ 
        lexer>> [ str>> ] [ pos>> ] bi cut [ dup length 5 > [ 5 tail* "..." prepend ] when ] 
        [ dup length 6 > [ 6 head "..." append ] when ] bi* append 
    ] [ msg>> ": " append ] bi prepend ;
! M: lexer-error pprint* summary text ;
CONSTANT: keywords { "if" "let" "fn" "consfn" "mac" "consmac" "quot" "uquot" "+" "-" "*" "/" }
<PRIVATE
DEFER: lex-val
DEFER: lex-until-semi

: current ( lexer -- char )  [ pos>> ] [ str>> ] bi nth ;
: writech ( char -- ) 1string write ;
: lexer-next ( lexer -- lexer char/f ) dup [ current ] [ [ 1 + ] change-pos ] bi [ pos>> ] [ str>> length ] bi < [ drop f ] unless ;
: valid-length? ( lexer -- lexer ? ) dup [ str>> length ] [ pos>> 1 + ] bi >= ;
: ?lexer-next ( lexer -- lexer char ? ) dup [ current ] [ [ 1 + ] change-pos ] bi [ pos>> ] [ str>> length ] bi < ;
: match-and-advance ( lexer char -- lexer matched? ) over valid-length? nip [ over current = [ [ lexer-next drop ] when ] keep ] dwhen% ;
: reset-if ( lexer quot: ( ..a lexer -- ..b lexer parsed? ) -- ..b lexer parsed? ) over clone [ call swap ] dip [ ? ] keepdd ; inline
: skip-whitespace ( lexer -- lexer ) valid-length? [ [ dup current blank? [ lexer-next ] when% ] loop ] when ;
: skip-non-newline-whitespace ( lexer -- lexer ) valid-length? [ [ dup current [ blank? ] [ CHAR: \n = not ] bi and [ lexer-next ] when% ] loop ] when ;
: valid-char? ( char -- ? ) 
    [ { [ CHAR: ( eq? ] [ CHAR: ) eq? ] [ CHAR: { eq? ] [ CHAR: } eq? ]  [ CHAR: [ eq? ] [ CHAR: ] eq? ] [ CHAR: ; eq? ] [ CHAR: @ eq? ] [ CHAR: $ eq? ] [ CHAR: " eq? ] } cleave ] output>array [  ] any? not ;
: lex-comment ( lexer -- lexer ) CHAR: # match-and-advance [ [ valid-length? [ dup current CHAR: \n = not [ lexer-next ] when% ] when% ] loop ] when ;
: lex-comment? ( lexer -- lexer ? ) CHAR: # match-and-advance [ [ valid-length? [ dup current CHAR: \n = not [ lexer-next ] when% ] when% ] loop t ] when% ;
: skip-useless ( lexer -- lexer ) [ valid-length? [ skip-whitespace valid-length? [ lex-comment? ] when% ] when% ] loop ;
: lex-word ( lexer -- lexer word/f ) t [ [ [ valid-length? [ dup current [ valid-char? ] [ blank? not ] bi and ] when% ] when% ] [ ?lexer-next [ writech ] dip ] while ] with-string-writer dup empty? [ drop f ] when ;
: lex-number ( lexer -- lexer number/f ) [ lex-word [ dec> [ "number" <val> ] when*% ] when*% ] reset-if ;
: lex-module-call ( lexer -- lexer module-call/f ) [ lex-word [ swap CHAR: @ match-and-advance [ lex-word [ swapd [ "symbol" <val> ] bi@ module-call boa "module-call" <val> ] [ nip f ] if* ] [ nip f ] if ] when*% ] reset-if ;
: lex-resolve ( lexer -- lexer resolve/f ) CHAR: $ match-and-advance [ [ lex-module-call [ value>> "module-resolve" <val> ]  [ lex-word [ "resolve" <val> ] when*% ] if* ] reset-if ] when%  ;
: lex-semi ( lexer -- lexer semi/f ) CHAR: ; match-and-advance [ t "semicolon" <val> ] when% ;
: lex-dec ( lexer -- lexer dec/f ) [ lex-word [ unclip-last CHAR: : = [ swap lex-until-semi swapd rix-decl boa "dec" <val> ] dwhen% ] when*% ] reset-if ;
: lex-list ( lexer -- lexer list/f ) 91 match-and-advance [ V{  } clone swap [ skip-useless 93 match-and-advance not ] [ lex-val swapd suffix! swap ] while swap "list" <val> ] when% ;
: lex-hash ( lexer -- lexer hash/f ) 
    CHAR: { match-and-advance 
    [ V{  } clone swap [ skip-useless CHAR: } match-and-advance not ] [ lex-val swapd suffix! swap ] while swap seq-to-pairs parse-hashtable "hash" <val> ] when% ;
: lex-symbol ( lexer -- lexer symbol/f ) 
    lex-word 
    [ 
        dup [ "tru" = ] [ "fal" = ] bi or 
        [ "tru" = "bool" <val> ] 
        [ 
            dup "nil" = 
            [ drop f "nil" <val> ]
            [ dup keywords index [ t swap <val> ] [ "symbol" <val> ] if ] if 
        ] if 
    ] when*%
    ;
: lex-quote ( lexer -- lexer quote/f ) CHAR: ' match-and-advance [ lex-val "quote" <val> ] when% ;
: lex-unquote ( lexer -- lexer unquote/f ) CHAR: , match-and-advance [ lex-val "unquote" <val> ] when% ;
: lex-splice ( lexer -- lexer splice/f ) CHAR: @ match-and-advance [ lex-val "splice" <val> ] when% ;
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

: lex-val ( lexer -- lexer val ) 
    skip-useless
    lex-quote
    [ lex-unquote ] unless*
    [ lex-splice ] unless*
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
    [ valid-length? not ] unless*
    [ skip-useless ] dip
    [ "Invalid expression" lexer-error ] unless*
    ;
! pos lexer
: lex-until-semi ( lexer -- lexer expr ) 
    [ pos>> ] keep V{ } clone swap [
        skip-useless
        lex-quote
        [ lex-unquote ] unless*
        [ lex-splice ] unless*
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
        [ valid-length? [ dup current CHAR: ; = not ] [ t ] if [ rot >>pos "expected semicolon but got EOF" lexer-error ] when f ] unless*
        [ swapd suffix! swap ] when*
        CHAR: ; match-and-advance not
    ] loop
    swap nipd
 ;

: can-continue? ( lexer -- lexer ? ) skip-whitespace lex-comment valid-length? ;
PRIVATE>
: lex-str ( str -- tokens ) 0 rix-lexer boa V{  } clone swap [ lex-val dup t = [ drop f ] [ swapd suffix! swap can-continue? ] if ] loop drop ;
