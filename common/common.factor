IN: rix.common
USING: kernel sequences io.files lexer parser environment splitting assocs vectors math arrays ;
TUPLE: rix-value value type description ;

ERROR: eval-error msg ;

: <val> ( value type -- rix-value ) "No description" rix-value boa ;

: hash-to-seq ( hash -- seq ) V{ } clone [ [ suffix! ] bi@ ] assoc-reduce ;

: seq-to-pairs ( seq -- pairs ) dup length 0 > [ unclip V{  } clone swap suffix! [ over last vector?
                                                                                   [ over last length 2 <= [ suffix! ] [ [ unclip-last ] dip suffix! suffix! ] if ] [ [ unclip-last ] dip 2array >vector suffix! ] if ] reduce ] when ;


TUPLE: rix-decl symbol expr ;

TUPLE: inl types param-names quot body ;
TUPLE: module-call symbol module ;
TUPLE: func types env param-names quot body ;
TUPLE: genr name params ;

SYNTAX: SYM: scan-token "symbol" <val> suffix! ;
SYNTAX: TYP: scan-token "type" <val> suffix! ;

: when% ( quot -- ? ) [ f ] if ; inline
: dwhen% ( quot -- ? ) [ drop f ] if ; inline
: when*% ( quot -- ? ) [ f ] if* ; inline

: get-paths ( -- paths ) "RIX-PATH" os-env [ ":" split ] [ { "~/.rix" } ] if* "." prefix ;

: (find-file-path) ( module-name -- incomplete-file-path ) "." "/" replace "|" "../" replace ".rix" append "/" prepend ;

: find-file-path ( module-name -- file-path ) (find-file-path) get-paths swap [ append ] curry map [ file-exists? ] find nip [ "Unknown module" eval-error f ] unless*  ;

: find-core-file-path ( -- file-path ) "/core.rix" get-paths swap [ append ] curry map [ file-exists? ] find nip [ "core.rix is missing" eval-error f ] unless* ;

