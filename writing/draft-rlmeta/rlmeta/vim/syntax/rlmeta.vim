if exists("b:current_syntax")
    finish
endif

syntax keyword metaKeyword compiler
syntax keyword metaKeyword Program
syntax keyword metaKeyword end
syntax keyword metaKeyword any
syntax keyword metaKeyword upper
syntax keyword metaKeyword lower
syntax keyword metaKeyword digit
syntax keyword metaKeyword space

syntax region metaTargetLanguage start=+"+ end=+"+ skip=+\\'\|\\"\|\\\\\|\\n\|\\t+

syntax region metaLiteral start=+'+ end=+'+ skip=+\\'\|\\"\|\\\\\|\\n\|\\t+

syntax match metaOperator +[=]+
syntax match metaOperator +[|]+
syntax match metaOperator +[;]+
syntax match metaOperator +[*]+
syntax match metaOperator +[!]+

syntax match metaOutputOperator +[.]+
syntax match metaOutputOperator +[>]+
syntax match metaOutputOperator +[<]+
syntax match metaOutputOperator +[#]+
syntax match metaOutputOperator +[@]+

syntax match metaReturn +[-][>]+

syntax match metaBlock +{+
syntax match metaBlock +}+

syntax region metaVerbatim start=+<<<+ end=+>>>+

syntax region metaCompilerLanguage start=+\$+ end=+\$+ skip=+\\\$+

syntax sync fromstart

hi def link metaKeyword           Keyword
hi def link metaTargetLanguage    Underlined
hi def link metaVerbatim          Underlined
hi def link metaCompilerLanguage  Special
hi def link metaReturn            Todo
hi def link metaLiteral           String
hi def link metaOutputOperator    Macro
hi def link metaOperator          Operator
hi def link metaBlock             Operator

let b:current_syntax = "rlmeta"
