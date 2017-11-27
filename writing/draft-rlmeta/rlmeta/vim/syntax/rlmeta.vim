if exists("b:current_syntax")
    finish
endif

syntax keyword metaKeyword compiler
syntax keyword metaKeyword Program
syntax keyword metaKeyword EOF

syntax region metaTargetLanguage start=+"+ end=+"+

syntax region metaRe start=+/+ end=+/+ skip=+\\/+
syntax region metaRe start=+'+ end=+'+ skip=+\\'+

syntax match metaRe +\\n+

syntax match metaOperator +=+
syntax match metaOperator +|+
syntax match metaOperator +;+
syntax match metaOperator +*+
syntax match metaOperator +!+

syntax match metaOutputOperator +[%]+
syntax match metaOutputOperator +[<]+
syntax match metaOutputOperator +[>]+
syntax match metaOutputOperator +[<]+
syntax match metaOutputOperator +[#]+
syntax match metaOutputOperator +[@]+

syntax match metaBlock +{+
syntax match metaBlock +}+

syntax region metaVerbatim start=+<<<+ end=+>>>+

syntax region metaCompilerLanguage start=+\$+ end=+\$+ skip=+\\\$+

syntax sync fromstart

hi def link metaKeyword           Keyword
hi def link metaTargetLanguage    Underlined
hi def link metaVerbatim          Underlined
hi def link metaCompilerLanguage  Special
hi def link metaRe                Type
hi def link metaOutputOperator    Operator
hi def link metaOperator          Operator
hi def link metaBlock             Operator

let b:current_syntax = "rlmeta"
