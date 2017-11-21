syntax keyword metaKeyword compiler

syntax region metaString start=+"+ end=+"+

syntax match metaOperator +=+
syntax match metaOperator +|+
syntax match metaOperator +;+

syntax match metaOutputOperator +%+
syntax match metaOutputOperator +<+
syntax match metaOutputOperator +>+
syntax match metaOutputOperator +[.]+

syntax match metaBlock +{+
syntax match metaBlock +}+

syntax region metaVerbatim start=+<<<+ end=+>>>+

hi def link metaKeyword         Keyword
hi def link metaString          String
hi def link metaVerbatim        Underlined
hi def link metaOperator        Operator
hi def link metaOutputOperator  Delimiter
hi def link metaBlock           Include
