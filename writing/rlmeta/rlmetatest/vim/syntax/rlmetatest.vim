if exists("b:current_syntax")
    finish
endif

syntax match headerPart +^# .*$+
syntax match headerPart +:+

syntax match bodyPart +==>+
syntax match bodyPart +---+

hi def link headerPart       Define
hi def link bodyPart         Operator

let b:current_syntax = "rlmetatest"
