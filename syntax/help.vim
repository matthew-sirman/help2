" Vim syntax file
" Language: HELP
" Maintainer: Matthew Sirman

if exists("b:current_syntax")
    finish
endif

" Keywords
syn keyword helpKeyword infix func value let in
syn keyword helpKeyword type contained

" Matches
syn match helpNumber "\(^\|\s\)\d\+"
syn match helpNumber "\(^\|\s\)\d\+\.\d\+"
syn match helpComment "//.*$"
syn match helpType "[A-Z]\w*" contained

syn match helpOp "|"
syn match helpOp ":\@<!\(=\|-\)>\?"
syn match helpOp "\$"

" Regions
syn region helpString start='"' end='"'
syn region helpTypeCtor start="type" end="::=\|;" transparent contains=helpType,helpKeyword
syn region helpFuncType start=": \@=" end=";" transparent contains=helpType,helpOp

let b:current_syntax = "help"

hi def link helpKeyword Statement
hi def link helpNumber Constant
hi def link helpString Constant
hi def link helpComment Comment
hi def link helpType Type
hi def link helpOp Operator
