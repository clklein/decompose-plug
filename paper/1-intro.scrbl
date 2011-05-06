#lang scribble/base

@title{Problem Setup}
@itemlist[
@item{The semantics of context-sensitive reduction semantics is straightforward,
until you permit holes in terms themselves (e.g., terms denoting reified 
continuations).}
@item{Related work: Felleisen, Danvy, and that rewriting guy who thinks he
invented context-sensitive rewriting. Do their definitions cover terms with 
holes? Do they define plug and decompose on a language-by-language basis?}
@item{Current Redex solution: hide-hole, the-hole, and the-not-hole.}
]
