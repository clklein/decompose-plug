Pattern from:
http://www.colourlovers.com/pattern/1925953/violety

share and share alike ...



https://github.com/clklein/decompose-plug/tree/master/aplas2011/slides

What's still needed is a brief overview of the semantics for reduction
(grammar for reduction rules, schema for judgment form, etc.) and some
examples to show that the rules need to cope with "path extension"
when you cons together two terms with holes. The examples in the
paper, and perhaps the one that convinced us to add `hide-hole', are
what I was imagining.

FWIW, here's the intro I had in mind:

PLT Redex is a domain-specific language for defining programming
languages and program transformations. Redex definitions look roughly
like the definitions you'd write on a whiteboard or your paper, but
they run. You can explore examples, write test cases, or check that
your definitions produce the same answers as any other implementations
you happen to have lying around. Once you're satisfied that your
definitions are correct, Redex can help you avoid typos in the figures
you put in your paper by generating Postscript directly from the
definitions you've been running. But for this process to go smoothly,
it's important that Redex interprets the definitions in the same way
that a reader would. As it turns out, making that happen has been more
difficult than we initially realized. The rest of the talk will
discuss some of the issues that make it tricky to mechanize the
semi-formal notation we use in papers.
