#lang scribble/base

@(require redex/pict
          redex/reduction-semantics
          "kont-model/model.rkt"
          "kont-model/util.rkt"
          "wfigure.rkt"
          "citations.rkt")


@title{Matching and Contexts}

Lorem ipsum dolor sit amet, consectetur adipiscing elit. Cras posuere, lectus a tincidunt ultrices, enim neque dictum diam, sit amet pretium sapien eros sed enim. Suspendisse potenti. Quisque tortor leo, sagittis quis rhoncus nec, sagittis eget est. Maecenas eu metus at risus porta posuere et sed elit. Nunc vulputate ultricies tellus, vitae accumsan nisl sollicitudin eu. Nam dapibus dui in erat elementum pulvinar. Quisque scelerisque, neque facilisis ullamcorper mattis, nunc purus tincidunt massa, eget mollis libero arcu vitae mi. Aliquam lobortis nisl in metus commodo sed auctor dolor volutpat. Morbi risus erat, ultricies at mollis et, sagittis ac tellus. Fusce consequat lorem id lectus semper semper. Aenean facilisis purus eget nunc viverra nec sagittis tortor tempor. Vestibulum scelerisque, orci at ultrices pharetra, ligula neque ullamcorper dolor, eget vestibulum nisi justo eu neque. Vestibulum lacinia volutpat felis, ac fermentum justo aliquam id.

@wfigure["fig:arith" "Arithmetic Expressions"]{
@(render-language arith)

@(render-language arith/red)

@(render-reduction-relation arith-red)
}


Aliquam tempor, enim quis dignissim viverra, nisi lacus consequat justo, vitae cursus orci lacus id elit. Ut placerat convallis venenatis. Fusce lacinia imperdiet feugiat. Suspendisse potenti. Phasellus in nunc diam. Praesent vel feugiat erat. Suspendisse urna quam, dictum porttitor eleifend in, lacinia vitae mauris. Suspendisse at eros vel libero pulvinar sodales. Phasellus magna nulla, gravida vitae congue at, tempor ac odio. Cum sociis natoque penatibus et magnis dis parturient montes, nascetur ridiculus mus. In hac habitasse platea dictumst. Duis id leo vitae enim sodales aliquam. In quis libero in risus ultrices ultrices. Maecenas volutpat, risus vitae dapibus tristique, est risus bibendum justo, et volutpat risus risus quis enim. Vestibulum ante ipsum primis in faucibus orci luctus et ultrices posuere cubilia Curae; Nullam scelerisque quam at sem vestibulum a tempus mauris sagittis. Duis metus neque, malesuada sit amet commodo in, gravida sed purus. Proin auctor varius molestie. Quisque dui ligula, tempor ut ultrices eu, volutpat venenatis orci.


Aliquam tempor, enim quis dignissim viverra, nisi lacus consequat justo, vitae cursus orci lacus id elit. Ut placerat convallis venenatis. Fusce lacinia imperdiet feugiat. Suspendisse potenti. Phasellus in nunc diam. Praesent vel feugiat erat. Suspendisse urna quam, dictum porttitor eleifend in, lacinia vitae mauris. Suspendisse at eros vel libero pulvinar sodales. Phasellus magna nulla, gravida vitae congue at, tempor ac odio. Cum sociis natoque penatibus et magnis dis parturient montes, nascetur ridiculus mus. In hac habitasse platea dictumst. Duis id leo vitae enim sodales aliquam. In quis libero in risus ultrices ultrices. Maecenas volutpat, risus vitae dapibus tristique, est risus bibendum justo, et volutpat risus risus quis enim. Vestibulum ante ipsum primis in faucibus orci luctus et ultrices posuere cubilia Curae; Nullam scelerisque quam at sem vestibulum a tempus mauris sagittis. Duis metus neque, malesuada sit amet commodo in, gravida sed purus. Proin auctor varius molestie. Quisque dui ligula, tempor ut ultrices eu, volutpat venenatis orci.


@wfigure[#:size 2.2 "fig:lc" "λ-calculus"]{
@(render-language Λ #:nts (remove* '(x y) (language-nts Λ)))

@(render-language Λ/red)

@(render-reduction-relation cbv-red)
}

Curabitur lobortis luctus libero. Lorem ipsum dolor sit amet, consectetur adipiscing elit. Morbi venenatis metus nec sem tincidunt accumsan in ac neque. Proin viverra sapien a sapien feugiat quis feugiat mi sagittis. Duis quis justo sem. Quisque id augue risus, quis convallis urna. Fusce facilisis luctus ligula at cursus. Vivamus eget erat eget erat ultrices luctus id sed tortor. Mauris pellentesque odio in tortor semper a aliquam ligula pulvinar. Phasellus egestas metus eu ligula interdum ac pulvinar massa lobortis. Aenean bibendum ultrices dui at mattis. Pellentesque habitant morbi tristique senectus et netus et malesuada fames ac turpis egestas. Suspendisse in velit ut augue malesuada sodales in a dui. Vivamus mattis viverra neque, eget molestie dolor malesuada non. Sed auctor, nulla vitae lobortis interdum, ipsum ante sollicitudin nunc, ac feugiat odio dolor quis nulla. Vestibulum eget dui a neque sagittis facilisis et vel tellus.

Curabitur lobortis luctus libero. Lorem ipsum dolor sit amet, consectetur adipiscing elit. Morbi venenatis metus nec sem tincidunt accumsan in ac neque. Proin viverra sapien a sapien feugiat quis feugiat mi sagittis. Duis quis justo sem. Quisque id augue risus, quis convallis urna. Fusce facilisis luctus ligula at cursus. Vivamus eget erat eget erat ultrices luctus id sed tortor. Mauris pellentesque odio in tortor semper a aliquam ligula pulvinar. Phasellus egestas metus eu ligula interdum ac pulvinar massa lobortis. Aenean bibendum ultrices dui at mattis. Pellentesque habitant morbi tristique senectus et netus et malesuada fames ac turpis egestas. Suspendisse in velit ut augue malesuada sodales in a dui. Vivamus mattis viverra neque, eget molestie dolor malesuada non. Sed auctor, nulla vitae lobortis interdum, ipsum ante sollicitudin nunc, ac feugiat odio dolor quis nulla. Vestibulum eget dui a neque sagittis facilisis et vel tellus.

@wfigure["fig:cbn" "Call-by-need"]{
@(render-language Λneed/red #:nts '(E))

@(parameterize ([render-reduction-relation-rules '("deref")])
   (render-reduction-relation cbn-red))
}

Praesent arcu felis, dictum quis lacinia id, volutpat non tellus. Proin pharetra scelerisque nibh, ut feugiat diam venenatis ut. Mauris bibendum arcu vitae elit ornare sed ultricies neque ornare. Maecenas sit amet erat quis magna pellentesque adipiscing. Suspendisse accumsan sapien vitae lectus aliquet sed accumsan justo tempor. Curabitur vitae neque ac sem varius molestie vitae in nisi. Mauris varius accumsan auctor. Fusce id eros lacus, hendrerit tempus nulla. Cum sociis natoque penatibus et magnis dis parturient montes, nascetur ridiculus mus. Morbi aliquam erat sed massa feugiat ac rhoncus urna ultrices.

Praesent arcu felis, dictum quis lacinia id, volutpat non tellus. Proin pharetra scelerisque nibh, ut feugiat diam venenatis ut. Mauris bibendum arcu vitae elit ornare sed ultricies neque ornare. Maecenas sit amet erat quis magna pellentesque adipiscing. Suspendisse accumsan sapien vitae lectus aliquet sed accumsan justo tempor. Curabitur vitae neque ac sem varius molestie vitae in nisi. Mauris varius accumsan auctor. Fusce id eros lacus, hendrerit tempus nulla. Cum sociis natoque penatibus et magnis dis parturient montes, nascetur ridiculus mus. Morbi aliquam erat sed massa feugiat ac rhoncus urna ultrices.

Praesent arcu felis, dictum quis lacinia id, volutpat non tellus. Proin pharetra scelerisque nibh, ut feugiat diam venenatis ut. Mauris bibendum arcu vitae elit ornare sed ultricies neque ornare. Maecenas sit amet erat quis magna pellentesque adipiscing. Suspendisse accumsan sapien vitae lectus aliquet sed accumsan justo tempor. Curabitur vitae neque ac sem varius molestie vitae in nisi. Mauris varius accumsan auctor. Fusce id eros lacus, hendrerit tempus nulla. Cum sociis natoque penatibus et magnis dis parturient montes, nascetur ridiculus mus. Morbi aliquam erat sed massa feugiat ac rhoncus urna ultrices.

@wfigure["fig:cont" "Continuations"]{
@(render-language Λk/red)

@(render-reduction-relation cont-red)
}

Duis hendrerit imperdiet nisl, et interdum orci sollicitudin a. Duis eu lectus justo, at tincidunt libero. Nam tempus rutrum nibh, vitae auctor est rhoncus sed. In justo diam, accumsan nec fermentum id, consectetur eu lectus. Vestibulum libero diam, volutpat at eleifend a, tempus eget ligula. Sed urna libero, eleifend vitae accumsan at, adipiscing et nisi. Morbi tincidunt, lectus ac ullamcorper iaculis, arcu est sagittis massa, bibendum tempus mi diam ac justo. Sed imperdiet velit in quam molestie aliquam. Fusce vitae condimentum elit. Nulla facilisi. Integer scelerisque rutrum dui nec aliquam. In hac habitasse platea dictumst. Donec dictum congue egestas. Nullam non turpis enim, eget gravida odio. In hac habitasse platea dictumst. Morbi nisi enim, cursus nec iaculis blandit, imperdiet sit amet est. Aenean adipiscing faucibus ante non condimentum. Mauris vel mi lectus. Cum sociis natoque penatibus et magnis dis parturient montes, nascetur ridiculus mus. Pellentesque sodales consectetur neque quis mollis.

Duis hendrerit imperdiet nisl, et interdum orci sollicitudin a. Duis eu lectus justo, at tincidunt libero. Nam tempus rutrum nibh, vitae auctor est rhoncus sed. In justo diam, accumsan nec fermentum id, consectetur eu lectus. Vestibulum libero diam, volutpat at eleifend a, tempus eget ligula. Sed urna libero, eleifend vitae accumsan at, adipiscing et nisi. Morbi tincidunt, lectus ac ullamcorper iaculis, arcu est sagittis massa, bibendum tempus mi diam ac justo. Sed imperdiet velit in quam molestie aliquam. Fusce vitae condimentum elit. Nulla facilisi. Integer scelerisque rutrum dui nec aliquam. In hac habitasse platea dictumst. Donec dictum congue egestas. Nullam non turpis enim, eget gravida odio. In hac habitasse platea dictumst. Morbi nisi enim, cursus nec iaculis blandit, imperdiet sit amet est. Aenean adipiscing faucibus ante non condimentum. Mauris vel mi lectus. Cum sociis natoque penatibus et magnis dis parturient montes, nascetur ridiculus mus. Pellentesque sodales consectetur neque quis mollis.

@wfigure["fig:delim" "Delimited Continuations"]{
@(render-language Λdk/red)

@(render-reduction-relation delim-red)
}

In ut ipsum tellus. In quis mauris mi. Nam non lacus ante. Nunc non orci arcu, nec porttitor massa. Donec non nunc leo, a pulvinar mi. Vestibulum aliquam, neque eu mattis ultrices, leo mauris blandit nulla, a ultricies enim urna sit amet justo. Vestibulum mollis lacinia turpis semper malesuada. Quisque ac justo vel turpis ornare convallis. Quisque feugiat purus a nulla euismod vitae blandit ligula hendrerit. Morbi eu purus posuere ligula tincidunt malesuada. Praesent at odio neque. Aenean eu ante et mauris consectetur congue a eu odio. Sed viverra adipiscing accumsan. Cras sapien enim, ultrices non dapibus vitae, tempus vel tortor. Proin imperdiet risus in massa hendrerit condimentum. Nulla rutrum, nulla ut eleifend tincidunt, dui velit suscipit odio, non pharetra est risus sed ligula. Vestibulum nec libero vitae quam bibendum tempus eget in lectus.

In ut ipsum tellus. In quis mauris mi. Nam non lacus ante. Nunc non orci arcu, nec porttitor massa. Donec non nunc leo, a pulvinar mi. Vestibulum aliquam, neque eu mattis ultrices, leo mauris blandit nulla, a ultricies enim urna sit amet justo. Vestibulum mollis lacinia turpis semper malesuada. Quisque ac justo vel turpis ornare convallis. Quisque feugiat purus a nulla euismod vitae blandit ligula hendrerit. Morbi eu purus posuere ligula tincidunt malesuada. Praesent at odio neque. Aenean eu ante et mauris consectetur congue a eu odio. Sed viverra adipiscing accumsan. Cras sapien enim, ultrices non dapibus vitae, tempus vel tortor. Proin imperdiet risus in massa hendrerit condimentum. Nulla rutrum, nulla ut eleifend tincidunt, dui velit suscipit odio, non pharetra est risus sed ligula. Vestibulum nec libero vitae quam bibendum tempus eget in lectus.

In ut ipsum tellus. In quis mauris mi. Nam non lacus ante. Nunc non orci arcu, nec porttitor massa. Donec non nunc leo, a pulvinar mi. Vestibulum aliquam, neque eu mattis ultrices, leo mauris blandit nulla, a ultricies enim urna sit amet justo. Vestibulum mollis lacinia turpis semper malesuada. Quisque ac justo vel turpis ornare convallis. Quisque feugiat purus a nulla euismod vitae blandit ligula hendrerit. Morbi eu purus posuere ligula tincidunt malesuada. Praesent at odio neque. Aenean eu ante et mauris consectetur congue a eu odio. Sed viverra adipiscing accumsan. Cras sapien enim, ultrices non dapibus vitae, tempus vel tortor. Proin imperdiet risus in massa hendrerit condimentum. Nulla rutrum, nulla ut eleifend tincidunt, dui velit suscipit odio, non pharetra est risus sed ligula. Vestibulum nec libero vitae quam bibendum tempus eget in lectus.

@(define-language ex2
   (C (in-hole C (f hole)) hole))
@wfigure["fig:wacky" "Wacky Context"]{
@(render-language ex2)
}
