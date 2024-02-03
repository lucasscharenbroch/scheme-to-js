# Scheme To JS

A Scheme-to-JavaScript transpiler

## Supported Syntax
- **Literals**
  - **Number**: *1*, *-2*, *3.5*
  - **Character**: *#\a*, *#\newline*
  - **String**: *"hello"*
  - **Bool**: *#t*, *#f*
- **Definition** *(define pi 3.141592)*
- **Assignment**: *(set! id val)*
- **Variadic functions**: *(define (+ . args) (foldl b+ 0 args))*
- **Quotation**: *'(+ 1 2)*
- **Vectors**: *'#(1 2 3)*
- **Lambdas**: *(lambda (x y) (+ x y))*
- **Special Forms**: *if*, *cond*, *and*, *or*, *let*, *let\**, *letrec*, *begin*

## Other Features
- Pretty-Printing: *(print (cons 1 (cons 2 3)))* => prints "(1 2 . 3)"
- Preprocessor Directive for File Inclusion: *;#include std.scm*
    - *./* and *./scm-lib/* are searched for matching files
    - Topological sort for dependencies
- Standard Library (std.scm)
  - Implementations of *eval* and *apply*
  - Most other standard scheme procedures
- Tail-Call Optimization
