# Paren-jQuery

Parenscript macros for jQuery

## Warning

This software is still BETA quality. The APIs will be likely to change.

## Installation

This system can be installed from [UltraLisp](https://ultralisp.org/) like this:

```common-lisp
(ql-dist:install-dist "http://dist.ultralisp.org/"
                      :prompt nil)
(ql:quickload "paren-jquery")
```

## Usage

```common-lisp
($ ".avatar" (show))
; => jQuery('.avatar').show();

($ (".avatar" ".block1") (show))
; => jQuery('.avatar', '.block1').show();

($ ".avatar" (show) (data :id 100))
; => jQuery('.avatar').show().data('id', 100);

($show ".avatar")
; => jQuery('.avatar').show();

($data ".avatar" :id 100)
; => jQuery('.avatar').data('id', 100);

($-> (get-j-s-o-n "https://example.org/api"))
; => jQuery.getJSON('https://example.org/api');

($-> (get-j-s-o-n "https://example.org/api") 
     (done (lambda () "Done!")))
; => jQuery.getJSON('https://example.org/api').done(function () {
;      return 'Done!';
;    });

($-get-json "https://example.org/api")
; => jQuery.getJSON('https://example.org/api');

```

[Full API]()
