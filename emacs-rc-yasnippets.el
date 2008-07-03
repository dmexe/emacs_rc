;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; html
;;
(yas/define-snippets 'html-mode
  '(("<t" "<${1:div}>$0</$1>" "<...><...>")
    ("<" "<${1:div}>$0</$1>" "<...><...>")
    ("<i" "<${1:div} id=\"${2:id}\">$0</$1>" "<... id=\"..\"><...>")
    ("<c" "<${1:div} class=\"${2:id}\">$0</$1>" "<... class=\"..\"><...>")
    ("cd" "<![CDATA[$0]]>" "CDATA")
    ("table" "<table>\n  <thead>\n  </thead>\n  <tbody>\n    $0\n  </tbody>\n</table>" "<table>")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; nxml
;;
(yas/define-snippets 'nxml-mode
  '(("decl" "<xsl:stylesheet version=\"1.0\"
  xmlns:xsl=\"http://www.w3.org/1999/XSL/Transform\"
  xmlns=\"http://www.w3.org/TR/xhtml1/strict\">
  $0
</xsl:stylesheet>" "<xsl:stylesheet ..>")
    ("xtm" "<xsl:template match=\"${match}\">
  $0
</xsl:template>" "<xsl:template match=\"..\">")
    ("xtmm" "<xsl:template match=\"${match}\" mode=\"${mode}\">
  $0
</xsl:template>" "<xsl:template match=\"..\" mode=\"...\">")
    ("xtn" "<xsl:template name=\"${match}\">
  $0
</xsl:template>" "<xsl:template name=\"..\">...")
    ("xat" "<xsl:apply-templates select=\"${match}\" />\n$0" "<xsl:apply-templates select=\"..\"/>")
    ("xatm" "<xsl:apply-templates select=\"${match}\" mode=\"${mode}\" />\n$0" "<xsl:apply-templates select=\"..\" mode=\"...\"/>")
    ("xvo" "<xsl:value-of select=\"${match}\" />\n$0" "<xsl:value-of select=\"..\"/>")
    ("xfe" "<xsl:for-each select=\"${match}\">
  $0
</xsl:for-each>" "<xsl:for-each select=\"..\"/>")
    )
  'html-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; emacs lisp
;;
(yas/define-snippets 'emacs-lisp-mode
  '(("lam" "'(lambda (${1:it}) ($0))" "lambda")
    ("let" "(let ((${sexp}))\n  ($0))" "let")))

