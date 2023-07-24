;; ~/.config/emacs/templates

prog-mode

(todo (if (derived-mode-p 'emacs-lisp-mode) ";; " comment-start) "TODO ")

python-base-mode

(try "try:" n> r> n "except " p " as " p ":" n> r> n "else:" n> r> n "finally:" n> r> n)
(iff "if " p ":" n> r> n "else:" n> r> n)
(main "if __name__ == '__main__':" n> r> n)

emacs-lisp-mode

(autoload ";;;###autoload")
(pt "(point)")
(var "(defvar " p "\n  \"" p "\")")
(local "(defvar-local " p "\n  \"" p "\")")
(const "(defconst " p "\n  \"" p "\")")
(custom "(defcustom " p "\n  \"" p "\"" n> ":type '" p ")")
(face "(defface " p " '((t :inherit " p "))\n  \"" p "\")")
(group "(defgroup " p " nil\n  \"" p "\"" n> ":group '" p n> ":prefix \"" p "-\")")
(macro "(defmacro " p " (" p ")\n  \"" p "\"" n> r> ")")
(alias "(defalias '" p " '" p ")")
(fun "(defun " p " (" p ")\n  \"" p "\"" n> r> ")")
(iflet "(if-let (" p ")" n> r> ")")
(whenlet "(when-let (" p ")" n> r> ")")
(whilelet "(while-let (" p ")" n> r> ")")
(andlet "(and-let* (" p ")" n> r> ")")
(cond "(cond" n "(" q "))" >)
(pcase "(pcase " (p "scrutinee") n "(" q "))" >)
(let "(let (" p ")" n> r> ")")
(lett "(let* (" p ")" n> r> ")")
(pcaselet "(pcase-let (" p ")" n> r> ")")
(pcaselett "(pcase-let* (" p ")" n> r> ")")
(rec "(letrec (" p ")" n> r> ")")
(dotimes "(dotimes (" p ")" n> r> ")")
(dolist "(dolist (" p ")" n> r> ")")
(loop "(cl-loop for " p " in " p " do" n> r> ")")
(command "(defun " p " (" p ")\n  \"" p "\"" n> "(interactive" p ")" n> r> ")")
(advice "(defun " (p "adv" name) " (&rest app)" n> p n> "(apply app))" n>
        "(advice-add #'" (p "fun") " " (p ":around") " #'" (s name) ")")
(header ";;; " (file-name-nondirectory (or (buffer-file-name) (buffer-name)))
        " -- " p " -*- lexical-binding: t -*-" n
        ";;; Commentary:" n ";;; Code:" n n)
(provide "(provide '" (file-name-base (or (buffer-file-name) (buffer-name))) ")" n
         ";;; " (file-name-nondirectory (or (buffer-file-name) (buffer-name)))
         " ends here" n)

org-mode

(src "#+begin_src " q n> r> n "#+end_src")
(q "#+begin_quote " q n> r> n "#+end_quote")
(el "#+begin_src emacs-lisp" n> r> n "#+end_src" :post (org-edit-src-code))
(jp "#+begin_src jupyter-python" n> r> n "#+end_src")
(ch "#+begin_src chatgpt-shell" n> r> n "#+end_src")
(ha "#+PROPERTY: header-args :session " p)
(dp ":display plain")

;; Local Variables:
;; mode: lisp-data
;; outline-regexp: "[a-z]"
;; End:
