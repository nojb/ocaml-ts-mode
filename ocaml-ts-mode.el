;;; ocaml-ts-mode.el --- tree-sitter support for OCaml  -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2023 Free Software Foundation, Inc.

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

(require 'treesit)

(declare-function treesit-parser-create "treesit.c")
(declare-function treesit-node-parent "treesit.c")
(declare-function treesit-node-start "treesit.c")
(declare-function treesit-node-end "treesit.c")
(declare-function treesit-node-child "treesit.c")
(declare-function treesit-node-child-by-field-name "treesit.c")
(declare-function treesit-node-type "treesit.c")
(declare-function treesit-node-prev-sibling "treesit.c")
(declare-function treesit-node-first-child-for-pos "treesit.c")
(declare-function treesit-node-next-sibling "treesit.c")
(declare-function treesit-parser-set-included-ranges "treesit.c")
(declare-function treesit-query-compile "treesit.c")

;;; Custom variables

;;; Syntax table

(defvar ocaml-ts-mode--syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?+   "."      table)
    (modify-syntax-entry ?-   "."      table)
    (modify-syntax-entry ?=   "."      table)
    (modify-syntax-entry ?%   "."      table)
    (modify-syntax-entry ?&   "."      table)
    (modify-syntax-entry ?|   "."      table)
    (modify-syntax-entry ?^   "."      table)
    (modify-syntax-entry ?!   "."      table)
    (modify-syntax-entry ?@   "."      table)
    (modify-syntax-entry ?~   "."      table)
    (modify-syntax-entry ?<   "."      table)
    (modify-syntax-entry ?>   "."      table)
    (modify-syntax-entry ?/   ". 124b" table)
    (modify-syntax-entry ?*   ". 23"   table)
    (modify-syntax-entry ?\n  "> b"    table)
    (modify-syntax-entry ?\^m "> b"    table)
    table)
  "Syntax table for `ocaml-ts-mode'.")

;;; Font-lock

(defvar ocaml-ts-mode--keywords
  '("let" "mod" "in" "open" "if" "then" "else" "for" "to" "downto"
    "do" "done" "while" "try" "with" "match" "and" "rec" "module"
    "class" "object" "struct" "functor" "begin" "end" "type"
    "of" "nonrec" "fun" "sig" "function" "val" "include" "when" "exception"
    "method" "private" "virtual")
  "OCaml keywords for tree-sitter font-locking.")

(defvar ocaml-ts-mode--font-lock-settings
  (treesit-font-lock-rules
   :language 'ocaml
   :feature 'keyword
   `([,@ocaml-ts-mode--keywords] @font-lock-keyword-face)

   :language 'ocaml
   :feature 'delimiter
   '([";" "." ","] @font-lock-delimiter-face)

   :language 'ocaml
   :feature 'type
   '((type_constructor) @font-lock-type-face)

   :language 'ocaml
   :feature 'comment
   '([(comment)] @font-lock-comment-face)

   :language 'ocaml
   :feature 'bracket
   '((["(" ")" "[" "]" "{" "}"]) @font-lock-bracket-face)

   :language 'ocaml
   :feature 'number
   '([(number)] @font-lock-number-face)

   :language 'ocaml
   :feature 'string
   '([(string) (character)] @font-lock-string-face)

   :language 'ocaml
   :feature 'function
   '(
     ;; (application_expression
     ;;  function: [(value_path [(value_name) @font-lock-function-name-face])])
     (value_definition
      (let_binding pattern: [(value_name) @font-lock-function-name-face]
                   (parameter)
                   )))

   :language 'ocaml
   :feature 'constructor
   '(["true" "false" (tag) (constructor_name)] @font-lock-constant-face)

   :language 'ocaml
   :feature 'module
   '([(module_name)] @font-lock-builtin-face)

   :language 'ocaml
   :feature 'variable
   '(
     (value_definition
      (let_binding pattern: [(value_name) @font-lock-variable-name-face]
                   )))
   ;; '([(value_name) (value_pattern)] @font-lock-variable-name-face))
   )
  "Tree-sitter font-lock settings for `ocaml-ts-mode'.")

;;; Imenu

(defun ocaml-ts-mode--imenu ()
  "Return Imenu alist for the current buffer."
  (let* ((node (treesit-buffer-root-node))
         ;; (enum-tree (treesit-induce-sparse-tree
         ;;             node "enum_item" nil))
         ;; (enum-index (rust-ts-mode--imenu-1 enum-tree))
         ;; (func-tree (treesit-induce-sparse-tree
         ;;             node "function_item" nil))
         ;; (func-index (rust-ts-mode--imenu-1 func-tree))
         ;; (impl-tree (treesit-induce-sparse-tree
         ;;             node "impl_item" nil))
         ;; (impl-index (rust-ts-mode--imenu-1 impl-tree))
         (mod-tree (treesit-induce-sparse-tree
                    node "module_binding" nil))
         (mod-index (ocaml-ts-mode--imenu-1 mod-tree)))
         ;; (struct-tree (treesit-induce-sparse-tree
         ;;               node "struct_item" nil))
         ;; (struct-index (rust-ts-mode--imenu-1 struct-tree))
         ;; (type-tree (treesit-induce-sparse-tree
         ;;             node "type_item" nil))
         ;; (type-index (rust-ts-mode--imenu-1 type-tree)))
    (append
     (when mod-index `(("Module" . ,mod-index))))))
     ;; (when enum-index `(("Enum" . ,enum-index)))
     ;; (when impl-index `(("Impl" . ,impl-index)))
     ;; (when type-index `(("Type" . ,type-index)))
     ;; (when struct-index `(("Struct" . ,struct-index)))
     ;; (when func-index `(("Fn" . ,func-index))))))

(defun ocaml-ts-mode--imenu-1 (node)
  "Helper for `ocaml-ts-mode--imenu'.
Find string representation for NODE and set marker, then recurse
the subtrees."
  (let* ((ts-node (car node))
         (children (cdr node))
         (subtrees (mapcan #'ocaml-ts-mode--imenu-1
                           children))
         (name (when ts-node
                 (pcase (treesit-node-type ts-node)
                   ;; ("enum_item"
                   ;;  (treesit-node-text
                   ;;   (treesit-node-child-by-field-name ts-node "name") t))
                   ;; ("function_item"
                   ;;  (treesit-node-text
                   ;;   (treesit-node-child-by-field-name ts-node "name") t))
                   ;; ("impl_item"
                   ;;  (let ((trait-node (treesit-node-child-by-field-name ts-node "trait")))
                   ;;    (concat
                   ;;     (treesit-node-text
                   ;;      trait-node t)
                   ;;     (when trait-node
                   ;;       " for ")
                   ;;     (treesit-node-text
                   ;;      (treesit-node-child-by-field-name ts-node "type") t))))
                   ("module_binding"
                    (treesit-node-text
                     (treesit-node-child-by-field-name ts-node "name") t)))))
                   ;; ("struct_item"
                   ;;  (treesit-node-text
                   ;;   (treesit-node-child-by-field-name ts-node "name") t))
                   ;; ("type_item"
                   ;;  (treesit-node-text
                   ;;   (treesit-node-child-by-field-name ts-node "name") t)))))
         (marker (when ts-node
                   (set-marker (make-marker)
                               (treesit-node-start ts-node)))))
    (cond
     ((or (null ts-node) (null name)) subtrees)
     (subtrees
      `((,name ,(cons name marker) ,@subtrees)))
     (t
      `((,name . ,marker))))))

;;; Indent

(defvar ocaml-ts-mode--indent-rules
  `((ocaml

     ))
  "Tree-sitter indent rules for `ocaml-ts-mode'.")

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.mli?\\'" . ocaml-ts-mode))

;;;###autoload
(define-derived-mode ocaml-ts-mode prog-mode "OCaml"
  "Major mode for editing OCaml, powered by tree-sitter."
  :group 'ocaml
  :syntax-table ocaml-ts-mode--syntax-table

  (when (treesit-ready-p 'ocaml)
    (treesit-parser-create 'ocaml)

    (setq-local treesit-font-lock-settings ocaml-ts-mode--font-lock-settings
                treesit-font-lock-feature-list
                '((keyword string module constructor type comment function delimiter variable) (bracket number)))

    ;; Imenu.
    (setq-local imenu-create-index-function #'ocaml-ts-mode--imenu)

    (setq-local indent-tabs-mode nil
                treesit-simple-indent-rules ocaml-ts-mode--indent-rules)

    (treesit-major-mode-setup)))

(provide 'ocaml-ts-mode)

;;; ocaml-ts-mode.el ends here
