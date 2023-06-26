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
(declare-function treesit-node-child-by-field-name "treesit.c")
(declare-function treesit-node-type "treesit.c")

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

(defvar ocaml-intf-ts-mode--keywords
  '("exception"
    "val"
    "type"
    "module"
    "sig"
    "of"
    "with"
    "include"
    "end"
    "method"
    "class"
    "open"
    "mutable"
    "nonrec"
    "struct"))

(defvar ocaml-impl-ts-mode--keywords
  (append ocaml-intf-ts-mode--keywords
          '("let"
            "in"
            "if"
            "then"
            "else"
            "for"
            "to"
            "downto"
            "do"
            "done"
            "while"
            "try"
            "match"
            "and"
            "rec"
            "object"
            "functor"
            "begin"
            "fun"
            "function"
            "when"
            "private"
            "virtual"
            "lazy"
            "inherit"
            "initializer"
            "new"
            "as")))

(defvar ocaml-intf-ts-mode--font-lock-settings
  (treesit-font-lock-rules
   :language 'ocaml_interface
   :feature 'keyword
   `([,@ocaml-intf-ts-mode--keywords] @font-lock-keyword-face)

   :language 'ocaml_interface
   :feature 'delimiter
   '([";" "." ","] @font-lock-delimiter-face)

   :language 'ocaml_interface
   :feature 'type
   '((type_constructor) @font-lock-type-face)

   :language 'ocaml_interface
   :feature 'comment
   '((comment) @font-lock-comment-face)

   :language 'ocaml_interface
   :feature 'number
   '((number) @font-lock-number-face)

   :language 'ocaml_interface
   :feature 'string
   '([(string) (character)] @font-lock-string-face)

   :language 'ocaml_interface
   :feature 'variable
   '((assert_expression "assert" @font-lock-warning-face))

   :language 'ocaml_interface
   :feature 'function
   '(
     (method_definition
      name: (method_name) @font-lock-function-name-face)
     (value_definition
      (let_binding pattern: (value_name) @font-lock-function-name-face
                   (parameter))))

   :language 'ocaml_interface
   :feature 'constructor
   '(["true" "false" (tag) (constructor_name)] @font-lock-constant-face)

   :language 'ocaml_interface
   :feature 'module
   '((module_name) @font-lock-function-name-face)

   :language 'ocaml_interface
   :feature 'variable
   '(
     ["[%" "[@" "[@@"] @font-lock-preprocessor-face
     (attribute_id) @font-lock-preprocessor-face
     "%" @font-lock-preprocessor-face
     (attribute_payload ":" @font-lock-preprocessor-face)
     (extension "]" @font-lock-preprocessor-face)
     (attribute "]" @font-lock-preprocessor-face)
     (labeled_argument "~" @font-lock-constant-face (label_name) @font-lock-constant-face)
     (value_pattern) @font-lock-variable-name-face
     (tuple_pattern (value_name) @font-lock-variable-name-face)
     (value_definition
      (let_binding pattern: [(value_name) @font-lock-variable-name-face]))
     (instance_variable_name) @font-lock-variable-name-face
     (field_pattern (field_path (field_name) @font-lock-variable-name-face))
     )

   :language 'ocaml_interface
   :feature 'bracket
   '(["(" ")" "[" "]" "{" "}"] @font-lock-bracket-face)))

(defvar ocaml-impl-ts-mode--font-lock-settings
  (treesit-font-lock-rules
   :language 'ocaml
   :feature 'keyword
   `([,@ocaml-impl-ts-mode--keywords] @font-lock-keyword-face)

   :language 'ocaml
   :feature 'delimiter
   '([";" "." ","] @font-lock-delimiter-face)

   :language 'ocaml
   :feature 'type
   '((type_constructor) @font-lock-type-face)

   :language 'ocaml
   :feature 'comment
   '((comment) @font-lock-comment-face)

   :language 'ocaml
   :feature 'number
   '((number) @font-lock-number-face)

   :language 'ocaml
   :feature 'string
   '([(string) (character)] @font-lock-string-face)

   :language 'ocaml
   :feature 'variable
   '((assert_expression "assert" @font-lock-warning-face))

   :language 'ocaml
   :feature 'function
   '(
     (method_definition
      name: (method_name) @font-lock-function-name-face)
     (value_definition
      (let_binding pattern: (value_name) @font-lock-function-name-face
                   (parameter))))

   :language 'ocaml
   :feature 'constructor
   '(["true" "false" (tag) (constructor_name)] @font-lock-constant-face)

   :language 'ocaml
   :feature 'module
   '((module_name) @font-lock-function-name-face)

   :language 'ocaml
   :feature 'variable
   '(
     ["[%" "[@" "[@@"] @font-lock-preprocessor-face
     (attribute_id) @font-lock-preprocessor-face
     "%" @font-lock-preprocessor-face
     (attribute_payload ":" @font-lock-preprocessor-face)
     (extension "]" @font-lock-preprocessor-face)
     (attribute "]" @font-lock-preprocessor-face)
     (labeled_argument "~" @font-lock-constant-face (label_name) @font-lock-constant-face)
     (value_pattern) @font-lock-variable-name-face
     (tuple_pattern (value_name) @font-lock-variable-name-face)
     (value_definition
      (let_binding pattern: [(value_name) @font-lock-variable-name-face]))
     (instance_variable_name) @font-lock-variable-name-face
     (field_pattern (field_path (field_name) @font-lock-variable-name-face))
     )

   :language 'ocaml
   :feature 'bracket
   '(["(" ")" "[" "]" "{" "}"] @font-lock-bracket-face)))

;;; Imenu

(defun ocaml-ts-mode--defun-name (node)
  (treesit-node-text
   (pcase (treesit-node-type node)
     ("module_binding" (treesit-node-child-by-field-name node "name"))
     ("let_binding" (treesit-node-child-by-field-name node "pattern"))
     ("type_binding" (treesit-node-child-by-field-name node "name"))
     ("class_type_binding" (treesit-node-child-by-field-name node "name"))
     ("module_type_definition" (treesit-node-child-by-field-name node "name"))
     ("method_definition" (treesit-node-child-by-field-name node "name")))))

(defvar ocaml-ts-mode--imenu-settings
  `(("Module" "module_binding" nil nil)
    ("Variable" "let_binding" nil nil)
    ("Type" "type_binding" nil nil)
    ("Method" "method_definition" nil nil)
    ("Class Type" "class_type_binding" nil nil)
    ("Module Type" "module_type_definition")))

;;; Indent

(defcustom ocaml-ts-mode-indent-offset 2
  "Number of spaces for each indentation step in `ocaml-ts-mode'."
  :version "29.1"
  :type 'integer
  :safe 'integerp
  :group 'ocaml)

(defcustom ocaml-ts-mode-indent-offset-with 0
  "Number of spaces for each indentation step in `ocaml-ts-mode'."
  :version "29.1"
  :type 'integer
  :safe 'integerp
  :group 'ocaml)

(defun special-parent (n p &rest _)
  (let ((start (treesit-node-start p)))
    (when (save-excursion  (goto-char start) (back-to-indentation) (< (point) start))
      (when (setq x (treesit-simple-indent p (treesit-node-parent p) (line-beginning-position)))
        (+ (car x) (cdr x))))))

(defvar ocaml-ts-mode--indent-rules
  (let ((offset ocaml-ts-mode-indent-offset)
        (offset-with ocaml-ts-mode-indent-offset-with))
    `((ocaml
       ((node-is "}") parent-bol 0)
       ((node-is ")") parent-bol 0)
       ((node-is "]") parent-bol 0)
       ((node-is "end") parent-bol 0)
       ((node-is "with") parent-bol 0)
       ((node-is "and") parent-bol 0)
       ((parent-is "type_binding") parent-bol ,offset)
       ((parent-is "or_pattern") parent-bol 0)
       ((match "structure" "module_binding" nil nil nil) parent-bol 0)
       ((match "signature" "module_type_definition" nil nil nil) parent-bol 0)
       ((parent-is "value_specification") parent-bol ,offset)
       ((match "function_type" "function_type" nil nil nil) parent-bol 0)
       ((parent-is "list_expression")  (or special-parent parent-bol) ,offset)
       ((parent-is "record_declaration") parent-bol ,offset)
       ((parent-is "let_binding") parent-bol ,offset)
       ((parent-is "labeled_argument") parent-bol ,offset)
       ((parent-is "module_binding") parent-bol ,offset)
       ((parent-is "application_expression") parent-bol ,offset)
       ((parent-is "module_application") parent-bol ,offset)
       ((parent-is "signature") parent-bol ,offset)
       ((parent-is "structure") parent-bol ,offset)
       ((parent-is "object_expression") parent-bol ,offset)
       ((parent-is "sequence_expression") parent-bol 0)
       ((parent-is "function_expression") parent-bol ,offset)
       ((parent-is "do_clause") parent-bol ,offset)
       ((match nil "match_expression" nil 1 1) parent-bol ,offset)
       ((parent-is "match_expression") (or special-parent parent-bol) ,offset-with)
       ((parent-is "match_case") parent-bol 4)
       ((parent-is "let_expression") parent-bol 0)
       ((parent-is "let_module_expression") parent-bol 0)
       ((parent-is "let_open_expression") parent-bol 0)
       ((parent-is "then_clause") parent-bol ,offset)
       ((parent-is "else_clause") parent-bol ,offset)
       ((parent-is "if_expression") parent-bol 0)
       ((parent-is "record_expression") parent-bol ,offset)
       ((parent-is "record_pattern") parent-bol ,offset)
       ((parent-is "field_pattern") parent-bol ,offset)
       ((match "|" "variant_declaration" nil nil nil) parent-bol 0)
       ((node-is "|") parent-bol 0)
       ((parent-is "parenthesized_expression") parent-bol ,offset)
       ((parent-is "infix_expression") parent-bol 0)
       ((match "cons_expression" nil "right") parent-bol 0)
       ((node-is "infix_operator") parent 0)
       ((parent-is "product_expression") parent-bol 0)
       ((parent-is "fun_expression") parent-bol ,offset)
       ((match nil "try_expression" nil 1 1) parent-bol ,offset)
       ((parent-is "method_definition") parent-bol ,offset)
       ((parent-is "class_body_type") parent-bol ,offset))))
  "Tree-sitter indent rules for `ocaml-ts-mode'.")

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.mli\\'" . ocaml-intf-ts-mode))

;;;###autoload
(define-derived-mode ocaml-intf-ts-mode prog-mode "OCaml/i"
  "Major mode for editing OCaml interfaces, powered by tree-sitter."
  :group 'ocaml
  :syntax-table ocaml-ts-mode--syntax-table

  (when (treesit-ready-p 'ocaml_interface)
    (treesit-parser-create 'ocaml_interface)

    (setq-local treesit-font-lock-settings ocaml-intf-ts-mode--font-lock-settings
                treesit-font-lock-feature-list
                '((keyword string module constructor type comment function delimiter variable) (bracket number)))

    ;; Comments
    (setq-local comment-start "(*")
    (setq-local comment-end "*)")
    (setq-local comment-start-skip "(\\*+ *")
    (setq-local comment-end-skip " *\\*+)")

    ;; Imenu.
    (setq-local treesit-simple-imenu-settings ocaml-ts-mode--imenu-settings)

    (setq-local indent-tabs-mode nil
                treesit-simple-indent-rules ocaml-ts-mode--indent-rules)

    (setq-local treesit-defun-name-function #'ocaml-ts-mode--defun-name)

    (setq-local treesit-defun-type-regexp "class_type_binding\\|let_binding\\|module_binding\\|type_binding\\|value_definition\\|type_definition\\|method_definition\\|module_type_definition")

    (treesit-major-mode-setup)))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.ml\\'" . ocaml-impl-ts-mode))

;;;###autoload
(define-derived-mode ocaml-impl-ts-mode prog-mode "OCaml"
  "Major mode for editing OCaml, powered by tree-sitter."
  :group 'ocaml
  :syntax-table ocaml-ts-mode--syntax-table

  (when (treesit-ready-p 'ocaml)
    (treesit-parser-create 'ocaml)

    (setq-local treesit-font-lock-settings ocaml-impl-ts-mode--font-lock-settings
                treesit-font-lock-feature-list
                '((keyword string module constructor type comment function delimiter variable) (bracket number)))

    ;; Comments
    (setq-local comment-start "(*")
    (setq-local comment-end "*)")
    (setq-local comment-start-skip "(\\*+ *")
    (setq-local comment-end-skip " *\\*+)")

    ;; Imenu.
    (setq-local treesit-simple-imenu-settings ocaml-ts-mode--imenu-settings)

    (setq-local indent-tabs-mode nil
                treesit-simple-indent-rules ocaml-ts-mode--indent-rules)

    (setq-local treesit-defun-name-function #'ocaml-ts-mode--defun-name)

    (setq-local treesit-defun-type-regexp "class_type_binding\\|let_binding\\|module_binding\\|type_binding\\|value_definition\\|type_definition\\|method_definition\\|module_type_definition")

    (treesit-major-mode-setup)))

(provide 'ocaml-ts-mode)

;;; ocaml-ts-mode.el ends here
