;;; ifene.el --- Major mode for editing Ifene programs

;; Author: Uwe Dauernheim <uwe@dauernheim.net>
;; Maintainer: Uwe Dauernheim <uwe@dauernheim.net>
;; Created: September 2010
;; Keywords: languages

;;; Commentary:

;; Currently a wacky mix of the original Erlang Emacs mode and the Python 
;; mode which part of GNU Emacs (Not python-mode).

;;; Todo:

;; Highlight records

;;; Fixme:

;; Decrease too strong atom detection

;;; Code:

;;;###autoload
(add-to-list 'auto-mode-alist (cons (purecopy "\\.ifn\\'")  'ifene-mode))

;; Ignore files ending in ".jam", ".vee", and ".beam" when performing
;; file completion.
(let ((ifn-ext '(".jam" ".vee" ".beam")))
  (while ifn-ext
    (let ((cie completion-ignored-extensions))
      (while (and cie (not (string-equal (car cie) (car ifn-ext))))
	(setq cie (cdr cie)))
      (if (null cie)
	  (setq completion-ignored-extensions
		(cons (car ifn-ext) completion-ignored-extensions))))
    (setq ifn-ext (cdr ifn-ext))))

;;;; Font lock:

(defvar ifene-font-lock-keywords
  ;; Comments
 `(
  (,(rx (group "#" (0+ nonl)))
    (1 font-lock-comment-face))
  ;; Definitions
  (,(rx line-start (group "@@" (1+ (or word ?_))))
    (1 font-lock-preprocessor-face))
  (,(rx line-start (group "@" (1+ (or word ?_))))
    (1 font-lock-preprocessor-face))
  ;; Function declaration
  (,(rx line-start (group (1+ (or word ?_))) (0+ space) "=")
    (1 font-lock-function-name-face))
  ;; Records & Structs (Types)
  ;; Record highlighting is not yet implemented (Issues to detect)
  (,(rx (group "@" (in "A-Z") (0+ (in "a-zA-Z0-9_"))))
    . font-lock-type-face)
  ;; Variables
  (,(rx symbol-start (in "A-Z") (0+ (in "a-zA-Z0-9_")) symbol-end)
    . font-lock-variable-name-face)
  ;; Keywords
  (,(rx symbol-start (or "after" "switch" "case" "catch" "else" "fn" 
    "object" "if" "record" "for" "in" "receive" "try" "when") symbol-end)
    . font-lock-keyword-face)
  (,(rx symbol-start (or "and" "andd" "not" "or" "xor" "orr") symbol-end)
    . font-lock-keyword-face)
  ;; Built-ins
  (,(rx (or line-start (not (any ". "))) (* " ") symbol-start (group (or
	  "abs" "append_element" "apply" "atom_to_list" "binary_to_list"
    "bitstring_to_list" "binary_to_term" "bit_size" "bump_reductions"
    "byte_size" "cancel_timer" "check_process_code" "delete_module"
    "demonitor" "disconnect_node" "display" "element" "erase" "exit"
    "float" "float_to_list" "fun_info" "fun_to_list"
    "function_exported" "garbage_collect" "get" "get_keys"
    "group_leader" "hash" "hd" "integer_to_list" "iolist_to_binary"
    "iolist_size" "is_atom" "is_binary" "is_bitstring" "is_boolean"
    "is_builtin" "is_float" "is_function" "is_integer" "is_list"
    "is_number" "is_pid" "is_port" "is_process_alive" "is_record"
    "is_reference" "is_tuple" "length" "link" "list_to_atom"
    "list_to_binary" "list_to_bitstring" "list_to_existing_atom"
    "list_to_float" "list_to_integer" "list_to_pid" "list_to_tuple"
    "load_module" "localtime_to_universaltime" "make_tuple" "md5"
    "md5_final" "md5_update" "memory" "module_loaded" "monitor"
    "monitor_node" "node" "nodes" "open_port" "phash" "phash2"
    "pid_to_list" "port_close" "port_command" "port_connect"
    "port_control" "port_call" "port_info" "port_to_list"
    "process_display" "process_flag" "process_info" "purge_module"
    "put" "read_timer" "ref_to_list" "register" "resume_process"
    "round" "send" "send_after" "send_nosuspend" "set_cookie"
    "setelement" "size" "spawn" "spawn_link" "spawn_monitor"
    "spawn_opt" "split_binary" "start_timer" "statistics"
    "suspend_process" "system_flag" "system_info" "system_monitor"
    "system_profile" "throw" "term_to_binary" "tl" "trace" "trace_delivered"
    "trace_info" "trace_pattern" "trunc" "tuple_size" "tuple_to_list"
    "universaltime_to_localtime" "unlink" "unregister" "whereis")) symbol-end)
    (1 font-lock-builtin-face))
  ;; Strings & Atoms
  ;; ""-strings are predefined by Emacs. Only work needed for atoms
  (,(rx "'" (0+ nonl) "'")
    . font-lock-string-face)
  ;; Constants
  (,(rx (or "true" "false" "yes" "no" "undefined" "ok"))
    . font-lock-constant-face)))

;;;; Modes:

;;;###autoload
(define-derived-mode ifene-mode fundamental-mode "Ifene"
  "Major mode for editing Ifene files.

\\{ifene-mode-map}"
  :group 'ifene
  (set (make-local-variable 'font-lock-defaults)
    '(ifene-font-lock-keywords nil nil nil nil
		  (font-lock-syntactic-keywords)))
  ;; ifene defines TABs as being 2-char wide.
  (set (make-local-variable 'tab-width) 2)
  (unless font-lock-mode (font-lock-mode 1)))

(provide 'ifene)

;;; ifene.el ends here
