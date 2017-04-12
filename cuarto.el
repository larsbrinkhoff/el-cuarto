;;; Forth model.
;;
;; This Forth executes inside a single bytecode function.
;;
;; The memory space is an Emacs string which is the bytecode for the
;; function.  It also holds the dictionary and all other data.
;;
;; The data stack is Emacs' bytecode stack.  The return stack is a
;; Lisp list.
;;
;; The Forth registers are 
;; - P: program counter.
;; - S: data stack pointer; handle by Emacs' bytecode VM.
;; - R: return stack pointer.
;;
;; The bytecode function constant vector is laid out like this:
;; - Slot 0: bytecode string.
;; - Slot 1: P, an integer address.
;; - Slot 2: R, a cons.

(require 'cl)

(defvar dictionary (make-string 20 0))

(defvar jump-table
  (let ((table (make-hash-table :test 'eq)))
    (dotimes (i 65535 table)
      (setf (gethash i table) i))))

(defvar here 0)
(defvar latest 0)

(defun c-comma (c)
  (setf (aref dictionary here) c)
  (incf here))

(defun comma (n)
  (c-comma (logand n 255))
  (c-comma (lsh n 8)))

(defmacro defword (name &rest code)
  ;; link
  ;; name
  (let ((string (byte-compile-lapcode (mapcar #'macroexpand code))))
    (dotimes (i (length string))
      (c-comma (aref string i)))))

(defmacro get-dictionary ()
  '(byte-constant . 0))

(defmacro next ()
  '(byte-nop))

(defword "exit"
  (byte-varref . 2)	;cons
  (byte-dup)		;cons cons
  (byte-cdr)		;cons cdr
  (byte-varset . 2)	;cons
  (byte-car)		;car
  (byte-varset . 1)
  (next))

(defword "c@"
  (byte-constant . 0)
  ;;(byte-swap)
  (byte-aref)
  (next))

(defword "c!"
  (byte-constant <dictionary>)
  ;; ...
  (byte-aset)
  (next))

(defword "dup"
  (byte-dup)
  (next))

(defword "drop"
  (byte-discard)
  (next))

(defword "="
  (byte-eq)
  (next))

(defword "+"
  (byte-plus)
  (next))

(defword "-"
  (byte-diff)
  (next))

(defword "bye"
  (byte-return))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun check ()
  (disassemble (make-byte-code 0 dictionary (vector dictionary) 256)))

(let ((lexical-binding t))
  (defun foo (x)
    (setq y x))
  (byte-compile 'foo)
  (disassemble 'foo))

(symbol-function 'foo)

(byte-compile-lapcode
 '((byte-constant . 0)	;42 dup +
   (byte-dup)
   (byte-plus)
   (byte-return)))
