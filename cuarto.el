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
;; - S: data stack pointer; handled by Emacs' bytecode VM.
;; - R: return stack pointer.
;; - W: word pointer.
;; - T: temporary.
;;
;; The bytecode function constant vector is laid out like this:
;; - Slot 0: bytecode string.
;; - Slot 1: P, an integer address.
;; - Slot 2: R, a cons.
;; - Slot 3: W, an integer address.
;; - Slot 4: T, an integer.

(require 'cl)

(defvar dictionary (make-string 1024 0))

(defvar jump-table
  (let ((table (make-hash-table :test 'eq)))
    (dotimes (i 65535 table)
      (setf (gethash i table) i))))

(defvar xt-table (make-hash-table :test 'equal))

(defvar here 0)
(defvar latest 0)
(defvar docol 42)

(defun c-comma (c)
  (setf (aref dictionary here) c)
  (incf here))

(defun comma (n)
  (c-comma (logand n 255))
  (c-comma (lsh n 8)))

(defun header (name)
  (when (symbolp name)
    (setq name (symbol-name name)))
  (setf (gethash name xt-table) here))

(defmacro defcode (name &rest code)
  (header name)
  (comma (+ here 2))
  (let ((string (byte-compile-lapcode (mapcar #'macroexpand code))))
    (dotimes (i (length string))
      (c-comma (aref string i)))))

(defmacro defword (name &rest code)
  (header name)
  (comma docol)
  (dolist (word code)
    nil))

(defmacro get-dictionary ()
  '(byte-constant . 0))

(defmacro byte-nop ()
  '(byte-discardN . 0))

(defmacro next ()
  '(byte-nop))

(defcode exit
  (byte-varref . 2)	;cons
  (byte-dup)		;cons cons
  (byte-cdr)		;cons cdr
  (byte-varset . 2)	;cons
  (byte-car)		;car
  (byte-varset . 1)
  (next))

(defcode c@
  (get-dictionary)
  ;;(byte-swap)
  (byte-aref)
  (next))

(defcode c!
  (get-dictionary)
  ;; ...
  (byte-aset)
  (next))

(defcode dup
  (byte-dup)
  (next))

(defcode 2dup
  (byte-stack-ref . 1)
  (byte-stack-ref . 1)
  (next))

(defcode over
  (byte-stack-ref . 1)
  (next))

(defcode drop
  (byte-discard)
  (next))

(defcode 2drop
  (byte-discardN . 2)
  (next))

(defcode 3drop
  (byte-discardN . 3)
  (next))

(defcode nip
  (byte-discardN . #x81)
  (next))

(defcode swap
  (byte-varset . 4)
  (next))

(defcode =
  (byte-eq)
  (next))

(defcode +
  (byte-plus)
  (next))

(defcode -
  (byte-diff)
  (next))

(defcode 1+
  (byte-add1)
  (next))

(defcode 1-
  (byte-sub1)
  (next))

(defcode "negate"
  (byte-negate)
  (next))

(defcode *
  (byte-mult)
  (next))

(defcode /
  (byte-quo)
  (next))

(defcode rem
  (byte-rem)
  (next))

(defcode max
  (byte-max)
  (next))

(defcode min
  (byte-min)
  (next))

;;(defcode branch
;;  (byte-goto . 0))

;;(defcode 0branch
;;  (byte-goto-if-nil . 0))

;;(defcode "(literal)"
;;  ...)

(defcode bye
  (byte-return))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-function (code constants)
  (make-byte-code 0 code constants 256))

(defun make-forth ()
  (let ((constants (vector dictionary 'P 'R 'W 'T)))
    (make-function dictionary constants)))

(defun check ()
  (disassemble (make-forth)))

(defun forth ()
  (interactive)
  (funcall (make-forth)))

(defun run (code &optional constants)
  (funcall (make-function (byte-compile-lapcode code) constants)))

(let ((lexical-binding t))
  (defun foo (x)
    (setq y x))
  (byte-compile 'foo)
  (disassemble 'foo))

(byte-compile-lapcode
 '((byte-constant . 0)	;42 dup +
   (byte-dup)
   (byte-plus)
   (byte-return)))

(run '((byte-constant . 0)
       (byte-constant . 1)
       (byte-discardN . 0)  ;nop
       (byte-return))
     [1 2])
