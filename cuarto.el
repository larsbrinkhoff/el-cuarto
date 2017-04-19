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

(defmacro byte-nop ()
  '(byte-discardN . 0))

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
  (get-dictionary)
  ;;(byte-swap)
  (byte-aref)
  (next))

(defword "c!"
  (get-dictionary)
  ;; ...
  (byte-aset)
  (next))

(defword "dup"
  (byte-dup)
  (next))

(defword "2dup"
  (byte-stack-ref . 1)
  (byte-stack-ref . 1)
  (next))

(defword "over"
  (byte-stack-ref . 1)
  (next))

(defword "drop"
  (byte-discard)
  (next))

(defword "2drop"
  (byte-discardN . 2)
  (next))

(defword "3drop"
  (byte-discardN . 3)
  (next))

(defword "nip"
  (byte-discardN . #x81)
  (next))

(defword "swap"
  (byte-varset . 4)
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

(defword "1+"
  (byte-add1)
  (next))

(defword "1-"
  (byte-sub1)
  (next))

(defword "negate"
  (byte-negate)
  (next))

(defword "*"
  (byte-mult)
  (next))

(defword "/"
  (byte-quo)
  (next))

(defword "rem"
  (byte-rem)
  (next))

(defword "max"
  (byte-max)
  (next))

(defword "min"
  (byte-min)
  (next))

;;(defword "branch"
;;  (byte-goto . 0))

;;(defword "0branch"
;;  (byte-goto-if-nil . 0))

;;(defword "(literal)"
;;  ...)

(defword "bye"
  (byte-return))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-function (code constants)
  (make-byte-code 0 code constants 256))

(defun check ()
  (let ((constants (vector dictionary 'P 'R 'W 'T)))
    (disassemble (make-function dictionary constants))))

(defun run (code &optional constants)
  (funcall (make-function (byte-compile-lapcode code) constants)))

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

(run '((byte-constant . 0)
       (byte-constant . 1)
       (byte-discardN . 0)  ;nop
       (byte-return))
     [1 2])
