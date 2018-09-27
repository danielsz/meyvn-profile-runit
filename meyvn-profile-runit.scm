#!/usr/bin/csi -script
(use s posix section-combinators srfi-69)

;; utils
(define (prompt-directory)
  (display "Please enter root directory\n")
  (let ((x (read-line (open-input-file "/dev/tty"))))
    (cond ((directory? x) (set! root-sv x))
	  (else (display "That directory doesn't exist. ")
		(prompt-directory)))))

(define (compare-dir-kv l1 l2)
  (if (lset= string= l1 l2)
      "Equal"
      (string-join (lset-difference string= l1 l2))))

(define (prompt-user s)
  (display s)
  (let ((answer (read-line)))
    (cond ((string-ci=? answer "y") (print "yes"))
	  ((string-ci=? answer "n") (print "no" ))
	  (else (prompt-user s)))))

(define (java-property-to-env s)
  (s-upcase (s-replace "." "_" s)))

;; core

(define h (make-hash-table))
(define sv-root)

(define (process line)
  (let ((x (string-split line "=")))
    (when (= 2 (length x))
      (hash-table-set! h (java-property-to-env (car x)) (cadr x)))))

(define (write-env)
  (display "Writing environment variables")
  (hash-table-for-each h (lambda (k v)
			   (let ((path (string-append root-sv "/" k)))
			     (with-output-to-file path (lambda () (display v)))))))

(if (terminal-port? (current-input-port))
    (for-each process (read-lines (open-input-pipe "myvnp -x list-profile -a org.danielsz:ninjasocks:1.0.0 -p production")))
    (for-each process (read-lines)))
(if (positive? (hash-table-size h))
    (begin
      (hash-table-for-each h (left-section printf "~A ~~ ~A ~%"))
      (prompt-directory)
      (if (null? (directory root-sv))
	  (write-env)
	  (display (string-append "Directory not empty. " (compare-dir-kv (directory root-sv) (hash-table-keys h))) )))
    (display "No key values found"))
