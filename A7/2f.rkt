;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname 2f) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require "a7,rkt")

;; f

;; (backup-fs fs t) consumes a filesystem and a timestamp
;;   and produces a filesystem that containing the files that
;;   are less than the given timestamp
;; backup-fs: Dir Nat -> Dir
;; Examples:
(check-expect (backup-fs fs1 6) 
              (make-dir "rootdir" (list file1 dir1 dir2b dir3)))

(define (backup-fs fs t)
  (cond
    [(empty? (dir-contents fs)) empty]
     [(file? (first (dir-contents fs))) 
     (cond
       [(< (file-timestamp (first (dir-contents fs))) t)
        (make-dir (dir-name fs)
                  (cons (first (dir-contents fs))
                        (cond
                          [(empty? (rest (dir-contents fs))) empty]
                          [else (dir-contents 
                               (backup-fs (make-dir (dir-name fs) 
                                                    (rest (dir-contents fs))) t))])))]
       [else (backup-fs (make-dir (dir-name fs)
                                  (rest (dir-contents fs))) t)])]
    [(empty? (dir-contents (first (dir-contents fs)))) 
     (backup-fs (make-dir (dir-name fs)
                          (rest (dir-contents fs))) t)]
    [else 
     (make-dir (dir-name fs) 
               (cons (backup-fs (first (dir-contents fs)) t)
                     (dir-contents (backup-fs (make-dir (dir-name fs)
                                                        (rest (dir-contents fs))) t))))]))

;; Tests:
(check-expect (backup-fs dir2 6)
              (make-dir "twofiles" (list file1)))

