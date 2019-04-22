;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname filedir) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; ############
;; Chenzheng Su
;; 20643469
;; CS 135
;; Winter 2016
;; Assignment 7
;; Problem 2
;; ############

(require "a7.rkt")


;; a

;; Template for File
;;(define (my-file-fn my-file)
;;   (...(file-name my-file)...
;;       (file-size my-file)...
;;		 (file-timestamp my-file)...))

;; Template for Dir
;;(define (my-dir-fn my-dir)
;;   (...(dir-name my-dir)...
;;		 (dir-contents my-dir)...))

;; Template for FileDir
;;(define (my-filedir-fn my-filedir)
;;   (cond
;;     [(file? my-filedir) ...]
;;     [(dir? my-filedir)]))

;; Template for FDList
;;(define (my-fdlist-fn my-fdlist)
;;  (cond
;;    [(empty? my-fdlist) ...]
;;    [else ...(first my-fdlist)...]))


;; b

(define file1 (make-file "oldfile" 1000 5))
(define file2 (make-file "newfile" 1000 55555555))
(define dir0 (make-dir "emptydir" empty))
(define dir1 (make-dir "onefile" (list file1)))
(define dir2 (make-dir "twofiles" (list file1 file2)))
(define dir2b (make-dir "twofiles" (list file1)))
(define dir3 (make-dir "onesies" (list dir1 dir1 dir1)))
(define fs1 (make-dir "rootdir" (list file1 dir0 dir1 dir2 dir3 file2))) 
(define fs2 (make-dir "u" (list file2 dir0 dir1)))

;;helper
;;(count-files2 fdl) consumes a FDList and produces the
;;  total number of files in this FDList
;; count-files2: FDList -> Num
(define (count-files2 fdl)
  (cond
    [(empty? fdl) 0]
    [(file? (first fdl)) 
     (+ (count-files2 (rest fdl)) 1)]
    [(dir? (first fdl))
     (+ (count-files2 (dir-contents (first fdl)))
        (count-files2 (rest fdl)))]))

;; (count-files fs) concumes a FileSystem and produces the
;;   total number of files in this file system
;; count-files: Dir -> Num
;; Examples:
(check-expect (count-files dir0) 0)

(define (count-files fs)
  (count-files2 (dir-contents fs)))

;; Tests:
(check-expect (count-files dir3) 3)
(check-expect (count-files fs1) 8)


;; c

;; (empty-dir-exist? fs) consumes a filesystem and produces
;;   true if empty dir exists in this filesymstem and
;;   false otherwise
;; empty-dir-exist?: Dir -> Bool
;; Examples:
(check-expect (empty-dir-exist? dir0) true)

(define (empty-dir-exist? fs)
  (cond
    [(empty? (dir-contents fs)) true]
    [(and (file? (first (dir-contents fs)))
          (empty? (rest (dir-contents fs)))) false]
    [(file? (first (dir-contents fs))) 
     (empty-dir-exist? (make-dir (dir-name fs)
                                 (rest (dir-contents fs))))]
    [else 
     (or (empty-dir-exist? (first (dir-contents fs)))
         (empty-dir-exist? (make-dir (dir-name fs) 
                                     (rest (dir-contents fs)))))]))

;; Tests:
(check-expect (empty-dir-exist? dir2) false)
(check-expect (empty-dir-exist? fs1) true)


;; d

;; helper
(define (oldest fs)
  (cond
    [(empty? (dir-contents fs)) empty]
    [(dir? (first (dir-contents fs))) 
     (older-file (oldest (first (dir-contents fs)))
                 (oldest (make-dir (dir-name fs) (rest (dir-contents fs)))))]
    [else
     (older-file (first (dir-contents fs))
                 (oldest (make-dir (dir-name fs) (rest (dir-contents fs)))))]))

(define (older-file a b)
  (cond
    [(and (file? a) (empty? b)) a]
    [(and (empty? a) (file? b)) b]
    [(< (file-timestamp a) (file-timestamp b)) a]
    [else b]))

;; (oldest-file fs) consumes a filesystem and produces
;;   the file name of the oldest file
;; oldest-file: Dir -> Str
;; Examples:
(check-expect (oldest-file dir2) "oldfile")

(define (oldest-file fs)
  (file-name (oldest fs)))

;; Tests:
(check-expect (oldest-file dir3) "oldfile")
(check-expect (oldest-file fs2) "oldfile")


;; e

;; helper
(define (lfp fs path)
  (cond
    [(empty? (dir-contents fs)) empty]
    [(file? (first (dir-contents fs))) 
     (cons (string-append path "/" 
                          (file-name (first (dir-contents fs))))
           (lfp (make-dir (dir-name fs) (rest (dir-contents fs))) path))]
    [(empty? (dir-contents (first (dir-contents fs)))) 
     (lfp (make-dir (dir-name fs) (rest (dir-contents fs))) path)]
    [else 
     (append (lfp (first (dir-contents fs)) 
                  (string-append path "/" 
                                 (dir-name (first (dir-contents fs)))))
             (lfp (make-dir (dir-name fs) (rest (dir-contents fs))) path))]))

;; (list-file-paths fs) consumes a filesystem and produces
;;   a list of str of the hierarchical names of all the files
;;   in the FileSystem
;; list-file-paths: Dir -> (listof Str)
;; Examples:
(check-expect (list-file-paths fs2)
              (list "u/newfile" "u/onefile/oldfile"))

(define (list-file-paths fs)
  (lfp fs (dir-name fs)))

;; Tests:
(check-expect (list-file-paths dir2)
              (list "twofiles/oldfile" "twofiles/newfile"))

;; f

;; (backup-fs fs t) consumes a filesystem and a timestamp
;;   and produces a filesystem that containing the files that
;;   are less than the given timestamp
;; backup-fs: Dir Nat -> Dir
;; Examples:
;;(check-expect (backup-fs fs1 6) 
  ;;            (make-dir "rootdir" (list file1 dir1 dir2b dir3)))

(define (backup-fs fs t)
  (cond
    [(empty? (dir-contents fs)) empty]
    [(check (dir-contents fs) t)
     (make-dir (dir-name fs) (del (dir-contents fs) t))]))

(define (del lof t)
  (cond
    [(empty? lof) empty]
    [(file? (first lof))
            (cond
              [(check-f (first lof) t)
               (cons (first lof) (del (rest lof) t))]
              [else (del (rest lof) t)])]
     [else (cons (backup-fs (first lof) t)
                 (del (rest lof) t))]))

(define (check lof t)
  (cond
    [(empty? (first lof)) false]
    [(file? (first lof))
     (cond
       [(check-f (first lof) t) true]
       [else (check (rest lof) t)])]
    [else (or (check (dir-contents (first lof)) t)
              (check (rest lof) t))]))

(define (check-f f t)
  (< (file-timestamp f) t))



;; Tests:
(check-expect (backup-fs dir2 6)
              (make-dir "twofiles" (list file1)))