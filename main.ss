#!/usr/bin/chezscheme --script

(define chart-dir (list-ref (command-line-arguments) 0))
(define source-file (list-ref (command-line-arguments) 1))
(define output-file (if (> (length (command-line-arguments)) 2) 
                        (list-ref (command-line-arguments) 2) 
                        (format "~a/~a.dot" "/tmp" source-file)))

(define prefix (path-parent output-file))

(load (format "~a/chart.ss" chart-dir))
(load source-file)
(graph-puts output-file #t)
(exit)
