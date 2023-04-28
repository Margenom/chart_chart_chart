#!/usr/bin/env chezscheme

#;("Система коробков
	- box - основной элемент содержит
		^ название - чтоб ссылаться на него далее
		^ формат обводки
		^ описание - текст с разметкой (таблицы, гиперссылки, форматирование, формулы)
			^ свойства форматирования
			^ свойства отображения
		^ коментарий - всплывающее окошко с доп описанием
			^ свойства форматирования
			^ свойства отображения
			^ форма окошка
		- box - их можно вкладывать в самих себя
		- image - тотже бокс но не бокс
			^ название
			^ url изображения
			^ url куданибуть
			^ popup
			^ масштабирование (или/и ppi)
			^ кадрирование

Транслирование в svg

v1.0.0 - просто надстройка над graphviz
	- опции что можно реализовать
	- cluster - обьект с вложениями
	- node - обьект без вложений или изображение
	- edge - || -
	- options = dot options



id* <- (box text . name opts) ~ "text" or name[label="text"] or "{label="text" cluster=true ...}"
id* <- (image url-img . name opts) ~ name[label="" shepe=none image="url-img"]
(line id0 . id1 ... (id* . label) ... (idn . label)) ~ id0 -> ... ; idn-1 -> idn [label="label"] 
(group id0 . id1 ... (id* . label) ... idn) ~ id0 -> id* [label="label"]; ... id0 -> idn;
(sub id0 . id1 ... idn) ~ {cluster=true label="id0.label" id1 ... idn}

(graph name type (palitre) (opts) . descr)
(graph-dot graph)
(line-opts palitre (opts) . descr) = edge
(cluster-opts palitre (opts) . descr)
(node-opts palitre (opts) . descr)
(group-opts palitre (opts) . descr) = cluster + node
(define sym id*) 

opts:
    - color - panillres сборка чветовое решение
        - название или номер
        - цвет обводки - color
        - цвет текста - fontcolor
        - цвет заливки опционально - 

struct
#f
  - cluster
    - cluster
       - objs
    - objs
    - objs
    - cluster
       - objs
         - cluster
         - objs
       - objs
    - objs
")

(define (cdr-list lvp) ((cond ((list? lvp)  cadr) ((pair? lvp) cdr) (else values)) lvp))

;Defaulst
(define root #f)

(define defs-sub '())
(define defs-box '())
(define defs-edge '())

(define graph-objects '()) ; name, type, opts
(define graph-edges '()) ; from, to, opts
(define graph-subs '()) ; cluster, subs, opts

;Generator
(define (graph-dot name direct palitre opts)
  (define (opts->dot opts) (apply string-append (map (lambda(o) (format " ~a=~s " (car o) (cdr-list o))) opts)))
  (define (edges->dot edges) (apply string-append (map (lambda(o) (format " ~s ~a ~s ~a;" (car o) (if direct "->" "--") (cadr o) (if (null? (caddr o)) "" (format "[~a]" (opts->dot (caddr o)))))) edges)))
  (define (nodes->dot nodes) (apply string-append (map (lambda(o) (format " ~s ~a\n" (car o) (if (null? (caddr o)) "" (format "[~a]" (opts->dot (caddr o)))))) nodes)))
  ;(define (get-gobj name) (assoc name graph-objects))
  (string-append
    (if direct "digraph" "graph") (format " ~s { " name)
    (opts->dot '((compound true) (layout dot) (nojustify true) (cluster false) (rankdir BT) (ranksep 1.5))) "\n"
    (nodes->dot graph-objects) "\n"
    (edges->dot graph-edges) "\n"
    #;(apply string-append (map (lambda(sub) (format "subgraph cluster_~a {cluster=true ~a \n~a}" (car sub) ))))
"\n}"))

(define (graph-puts name direct . etc) (with-output-to-file (format "~a" name) (lambda()
   (display (apply graph-dot name direct (if (null? etc) '(()()) etc)))) 'replace))



;Edges
(define (line first . ids) (let rc ((from first) (ost ids))
    (if (null? ost) #f (begin (set! graph-edges (cons 
      (if (pair? (car ost)) 
          (list from (caar ost) (cons `(label ,(cdr-list (car ost))) defs-edge))
          `(,from ,(car ost) ,defs-edge))
    graph-edges)) (rc (car ost) (cdr ost))))))

(define (group from . ids)
  (if (null? ids) #f (begin (set! graph-edges (cons 
      (if (pair? (car ids)) 
          (list from (caar ids) (cons `(label ,(cdr-list (car ids))) defs-edge))
          `(,from ,(car ids) ,defs-edge))
  graph-edges)) (apply group from (cdr ids)))))

(define (toin . ids) (apply group (reverse ids)))

;Clustes
(define (sub root . ids) (set! graph-subs (cons `(,root ,ids ,defs-sub) graph-edges)))

;Objects
;op: name, opts dot
(define (box text . opts) 
  (define name (and (not (null? opts)) (string? (car opts)) (car opts)))
  (define (opts-def defs opts) ((lambda(d) (append opts d)) (filter (lambda(d) (not (member (car d) opts))) defs)))
  (if root (sub root (or name text)))
  (set! graph-objects (cons (list (or name text) 'box (opts-def defs-box (append 
    `((label . ,text) (shape . "box")) 
    (if name (cdr opts) opts))))
  graph-objects)) (or name text))

(define (img url-img . opts) 
  (define name (and (not (null? opts)) (string? (car opts)) (car opts))) 
  (if root (sub root (or name text)))
  (set! graph-objects (cons (list (or name url-img) 'image (append 
    `((image . ,url-img) (label . "") (shape . "none")) 
    (if name (cdr opts) opts)))
  graph-objects)) (or name url-img))

(define (table fields . op/ts)
  (define text 
   (let ((d0-cons (lambda (d0) (fold-left (lambda(out di) (format "~a|~a" out di)) (car d0) (cdr d0)))))
    (d0-cons 
     (let rc ((dn fields)) 
       (if (null? dn) '() 
           (cons (let ((di (car dn)))
                   (if (list? di) (format "{~a}" (d0-cons (rc di))) 
                       (if (pair? di) (format "<~a>~a" (car di) (cdr di))
                           (format "~a" di))))
                 (rc (cdr dn))))))))

  (define name (and (not (null? opts)) (string? (car opts)) (car opts)))
  (define (opts-def defs opts) ((lambda(d) (append opts d)) (filter (lambda(d) (not (member (car d) opts))) defs)))
  (if root (sub root (or name text)))
  (set! graph-objects (cons (list (or name text) 'box (opts-def defs-box (append 
    `((label . ,text) (shape . "record")) 
    (if name (cdr opts) opts))))
  graph-objects)) (or name text))
(define (tb table-name table-field) (format "~a:~a" table-name table-field))

(import (cairo))

(cairo-library-init)
(define pi (* 2 (acos 0)))

;op: name, opts dot
(define (draw surf . op)
  (let* ([name (or (and (> (length op) 1) (list-ref op 1)) (format "draw~a" (length graph-objects)))]
         [path (format "~a/~a.png" prefix name)])
    (cairo-surface-write-to-png surf path)
    (apply img path name (if (> (length op) 2) (list-tail op 1) '())) ))

;op: format
(define (draw-surface size proc . op)
  (let* ([form (or (and (> (length op) 1) (list-ref op 0)) 'argb-32)]
         [surface (apply cairo-image-surface-create (cairo-format form) size)]
	 [cr (cairo-create surface)])
    (cairo-select-font-face cr "Sans" (cairo-font-slant 'normal) (cairo-font-weight 'normal)) 
    (cairo-set-font-size cr 15.0)
    (proc surface cr (cdr-list size) (car size))
    surface))

(define (draw-textc cr -x -y text)
    (let ([extents (cairo-text-extents-create)])
      (cairo-text-extents cr text extents)
      (let-struct extents cairo-text-extents-t (width height x-bearing y-bearing)
        (let ([x (- -x (+ (/ width 2) x-bearing))]
              [y (- -y (+ (/ height 2) y-bearing))])
          (cairo-move-to cr x y) 
          (cairo-show-text cr text)
          ; draw helping lines 
          ;(cairo-set-source-rgba cr 1 0.2 0.2 0.6) 
          ;(cairo-set-line-width cr 6.0) 
          #;(cairo-arc cr x y 10.0 0 (* 2 pi))))))
