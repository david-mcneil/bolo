(ns bolo.op-demo
  (:use [bolo.base :only (new-op)]))

;; example of how an operator works

;; an operator is similar to a Clojure record. The main difference is
;; that an operator presents itself as a Clojure List, whereas a record
;; presents itself as a Clojure Map.


;; define a new operator:

(new-op foo [x y z])

;; create an instance of our new operator

(foo 1 2 3)
;; => (foo 1 2 3)

;; for each operator a new Java Class is generated

(class (foo 1 2 3))
;; => bolo.ops.Foo

;; the operator instances are associative. Fields are accessed by keyword

(:x (foo 1 2 3))
;; => 1

;; operators can be constructed in trees

(foo 1 (foo 10 20 30) 3)
;; => (foo 1 (foo 10 20 30) 3)

(:z (:y (foo 1 (foo 10 20 30) 3)))
;; => 30

;; the instances are Clojure lists.

(list? (foo 1 2 3))
;; => true

;; and Clojure seqs

(seq? (foo 1 2 3))
;; => true

(first (foo 1 2 3))
;; => foo

(second (foo 1 2 3))
;;=> 1

(class (first (foo 1 2 3)))
;; => clojure.lang.Symbol

;; The facts that operator instances are Clojure seqs and that the
;; first item in them are unqualified symbols governs how
;; operator instances are evaluated by the Clojure evaluator. This
;; evalution "trick" is a bit subtle to understand at first, but it is
;; the key to the usefulness of operators. The context in which an
;; operator instance is evaluated will dictate which definition of the
;; symbols is used and thus what the result of the evaluation is.
