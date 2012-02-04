(ns bolo.ops
  (:use [bolo.base :only (new-op)]))

;; define the operators for our tank DSL

(new-op main [& commands])

(new-op forward [power])
(new-op backward [power])
(new-op turn-left [power])
(new-op turn-right [power])

(new-op wait [time])
(new-op duration [time & commands])

(new-op block [& commands])
