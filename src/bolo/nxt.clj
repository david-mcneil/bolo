(ns bolo.nxt
  (:use [bolo.base :only (dbg)]))

;; define the implementation of the bolo.ops operators for the Lego
;; MindStorm NXT tank. Use NXC [1] to compile the code emitted by this
;; module.
;; [1] - http://bricxcc.sourceforge.net/nxc/

;; this is a macro simply for the convenience of ~@
(defmacro main [& commands]
  `(str "task main() {" ~@commands "
}
"))

(defn forward [power]
  (str "
  OnFwd(OUT_AB, " power ");"))

(defn backward [power]
  (str "
  OnRev(OUT_AB, " power ");"))

(defn turn-left [power]
  (str "
  OnFwd(OUT_A, " power ");
  OnRev(OUT_B, " power ");"))

(defn turn-right [power]
  (str "
  OnFwd(OUT_B, " power ");
  OnRev(OUT_A, " power ");"))

(defn wait [time]
  (str "
  Wait(" time ");"))

(defmacro duration [time & commands]
  `(str ~@commands "
  Wait(" ~time ");"))

(defmacro block [& commands]
  `(str ~@commands))
