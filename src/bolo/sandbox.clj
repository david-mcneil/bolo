(ns bolo.sandbox
  (:require [bolo.nxt]
            [bolo.ino])
  (:use [bolo.base :only (dbg eval-in-ns)]
        [bolo.ops :only (main forward backward turn-left turn-right wait duration block)]
        [clojure.pprint :only (pprint)])
  (:import [bolo.ops Wait]))

;; make a program in our tank DSL

(main (forward 70)
      (wait 3000))
;; => (main (forward 70) (wait 3000))

;; a program is a data structure

(class (second (:commands (main (forward 70)
                                (wait 3000)))))
;; => bolo.ops.Wait

(:time (second (:commands (main (forward 70)
                                (wait 3000)))))
;; => 3000

;; emit NXT code

;; when a program is evaluated in the bolo.nxt namespace then Lego
;; MindStorm NXT code  is emitted
(print (eval-in-ns bolo.nxt (main (forward 70)
                                  (wait 3000)
                                  (backward 70)
                                  (wait 4000))))
;; =>
"task main() {
  OnFwd(OUT_AB, 70);
  Wait(3000);
  OnRev(OUT_AB, 70);
  Wait(4000);
}"

;; helper emit macro that shortens the incantation to emit code

(defmacro emit [target e]
  `(eval-in-ns ~({'nxt 'bolo.nxt
                  'ino 'bolo.ino} target) ~e))

(print (emit nxt (main (forward 70)
                       (wait 3000)
                       (backward 70)
                       (wait 4000)
                       (turn-left 80)
                       (wait 2000))))
;; =>
"task main() {
  OnFwd(OUT_AB, 70);
  Wait(3000);
  OnRev(OUT_AB, 70);
  Wait(4000);
  OnFwd(OUT_A, 80);
  OnRev(OUT_B, 80);
  Wait(2000);
}"


;; emit Arduino code
;; when a program is evaluated in the bolo.ino namespace then Arduino
;; code is emitted

(print (emit ino (main (forward 70)
                       (wait 3000)
                       (backward 70)
                       (wait 4000))))
;; =>
"
#define FORWARDS 1
#define BACKWARDS 0

int powerR = 3;
int powerL = 11;
int dirR = 12;
int dirL = 13;

void setup(){
  Serial.begin(9600);
  pinMode(powerR, OUTPUT);
  pinMode(powerL, OUTPUT);
  pinMode(dirR, OUTPUT);
  pinMode(dirL, OUTPUT);
}

void loop() {
  digitalWrite(dirR, FORWARDS);
  digitalWrite(dirL, FORWARDS);
  digitalWrite(powerR, HIGH);
  digitalWrite(powerL, HIGH);

  delay(3000);
  digitalWrite(dirR, BACKWARDS);
  digitalWrite(dirL, BACKWARDS);
  digitalWrite(powerR, HIGH);
  digitalWrite(powerL, HIGH);

  delay(4000);
}"

;; programs are data and can be saved in variables

(def p (main (forward 70)
             (wait 3000)
             (backward 70)
             (wait 4000)))

(print (emit nxt p))
;; =>
"task main() {
  OnFwd(OUT_AB, 70);
  Wait(3000);
  OnRev(OUT_AB, 70);
  Wait(4000);
}"


;; duration provides structured waiting

(print (emit nxt (main (duration 3000
                                 (forward 70))
                       (duration 4000
                                 (backward 70)))))
;; =>
" task main() {
  OnFwd(OUT_AB, 70);
  Wait(3000);
  OnRev(OUT_AB, 70);
  Wait(4000);
}"

;; with programs as data, functions can make programs

(defn make-walker [t x]
  (map #(duration t (forward %)) (range x)))

(def p-list (cons 'main (make-walker 1000 15)))

(def p (eval p-list))

(print (emit nxt p))
;; =>
" task main() {
  OnFwd(OUT_AB, 0);
  Wait(1000);
  OnFwd(OUT_AB, 1);
  Wait(1000);
  OnFwd(OUT_AB, 2);
  Wait(1000);
  // ...
}"

;; to avoid explicit eval call, use macros instead

(defmacro make-walker2 [t x]
  `(block ~@(map #(duration t (forward %)) (range x))))

(def p2 (main (make-walker2 1000 5)))

(print (emit nxt p2))
;; =>
"task main() {
  OnFwd(OUT_AB, 0);
  Wait(1000);
  OnFwd(OUT_AB, 1);
  Wait(1000);
  OnFwd(OUT_AB, 2);
  Wait(1000);
  OnFwd(OUT_AB, 3);
  Wait(1000);
  OnFwd(OUT_AB, 4);
  Wait(1000);
}"

;; we can manipulate programs programatically

(let [waits [(wait 1)
             (wait 3)
             (wait 5)]]
  (reduce #(wait (+ (:time %1) (:time %2)))
          waits))
;; => (wait 9)

;; define an automated refactoring that will combine sequences of wait
;; calls

(defn combine-waits [m]
  (assoc m :commands (let [commands (:commands m)]
                       (loop [p []
                              c commands]
                         (if (seq c)
                           (let [[waits remaining] (split-with #(instance? Wait %) c)]
                             (if (seq waits)
                               (recur (conj p (reduce #(wait (+ (:time %1)
                                                                (:time %2))) (wait 0) waits))
                                      remaining)
                               (recur (conj p (first c))
                                      (rest c))))
                           p)))))

(pprint (combine-waits (main (forward 10)
                             (wait 20)
                             (wait 40)
                             (wait 7)
                             (forward 10))))
;; => (main (forward 10) (wait 67) (forward 10))

;; taking in programs as strings

;; read in the program, eval it, then apply the automated refactoring,
;; and finally emit NXT code

(let [p (combine-waits (eval (read-string
                              "(main
                                (forward 9)
                                (wait 10)
                                (forward 10)
                                (wait 20)
                                (wait 40)
                                (wait 7)
                                (forward 10))")))]
  (print (emit nxt p)))
;; =>
"task main() {
  OnFwd(OUT_AB, 9);
  Wait(10);
  OnFwd(OUT_AB, 10);
  Wait(67);
  OnFwd(OUT_AB, 10);
}"

;; define a helper function to read in a program as string and to
;; produce output code for the specified target

(defmacro load-p [target p-str]
  (let [p (combine-waits (eval (read-string p-str)))]
    `(emit ~target ~p)))

(print (load-p nxt "(main
                        (forward 80)
                        (wait 3000))"))
;; =>
"task main() {
  OnFwd(OUT_AB, 80);
  Wait(3000);
}"

;; spit the emitted code out to a file and use NXC [1] to compile it for
;; the Lego MindStorm
;;
;; [1] - http://bricxcc.sourceforge.net/nxc/

(spit "out.nxc" (load-p nxt "(main
                                (forward 60)
                                (wait 2000)
                                (backward 60)
                                (wait 2000)
                                (turn-right 60)
                                (wait 2000)
                                (turn-left 60)
                                (wait 2000))"))
