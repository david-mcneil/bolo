(ns bolo.ino
  (:use [bolo.base :only (dbg deft)]))

;; define the implementation of the bolo.ops operators for the arduino tank

(defmacro main [& commands]
  `(str "#define FORWARDS 1
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

void loop() {" ~@commands "
}"))

(defn forward [power]
  "
  digitalWrite(dirR, FORWARDS);
  digitalWrite(dirL, FORWARDS);
  digitalWrite(powerR, HIGH);
  digitalWrite(powerL, HIGH);
")

(defn backward [power]
  "
  digitalWrite(dirR, BACKWARDS);
  digitalWrite(dirL, BACKWARDS);
  digitalWrite(powerR, HIGH);
  digitalWrite(powerL, HIGH);
")

(defn turn-left [power]
  "
  digitalWrite(dirR, FORWARDS);
  digitalWrite(dirL, BACKWARDS);
  digitalWrite(powerR, HIGH);
  digitalWrite(powerL, HIGH);
")

(defn turn-right [power]
  "
  digitalWrite(dirR, BACKWARDS);
  digitalWrite(dirL, FORWARDS);
  digitalWrite(powerR, HIGH);
  digitalWrite(powerL, HIGH);
")

(defn wait [time]
  (str "
  delay(" time ");"))

(defmacro duration [time & commands]
  `(str ~@commands "
  delay(" ~time ");"))

(defmacro block [& commands]
  `(str ~@commands))
