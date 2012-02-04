(ns bolo.base
  (:use [clojure.string :only (capitalize)]
        [clojure.string :only (join)]
        [clojure.template :only (apply-template)] 
        [clojure.pprint :only (pprint simple-dispatch)])
  (:import [java.io BufferedReader InputStreamReader]))

;; defines some base functions (that are independent of the tank DSL
;; application) that the tank DSL is built on

(defmacro deft
  "Alternative to defmacro. Replaces each occurrence of each arg symbol in the body, so this can unintentionally capture symbols in the body. This is particularly an issue if the body expression came from 'elsewhere'. Unlike defmacro, symbols in the expanded form are not automatically namespace qualified and no gensyms are used. As compared to defmacro this is good for producing forms that are more like what someone would write by hand and are more pleasant/compact to read."
  [name args body]
  `(defn ~name ~args
     (apply-template '~args
                     '~body
                     ~args)))

;; from: http://learnclojure.blogspot.com/2010/09/clojure-macro-tutorial-part-i-getting.html
(defmacro dbg
  "Wrap any expression in a call to dbg and the expression will be printed along with the namespace it is found in. The expresssion is passed through so this can be added without changing the evaluation of the expression."
  [x]
  (let [ns# *ns*]
    `(let [x# ~x]
       (println (str ~ns#) ":" '~x "=")
       (pprint x#)
       (if (meta x#)
         (do (println "with meta")
             (pprint (meta x#))))
       x#)))

;;;; operators

(defn augment-contents [type contents]
  (conj contents type))

(defmulti map-contents (fn [type contents]
                         type))

(defmulti contents-from-map (fn [type m]
                              type))

(defmulti op-keys 
  "clojure.core/keys for operators"
  first)

(deft gen-op-type [_type _op]
  (deftype _type [contents]
    clojure.lang.IPersistentList

    clojure.lang.Sequential

    clojure.lang.IObj
    (withMeta [_ m]
      (new _type (with-meta contents m)))

    clojure.lang.IMeta
    (meta [_]
      (meta contents))
    
    clojure.lang.IPersistentStack
    (peek [_]
      (.peek (bolo.base/augment-contents '_op contents)))
    (pop [_]
      (.pop (bolo.base/augment-contents '_op contents)))
    
    java.lang.Iterable
    (iterator [_]
      (.iterator (bolo.base/augment-contents '_op contents)))

    clojure.lang.IPersistentCollection
    (count [_]
      (.count (bolo.base/augment-contents '_op contents)))
    (cons [_ o]
      (new _type (.cons contents o)))
    (empty [_]
      false)
    (equiv [_ o]
      (.equiv (bolo.base/augment-contents '_op contents) o))

    clojure.lang.Seqable
    (seq [_]
      (seq (bolo.base/augment-contents '_op contents)))

    java.util.List
    (add [_ x]
      (new _type (.add contents x)))
    (add [_ n x]
      (new _type (.add contents (dec n) x)))
    (addAll [_ c]
      (new _type (.addAll contents c)))
    (clear [_]
      (new _type '()))
    (contains [_ x]
      (.contains (bolo.base/augment-contents '_op contents) x))
    (containsAll [_ c]
      (.containsAll (bolo.base/augment-contents '_op contents) c))
    (equals [_ o]
      (.equals (bolo.base/augment-contents '_op contents) o))
    (get [_ n]
      (.get (bolo.base/augment-contents '_op contents) n))
    (hashCode [_]
      (.hashCode (bolo.base/augment-contents '_op contents)))
    (indexOf [_ o]
      (.indexOf (bolo.base/augment-contents '_op contents) o))
    (isEmpty [_]
      (.isEmpty (bolo.base/augment-contents '_op contents)))
    (lastIndexOf [_ o]
      (.lastIndexOf (bolo.base/augment-contents '_op contents) o))
    (listIterator [_]
      (.listIterator (bolo.base/augment-contents '_op contents)))
    (listIterator [_ n]
      (.listIterator (bolo.base/augment-contents '_op contents) n))
    (remove [_ ^int n]
      (.remove (bolo.base/augment-contents '_op contents) n))
    (^boolean remove [_ ^Object o]
      (.remove (bolo.base/augment-contents '_op contents) o))
    (removeAll [_ c]
      (.removeAll (bolo.base/augment-contents '_op contents) c))
    (retainAll [_ c]
      (.retaingAll (bolo.base/augment-contents '_op contents) c))
    (set [_ n e]
      (.set (bolo.base/augment-contents '_op contents) n e))
    (size [_]
      (.size (bolo.base/augment-contents '_op contents)))
    (subList [_ s e]
      (.subList (bolo.base/augment-contents '_op contents) s e))
    (toArray [_]
      (.toArray (bolo.base/augment-contents '_op contents)))
    (toArray [_ a]
      (.toArray (bolo.base/augment-contents '_op contents) a))

    clojure.lang.ISeq
    (first [_]
      (.first (bolo.base/augment-contents '_op contents)))
    (next [_]
      (.next (bolo.base/augment-contents '_op contents)))
    (more [_]
      (.more (bolo.base/augment-contents '_op contents)))

    clojure.lang.ILookup
    (valAt [_ k]
      (.valAt (bolo.base/map-contents '_op contents) k))
    (valAt [_ k not-found]
      (.valAt (bolo.base/map-contents '_op contents) k not-found))

    clojure.lang.Associative
    (containsKey [_ k]
      (.containsKey (bolo.base/map-contents '_op contents) k))
    (entryAt [_ k]
      (.entryAt (bolo.base/map-contents '_op contents) k))
    (assoc [_ k v]
      (new _type (bolo.base/contents-from-map '_op
                                              (assoc (bolo.base/map-contents '_op contents) k v))))))

(defn remove-& [args]
  (remove #(= '& %) args))

(defn varargs? [args]
  (not (empty? (filter #(= '& %) args))))

(defn field-list [fields]
  (if (varargs? fields)
    `(concat (list ~@(reverse (drop 2 (reverse fields))))
             ~(last fields))
    `(list ~@fields)))

(defmacro make-map-contents [op fields]
  `(defmethod bolo.base/map-contents '~op [_# contents#]
     (let [~(vec fields) contents#]
       ~(apply hash-map (mapcat #(vector (keyword %) %) (remove-& fields))))))

(defmacro make-contents-from-map [op fields]
  `(defmethod bolo.base/contents-from-map '~op [_# m#]
     (let [{:keys ~(remove-& fields)} m#]
       (if ~(last fields)
         ~(field-list fields)
         ~(reverse (rest (reverse (field-list fields))))))))

(defmacro make-ctor-function [op fields]
  `(defn ~(symbol (str "new-" (str op))) ~(vec fields)
     (new ~(symbol (capitalize (str op)))
          (if ~(last fields)
            ~(field-list fields)
            ~(reverse (rest (reverse (field-list fields))))))))

(defmacro make-op-macro [op type fields]
  (let [fields (vec fields)
        first-fields (vec (reverse (rest (reverse (remove #(= '& %) fields)))))
        last-field (first (reverse fields))
        var-args? (some #(= '& %) fields)]
    (if (empty? fields)
      `(defmacro ~op []
         `(new ~~type (list)))
      (if var-args?
        `(defmacro ~op ~fields
           `(new ~~type (apply list (into ~~first-fields (vector ~@~last-field)))))
        `(defmacro ~op
           (~first-fields
            `(new ~~type (list ~~@first-fields)))
           (~fields
            (if ~last-field
              `(new ~~type (list ~~@fields))
              `(new ~~type (list ~~@first-fields)))))))))

(defmacro make-op-keys [op fields]
  `(defmethod op-keys '~op [& _#]
     '~(map keyword (remove-& fields))))

(defmacro new-op [op fields]
  (let [type (symbol (capitalize (name op)))]
    `(do
       (eval (gen-op-type '~type '~op))
       (make-op-keys ~op ~fields)
       (make-map-contents ~op ~fields)
       (make-contents-from-map ~op ~fields)
       (make-ctor-function ~op ~fields)
       (make-op-macro ~op ~type ~fields))))

;;;;

(defmacro eval-in-ns
  "Evaluate an expr in the context of the given ns."
  [ns expr]
  `(binding [*ns* (find-ns '~ns)]
     (eval ~expr)))
