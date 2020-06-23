(ns gradual.impl.util
  "Adapted from the Quantum library (alexandergunnarson/quantum)."
         (:refer-clojure :exclude
           [any? boolean? double? ident? pos-int? qualified-keyword? seqable? simple-symbol?])
         (:require
           [clojure.core           :as core]
           [clojure.spec.alpha     :as s]
           [clojure.spec.gen.alpha :as gen])
#?(:cljs (:require-macros
           [gradual.impl.util
             :refer [defalias]])))

;; ===== `form.evaluate` ===== ;;

(defn cljs-env?
  "Given an &env from a macro, tells whether it is expanding into CLJS."
  {:from "https://groups.google.com/d/msg/clojurescript/iBY5HaQda4A/w1lAQi9_AwsJ"}
  [env]
  (boolean (:ns env)))

(defn case-env|matches? [env k]
  (case k
    :clj  (not (cljs-env? env)) ; TODO should make this branching
    :cljs (cljs-env? env)
    :clr  (throw (ex-info "TODO: Conditional compilation for CLR not supported" {:platform :clr}))
    (throw (ex-info "Conditional compilation for platform not supported" {:platform k}))))

#?(:clj
(defmacro case-env*
  "Conditionally compiles depending on the supplied environment (e.g. CLJ, CLJS, CLR)."
  {:usage `(defmacro abcde [a]
             (case-env* &env :clj `(+ ~a 2) :cljs `(+ ~a 1) `(+ ~a 3)))
   :todo  {0 "Not sure how CLJ environment would be differentiated from others"}}
  ([env]
    `(throw (ex-info "Compilation unhandled for environment" {:env ~env})))
  ([env v] v)
  ([env k v & kvs]
    `(let [env# ~env]
       (if (case-env|matches? env# ~k)
           ~v
           (case-env* env# ~@kvs))))))

#?(:clj
(defmacro case-env
  "Conditionally compiles depending on the supplied environment (e.g. CLJ, CLJS, CLR)."
  {:usage `(defmacro abcde [a]
             (case-env :clj `(+ ~a 2) :cljs `(+ ~a 1) `(+ ~a 3)))}
  ([& args] `(case-env* ~'&env ~@args))))

#?(:clj (defmacro env-lang [] (case-env :clj :clj :cljs :cljs :clr :clr)))

;; ===== `var` ===== ;;

#?(:clj
(defn defalias* [^clojure.lang.Var orig-var ns-name- var-name]
  (let [;; to avoid warnings
        var-name' (with-meta var-name (-> orig-var meta (select-keys [:dynamic])))
        ^clojure.lang.Var var-
          (if (.hasRoot orig-var)
              (intern ns-name- var-name' @orig-var)
              (intern ns-name- var-name'))]
    ;; because this doesn't always get set correctly
    (cond-> var-
      (.isDynamic orig-var)
      (doto (.setDynamic))))))

#?(:clj
(defmacro defalias
  "Defines an alias for a var: a new var with the same root binding (if
  any) and similar metadata. The metadata of the alias is its initial
  metadata (as provided by def) merged into the metadata of the original."
  {:attribution  'clojure.contrib.def/defalias
   :contributors ["Alex Gunnarson"]}
  ([orig]
    `(defalias ~(symbol (name orig)) ~orig))
  ([name orig]
    `(doto ~(case-env
               :clj  `(defalias* (var ~orig) '~(ns-name *ns*) '~name)
               :cljs `(def ~name (-> ~orig var deref)))
            (alter-meta! merge (meta (var ~orig)))))
  ([name orig doc]
     (list `defalias (with-meta name (assoc (meta name) :doc doc)) orig))))

;; ===== `predicate` ===== ;;

;; The reason we use `resolve` and `eval` here is that currently we need to prefer built-in impls
;; where possible in order to leverage their generators

#?(:clj
(try (require '[clojure.future :as fcore])
  (catch Throwable _)))

#?(:clj  (eval `(defalias ~(if (resolve `fcore/any?)
                               `fcore/any?
                               `core/any?)))
   :cljs (defalias core/any?))

#?(:clj  (eval `(defalias ~(if (resolve `fcore/boolean?)
                               `fcore/boolean?
                               `core/boolean?)))
   :cljs (defalias core/boolean?))

#?(:clj  (eval `(defalias ~(if (resolve `fcore/double?)
                               `fcore/double?
                               `core/double?)))
   :cljs (defalias core/double?))

#?(:clj  (eval `(defalias ~(if (resolve `fcore/ident?)
                               `fcore/ident?
                               `core/ident?)))
   :cljs (defalias core/ident?))

#?(:clj  (eval `(defalias ~(if (resolve `fcore/pos-int?)
                               `fcore/pos-int?
                               `core/pos-int?)))
   :cljs (defalias core/pos-int?))

#?(:clj  (eval `(defalias ~(if (resolve `fcore/qualified-keyword?)
                               `fcore/qualified-keyword?
                               `core/qualified-keyword?)))
   :cljs (defalias core/qualified-keyword?))

#?(:clj  (eval `(defalias ~(if (resolve `fcore/seqable?)
                               `fcore/seqable?
                               `core/seqable?)))
   :cljs (defalias core/seqable?))

#?(:clj  (eval `(defalias ~(if (resolve `fcore/simple-symbol?)
                               `fcore/simple-symbol?
                               `core/simple-symbol?)))
   :cljs (defalias core/simple-symbol?))

(def val? some?)

;; ===== `fn` ===== ;;

#?(:clj (defmacro fn1 [f & args] `(fn fn1# [arg#] (~f arg# ~@args)))) ; analogous to ->
#?(:clj (defmacro fnl [f & args] `(fn fnl# [arg#] (~f ~@args arg#)))) ; analogous to ->>

;; ===== `identifier` ===== ;;

(defn >keyword [x]
  (cond (keyword? x) x
        (symbol?  x) (keyword (namespace x) (name x))
        :else        (-> x str keyword)))

;; ===== `collection` ===== ;;

(defn reduce-2
  "Reduces over two seqables at a time."
  {:todo #{"`defnt` this and have it dispatch to e.g. reduce-2:indexed"}}
  ([f xs0 xs1] (reduce-2 f nil xs0 xs1))
  ([f init xs0 xs1] (reduce-2 f init xs0 xs1 false))
  ([f init xs0 xs1 assert-same-count?]
    (loop [ret init xs0' xs0 xs1' xs1]
      (cond (reduced? ret)
            @ret
            (or (empty? xs0') (empty? xs1'))
            (do (when (and assert-same-count?
                           (or (and (empty? xs0') (seq    xs1'))
                               (and (seq    xs0') (empty? xs1'))))
                  (throw (ex-info "Seqables are not the same count" {})))
                ret)
            :else (recur (f ret (first xs0') (first xs1'))
                         (next xs0')
                         (next xs1'))))))

(defn reducei
  "`reduce`, indexed."
  [f init xs]
  (let [f' (let [*i (volatile! -1)]
              (fn ([ret x]
                    (f ret x (vreset! *i (unchecked-inc (long @*i)))))))]
    (reduce f' init xs)))

;; ===== `spec ===== ;;

(defn validate [spec x]
  (let [conformed (s/conform spec x)]
    (if (s/invalid? conformed)
        (let [ed (core/merge (assoc (s/explain-data* spec [] [] [] x)
                               ::s/failure :assertion-failed))]
          (throw (ex-info
                   (str "Spec assertion failed\n" (with-out-str (s/explain-out ed)))
                   ed)))
        conformed)))

(defn kv
  "Based on `s/map-spec-impl`"
  ([k->s #_(s/map-of any? specable?)] (kv k->s nil))
  ([k->s #_(s/map-of any? specable?) gen-fn #_(? fn?)]
    (let [id #?(:clj (java.util.UUID/randomUUID) :cljs random-uuid)
          k->s|desc (->> k->s
                         (map (fn [[k specable]]
                                [k (if (ident? specable) specable (s/describe specable))]))
                         (into {}))]
      (reify
        s/Specize
          (specize* [this] this)
          (specize* [this _] this)
        s/Spec
          (conform* [_ x]
            (reduce
              (fn [x' [k s]]
                (let [v  (get x' k)
                      cv (s/conform s v)]
                  (if (s/invalid? cv)
                      ::s/invalid
                      (if (identical? cv v)
                          x'
                          ;; TODO we might want to do `assoc?!`, depending
                          (assoc x' k cv)))))
              x
              k->s))
          (unform* [_ x]
            (reduce
              (fn [x' [k s]]
                (let [cv (get x' k)
                      v  (s/unform s cv)]
                  (if (identical? cv v)
                      x'
                      ;; TODO we might want to do `assoc?!`, depending
                      (assoc x' k v))))
              x
              k->s))
          (explain* [_ path via in x]
            (if-not ;; TODO we might want a more generalized `map?` predicate like `t/map?`, depending,
                    ;; which would affect more code below
                    (map? x)
              [{:path path :pred 'map? :val x :via via :in in}]
              ;; TODO use reducers?
              (->> k->s
                   (map (fn [[k s]]
                          (let [v (get x k)]
                            (when-not (s/valid? s v)
                              (@#'s/explain-1 (get k->s|desc k) s (conj path k) via (conj in k) v)))))
                   (filter some?)
                   (apply concat))))
          (gen* [_ overrides path rmap]
            (if gen-fn
                (gen-fn)
                (let [rmap (assoc rmap id (inc (core/or (get rmap id) 0)))
                      gen  (fn [[k s]]
                             (when-not (@#'s/recur-limit? rmap id path k)
                               [k (gen/delay (@#'s/gensub s overrides (conj path k) rmap k))]))
                      gens (->> k->s (map gen) (remove nil?) (into {}))]
                  (gen/bind (gen/choose 0 (count gens))
                            (fn [n]
                              (let [args (-> gens seq shuffle)]
                                (->> args
                                     (take n)
                                     (apply concat)
                                     (apply gen/hash-map))))))))
          (with-gen* [_ gen-fn'] (kv k->s gen-fn'))
          (describe* [_] `(kv ~k->s|desc))))))

(defn with-gen-spec-impl
  "Do not call this directly; use 'with-gen-spec'."
  [extract-f extract-f|form gen-spec gen-spec|form]
  (if (fn? gen-spec)
      (let [form      `(with-gen-spec ~extract-f|form ~gen-spec|form)
            gen-spec' (fn [x]
                        (let [spec (gen-spec x)
                              desc (s/describe spec)
                              desc (if (= desc ::s/unknown)
                                       (list 'some-generated-spec gen-spec|form)
                                       desc)]
                          (s/nonconforming (s/and (s/conformer extract-f)
                                                  (@#'s/spec-impl desc spec nil nil)))))]
        (reify
          s/Specize
            (s/specize*  [this] this)
            (s/specize*  [this _] this)
          s/Spec
            (s/conform*  [_ x] (s/conform* (gen-spec' x) x))
            (s/unform*   [_ x] (s/unform* (gen-spec' x) x))
            (s/explain*  [_ path via in x] (s/explain* (gen-spec' x) path via in x))
            (s/gen*      [_ _ _ _] (gen/gen-for-pred gen-spec))
            (s/with-gen* [_ _] (throw (ex-info "TODO" {})))
            (s/describe* [_] form)))
      (throw (ex-info "`wrap-spec` may only be called on fns" {:input gen-spec}))))

#?(:clj
(defmacro with-gen-spec
  "`gen-spec` : an fn that returns a spec based on the input.
   `extract-f`: extracts the piece of data from the input that the generated spec will validate.
   E.g.:
   (s/explain
     (s/with-gen-spec (fn [{:keys [a]}] a) (fn [{:keys [b]}] #(> % b)))
     {:a 1 :b 1})"
  [extract-f gen-spec]
  `(with-gen-spec-impl ~extract-f '~extract-f ~gen-spec '~gen-spec)))
