(ns gradual.spec
  (:refer-clojure :exclude
    [any? fn defn defn- ident? qualified-keyword? seqable? simple-symbol?])
  (:require
    [clojure.core           :as core]
    [clojure.set            :as set]
    [clojure.spec.alpha     :as s]
    [clojure.spec.gen.alpha :as gen]
    [gradual.impl.util      :as u
      :refer [any? ident? qualified-keyword? seqable? simple-symbol?]]
    [linked.core            :as linked]))

;; ===== Specs ===== ;;

(s/def :gradual/meta map?)

(s/def :gradual/or (s/map-of simple-symbol? any?))

(s/def :gradual/fn|name simple-symbol?)

(s/def :gradual/docstring string?)

(s/def :gradual/fn|unique-doc
  #(->> [(:gradual/docstring %)
         (-> % :gradual/fn|name meta :doc)
         (-> % :pre-meta             :doc)
         (-> % :post-meta            :doc)]
        (filter u/val?)
        count
        ((core/fn [x] (<= x 1)))))

(s/def :gradual/fn|unique-meta
  #(empty? (set/intersection
             (-> % :gradual/fn|name meta keys set)
             (-> % :pre-meta                        keys set)
             (-> % :post-meta                       keys set))))

(s/def :gradual/fn|aggregate-meta
  (s/conformer
    (core/fn [{:keys [:gradual/fn|name :gradual/docstring pre-meta post-meta] :as m}]
      (-> m
          (dissoc :gradual/docstring :pre-meta :post-meta)
          (cond-> fn|name
            (update :gradual/fn|name with-meta
              (-> (merge (meta fn|name) pre-meta post-meta) ; TODO use `merge-unique` instead of `:gradual/defn|unique-meta`
                  (cond-> docstring (assoc :doc docstring)))))))))

(s/def :gradual/fn|postchecks
  (s/and (s/conformer
           (core/fn [v]
             (let [[overloads-k overloads-v] (get v :overloads)
                   overloads
                    (-> (case overloads-k
                          :overload-1 {:overloads [overloads-v]}
                          :overload-n overloads-v)
                        (update :overloads
                          (u/fnl mapv
                            (u/fn1 update :body
                              (core/fn [[k v]]
                                (case k
                                  :body         {:body v}
                                  :prepost+body v))))))]
               (assoc v :post-meta (:post-meta overloads)
                        :overloads (:overloads overloads)))))
         :gradual/fn|unique-doc
         :gradual/fn|unique-meta
         ;; TODO validate metadata like return value etc.
         :gradual/fn|aggregate-meta))

(s/def :gradual/local-name
  (s/and simple-symbol? (complement #{'& '| '> '?})))

(s/def :gradual/spec
  (s/alt :infer #{'?}
         :any   #{'_}
         :spec   any?))

;; ----- General destructuring ----- ;;

(s/def :gradual/binding-form
  (s/alt :sym :gradual/local-name
         :seq :gradual/seq-binding-form
         :map :gradual/map-binding-form))

(s/def :gradual/speced-binding
  (s/cat :binding-form :gradual/binding-form
         :spec         :gradual/spec))

;; ----- Sequential destructuring ----- ;;

(s/def :gradual/seq-binding-form
  (s/and vector?
         (s/cat :elems (s/* :gradual/speced-binding)
                :rest  (s/? (s/cat :amp #{'&}  :form :gradual/speced-binding))
                :as    (s/? (s/cat :as  #{:as} :sym  :gradual/local-name)))))

;; ----- Map destructuring ----- ;;

(core/defn- >keys|syms|strs [spec]
  (s/and vector?
    (s/spec (s/* (s/cat :binding-form spec
                        :spec         :gradual/spec)))))

(s/def :gradual/keys (>keys|syms|strs ident?))
(s/def :gradual/syms (>keys|syms|strs symbol?))
(s/def :gradual/strs (>keys|syms|strs simple-symbol?))

(s/def :gradual/as :gradual/local-name)

(s/def :gradual/map-special-binding
  (s/keys :opt-un [:gradual/as   :gradual/or
                   :gradual/keys :gradual/syms :gradual/strs]))

(s/def :gradual/map-binding
  (s/spec (s/cat :binding-form :gradual/binding-form
                 :key+spec     (s/spec (s/cat :key any? :spec :gradual/spec)))))

(s/def :gradual/ns-keys
  (s/tuple
    (s/and qualified-keyword? #(-> % name #{"keys" "syms"}))
    (>keys|syms|strs simple-symbol?)))

(s/def :gradual/map-binding-form
  (s/and :gradual/map-special-binding
         (s/coll-of (s/or :map-binding :gradual/map-binding
                          :ns-keys     :gradual/ns-keys
                          :special     (s/tuple #{:as :or :keys :syms :strs} any?)) :into {})))

;; ----- Args ----- ;;

(s/def :gradual/output-spec
  (s/? (s/cat :sym #(= % '>) :spec :gradual/spec)))

(s/def :gradual/arglist
  (s/and vector?
         (s/spec
           (s/cat :args    (s/* :gradual/speced-binding)
                  :varargs (s/? (s/cat :sym            #(= % '&)
                                       :speced-binding :gradual/speced-binding))
                  :pre     (s/? (s/cat :sym            #(= % '|)
                                       :spec           (s/or :any-spec #{'_} :spec any?)))
                  :post    :gradual/output-spec))
         (s/conformer
           #(cond-> % (contains? % :varargs) (update :varargs :speced-binding)
                      (contains? % :pre    ) (update :pre     :spec)
                      (contains? % :post   ) (update :post    :spec)))
         (core/fn [{:keys [args varargs]}]
           ;; so `env` in `gradual.type/fn` can work properly in the analysis
           ;; TODO need to adjust for destructuring
           (distinct?
             (concat (map :binding-form args)
                     [(:binding-form varargs)])))))

(s/def :gradual/body (s/alt :body (s/* any?)))

(s/def :gradual/arglist+body
  (s/cat :arglist :gradual/arglist
         :body    :gradual/body))

(s/def :gradual/overloads
  (s/alt :overload-1 :gradual/arglist+body
         :overload-n (s/cat :overloads (s/+ (s/spec :gradual/arglist+body)))))

(s/def :gradual/postchecks
  (s/conformer
    (core/fn [f]
      (-> f (update :overloads
              #(mapv (core/fn [overload]
                          (let [overload' (update overload :body :body)]
                            (if-let [output-spec (-> f :output-spec :spec)]
                              (do (u/validate nil? (-> overload' :arglist :post))
                                  (assoc-in overload' [:arglist :post] output-spec))
                              overload'))) %))
            (dissoc :output-spec)))))

(s/def :gradual.type/fn
  (s/and (s/spec
           (s/cat
             :gradual/fn|name   (s/? :gradual/fn|name)
             :gradual/docstring (s/? :gradual/docstring)
             :pre-meta          (s/? :gradual/meta)
             :output-spec       :gradual/output-spec
             :overloads         :gradual/overloads))
         :gradual/fn|postchecks
         :gradual/postchecks))

(s/def :gradual.spec/fn|code :gradual.type/fn)

(s/def :gradual.type/defn
  (s/and (s/spec
           (s/cat
             :gradual/fn|name   :gradual/fn|name
             :gradual/docstring (s/? :gradual/docstring)
             :pre-meta          (s/? :gradual/meta)
             :output-spec       :gradual/output-spec
             :overloads         :gradual/overloads))
         :gradual/fn|postchecks
         :gradual/postchecks))

(s/def :gradual.spec/defn|code :gradual.type/defn)

(s/def :gradual/binding-form
  (s/alt :sym :gradual/local-name
         :seq :gradual/seq-binding-form
         :map :gradual/map-binding-form))

;; ===== Implementation ===== ;;

#?(:clj
(defmacro ^:internal s*
  "Qualifies the calling symbol with `(clojure|cljs).spec.alpha`, depending on the evaluating lang."
  [caller-sym & args]
  (let [caller-sym' (symbol
                      (case (u/env-lang)
                        :clj  "clojure.spec.alpha"
                        :cljs "cljs.spec.alpha")
                      (name caller-sym))]
    (list* caller-sym' args))))

(core/defn ^:internal >seq-destructuring-spec
  "Creates a spec that performs seq destructuring, and provides a default generator for such based
   on the generators of the destructured args."
  [positional-destructurer most-complex-positional-destructurer kv-spec or|conformer seq-spec
   {:as opts generate-from-seq-spec? :gen?}]
  (let [or|unformer (s/conformer second)
        most-complex-positional-destructurer|unformer
          (s/conformer (core/fn [x] (s/unform most-complex-positional-destructurer x)))]
    (cond->
      (s/and seq-spec
             (s/conformer (core/fn [xs] {:xs xs :xs|destructured xs}))
             (u/kv {:xs|destructured (s/and positional-destructurer
                                            or|unformer
                                            kv-spec)})
             (s/conformer (core/fn [m] (assoc m :xs|positionally-destructured|ct
                                         (when-not (-> m :xs|destructured (contains? :varargs))
                                           (-> m :xs|destructured count)))))
             (u/kv {:xs|destructured
                     (s/and or|conformer
                            (s/conformer (core/fn [x] (s/unform positional-destructurer x))))})
             (s/conformer (core/fn [{:keys [xs xs|destructured xs|positionally-destructured|ct]}]
                            (if xs|positionally-destructured|ct
                                (concat xs|destructured (drop xs|positionally-destructured|ct xs))
                                xs|destructured))))
      (not generate-from-seq-spec?)
      (s/with-gen
        #(->> (s/gen kv-spec)
              (gen/fmap (core/fn [x]
                          (s/conform most-complex-positional-destructurer|unformer x))))))))

#?(:clj
(defmacro ^:internal seq-destructure
  "If `generate-from-seq-spec?` is true, generates from `seq-spec`'s generator instead of the
   default generation strategy based on the generators of the destructured args."
  [seq-spec #_any? args #_(s/* (s/cat :k keyword? :spec any?))
   & [varargs #_(s/nilable (s/cat :k keyword? :spec any?))]]
  (let [opts    (meta seq-spec)
        args    (u/validate (s/* (s/cat :k keyword? :spec any?)) args)
        varargs (u/validate (s/nilable (s/cat :k keyword? :spec any?)) varargs)
        args-ct>args-kw #(keyword (str "args-" %))
        arity>cat (core/fn [arg-i]
                   `(s* cat ~@(->> args (take arg-i)
                                        (map (core/fn [{:keys [k spec]}] [k `any?]))
                                        (apply concat))))
        most-complex-positional-destructurer-sym (gensym "most-complex-positional-destructurer")]
   `(let [~most-complex-positional-destructurer-sym
            (s* cat ~@(->> args
                           (map (core/fn [{:keys [k]}] [k `any?]))
                           (apply concat))
                    ~@(when varargs [(:k varargs) `(s* & (s* + any?) (s* conformer seq identity))]))
          positional-destructurer#
            (s* or :args-0 (s* cat)
                    ~@(->> (range (count args))
                           (map (core/fn [i] [(args-ct>args-kw (inc i)) (arity>cat (inc i))]))
                           (apply concat))
                    ~@(when varargs [:varargs most-complex-positional-destructurer-sym]))
          kv-spec#
            (u/kv (linked/map
                    ~@(apply concat
                        (cond-> (->> args (map (core/fn [{:keys [k spec]}] [k spec])))
                          varargs (concat [[(:k varargs) (:spec varargs)]])))))
          or|conformer#
            (s* conformer
              (core/fn or|conformer# [m#]
                [(case (count m#)
                    ~@(->> (range (inc (count args)))
                           (map (juxt identity args-ct>args-kw))
                           (apply concat))
                    ~@(when varargs [:varargs]))
                 m#]))]
      (>seq-destructuring-spec positional-destructurer# ~most-complex-positional-destructurer-sym
        kv-spec# or|conformer# ~seq-spec ~opts)))))

#?(:clj
(defmacro ^:internal map-destructure [map-spec #_any? kv-specs #_(s/map-of any? any?)]
  (let [kv-spec-sym (gensym "kv-spec")
        {:as opts generate-from-map-spec? :gen?} (meta map-spec)]
    `(let [~kv-spec-sym (u/kv ~kv-specs)]
       ~(if generate-from-map-spec?
            `(s* and ~map-spec ~kv-spec-sym)
            `(s* with-gen (s* and ~map-spec ~kv-spec-sym) (core/fn [] (s* gen ~kv-spec-sym))))))))

(core/defn ^:internal speced-binding>binding
  [{[kind binding-] :binding-form} #_:gradual/speced-binding]
  (case kind
    :sym binding-
    :seq (let [{:keys [as elems] rest- :rest} binding-]
           (cond-> (mapv speced-binding>binding elems)
                   rest- (conj '&  (-> rest- :form speced-binding>binding))
                   as    (conj :as (:sym as))))
    :map (->> binding-
              (map (core/fn [[k v]]
                     (case k
                       :as                 [k (second v)]
                       :or                 [k v]
                       (:keys :syms :strs) [k (->> v second (mapv :binding-form))]
                       [(speced-binding>binding v)
                        (get-in v [:key+spec :key])])))
              (into {}))))

(core/defn ^:internal speced-binding>arg-ident
  [{[kind binding-] :binding-form} #_:gradual/speced-binding & [i|arg] #_(? nneg-integer?)]
  (u/>keyword
    (case kind
      :sym binding-
      (:seq :map)
        (let [ks (if (= kind :seq) [:as :sym] [:as 1])]
          (or (get-in binding- ks)
              (gensym (if i|arg (str "arg-" i|arg "-") "varargs")))))))

(declare speced-binding>spec)

(core/defn- speced-binding|seq>spec
  [{:as speced-binding [kind binding-] :binding-form [spec-kind spec] :spec}]
  `(seq-destructure ~(if (= spec-kind :spec) spec `seqable?)
    ~(->> binding- :elems
          (map-indexed
            (core/fn [i|arg arg|speced-binding]
              [(speced-binding>arg-ident arg|speced-binding i|arg)
               (speced-binding>spec arg|speced-binding)]))
          (apply concat)
          vec)
    ~@(when-let [varargs|speced-binding (get-in binding- [:rest :form])]
        [[(speced-binding>arg-ident varargs|speced-binding)
          (speced-binding>spec varargs|speced-binding)]])))

(core/defn- keys||strs||syms>key-specs [kind #_#{:keys :strs :syms} speced-bindings]
  (let [binding-form>key
          (case kind :keys u/>keyword :strs name :syms identity)]
    (->> speced-bindings
         (filter (core/fn [{[spec-kind _] :spec}] (= spec-kind :spec)))
         (map (core/fn [{:keys [binding-form #_symbol?] [_ spec] :spec}]
                [(binding-form>key binding-form) spec])))))

(core/defn- speced-binding|map>spec
  [{:as speced-binding [kind binding-] :binding-form [spec-kind spec] :spec}]
  `(map-destructure ~(if (= spec-kind :spec) spec `map?)
    ~(->> (dissoc binding- :as :or)
          (map (core/fn [[k v]]
                 (case k
                   (:keys :strs :syms)
                     (keys||strs||syms>key-specs k (second v))
                   [[(get-in v [:key+spec :key])
                     (speced-binding>spec
                       (assoc v :spec (get-in v [:key+spec :spec])))]])))
          (apply concat)
          (into {}))))

(core/defn speced-binding>spec
  [{:as speced-binding [kind binding-] :binding-form [spec-kind spec] :spec}]
  (case kind
    :sym (if (= spec-kind :spec) spec `any?)
    :seq (speced-binding|seq>spec speced-binding)
    :map (speced-binding|map>spec speced-binding)))

(core/defn arglist>spec-form|arglist
  [args+varargs kw-args #_:gradual/map-binding-form]
  `(s* cat ~@(u/reduce-2
               (core/fn [ret speced-binding [_ kw-arg]]
                 (conj ret kw-arg (speced-binding>spec speced-binding)))
               []
               args+varargs kw-args)))

;; TODO handle duplicate bindings (e.g. `_`) by `s/cat` using unique keys — e.g. :b|arg-2
(core/defn fn|code [kind lang args]
  (assert (= lang #?(:clj :clj :cljs :cljs)) lang)
  (when (= kind :fn) (println "WARNING: `fn` will ignore spec validation"))
  (let [{:keys [:gradual/fn|name overloads :gradual/meta] :as args'}
          (u/validate (case kind (:defn :defn-) :gradual.spec/defn|code
                                 :fn            :gradual.spec/fn|code) args)
        ret-sym (gensym "ret") arity-kind-sym (gensym "arity-kind") args-sym (gensym "args")
        {:keys [overload-forms spec-form|args spec-form|fn]}
          (reduce
            (core/fn [ret {{:keys [args varargs] [pre-kind pre] :pre [_ post] :post :as arglist} :arglist :keys [body]} #_:gradual/arglist+body]
              (let [{:keys [fn-arglist kw-args]}
                      (u/reducei
                        (core/fn [ret {:as speced-binding :keys [varargs?]} i|arg]
                          (let [arg-ident (speced-binding>arg-ident speced-binding i|arg)
                                binding-  (speced-binding>binding speced-binding)]
                            (-> ret (cond-> varargs? (update :fn-arglist conj '&))
                                    (update :fn-arglist conj  binding-)
                                    (update :kw-args    assoc binding- arg-ident))))
                        {:fn-arglist [] :kw-args (linked/map)}
                        (cond-> args varargs (conj (assoc varargs :varargs? true))))
                    overload-form     (list* fn-arglist body)
                    arity-ident       (keyword (str "arity-" (if varargs "varargs" (count args))))
                    spec-form|arglist (arglist>spec-form|arglist (cond-> args varargs (conj varargs)) kw-args)
                    spec-form|pre     (when (and (contains? arglist :pre) (= pre-kind :spec))
                                        `(core/fn [~kw-args] ~pre))
                    spec-form|args*   (if spec-form|pre
                                          `(s* and ~spec-form|arglist ~spec-form|pre)
                                          spec-form|arglist)
                    spec-form|fn*     (if (contains? arglist :post)
                                          `(let [~kw-args ~args-sym] (s* spec ~post))
                                          `(s* spec any?))]
                (-> ret
                    (update :overload-forms conj overload-form)
                    (update :spec-form|args conj arity-ident spec-form|args*)
                    (update :spec-form|fn   conj arity-ident spec-form|fn*))))
            {:overload-forms []
             :spec-form|args []
             :spec-form|fn   []}
            overloads)
        spec-form (when (#{:defn :defn-} kind)
                    `(s* fdef ~fn|name :args (s* or ~@spec-form|args)
                                       :fn   (u/with-gen-spec (core/fn [{~ret-sym :ret}] ~ret-sym)
                                               (core/fn [{[~arity-kind-sym ~args-sym] :args}]
                                                 (case ~arity-kind-sym ~@spec-form|fn)))))
        fn-form (case kind
                  :fn    (list* 'fn (concat (when (contains? args' :gradual/fn|name)
                                              [fn|name])
                                            overload-forms))
                  :defn  (list* 'defn fn|name overload-forms)
                  :defn- (list* 'defn- fn|name overload-forms))
        code `(do ~spec-form ~fn-form)]
    code))

#?(:clj
(defmacro fn
  "Like `gradual.type/fn`, but relies entirely on runtime spec checks. Ignores type inference
   requests, but allows them for compatibility with `gradual.type/defn`."
  [& args] (fn|code :fn (u/env-lang) args)))

#?(:clj
(defmacro defn
  "Like `gradual.type/defn`, but relies entirely on runtime spec checks. Ignores type inference
   requests, but allows them for compatibility with `gradual.type/defn`."
  [& args] (fn|code :defn (u/env-lang) args)))

#?(:clj
(defmacro defn-
  "defn : defn- :: core/defn : core/defn-"
  [& args] (fn|code :defn- (u/env-lang) args)))
