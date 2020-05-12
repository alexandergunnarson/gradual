# `gradual`

![.github/workflows/test.yml](https://github.com/alexandergunnarson/gradual/workflows/.github/workflows/test.yml/badge.svg)
[![Clojars Project](https://img.shields.io/clojars/v/gradual.svg)](https://clojars.org/gradual)

Gradual, dependent typing for Clojure. Also comes with [`clojure.spec`](https://clojure.org/about/spec) interop (see `gradual.spec`).

The successor of https://github.com/alexandergunnarson/defnt, which is too narrowly named.

Related Works
---

- [Orchestra](https://github.com/jeaye/orchestra)'s `defn-spec` was developed independently, but surprisingly yields some very similar ideas with respect to `gradual.spec/defn` (see below) and even a similar interface for specs on function arguments.
- [Spectrum](https://github.com/arohner/spectrum) was also developed independently, and shares `gradual.typed`'s idea of performing spec conformance checks at compile time (though it aims exclusively for "there-exists" checks via generative testing; `gradual.type` also provides as many compile-time "for-all" proofs as it can before falling back to compile-time generative testing and/or runtime spec checks).

# Typed bindings

*(Work in progress — check back later.)*

# Spec bindings (`gradual.spec`)

Rationale and Summary
---

Fundamentally, `gradual.spec/defn` **provides spec information in context rather than drastically increasing cognitive overhead and code duplication via `s/fdef`-style decontextualization.** (There are other features and benefits that will be approached as development progresses.)

`clojure.spec` is a great leap forward for Clojure. It is difficult to overstate the value that it delivers via the expressive and composable data specifications it supports, and the corresponding generative tests they in turn yield out of the box. But, as ever, there is room for improvement. It is a perfectly defensible design decision for the creators of `clojure.spec` to have decoupled specs from the implementation they specify. However, the position `gradual.spec/defn` takes is that there is much greater value in colocating the spec with the spec'ed. This approach yields the following benefits:

- It is much more terse than `fdef` + `defn`
- It is much easier to follow
- It is much easier to debug and maintain
- As such, it is much better about encouraging and facilitating the spec'ing of functions.

At the moment, it has only been tested with Clojure 1.8 and 1.9, but it should be trivial to test with ClojureScript in a coming release, as there is no platform-specific code.

Usage
---

To take a reasonably simple example:

```clojure
(require
  '[[clojure.spec.alpha :as s]
    [gradual.spec       :as gs]])

(gs/defn abc
  [a pos-int?, b (s/and double? #(> % 3))
   | (> b a)
   > (s/and map? #(= (:a %) a))]
  (sorted-map :a a :b (+ b 8)))
```

Deconstructed, the above code defines a function `abc` with only one overload, such that:
- The overload takes two parameters, `a` and `b`
- `a` must satisfy `pos-int?`
- `b` must satisfy `(s/and double? #(> % 3))`
- `(> b a)` must hold true
  - `|` defines an overload's precondition similarly to `:pre` (but implementationally the precondition becomes part of `fdef` and does not use `:pre` in any way)
  - Preconditions are optional
- The return value from the overload must satisfy `(s/and map? #(= (:a %) a))`
  - `>` defines an overload's postcondition similarly to `:post` (but like `|`, implementationally the precondition becomes part of `fdef` and does not use `:post` in any way)
  - Postconditions are optional

The above `gs/defn` code generates the following:

```clojure
(s/fdef abc
  :args (s/or :arity-2
          (s/and
            (s/cat
              :a pos-int?
              :b (s/and double? (fn* [p1__3954#] (> p1__3954# 3))))
            (fn [{a :a, b :b}] (> b a))))
  :fn   (gradual.spec.util/with-gen-spec
          (fn [{ret# :ret}] ret#)
          (fn [{[arity-kind# args#] :args}]
            (case arity-kind#
              :arity-2
                (let [{a :a, b :b} args#]
                  (s/spec (s/and map? #(= (:a %) a))))))))

(defn abc [a b] (sorted-map :a a :b (+ b 8)))
```

where `gs.util/with-gen-spec` is a low-complexity, few-LOC macro that assists in spec auditability and data flow.

Advanced Usage
---

Note that spec'ing destructurings is also possible. Take the more complex example below:

```clojure
(gs/defn abcde "Documentation" {:whatever-metadata "fhjik"}
  ([a number? > number?] (inc a))
  ([a pos-int?, b pos-int?
    | (> a b)
    > (s/and number? #(> % a) #(> % b))] (+ a b))
  ([a #{"a" "b" "c"}
    b boolean?
    {:as   c
     :keys [ca keyword? cb string?]
     {:as cc
      {:as   cca
       :keys [ccaa keyword?]
       [[ccabaa some? {:as ccabab :keys [ccababa some?]} some?] some? ccabb some? & ccabc some? :as ccab]
       [:ccab seq?]}
      [:cca map?]}
     [:cc map?]}
    #(-> % count (= 3))
    [da double? & db seq? :as d] sequential?
    [ea symbol?] ^:gen? (s/coll-of symbol? :kind vector?)
    & [fa #{"a" "b" "c"} :as f] seq?
    | (and (> da 50) (contains? c a)
           a b c ca cb cc cca ccaa ccab ccabaa ccabab ccababa ccabb ccabc d da db ea f fa)
    > number?] 0))
```

which expands to:

```clojure
(s/fdef abcde
  :args
    (s/or
      :arity-1 (s/cat :a number?)
      :arity-2 (s/and (s/cat :a pos-int?
                             :b pos-int?)
                      (fn [{a :a b :b}] (> a b)))
      :arity-varargs
        (s/and
          (s/cat
            :a      #{"a" "b" "c"}
            :b      boolean?
            :c      (gs/map-destructure #(-> % count (= 3))
                      {:ca keyword?
                       :cb string?
                       :cc (gs/map-destructure map?
                             {:cca
                               (gs/map-destructure map?
                                 {:ccaa keyword?
                                  :ccab
                                    (gs/seq-destructure seq?
                                      [:arg-0 (gs/seq-destructure some?
                                                [:ccabaa some?
                                                 :ccabab (gs/map-destructure some? {:ccababa some?})])
                                       :ccabb some?]
                                      [:ccabc some?])})})})
            :d      (gs/seq-destructure sequential? [:da double?] [:db seq?])
            :arg-4# (gs/seq-destructure ^{:gen? true} (s/coll-of symbol? :kind vector?) [:ea symbol?])
            :f      (gs/seq-destructure seq? [:fa #{"a" "b" "c"}]))
          (fn [{a :a
                b :b
                {:as c
                 :keys [ca cb]
                 {:as cc
                  {:as cca
                   :keys [ccaa]
                   [[ccabaa {:as ccabab :keys [ccababa]}] ccabb & ccabc :as ccab] :ccab} :cca} :cc} :c
                [da & db :as d] :d
                [ea] :arg-4#
                [fa :as f] :f :as X}]
            (and (> da 50) (= a fa)
                 a b c ca cb cc cca ccaa ccab ccabaa ccabab ccababa ccabb ccabc d da db ea f fa))))
   :fn
     (gradual.spec.util/with-gen-spec (fn [{ret# :ret}] ret#)
       (fn [{[arity-kind# args#] :args}]
         (case arity-kind#
           :arity-1
             (let [{a :a} args#] (s/spec number?))
           :arity-2
             (let [{a :a b :b} args#] (s/spec (s/and number? #(> % a) #(> % b))))
           :arity-varargs
             (let [{a :a
                    b :b
                    {:as c
                     :keys [ca cb]
                     {:as cc
                      {:as cca
                       :keys [ccaa]
                       [[ccabaa {:as ccabab :keys [ccababa]}] ccabb & ccabc :as ccab] :ccab} :cca} :cc} :c
                    [da & db :as d] :d
                    [ea] :arg-4#
                    [fa :as f] :f} args#] (s/spec number?))))))

(defn abcde
  "Documentation"
  {:whatever-metadata "fhjik"}
  ([a] (inc a))
  ([a b] (+ a b))
  ([a b
    {:as c
     :keys [ca cb]
     {:as cc
      {:as cca
       :keys [ccaa]
       [[ccabaa {:as ccabab :keys [ccababa]}] ccabb :as ccab] :ccab} :cca} :cc}
    [da & db :as d]
    [ea]
    &
    [fa :as f]]
   0))
```

### Map destructuring:
- In order to not have the following:
  `(gs/defn abc [{:keys [a a-conformer, b b-conformer] :as c} c-conformer] ...)`
  where `a` is conformed, `b` is conformed, and `c` is conformed separately such that `(not= a (first c))`:
  - `c` is conformed first, then from it is destructured `a` and `b`
  - `a` and `b` are conformed
  - `a` and `b` are respectively associated into `c` if their conformed values are non-identical
    - For now we will only support destructuring of objects satisfying `core/map?`
  - `c` is intentionally not re-conformed
- All destructuring-keys are considered optional, but each of their values must conform to its spec.
  There is no support for required keys yet; this is currently handled at the map level.
- Destructuring-keys may be any arbitrary object, not just keywords, symbols, or strings.

### Seq destructuring
- In order to not have the following:
  `(gs/defn abc [[a a-conformer, b b-conformer :as c] c-conformer] ...)`
  where `a` is conformed, `b` is conformed, and `c` is conformed separately such that `(not= a (first c))`:
  - `c` is conformed first, then from it is destructured `a` and `b`
  - `a` and `b` are conformed
  - `a` and `b` are respectively `concat`'ed onto the rest of `c` (a later optimization will be that
    this is done only if their conformed values are non-identical). Thus `c` will always end up
    being a seq.
    - We support destructuring of any object satisfying `core/seqable?`
  - `c` is intentionally not re-conformed
- All destructuring-arguments are considered optional, but each argument must conform to its spec.

Copyright and License
---
*Copyright © 2020 Alex Gunnarson*

*Distributed under the Creative Commons Attribution-ShareAlike 3.0 US (CC-SA) license.*

**For normal people who don't speak legalese, this means:**

* You **can** modify the code
* You **can** distribute the code
* You **can** use the code for commercial purposes

But:

* You **have to** give credit / attribute the code to the author (Alex Gunnarson)
* You **have to** state the name of the author (Alex Gunnarson) and the title of this project in the attribution
* You **have to** say in the attribution that you modified the code if you did

Pretty easy, common-sense, decent stuff! Thanks :)

*For more information, see [tldrlegal's summary](https://tldrlegal.com/license/creative-commons-attribution-share-alike-(cc-sa)) of the CC-SA license.*
