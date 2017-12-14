(ns schema-generators.scaled-generators
  "Experimental support for compiling schemas to test.check generators."
  (:require
   [clojure.test.check.generators :as generators]
   [schema.spec.core :as spec :include-macros true]
   [schema-generators.schemas :refer :all]
   [schema-generators.schema-depth :as schema-depth]
   schema.spec.collection
   schema.spec.leaf
   schema.spec.variant
   [schema.core :as s :include-macros true]
   #?(:clj [schema.macros :as macros]))
  #?(:cljs (:require-macros [schema.macros :as macros] schema.core)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Private helpers for composite schemas

(declare scale-deep-collection-generator)

(defn g-by [f & args]
  (generators/fmap
   (partial apply f)
   (apply generators/tuple args)))

(defn g-apply-by [f args]
  (generators/fmap f (apply generators/tuple args)))

(defn- sub-generator
  [{:keys [schema]}
   {:keys [subschema-generator #?@(:clj [^java.util.Map cache] :cljs [cache])] :as params}]
  (spec/with-cache cache schema
    (fn [d] (#'generators/make-gen (fn [r s] (generators/call-gen @d r (quot s 2)))))
    (fn [] (subschema-generator schema params))))


;; Helpers for collections

(declare elements-generator)

(defn basic-schema-spec-element? [schema-spec-element]
  "A basic schema spec element is a map with the keys :schema and :parser.
   The value behind :schema should be a valid schema."
  (and (map? schema-spec-element)
       (contains? schema-spec-element :schema)
       (contains? schema-spec-element :parser)))

(def schema-spec-element-tags
  #{::schema.spec.collection/optional
    ::schema.spec.collection/remaining})

(defn tag [schema-spec-element]
  (first schema-spec-element))

(defn basic-schema-spec
  [schema-spec-element]
  (second schema-spec-element))

(defn common-tagged-schema-spec-element?
  [schema-spec-element]
  (and (vector? schema-spec-element)
       (<= 2
           (count schema-spec-element)
           3)
       (contains? schema-spec-element-tags
                  (tag schema-spec-element))
       (basic-schema-spec-element? (basic-schema-spec schema-spec-element))))

(defn simple-tagged-schema-spec-element?
  [schema-spec-element]
  (and (common-tagged-schema-spec-element? schema-spec-element)
       (= 2 (count schema-spec-element))))

(defn nested-tagged-schema-spec-element?
  [schema-spec-element]
  (and (common-tagged-schema-spec-element? schema-spec-element)
       (= 3 (count schema-spec-element))
       (= ::schema.spec.collection/optional
          (tag schema-spec-element))))

(defn tagged-schema-spec-element? [schema-spec-element]
  "A valid tagged schema spec element is a two or three element vector, where
   the first element is an element type tag (options described above) and the
   second element is a basic-schema-spec-element and the last optional element
   is another schema-spec-element"
  (if-not (vector? schema-spec-element)
    false
    (let [len (count schema-spec-element)]
      (if (= 2 len)
        (simple-tagged-schema-spec-element? schema-spec-element)
        (nested-tagged-schema-spec-element? schema-spec-element)))))

(defn schema-spec-element->basic-spec [schema-spec-element]
  (cond (tagged-schema-spec-element? schema-spec-element)
        (basic-schema-spec schema-spec-element)

        (basic-schema-spec-element? schema-spec-element)
        schema-spec-element

        :else
        nil))

(defn schema-spec-element->schema [schema-spec-element]
  (-> schema-spec-element
      schema-spec-element->basic-spec
      :schema))

(defn element-generator [e params]
  (println "e:")
  (clojure.pprint/pprint e)
  (println)
  (if (vector? e)
    (case (first e)
      ::schema.spec.collection/optional
      (generators/one-of
       [(generators/return nil)
        (elements-generator (next e) params)])

      ::schema.spec.collection/remaining
      (do (macros/assert! (= 2 (count e)) "remaining can have only one schema.")
          ;; if the schema for remaining collection elements has depth of at least one,
          ;; then a collection of them has depth of at least two and needs to be scaled
          (let [unscaled (generators/vector (sub-generator (second e) params))
                schema (schema-spec-element->schema e)]
            (if (<= 1 (schema-depth/depth schema))
              (do
                (println "scaling schema: " (pr-str schema))
                (scale-deep-collection-generator unscaled))
              unscaled))

          ))
    (generators/fmap vector (sub-generator e params))))

(defn elements-generator [elts params]
  (->> elts
       (map #(element-generator % params))
       (apply generators/tuple)
       (generators/fmap (partial apply concat))))


(defprotocol CompositeGenerator
  (composite-generator [s params]))

(extend-protocol CompositeGenerator
  schema.spec.variant.VariantSpec
  (composite-generator [s params]
    (generators/such-that
     (fn [x]
       (let [pre (.-pre ^schema.spec.variant.VariantSpec s)
             post (.-post ^schema.spec.variant.VariantSpec s)]
         (not
          (or (pre x)
              (and post (post x))))))
     (generators/one-of
      (for [o (macros/safe-get s :options)]
        (if-let [g (:guard o)]
          (generators/such-that g (sub-generator o params))
          (sub-generator o params))))))

  ;; TODO: this does not currently capture proper semantics of maps with
  ;; both specific keys and key schemas that can override them.
  schema.spec.collection.CollectionSpec
  (composite-generator [s params]
    (generators/such-that
     (complement (.-pre ^schema.spec.collection.CollectionSpec s))
     (generators/fmap (:constructor s) (elements-generator (:elements s) params))))

  schema.spec.leaf.LeafSpec
  (composite-generator [s params]
    (macros/assert! false "You must provide a leaf generator for %s" s)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Public


(def +primitive-generators+
  #?(:clj {Double generators/double
           ;; using unchecked-float here will unfortunately generate a lot of
           ;; infinities, since lots of doubles are out of the float range
           Float (generators/fmap unchecked-float generators/double)
           Long generators/large-integer
           Integer (generators/fmap unchecked-int generators/large-integer)
           Short (generators/fmap unchecked-short generators/large-integer)
           Character (generators/fmap unchecked-char generators/large-integer)
           Byte (generators/fmap unchecked-byte generators/large-integer)
           Boolean generators/boolean}
     :cljs {js/Number (generators/one-of [generators/large-integer generators/double])
            js/Boolean generators/boolean}))

(def +simple-leaf-generators+
  (merge
   +primitive-generators+
   {s/Str generators/string-ascii
    s/Bool generators/boolean
    s/Num (generators/one-of [generators/large-integer generators/double])
    s/Int (generators/one-of
           [generators/large-integer
            #?@(:clj [(generators/fmap unchecked-int generators/large-integer)
                      (generators/fmap bigint generators/large-integer)])])
    s/Keyword generators/keyword
    #?(:clj clojure.lang.Keyword
       :cljs cljs.core/Keyword) generators/keyword
    s/Symbol (generators/fmap (comp symbol name) generators/keyword)
    #?(:clj Object :cljs js/Object) generators/any
    s/Any generators/any
    s/Uuid generators/uuid
    s/Inst (generators/fmap (fn [ms] (#?(:clj java.util.Date. :cljs js/Date.) ms)) generators/int)}
   #?(:clj (into {}
                 (for [[f ctor c] [[doubles double-array Double]
                                   [floats float-array Float]
                                   [longs long-array Long]
                                   [ints int-array Integer]
                                   [shorts short-array Short]
                                   [chars char-array Character]
                                   [bytes byte-array Byte]
                                   [booleans boolean-array Boolean]]]
                   [f (generators/fmap ctor
                                       (generators/vector
                                        (macros/safe-get
                                         +primitive-generators+ c)))])))))


(defn eq-generators [s]
  (when (instance? schema.core.EqSchema s)
    (generators/return (.-v ^schema.core.EqSchema s))))

(defn enum-generators [s]
  (when (instance? schema.core.EnumSchema s)
    (let [vs (vec (.-vs ^schema.core.EqSchema s))]
      (generators/fmap #(nth vs %) (generators/choose 0 (dec (count vs)))))))


(defn default-leaf-generators
  [leaf-generators]
  (some-fn
   leaf-generators
   +simple-leaf-generators+
   eq-generators
   enum-generators))

(defn always [x] (generators/return x))

(defn such-that
  "Helper wrapper that filters to values that match predicate."
  [f]
  (partial generators/such-that f))

(defn fmap
  "Helper wrapper that maps f over all values."
  [f]
  (partial generators/fmap f))

(defn merged
  "Helper wrapper that merges some keys into a schema"
  [m]
  (fmap #(merge % m)))

(defn collection-schema? [schema]
  "returns true if parameter schema describes a collection with arbitrary
   amount of elements, like [Str] or
   {:required-key Str, (optional-key :b) Int, Keyword Int}"
  (let [spec (s/spec schema)]
    (if (instance? schema.spec.collection.CollectionSpec spec)
      (not (empty? (:remaining (schema-depth/spec->element-types spec))))
      false)))

(defn deep-collection-schema? [schema]
  "Deep collections, like nested vectors, need scaling"
  (and (collection-schema? schema)
       (< 1 (schema-depth/depth schema))))

(defn scaling-function [n]
  (-> n
      java.lang.Math/sqrt
      java.lang.Math/ceil
      int
      (* 2)))

(defn scale-deep-collection-generator [generator]
  (generators/scale scaling-function generator))

(defn scaled-composite-generator [schema params]
  (composite-generator (s/spec schema) params))

(s/defn generator :- Generator
  "Produce a test.check generator for schema.

   leaf-generators must return generators for all leaf schemas, and can also return
   generators for non-leaf schemas to override default generation logic.

   constraints is an optional mapping from schema to wrappers for the default generators,
   which can impose constraints, fix certain values, etc."
  ([schema]
     (generator schema {}))
  ([schema leaf-generators]
     (generator schema leaf-generators {}))
  ([schema :- Schema
    leaf-generators :- LeafGenerators
    wrappers :- GeneratorWrappers]
     (let [leaf-generators (default-leaf-generators leaf-generators)
           gen (fn [s params]
                 ((or (wrappers s) identity)
                  (or (leaf-generators s)
                      (scaled-composite-generator s params)
                      (composite-generator (s/spec s) params))))]
       (generators/fmap
        (s/validator schema)
        (gen schema {:subschema-generator gen
                     :cache #?(:clj (java.util.IdentityHashMap.)
                               :cljs (atom {}))
                     :scaled false})))))

(s/defn generator-2
  [params :- {:schema Schema
              :leaf-generators LeafGenerators
              :leaf-depths LeafDepths
              :wrappers GeneratorWrappers}])

(s/defn sample :- [s/Any]
  "Sample k elements from generator."
  [k & generator-args]
  (generators/sample (apply generator generator-args) k))

(s/defn generate :- s/Any
  "Sample a single element of low to moderate size."
  [& generator-args]
  (generators/generate (apply generator generator-args) 10))
