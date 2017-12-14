(ns schema-generators.schema-depth
  (:require [schema.core :as s]
            [clojure.test.check.generators :as generators]))

(defprotocol SchemaDepth
  (schema-depth
    #_"When generating values with size parameter n,the results size for given
    `schema` is around O(n^d) where d is given by

       (depth schema) => d.

     That is, to get something of size around n, the generator needs to be
     scaled with the d'th root function"
    [schema leaf-depths]))

(declare depth)
(declare collection-spec-schema-depth)

(defn max-depth [coll leaf-depths]
  (let [depths (map #(depth % leaf-depths) coll)]
    (if (empty? coll)
      0
      (apply max depths))))

(extend-protocol SchemaDepth
  ;; Simple schemas
  schema.core.AnythingSchema
  (schema-depth [_ _] 1)

  schema.core.EqSchema
  (schema-depth [_ _] 0)

  schema.core.EnumSchema
  (schema-depth [_ _] 0)

  ;; Simple parameterized schemas
  schema.core.Maybe
  (schema-depth [maybe-schema leaf-depths]
    (depth (.-schema maybe-schema) leaf-depths))

  schema.core.NamedSchema
  (schema-depth [named-schema leaf-depths]
    (depth (.-schema named-schema) leaf-depths))

  schema.core.Atomic
  (schema-depth [atomic-schema leaf-depths]
    (depth (.-schema atomic-schema) leaf-depths))

  ;; Schemas with variable amount of parameter schemas
  schema.core.Either
  (schema-depth [either-schema leaf-depths]
    (max-depth (.-schemas either-schema) leaf-depths))

  schema.core.ConditionalSchema
  (schema-depth [conditional-schema leaf-depths]
    (let [preds-and-schemas (.-preds-and-schemas conditional-schema)
          schemas (map second preds-and-schemas)]
      (max-depth schemas leaf-depths)))

  schema.core.CondPre
  (schema-depth [cond-pre-schema leaf-depths]
    (max-depth (.-schemas cond-pre-schema) leaf-depths))

  schema.core.Both
  (schema-depth [both-schema leaf-depths]
    (max-depth (.-schemas both-schema) leaf-depths))

  ;; smaller pieces for collections
  schema.core.RequiredKey
  (schema-depth [_ _] 0)

  ;;keywords are used as required keys in map-schemas
  #?(:clj clojure.lang.Keyword
     :cljs Keyword)
  (schema-depth [_ _] 0)

  schema.core.OptionalKey
  (schema-depth [_ _] 0)

  schema.core.MapEntry
  (schema-depth [map-entry-schema leaf-depths]
    (max (depth (.-key-schema map-entry-schema) leaf-depths)
         (depth (.-val-schema map-entry-schema) leaf-depths)))

  schema.core.One
  (schema-depth [one-schema leaf-depths]
    (depth (.-schema one-schema) leaf-depths))

  ;;TODO: Queue

  clojure.lang.APersistentMap
  (schema-depth [map-schema leaf-depths]
    (collection-spec-schema-depth (s/spec map-schema) leaf-depths))

  clojure.lang.IPersistentMap
  (schema-depth [map-schema leaf-depths]
    (collection-spec-schema-depth (s/spec map-schema) leaf-depths))

  clojure.lang.APersistentVector
  (schema-depth [vec-schema leaf-depths]
    (collection-spec-schema-depth (s/spec vec-schema) leaf-depths))

  clojure.lang.IPersistentVector
  (schema-depth [vec-schema leaf-depths]
    (collection-spec-schema-depth (s/spec vec-schema) leaf-depths))

  clojure.lang.APersistentSet
  (schema-depth [set-schema leaf-depths]
    (collection-spec-schema-depth (s/spec set-schema) leaf-depths))

  )

(defn optional? [spec-element]
  "optional collection element schemas are represented with tagged vectors
   [:schema.spec.collection/optional
    {:schema ...schema...
     :parsers ...parser...}
    nested-optional-or-remaining]

    where nested-optional-or-remaining is not mandatory, but if it exists,
    it is either a vector matching optional? or vector matching remaining?"
  (and (vector? spec-element)
       (= :schema.spec.collection/optional
          (first spec-element))))

(defn remaining? [spec-element]
  "remaining schema is represented in spec as a vector
   [:schema.spec.collection/remaining
    {:schema ...schema...
     :parser ...parser...}]"
  (and (vector? spec-element)
       (= :schema.spec.collection/remaining
          (first spec-element))))

(defn flatten-optionals [[tag schema-map rst]]
  (if (empty? rst)
    [[tag schema-map]]
    (cons [tag schema-map]
          (flatten-optionals rst))))

(defn spec->element-types [spec]
  (let [elems (:elements spec)
        required (filter map? elems)
        optionals (filter optional? elems)
        optionals (mapcat flatten-optionals optionals)
        remaining (concat (filter remaining? elems)
                          (filter remaining? optionals))
        optionals (filter optional? optionals)]
    (into {:required required
           :optionals optionals}
          (if (empty? remaining)
            []
            {:remaining (first remaining)}))))

(defn spec-elem->schema [spec-elem]
  (if (map? spec-elem)
    (:schema spec-elem)
    (:schema (second spec-elem))))

(defn collection-schema? [schema]
  "returns true if parameter schema describes a collection with arbitrary
   amount of elements, like [Str] or
   {:required-key Str, (optional-key :b) Int, Keyword Int}"
  (let [spec (s/spec schema)]
    (if (instance? schema.spec.collection.CollectionSpec spec)
      (not (empty? (:remaining (spec->element-types spec))))
      false)))

(defn deep-collection-schema? [schema]
  "Deep collections, like nested vectors, need scaling"
  (and (collection-schema? schema)
       (< 1 (depth schema))))

(defn scaling-function [n]
  (-> n
      java.lang.Math/sqrt
      java.lang.Math/ceil
      int
      (* 2)))

(defn scale-deep-collection-generator [generator]
  (generators/scale scaling-function generator))

#_(defn scaled-composite-generator [schema params]
  (if (deep-collection-schema? schema)
    (scale-deep-collection-generator
      (composite-generator (s/spec schema) params))))

(defn collection-spec-schema-depth [spec leaf-depths]
  (let [elements (spec->element-types spec)
        {:keys [required optionals remaining]} elements
        required-schemas (map spec-elem->schema required)
        optional-schemas (map spec-elem->schema optionals)
        remaining-depth (if remaining
                          (inc (depth (spec-elem->schema remaining) leaf-depths))
                          0)]
    (max (max-depth required-schemas leaf-depths)
         (max-depth optional-schemas leaf-depths)
         remaining-depth)))

(def +primitive-depths+
  (let [number-types #?(:clj  [Double Float Long Integer
                               Short Character Byte Boolean]
                        :cljs [js/Number js/Boolean])
        entries (for [type number-types]
                  [type 0])]
    (into {} entries)))

(def +simple-leaf-depths+
  (merge
    +primitive-depths+
    {s/Str 1
     s/Bool 0
     s/Num 0
     s/Int 0
     s/Keyword 1
     #?(:clj  clojure.lang.Keyword
        :cljs cljs.core/Keyword) 1
     s/Symbol 1
     s/Any 1
     #?(:clj Object
        :cljs js/Object) 1
     s/Uuid 0
     s/Inst 0}
     ;; primitive arrays have length relative to generator size, giving us
     ;; depth of 1
    #?(:clj (into {}
                  (for [schema [doubles floats longs ints
                                shorts chars bytes booleans]]
                    [schema 1])))
     ))

(defn eq-depths [s]
  "eq-schemas define constant values, which have constant size and depth 0"
  (when (instance? schema.core.EqSchema s)
    0))

(defn enum-depths [s]
  "enums are always constant size, and therefore have depth 0"
  (when (instance? schema.core.EnumSchema s)
    0))

(defn default-leaf-depths [leaf-depths]
  (some-fn leaf-depths
           +simple-leaf-depths+
           eq-depths
           enum-depths))

(defn depth
  ([schema]
   (depth schema {}))
  ([schema leaf-depths]
   (let [leaf-depths (default-leaf-depths leaf-depths)]
     ;;TODO: this keeps rewrapping the default leafs again and again
     (or (leaf-depths schema)
         (schema-depth schema leaf-depths)))))
