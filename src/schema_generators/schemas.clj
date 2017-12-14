(ns schema-generators.schemas
  (:require [schema.core :as s]))

(def Schema
  "A Schema for Schemas"
  (s/protocol s/Schema))

(def Generator
  "A test.check generator"
  s/Any)

(def LeafGenerators
  "A mapping from schemas to generating functions that should be used."
  (s/=> (s/maybe Generator) Schema))

(def LeafDepths
  "A mapping from schemas to depths."
  (s/=> (s/maybe s/Int) Schema))

(def GeneratorWrappers
  "A mapping from schemas to wrappers that should be used around the default
   generators."
  (s/=> (s/maybe (s/=> Generator Generator))
        Schema))
