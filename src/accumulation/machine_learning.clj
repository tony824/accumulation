(ns accumulation.machine-learning
  (:require [clojure-tensorflow.ops :as tf]
            [clojure-tensorflow.layers :as layer]
            [clojure-tensorflow.optimizers :as optimize]
            [clojure-tensorflow.core :refer [run with-graph with-session]]))

;; Training data
(def input (tf/constant [[0. 1.] [0. 0.] [1. 1.] [1. 0.]]))
(def target (tf/constant [[0.] [0.] [1.] [1.]]))

;; Define network / model
(def network
  ;; first layer is just the training input data
  (-> input
      ;; next is a hidden layer of six neurons
      ;; we can set the activation function like so
      (layer/linear 6 :activation tf/sigmoid)
      ;; next is a hidden layer of eight neurons
      ;; tf/sigmoid is used by default when we dont
      ;; specify an activation fn
      (layer/linear 8)
      ;; our last layer needs to be the same size as
      ;; our training target data, so one neuron.
      (layer/linear 1)))


;; Cost function; we're using the squared difference
(def error (tf/square (tf/sub target network)))

;; Initialize global variables
(run (tf/global-variables-initializer))

;; Train Network 1000 epochs
(run (repeat 1000 (optimize/gradient-descent error)))

;; Initialize global variables
(run (tf/mean error))
;; => [9.304908E-5]
;; the error is now incredibly small

