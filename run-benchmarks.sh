#!/bin/sh

bench () {
  NS=$1
  echo "$NS START " `date`
  sed --in-place=.orig "s/(:use \[active.clojure.logger.metric-accumulator\])/(:use [$NS])/" bench/bench.clj
  lein bench
  mv bench/bench.clj.orig bench/bench.clj
  echo "$NS DONE " `date`
}

bench active.clojure.logger.metric-accumulator

bench active.clojure.logger.benchmark.clojure-map-record

bench active.clojure.logger.benchmark.java-map-record

bench active.clojure.logger.benchmark.clojure-map-string

bench active.clojure.logger.benchmark.java-map-string

bench active.clojure.logger.benchmark.clojure-map-vector

bench active.clojure.logger.benchmark.java-map-vector
