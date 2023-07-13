#!/bin/sh

echo "Bench Start------------------------------------------"
date

cp bench-variations/bench-clojure-map-record.clj test/active/clojure/logger/benchmark/bench.clj
lein test :only active.clojure.logger.benchmark.bench

date
cp bench-variations/bench-java-map-record.clj test/active/clojure/logger/benchmark/bench.clj
lein test :only active.clojure.logger.benchmark.bench

date
cp bench-variations/bench-clojure-map-string.clj test/active/clojure/logger/benchmark/bench.clj
lein test :only active.clojure.logger.benchmark.bench

date
cp bench-variations/bench-java-map-string.clj test/active/clojure/logger/benchmark/bench.clj
lein test :only active.clojure.logger.benchmark.bench

date
cp bench-variations/bench-clojure-map-vector.clj test/active/clojure/logger/benchmark/bench.clj
lein test :only active.clojure.logger.benchmark.bench

date
cp bench-variations/bench-java-map-vector.clj test/active/clojure/logger/benchmark/bench.clj
lein test :only active.clojure.logger.benchmark.bench

date
rm test/active/clojure/logger/benchmark/bench.clj

echo "Bench Finished---------------------------------------"
date

# systemctl poweroff
