(ns massage.test.json
    (:use clojure.test
          massage.json))

(defn try-parse [json tpl res]
    (= (parse-json json tpl) res))

(deftest numbers
    (is (try-parse  1  '(:number) 1))
    (is (try-parse "1" '(:number) 1))
)
