(ns massage.test.json
    (:use clojure.test
          massage.json))

(defmacro try-parse [json tpl res]
    `(is (= (parse-json ~json ~tpl) ~res)))

(deftest numbers
    (try-parse  1  '(:number) 1)
    (try-parse "1" '(:number) 1)
    (try-parse "b" '(:number)
        {:error :wrong_type :should_be :number})
    (try-parse  1  '(:number (:max 3) (:min 1)) 1)
    (try-parse  0  '(:number (:max 3) (:min 1))
        {:error :too_small :min 1})
    (try-parse  4  '(:number (:max 3) (:min 1))
        {:error :too_big :max 3})
)

(deftest strings
    (try-parse "hi"   '(:string) "hi")
    (try-parse 1      '(:string) "1")
    (try-parse :blah  '(:string)
        {:error :wrong_type :should_be :string})
    (try-parse "hi"   '(:string (:maxlength 3) (:minlength 2)) "hi")
    (try-parse "h"    '(:string (:maxlength 3) (:minlength 2))
        {:error :too_short :min_length 2})
    (try-parse "hill" '(:string (:maxlength 3) (:minlength 2))
        {:error :too_long :max_length 3})
    (try-parse "aa00" '(:string (:regex #"[0-9]")) "aa00")
    (try-parse "aaaa" '(:string (:regex #"[0-9]"))
        {:error :bad_string})
    (try-parse "aa00" '(:string (:regex #"[a-z]")) "aa00")
    (try-parse "0000" '(:string (:regex #"[a-z]"))
        {:error :bad_string})
)

(deftest bools
    (try-parse  true   '(:bool) true)
    (try-parse "true"  '(:bool) true)
    (try-parse  false  '(:bool) false)
    (try-parse "false" '(:bool) false)
    (try-parse "blah"  '(:bool)
        {:error :wrong_type :should_be :bool})
)

(deftest lists
    (try-parse [1 2]       '(:list) [1 2])
    (try-parse :blah       '(:list)
        {:error :wrong_type :should_be :list})
    (try-parse [1 2]       '(:list (:minlength 2) (:maxlength 3)) [1 2])
    (try-parse [1]         '(:list (:minlength 2) (:maxlength 3))
        {:error :too_short :min_length 2})
    (try-parse [1 2 3 4]   '(:list (:minlength 2) (:maxlength 3))
        {:error :too_long :max_length 3})
    (try-parse [1 2 "3" 4] '(:list (:map-tpl (:number))) [1 2 3 4])
    (try-parse [1 2 "b" 4] '(:list (:map-tpl (:number)))
        {:error :wrong_type :should_be :number})
    (try-parse [{:a 1} {:a 2} {:a "3"}] '(:list (:map-tpl {:a (:number)}))
        [{:a 1} {:a 2} {:a 3}])
    (try-parse [{:a 1} {:a 2} {:a "a"}] '(:list (:map-tpl {:a (:number)}))
        {:error :wrong_type :should_be :number :key :a})
)

(deftest objects
    (try-parse {:a "a" :b "1"} { :a '(:string)
                                 :b '(:number)
                                 :c '(:string) }
        {:error :missing_key :key :c})

    (try-parse {:a "a" :b "1"} { :a '(:string)
                                 :b '(:number)
                                 :c '(:string :optional) }
        {:a "a" :b 1 :c nil})

    (try-parse {:a "a" :b "1"} { :a '(:string)
                                 :b '(:number)
                                 :c '(:string (:optional :default)) }
        {:a "a" :b 1 :c :default})

    (try-parse {:embedded {:a "a" :b "1"}}
               {:embedded '(:object (:tpl
                               { :a (:string)
                                 :b (:number)
                                 :c (:string (:optional :default)) }))}
        {:embedded {:a "a" :b 1 :c :default}})
)
