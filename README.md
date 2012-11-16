# massage

Clojure library for massaging a data structure into a specified schema

## Usage

## JSON

Currently there is only support for massaging data returned by a json parser library like (Cheshire)[https://github.com/dakrone/cheshire], but I've left room
for types returned by other protocols and libraries as well.

### Examples

```clojure
(use 'massage.json)

;Parsing a number
=> (parse-json 1 '(:number))
1 

;We can also easily cast from string
=> (parse-json "1" '(:number))
1

;If it can't be cast massage returns an error map
=> (parse-json "what" '(:number))
{:error :wrong_type, :should_be :number}

;We can specify other constraints for numbers as well
=> (parse-json 1 '(:number (:max 10) (:min 1)))
1

=> (parse-json 0 '(:number (:max 10) (:min 1)))
{:error :too_small, :min 1}

;Casting and constraints for strings:
=> (parse-json "string" '(:string))
"string"

=> (parse-json 1 '(:string))
1

=> (parse-json [:not :a :string] '(:string))
{:error :wronge_type, :should_be :string}

=> (parse-json "some-string" '(:string (:maxlength 20)
                                       (:minlength 5)
                                       (:regex  #"-")       ;Ensure the string matches this regex
                                       (:!regex #"[0-9]")   ;Ensure the string doesn't match this one
                                       ))
"some-string"

;Templating objects
=> (parse-json {:key1 "string"} {:key1 '(:string) 
                                 :key2 '(:string :optional (:maxlength 10))})
{:key1 "string", :key2 nil}

;Recursively templating objects
=> (parse-json {:key1 {:a 1 :b "2"}} {:key1 '(:object (:tpl { :a (:number) 
                                                              :b (:number) 
                                                              :c (:number :optional) }))})
{:key1 {:c nil, :b 2, :a 1}}

;Template errors include key error occured on
=> (parse-json {:key1 "string"} {:key1 '(:number)})
{:key :key1, :error :wrong_type, :should_be :number}

;Templating and constraining lists/vectors
=> (parse-json [1 2 3 4] '(:list (:minlength 3) (:maxlength 10)))
[1 2 3 4]

=> (parse-json [1 2 "3" "4"] '(:list (:map-tpl '(:number))))
(1 2 3 4)

=> (parse-json [{:a 1} {:a 2} {:a "3"}] '(:list (:map-tpl {:a (:number)})))
({:a 1} {:a 2} {:a 3})
```

## License

Copyright © 2012 Brian Picciano

Distributed under the Eclipse Public License, the same as Clojure.
