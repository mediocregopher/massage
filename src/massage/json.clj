(ns massage.json)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Given to parse-json:
; json-data -> The decoded json object
; template  -> @template object
;
; @template -> { @key @keyspec [@key @keyspec ...] }
; @key      -> :atom | :massage/*
; @keyspec  -> ( @keytype [@option ...] )
; @keytype  -> :bool | :string | :number | :list | :object
;
; @options (any key type) -> :optional
;
; @options (:string)      -> ( :maxlength @int )
;                            ( :minlength @int )
;                            ( :regex     @regex )
;                            ( :!regex    @regex )
;        
; @options (:int)         -> ( :max       @int )
;                            ( :min       @int )
;
; @options (:list)        -> ( :maxlength @int )
;                            ( :minlength @int )
;                            ( :map-tpl   @template )
;
; @options (:object)      -> ( :tpl   @template )
;
;Returns: { :error @atom ... } | @map | @list | 
;                                @int | @string
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declare parse-json)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Methods which process each given option or type
; constraint, ensuring the value fits the constraint
; and possibly casting when necessary
;
(defn process-type-error 
    "Easily return an error for when given-value can't be casted to keytype" 
    [keytype] {:error :wrong_type :should_be keytype})

(defmulti process-option 
    "Given a value and an option, attempt to match the value to the option.
    If successful returns the (potentially massaged) value, otherwise returns
    a map with the :error key set"
    (fn [given-value option] 
        (if (list? option) (first option)
            option)))

(defmethod process-option :optional [given-value option]
    given-value)

(defmethod process-option :bool [given-value keytype]
    (case given-value
         true   true
        "true"  true
        :true   true
         false  false
        "false" false
        :false  false
        (process-type-error keytype)))

(defmethod process-option :string [given-value keytype]
    "Tests for a string, possibly casts a number to a string"
    (cond (string? given-value) given-value
          (number? given-value) (str given-value)
          :else                 (process-type-error keytype)))

(defmethod process-option :maxlength [given-value option]
    "Makes sure a string or list isn't larger than a certain number"
    (if (> (count given-value) (last option))
        {:error :too_long :max_length (last option)}
        given-value))

(defmethod process-option :minlength [given-value option]
    "Makes sure a string or list isn't smaller than a certain number"
    (if (< (count given-value) (last option))
        {:error :too_short :min_length (last option)}
        given-value))

(defmethod process-option :regex [given-value option]
    "Makes sure a string matches a given regex"
    (if (nil? (re-find (last option) given-value))
        {:error :bad_string}
        given-value))

(defmethod process-option :!regex [given-value option]
    "Makes sure a string does not match a given regex"
    (if (nil? (re-find (last option) given-value))
        given-value
        {:error :bad_string}))

(defmethod process-option :number [given-value keytype]
    "Tests for a number, possibly casting a string to a number"
    (cond (number? given-value) given-value
          (string? given-value)
            (if (not (re-find #"^-?[0-9]+\.?[0-9]*e?[0-9]*$" given-value))
                (process-type-error keytype)
                (read-string given-value))
          :else                 (process-type-error keytype)))

(defmethod process-option :max [given-value option]
    "Makes sure a number isn't greater than a certain value"
    (if (> given-value (last option))
        {:error :too_big :max (last option)}
        given-value))

(defmethod process-option :min [given-value option]
    "Makes sure a number isn't less than a certain value"
    (if (< given-value (last option))
        {:error :too_small :min (last option)}
        given-value))

(defmethod process-option :list   [given-value keytype]
    "Tests for a list"
    (if (or (list? given-value) (vector? given-value))
        given-value
        (process-type-error keytype)))

(defmethod process-option :object [given-value keytype]
    "Tests for an object (represented by a map)"
    (if (map? given-value)
        given-value
        (process-type-error keytype)))

(defmethod process-option :tpl [given-value option]
    "Recursively call parse-json using the given-value and template given in
    the option"
    (parse-json given-value (last option)))

(defmethod process-option :map-tpl [given-value option]
    "Call parse-json on each item in the list with the given template. If any
    of these calls return an error this will forward that error directly, not
    in the list"
    (let [mapped (map #(parse-json % (last option)) given-value)]
        (if-let [error (first (filter #(contains? % :error) mapped))]
            error
            mapped)))

;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn check-all-options [given-value template-value]
    "For each option in the template call process-option. If this returns
    an error short-circuit and return that error. Otherwise continue on
    and pass the return value to the next iteration of the loop. Since we
    always start a template with a type, the casted value of that type
    automatically gets sent into the process-option for the next option
    in the options list"
    (loop [ current-value given-value
            option  (first template-value)
            options (rest template-value) ]
        (if (nil? option) current-value
            (let [result (process-option current-value option)]
                (if (contains? result :error) result
                    (recur result (first options) (rest options)))))))

(defn is-optional 
    "Given a list of options, returns the default value specified by the :optional
    option, or :massage/not-optional if the parameter isn't optional"
    [options]
    (loop [ opt      (first options)
            opt-tail (rest  options) ]
        (if (nil? opt) :massage/not-optional
            (cond (= :optional opt) nil
                  (and (seq? opt) (= :optional (first opt))) (second opt)
                  :else (recur (first opt-tail) (rest opt-tail))))))

(defn check-data 
    "Given a key, the value for that key given in by the json (given-value) and
    the template, check given-value with check-all-options. If the return is an
    error return that error with :key set. Otherwise simply return the result.
    If given-value is nil we assume it is not set in the json, which is only
    allowed if the :optional option is given for this key in the template"
    [data-key given-value template-value]
    (let [ keytype (first template-value)
           options (rest  template-value) ]
        (if (nil? given-value)
            (let [result (is-optional options)]
                (if (= result :massage/not-optional) 
                    {:error :missing_key :key data-key}
                    result))
            (let [result (check-all-options given-value template-value)]
                (if (and (contains? result :error) (not (contains? result :key)))
                    (assoc result :key data-key) result)))))

(defn fill-template [json-data template]
    (if-let [template-default (template :massage/*)]
        (loop [json-head (first json-data)
               json-tail (rest json-data)
               tpl       template]
            (if (nil? json-head) (dissoc tpl :massage/*)
                (let [json-key (key json-head)
                      json-val (val json-head)]
                    (if (tpl json-key)
                        (recur (first json-tail) (rest json-tail) tpl)
                        (recur (first json-tail) (rest json-tail) (assoc tpl json-key template-default))))))
        template))

(defmulti parse-json
    "Main entry point of this module. Given json-data and a template, returns
    either a representation of the json data massaged to fit the template, or
    a map with an :error key set if the data could not be massaged. See the 
    README for more details on how to use templates"
    (fn [json-data template]
        (if (map? template) :map
                            :seq)))

(defmethod parse-json :seq [json-data options]
    "If our second arg is a seq then we know it's an options and can be passed
    straight through to check-all-options"
    (check-all-options json-data options))

(defmethod parse-json :map [json-data template]
    "If the second arg is a map then we go through each key and its value and send
    them to check-data. If it returns an error we immediately return that error,
    otherwise continue on to the next key/value in the template map"
    (if (map? json-data)
        (loop [ tpl-seq (fill-template json-data template)
                ret-map {} ]
            (let [ tpl-seq-tail (rest  tpl-seq)
                   tpl          (first tpl-seq)
                   tpl-key      (key tpl)
                   tpl-val      (val tpl)
                   result       (check-data tpl-key (json-data tpl-key) tpl-val) ]
                (if (contains? result :error) result
                    (let [filled-ret-map (assoc ret-map tpl-key result) ]
                    (if (empty? tpl-seq-tail) filled-ret-map
                        (recur tpl-seq-tail filled-ret-map))))))
        (process-type-error :object)))
