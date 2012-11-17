(ns massage.json)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Given to parse-json:
; json-data -> The decoded json object
; template  -> @template object
;
; @template -> { @key @keyspec [@key @keyspec ...] }
; @key      -> :atom
; @keyspec  -> ( @keytype [@option ...] )
; @keytype  -> :string | :number | :list | :object
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
(defn process-type-error [keytype] {:error :wrong_type :should_be keytype})

(defmulti process-option (fn [given-value option] 
    (if (list? option) (first option)
        option)))

(defmethod process-option :string [given-value keytype]
    (cond (string? given-value) given-value
          (number? given-value) (str given-value)
          :else                 (process-type-error keytype)))

(defmethod process-option :maxlength [given-value option]
    (if (> (count given-value) (last option))
        {:error :too_long :min_length (last option)}
        given-value))

(defmethod process-option :minlength [given-value option]
    (if (< (count given-value) (last option))
        {:error :too_short :min_length (last option)}
        given-value))

(defmethod process-option :regex [given-value option]
    (if (nil? (re-find (last option) given-value))
        {:error :bad_string}
        given-value))

(defmethod process-option :!regex [given-value option]
    (if (nil? (re-find (last option) given-value))
        given-value
        {:error :bad_string}))

(defmethod process-option :number [given-value keytype]
    (cond (number? given-value) given-value
          (string? given-value)
            (if (not (re-find #"-?[0-9]+\.?[0-9]*e?[0-9]*" given-value))
                (process-type-error keytype)
                (read-string given-value))
          :else                 (process-type-error keytype)))

(defmethod process-option :max [given-value option]
    (if (> given-value (last option))
        {:error :too_big :max (last option)}
        given-value))

(defmethod process-option :min [given-value option]
    (if (< given-value (last option))
        {:error :too_small :min (last option)}
        given-value))

(defmethod process-option :list   [given-value keytype]
    (if (or (list? given-value) (vector? given-value))
        given-value
        (process-type-error keytype)))

(defmethod process-option :object [given-value keytype]
    (if (map? given-value)
        given-value
        (process-type-error keytype)))

(defmethod process-option :tpl [given-value option]
    (parse-json given-value (last option)))

(defmethod process-option :map-tpl [given-value option]
    (let [mapped (map #(parse-json % (last option)) given-value)]
        (if-let [error (first (filter #(contains? % :error) mapped))]
            error
            mapped)))

;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn check-all-options [given-value template-value]
    (loop [ current-value given-value
            option  (first template-value)
            options (rest template-value) ]
        (if (nil? option) current-value
            (let [result (process-option current-value option)]
                (if (contains? result :error) result
                    (recur result (first options) (rest options)))))))

(defn check-data [tpl-key given-value template-value]
    (let [ keytype (first template-value)
           options (rest  template-value) ]
        (if (nil? given-value)
            (when-not (some #(= :optional %) options)
                {:error :missing_key :key tpl-key})
            (let [result (check-all-options given-value template-value)]
                (if (and (contains? result :error) (not (contains? result :key)))
                    (assoc result :key tpl-key) result)))))

(defmulti parse-json (fn [json-data template]
    (if (map? template) :map
                        :list)))

(defmethod parse-json :list [json-data options]
    (check-all-options json-data options))

(defmethod parse-json :map [json-data template]
    (loop [ tpl-seq template
            ret-map {} ]
        (let [ tpl-seq-tail (rest  tpl-seq)
               tpl          (first tpl-seq)
               tpl-key      (key tpl)
               tpl-val      (val tpl)
               result       (check-data tpl-key (json-data tpl-key) tpl-val) ]
            (if (contains? result :error) result
                (let [filled-ret-map (assoc ret-map tpl-key result) ]
                (if (empty? tpl-seq-tail) filled-ret-map
                    (recur tpl-seq-tail filled-ret-map)))))))
