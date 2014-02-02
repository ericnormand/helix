(ns tdop.core)

;; actually, this is gep

(defn terminal? [x]
  (number? x))

(defn function? [x]
  (symbol? x))

(defn arity [x]
  (if (terminal? x)
    0
    (:arity x)))

(defn lookup [env x]
  (if (terminal? x)
    x
    (get env x)))

(defn parse*
  ([env exp]
     (first (parse* env 1 exp)))
  ([env n exp]
     (when (pos? n)
       (let [[t o] (split-at n exp)
             t' (mapv (partial lookup env) t)
             arities (map arity t')
             incremental (reductions + 0 arities)

             a (last incremental)
             nxt (parse* env a o)
             t'' (mapv (fn [x off]
                         (if (map? x)
                           (cons (:function x) (subvec nxt off (+ off (arity x))))
                           x))
                       t' incremental)]
         t''))))

(defn eval*
  [exp]
  (if (seq? exp)
    (let [[f & args] exp]
      (apply f args))
    exp))

(def arithmetic {'+ {:arity 2
                     :function (fn [a b]
                                 (+ (eval* a) (eval* b)))}
                 '* {:arity 2
                     :function (fn [a b]
                                 (* (eval* a) (eval* b)))}
                 '- {:arity 2
                     :function (fn [a b]
                                 (- (eval* a) (eval* b)))}
                 'div {:arity 2
                       :function (fn [a b]
                                   (/ (eval* a) (eval* b)))}

                 'mod {:arity 2
                       :function (fn [a b]
                                   (mod (eval* a) (eval* b)))}

                 '= {:arity 2
                     :function (fn [a b]
                                 (if (= (eval* a) (eval* b))
                                   1
                                   0))}

                 'if {:arity 3
                      :function (fn [test then else]
                                  (if (not= 0 (eval* test))
                                    (eval* then)
                                    (eval* else)))}})



(defn tail-size [h env]
  (let [m (reduce max 0 (map arity (vals env)))]
    (inc (* h (dec m)))))

(defn random [h e t-r trate]
  (let [fns (vec (keys e))
        t (tail-size h e)
        r (fn []
            (let [x (rand)]
              (if (< x trate)
                (t-r)
                (rand-nth fns))))]
    (vec
     (concat
      (repeatedly h r)
      (repeatedly t t-r)))))
