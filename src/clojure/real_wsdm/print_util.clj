(ns real-wsdm.print-util)

(defn print-in-rects
  [& args]
  (letfn [(print-horizontal-bound
            []
            (print \+)
            (doseq [arglen (map count args)]
              (print (apply str (repeat arglen \-)))
              (print \+)))
          (print-args
            []
            (print \|)
            (doseq [arg args]
              (print arg)
              (print \|)))]
    (print-horizontal-bound)
    (println)
    (print-args)
    (println)
    (print-horizontal-bound)))

(defn println-in-rects
  [& args]
  (apply print-in-rects args)
  (println))