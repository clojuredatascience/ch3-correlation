(ns cljds.ch3.examples
  (:require [cljds.ch3.data :refer :all]
            [cljds.ch3.stats :refer :all]
            [clj-time.coerce :as coerce]
            [clj-time.core :as time]
            [incanter.charts :as c]
            [incanter.core :as i]
            [incanter.excel :as xls]
            [incanter.stats :as s]
            [incanter.svg :as svg]))

(defn ex-3-1 []
  (i/view (athlete-data)))

(defn ex-3-2 []
  (-> (remove nil? (i/$ "Height, cm" (athlete-data)))
      (c/histogram :nbins 20
                   :x-label "Height, cm"
                   :y-label "Frequency")
      (i/view)))

(defn ex-3-3 []
  (-> (remove nil? (i/$ "Weight" (athlete-data)))
      (c/histogram :nbins 20
                   :x-label "Weight"
                   :y-label "Frequency")
      (i/view)))

(defn ex-3-4 []
  (->> (swimmer-data)
       (i/$ "Weight")
       (remove nil?)
       (s/skewness)))

(defn ex-3-5 []
  (-> (remove nil? (i/$ "Weight" (athlete-data)))
      (i/log)
      (c/histogram :nbins 20
                   :x-label "log(Weight)"
                   :y-label "Frequency")
      (i/view)))

(defn ex-3-6 []
  (let [data (swimmer-data)
        heights (i/$ "Height, cm" data)
        weights (i/log (i/$ "Weight" data))]
    (-> (c/scatter-plot heights weights
                        :x-label "Height, cm"
                        :y-label "log(Weight)")
        (i/view))))

(defn ex-3-7 []
  (let [data (swimmer-data)
        heights (->> (i/$ "Height, cm" data)
                     (map (jitter 0.5)))
        weights (->> (i/$ "Weight" data)
                     (map (jitter 0.5))
                     (i/log))]
    (-> (c/scatter-plot heights weights
                        :x-label "Height, cm"
                        :y-label "log(Weight)")
        (i/view))))

(defn ex-3-8 []
  (let [data (swimmer-data)
        heights (i/$ "Height, cm" data)
        weights (i/log (i/$ "Weight" data))]
    (correlation heights weights)))

(defn ex-3-9 []
  (let [data (swimmer-data)
        heights (i/$ "Height, cm" data)
        weights (i/log (i/$ "Weight" data))
        t-value (t-statistic heights weights)
        df (- (count heights) 2)
        p  (* 2 (s/cdf-t t-value :df df :lower-tail? false))]
    (println "t-value" t-value)
    (println "P value " p)))

(defn ex-3-10 []
  (let [data (swimmer-data)
        heights  (i/$ "Height, cm" data)
        weights  (i/log (i/$ "Weight" data))
        interval (r-confidence-interval 1.96 heights weights)]
    (println "Confidence Interval (95%): " interval)))

(defn ex-3-11 []
  (-> (c/function-plot celsius->fahrenheit -10 40
                       :x-label "Celsius"
                       :y-label "Fahrenheit")
      (i/view)))

(defn ex-3-12 []
  (let [data (swimmer-data)
        heights (i/$ "Height, cm" data)
        weights (i/log (i/$ "Weight" data))
        a (intercept heights weights)
        b (slope heights weights)]
    (println "Intercept: " a)
    (println "Slope: " b)))

(defn ex-3-13 []
  (let [data (swimmer-data)
        heights (->> (i/$ "Height, cm" data)
                     (map (jitter 0.5)))
        weights (i/log (i/$ "Weight" data))
        a (intercept heights weights)
        b (slope heights weights)]
    (-> (c/scatter-plot heights weights
                        :x-label "Height, cm"
                        :y-label "log(Weight)")
        (c/add-function (regression-line a b) 150 210)
        (i/view))))

(defn ex-3-14 []
  (let [data (swimmer-data)
        heights (->> (i/$ "Height, cm" data)
                     (map (jitter 0.5)))
        weights (i/log (i/$ "Weight" data))
        a (intercept heights weights)
        b (slope heights weights)]
    (-> (c/scatter-plot heights (residuals a b heights weights)
                        :x-label "Height, cm"
                        :y-label "Residuals")
        (c/add-function (constantly 0) 150 210)
        (i/view))))

(defn ex-3-15 []
  (let [data (swimmer-data)
        heights (i/$ "Height, cm" data)
        weights (i/log (i/$ "Weight" data))
        a (intercept heights weights)
        b (slope heights weights)]
    (r-squared a b heights weights)))

(defn ex-3-16 []
  (->> (swimmer-data)
       (i/$ ["Height, cm" "Weight"])
       (i/to-matrix)))

(defn ex-3-17 []
  (->> (swimmer-data)
       (i/$ "Height, cm")
       (i/matrix)))

(defn feature-matrix [col-names dataset]
  (-> (i/$ col-names dataset)
      (i/to-matrix)))

(defn add-bias [x]
  (i/bind-columns (repeat (i/nrow x) 1) x))

(defn ex-3-18 []
  (let [data (swimmer-data)
        x (i/matrix (i/$ "Height, cm" data))
        y (i/matrix (i/log (i/$ "Weight" data)))]
    (normal-equation (add-bias x) y)))

(defn ex-3-19 []
  (feature-matrix ["Height, cm" "Age"] (swimmer-data)))

(defn ex-3-20 []
  (let [data (swimmer-data)
        x (->> data
                (feature-matrix ["Height, cm" "Age"])
                (add-bias))
        y (->> (i/$ "Weight" data)
                (i/log)
                (i/matrix))]
    (normal-equation x y)))

(defn ex-3-21 []
  (let [data (swimmer-data)
        x (->> (feature-matrix ["Height, cm" "Age"] data)
               (add-bias))
        y (->> (i/$ "Weight" data)
               (i/log)
               (i/matrix))
        beta (normal-equation x y)]
    (r-squared beta x y)))

(defn ex-3-22 []
  (let [data (swimmer-data)
        x (->> (feature-matrix ["Height, cm" "Age"] data)
               (add-bias))
        y (->> (i/$ "Weight" data)
               (i/log)
               (i/matrix))
        beta (normal-equation x y)]
    (adj-r-squared beta x y)))

(defn ex-3-23 []
  (let [data (swimmer-data)
        x (->> (feature-matrix ["Height, cm" "Age"] data)
               (add-bias))
        y (->> (i/$ "Weight" data)
               (i/log))
        beta (:coefs (s/linear-model y x :intercept false))]
    (f-test beta x y)))

(defn ex-3-24 []
  (let [data (s/sample (swimmer-data) :size 5)
        x (->> (feature-matrix ["Height, cm" "Age"] data)
               (add-bias))
        y (->> (i/$ "Weight" data)
               (i/log))
        beta (:coefs (s/linear-model y x :intercept false))]
    (f-test beta x y)))

(defn ex-3-25 []
  (let [data (->> (swimmer-data)
                  (i/add-derived-column "Dummy MF"
                                        ["Sex"]
                                        dummy-mf))
        x (->> data
               (feature-matrix ["Height, cm"
                                "Age"
                                "Dummy MF"])
               (add-bias))
        y (->> (i/$ "Weight" data)
               (i/log)
               (i/matrix))
        beta (normal-equation x y)]
    (adj-r-squared beta x y)))

(defn ex-3-26 []
  (let [data (->> (swimmer-data)
                  (i/add-derived-column "Dummy MF"
                                        ["Sex"]
                                        dummy-mf))
        x (->> data
               (feature-matrix ["Height, cm"
                                "Age"
                                "Dummy MF"])
               (add-bias))
        y (->> (i/$ "Weight" data)
               (i/log)
               (i/matrix))
        beta (normal-equation x y)]
    (beta-weight beta x y)))

(defn ex-3-27 []
  (let [data (->> (swimmer-data)
                  (i/add-derived-column "Dummy MF"
                                        ["Sex"]
                                        dummy-mf)
                  (i/add-derived-column "Year of birth"
                                        ["Date of birth"]
                                        to-year))
        x (->> data
               (feature-matrix ["Height, cm"
                                "Age"
                                "Dummy MF"
                                "Year of birth"])
               (add-bias))
        y (->> (i/$ "Weight" data)
               (i/log)
               (i/matrix))
        beta (normal-equation x y)]
    (beta-weight beta x y)))

(defn ex-3-28 []
  (let [data (->> (swimmer-data)
                  (i/add-derived-column "Year of birth"
                                        ["Date of birth"]
                                        to-year))
        x (->> (i/$ "Age" data)
               (map (jitter 0.5)))
        y (i/$ "Year of birth" data)]
    (-> (c/scatter-plot x y
                        :x-label "Age"
                        :y-label "Year of birth")
        (i/view))))

(defn ex-3-29 []
  (let [data (->> (swimmer-data)
                  (i/add-derived-column "Dummy MF"
                                        ["Sex"]
                                        dummy-mf)
                  (i/add-derived-column "Year of birth"
                                        ["Date of birth"]
                                        to-year))
        x (->> data
               (feature-matrix ["Height, cm"
                                "Dummy MF"
                                "Year of birth"])
               (add-bias))
        y (->> (i/$ "Weight" data)
               (i/log)
               (i/matrix))
        beta (normal-equation x y)
        xspitz (i/matrix [1.0 183 1 1950])]
    (i/exp (predict beta xspitz))))

(defn ex-3-30 []
  (let [data (->> (swimmer-data)
                  (i/add-derived-column "Dummy MF"
                                        ["Sex"]
                                        dummy-mf)
                  (i/add-derived-column "Year of birth"
                                        ["Date of birth"]
                                        to-year))
        x (->> data
               (feature-matrix ["Height, cm"
                                "Dummy MF"
                                "Year of birth"])
               (add-bias))
        y (->> (i/$ "Weight" data)
               (i/log)
               (i/matrix))
        xspitz (i/matrix [1.0 183 1 1950])]
    (i/exp (prediction-interval x y xspitz))))

(defn ex-3-31 []
  (let [data (-> (swimmer-data)
                 (s/sample :size 5))
        x (add-bias (feature-matrix ["Height, cm"] data))
        y (i/log (i/$ "Weight" data))
        lower-interval (fn [xp]
                         (first (prediction-interval x y (vector 1 xp))))
        upper-interval (fn [xp]
                         (last (prediction-interval x y (vector 1 xp))))]
    (-> (c/scatter-plot (i/$ :all 1 x) y
                        :x-label "Height, cm"
                        :y-label "log(Weight)")
        (c/add-function lower-interval 150 210)
        (c/add-function upper-interval 150 210)
        (i/view))))

(defn ex-3-32 []
  (let [data (->> (swimmer-data)
                  (i/add-derived-column "Dummy MF"
                                        ["Sex"]
                                        dummy-mf))
        x (->> data
               (feature-matrix ["Height, cm"
                                "Dummy MF"
                                "Age"])
               (add-bias))
        y (->> (i/$ "Weight" data)
               (i/log)
               (i/matrix))
        beta (normal-equation x y)
        xspitz (i/matrix [1.0 185 1 22])]
    (i/exp (predict beta xspitz))))
