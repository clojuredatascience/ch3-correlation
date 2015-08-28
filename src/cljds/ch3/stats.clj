(ns cljds.ch3.stats
  (:require [incanter.stats :as s]
            [incanter.core :as i]
            [clj-time.coerce :as coerce]
            [clj-time.core :as time]))

(defn jitter [limit]
  (fn [x]
    (let [amount (- (rand (* 2 limit)) limit)]
      (+ x amount))))

(defn covariance [xs ys]
  (let [x-bar (s/mean xs)
        y-bar (s/mean xs)
        dx (map (fn [x] (- x x-bar)) xs)
        dy (map (fn [y] (- y y-bar)) ys)]
    (s/mean (map * dx dy))))

(defn variance [xs]
  (let [x-bar (s/mean xs)
        square-error (fn [x]
                       (i/pow (- x x-bar) 2))]
    (s/mean (map square-error xs))))

(defn standard-deviation [xs]
  (i/sqrt (variance xs)))

(defn correlation [x y]
  (/ (covariance x y)
     (* (standard-deviation x)
        (standard-deviation y))))

(defn t-statistic [x y]
  (let [r (correlation x y)
        r-square (* r r)
        df (- (count x) 2)]
    (/ (* r df)
       (i/sqrt (- 1 r-square)))))

(defn z->r [z]
  (/ (- (i/exp (* 2 z)) 1)
     (+ (i/exp (* 2 z)) 1)))

(defn r-confidence-interval [crit x y]
  (let [r   (correlation x y)
        n   (count x)
        zr  (* 0.5 (i/log (/ (+ 1 r)
                             (- 1 r))))
        sez (/ 1 (i/sqrt (- n 3)))]
    [(z->r (- zr (* crit sez)))
     (z->r (+ zr (* crit sez)))]))

(defn celsius->fahrenheit [c]
  (+ 32 (* 1.8 c)))

(defn regression-line [a b]
  (fn [x]
    (+ a (* b x))))

(defn slope [x y]
  (/ (covariance x y)
     (variance x)))

(defn intercept [x y]
  (- (s/mean y)
     (* (s/mean x)
        (slope x y))))

(defn residuals [a b x y]
  (let [estimate (regression-line a b)
        residual (fn [x y]
                   (- y (estimate x)))]
    (map residual x y)))

(defn r-squared
  ([a b x y]
   (let [r-var (variance (residuals a b x y))
         y-var (variance y)]
     (- 1 (/ r-var y-var))))
  
  ([coefs x y]
   (let [fitted      (i/mmult x coefs)
         residuals   (i/minus y fitted)
         differences (i/minus y (s/mean y))
         rss         (i/sum-of-squares residuals)
         ess         (i/sum-of-squares differences)]
     (- 1 (/ rss ess)))))

(defn normal-equation [x y]
  (let [xtx  (i/mmult (i/trans x) x)
        xtxi (i/solve xtx)
        xty  (i/mmult (i/trans x) y)]
    (i/mmult xtxi xty)))

(defn adj-r-squared [coefs x y]
  (let [rs (r-squared coefs x y)
        n  (count y)
        p  (count coefs)]
    (- 1
       (* (- 1 rs)
          (/ (- n 1)
             (- n p 1))))))

(defn f-test [coefs x y]
  (let [fitted      (i/mmult x coefs)
        difference  (i/minus fitted (s/mean y))
        residuals   (i/minus y fitted)
        ess         (i/sum-of-squares difference)
        rss         (i/sum-of-squares residuals)
        p           (i/ncol x)
        n           (i/nrow y)
        dfm         (- p 1)
        dfe         (- n p)
        msm         (/ ess dfm)
        mse         (/ rss dfe)
        f-stat      (/ msm mse)]
    (s/cdf-f f-stat :df1 dfm :df2 dfe :lower-tail? false)))

(defn dummy-mf [sex]
  (if (= sex "F")
    0.0 1.0))

(defn beta-weight [coefs x y]
  (let [sdx (map s/sd (i/trans x))
        sdy (s/sd y)]
    (map #(/ (* %1 %2) sdy) sdx coefs)))

(defn to-year [str]
  (-> (coerce/from-date str)
      (time/year)))

(defn predict [coefs x]
  (-> (i/trans coefs)
      (i/mmult x)
      (first)))

(defn prediction-interval [x y xp]
  (let [xtx    (i/mmult (i/trans x) x)
        xtxi   (i/solve xtx)
        xty    (i/mmult (i/trans x) y)
        coefs  (i/mmult xtxi xty)
        fitted (i/mmult x coefs)
        resid  (i/minus y fitted)
        rss    (i/sum-of-squares resid)
        n      (i/nrow y)
        p      (i/ncol x)
        dfe    (- n p)
        mse    (/ rss dfe)
        se-y   (first (i/mmult (i/trans xp) xtxi xp))
        t-stat (i/sqrt (* mse (+ 1 se-y)))
        intl   (* (s/quantile-t 0.975 :df dfe) t-stat)
        yp     (first (i/mmult (i/trans coefs) xp))]
    (vector (- yp intl) (+ yp intl))))
