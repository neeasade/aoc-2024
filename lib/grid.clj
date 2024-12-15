;; I am inventing new footguns every day
(ns lib.grid (:require [clojure.string :as string]))

(def grid (parse "123\n456\n789"))

(defn parse [input]
  (let [grid (mapv vec (string/split-lines input))]
    {:width (count (first grid))
     :height (count grid)
     :grid grid}))

(defmacro points
  ([grid] `(points ~grid true))
  ([grid condition]
   `(let [g# ~grid]
      (for [~'x (range (:width g#))
            ~'y (range (:height g#))
            :when ~condition]
        [~'x ~'y]))))

(defn gassoc [grid coord value]
  (assoc-in grid `[:grid ~@coord] value))

(defn gupdate [grid coord f]
  (update-in grid `[:grid ~@coord] f ))

(defn gmap [grid f]
  (reduce (fn [g point]
            (gupdate g point f))
          grid
          (points grid)))

(defn look [grid [x y]]
  (-> (:grid grid) (nth y []) (nth x nil)))

(defn within? [grid [x y]]
  (and (< -1 x (:width grid))
       (< -1 y (:height grid))))

(defn wrap [{:keys [width height]} [x y]]
  [(mod x width) (mod y height)])

(defn gprint [grid]
  ;; pretty
  (let [line (format (format "%%-%ss%%s" (+ 3 (:width grid))) "+" "+")]
    (println (string/replace line " " "-"))
    (run! #(println "|" % "|")
          (map (partial apply str) (:grid grid)))
    (println (string/replace line " " "-"))))

(comment
  (map (partial look (parse "123\n456\n789"))
       (points (parse "123\n456\n789")))

  (let [grid (parse "123\n456\n789")]
    (points grid
            (even?
             (parse-long (str (look grid [x y])))))))
