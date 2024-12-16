;; I am inventing new footguns every day
(ns lib.grid
  (:require [clojure.string :as string]))

;; todo: dir-look?

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

(defn look [grid [x y]]
  (-> (:grid grid) (nth y []) (nth x nil)))

(defn gassoc [grid coord value]
  (assoc-in grid `[:grid ~@(reverse coord)] value))

(defn gupdate [grid coord f]
  (update-in grid `[:grid ~@(reverse coord)] f ))

(defn gmap [grid f]
  (reduce (fn [g point]
            (gupdate g point f))
          grid
          (points grid)))

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

;; 123
;; 4@6
;; 789
(defn neighbors
  "return surrounding cells in the form [[x y] look]"
  ([grid coord] (neighbors grid coord 12346789))
  ([grid coord neighbors]
   (let [coords (map (fn [i] (get [[-1 -1] [0 -1] [1 -1] [-1 0] [0 0] [1 0] [-1 1] [0 1] [1 1]] (dec i)))
                     (map (comp Integer/parseInt str) (seq (str neighbors))))]
     (map (fn [dir]
            ((juxt identity (partial look grid))
             (mapv + coord dir)))
          coords)))
  ([grid coord grabs look-val]
   (keep (fn [[coord v]]
           (when (= v look-val)
             coord))
         (neighbors grid coord grabs))))

(comment
  (neighbors (parse "123\n456\n789") [1 1] 123)

  (neighbors (parse "123\n456\n789") [1 1] 123 \2)

  (map (partial look (parse "123\n456\n789"))
       (points (parse "123\n456\n789")))

  (let [grid (parse "123\n456\n789")]
    (points grid
            (even?
             (parse-long (str (look grid [x y])))))))
