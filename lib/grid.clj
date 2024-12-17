;; I am inventing new footguns every day
(ns lib.grid
  (:require [clojure.string :as string]))

;; todo: dir-look?

(defn make
  ([w h] (make w h \.))
  ([w h c] {:width w
            :height h
            :grid (mapv (fn [_] (vec (repeat w c)))
                        (range h))}))

(defn parse [input]
  (let [grid (mapv vec (string/split-lines input))]
    {:width (count (first grid))
     :height (count grid)
     :grid grid}))

(defmacro points
  "Get all the points in the grid. Optionally takes an anaphoric argument for
  the :when condition (bound: x y g)"
  ([grid] `(points ~grid true))
  ([grid condition]
   `(let [~'g ~grid]
      (for [~'x (range (:width ~'g))
            ~'y (range (:height ~'g))
            :when ~condition]
        [~'x ~'y]))))

(defn look
  ([grid [x y]] (-> (:grid grid) (nth y []) (nth x nil)))
  ([grid [x y] val]
   ;; return value or nil
   (let [looking-for (if (coll? val) (set val) (set [val]))]
     (looking-for (look grid [x y])))))

(defn within? [grid [x y]]
  (and (< -1 x (:width grid))
       (< -1 y (:height grid))))

(defn wrap [{:keys [width height]} [x y]]
  [(mod x width) (mod y height)])

(defn gassoc [grid value coords]
  (reduce (fn [g coord]
            (assoc-in g `[:grid ~@(reverse coord)] value))
          grid coords))

(defn gupdate [grid f coords]
  (reduce (fn [g coord]
            (update-in g `[:grid ~@(reverse coord)] f))
          grid coords))

(defn gmap [grid f] (gupdate grid f (points grid)))

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
   (->> (map (fn [i] (get [[-1 -1] [0 -1] [1 -1] [-1 0] [0 0] [1 0] [-1 1] [0 1] [1 1]] (dec i)))
             (map (comp Integer/parseInt str) (seq (str neighbors))))
        (keep (fn [dir] (let [c (mapv + coord dir)]
                          (when (within? grid c)
                            [c (look grid c)]))))))
  ([grid coord grabs look-val]
   (->> (neighbors grid coord grabs)
        (keep (fn [[coord v]]
                (let [looking-for (if (coll? look-val)
                                    (set look-val)
                                    (set [look-val]))]
                  (and (looking-for v) coord)))))))

(comment
  (neighbors (parse "123\n456\n789") [0 0] \2)

  (look (parse "123\n456\n789") [0 0] \1)


  (neighbors (parse "123\n456\n789") [1 1] 123 #{\2 \3})

  (neighbors (parse "123\n456\n789") [1 1] 123)

  (neighbors (parse "123\n456\n789") [1 1] 2468 \2)

  (points (parse "123\n456\n789"))

  (points (parse "123\n456\n789") (= (look g [x y]) \2))

  (gmap (parse "123\n456\n789") (comp parse-long str))

  ;; (points  (= (look g [x y]) \2))


  (map (partial look (parse "123\n456\n789"))
       (points (parse "123\n456\n789")))

  (let [grid (parse "123\n456\n789")]
    (points grid
            (even?
             (parse-long (str (look grid [x y])))))))


