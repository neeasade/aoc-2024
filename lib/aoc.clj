(ns lib.aoc
  (:require [clojure.java.shell :as shell]
            [clojure.string :as string]
            [babashka.fs :as fs]))

(defn get-input [day]
  ;; ~/.cache/aoc
  (let [year 2024
        cache-file (fs/file (fs/xdg-cache-home) (format "aoc/%s/%s.txt" year day))
        ;; https://github.com/wimglenn/advent-of-code-wim/issues/1
        session (string/trim (:out (shell/sh "bash" "-i" "-c" "echo $AOC_SESSION")))]
    (fs/create-dirs (fs/parent cache-file))
    ;; (fs/delete cache-file)
    (when-not (fs/exists? cache-file)
      (spit (str (fs/strip-ext cache-file) "_example.txt")
            (->> (format "curl 'https://adventofcode.com/%s/day/%s' | pup ':contains(\"example\") + pre:first-of-type text{} | recode \"html..text\"'" year day)
                 (shell/sh "sh" "-c")
                 (:out)))

      (spit cache-file
            (:out (shell/sh
                   "curl" (format "https://adventofcode.com/%s/day/%s/input" year day)
                   "-X" "GET"
                   "-H" (format "Cookie: session=%s" session)))))
    (slurp cache-file)))
