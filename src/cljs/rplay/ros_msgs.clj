(ns rplay.ros-msgs
  (:require [camel-snake-kebab.core :as csk]
            [digest])
  (:import [org.apache.commons.io FilenameUtils]))

(defmacro define-ros-messages [& paths]
  (let [forms (for [path paths
                    :let [filenames (into []
                                          (comp (filter #(.isFile %))
                                                (map #(.getPath %)))
                                          (file-seq (clojure.java.io/file path)))
                          defs (map (fn [filename]
                                      `(def
                                         ~(symbol (str (csk/->kebab-case
                                                        (FilenameUtils/getBaseName filename))
                                                       "-hash"))
                                         ~(digest/md5 (slurp filename))))
                                    filenames)]]
                `(do (~'ns ~(symbol (csk/->kebab-case (.replace path "/" "."))))
                     ~@defs))]
    `(do ~@forms)))
