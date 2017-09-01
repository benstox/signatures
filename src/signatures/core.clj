(ns signatures.core
  (:gen-class))

(defn char-range [start end]
  (map char (range (int start) (inc (int end)))))

(defn parse-int [s]
  (Integer/parseInt (re-find #"\A-?\d+" s)))

(defn get-printer [first-page last-page]
  "Give the page numbers of a range of pages to print
  in a PDF document, print out some stats about printing
  and binding."
  (let [page-nos (range first-page (inc last-page))
        no-pages (count page-nos)
        no-sheets (int (Math/ceil (/ no-pages 4)))
        no-signatures (int (Math/ceil (/ no-pages 16)))
        signatures (partition 16 16 nil page-nos)
        alphabet (char-range \A \Z)]
    (println (str "Number of document pages to print: " no-pages))
    (println (str "Number of sheets to print: " no-sheets))
    (println (str "Number of 4-sheet signatures to bind: " no-signatures))
    (println "#####################################")
    (doseq [[letter signature] (map vector alphabet signatures)]
      (println (str "Signature " letter ". First page: " (first signature) ", last page: " (last signature))))
    (println "#####################################")))

(defn -main
  "Convert the arguments to integers and get some stats."
  [first-page last-page]
  (get-printer (parse-int first-page) (parse-int last-page)))
