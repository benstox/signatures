(ns signatures.core
  (:gen-class))

(defn char-range [start end]
  (map char (range (int start) (inc (int end)))))

(def A-start (int \A))

(def alphabet-length (count (char-range \A \Z)))

(defn char-to-hexavigesimal [character]
  (mod (- (int character) A-start) alphabet-length))

(defn hexavigesimal-to-char [xxvi]
  (char (+ xxvi A-start)))

(defn inc-hexavigesimal [xxvi-list]
  (let [c (reverse xxvi-list)]
    (if (zero? (mod (inc (first c)) alphabet-length))
      (if (empty? (rest c))
        [0 0] ;; add a hexavigesimal place
        (reverse (cons 0 (reverse (inc-hexavigesimal (reverse (rest c))))))) ;; carry the one
      (reverse (cons (inc (first c)) (rest c)))))) ;; simple increments

(defn inc-chars [char-list]
  (let [xxvi-list (map char-to-hexavigesimal char-list)]
    (map
      hexavigesimal-to-char
      (inc-hexavigesimal xxvi-list))))

(defn alphabet-index
  ([]
    (alphabet-index [\A]))
  ([x]
    (lazy-seq
      (cons x (alphabet-index (inc-chars x))))))

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
        signatures (partition 16 16 nil page-nos)]
    (println (str "Number of document pages to print: " no-pages))
    (println (str "Number of sheets to print: " no-sheets))
    (println (str "Number of 4-sheet signatures to bind: " no-signatures))
    (println "#####################################")
    (doseq [[letters signature] (map vector (alphabet-index) signatures)]
      (println (str "Signature " (apply str letters) ". First page: " (first signature) ", last page: " (last signature))))
    (println "#####################################")))

(defn -main
  "Convert the arguments to integers and get some stats."
  [first-page last-page]
  (get-printer (parse-int first-page) (parse-int last-page)))
