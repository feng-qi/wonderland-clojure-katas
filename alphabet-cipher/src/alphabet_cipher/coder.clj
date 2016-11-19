(ns alphabet-cipher.coder)

(def map-table
  ["abcdefghijklmnopqrstuvwxyz"
   "bcdefghijklmnopqrstuvwxyza"
   "cdefghijklmnopqrstuvwxyzab"
   "defghijklmnopqrstuvwxyzabc"
   "efghijklmnopqrstuvwxyzabcd"
   "fghijklmnopqrstuvwxyzabcde"
   "ghijklmnopqrstuvwxyzabcdef"
   "hijklmnopqrstuvwxyzabcdefg"
   "ijklmnopqrstuvwxyzabcdefgh"
   "jklmnopqrstuvwxyzabcdefghi"
   "klmnopqrstuvwxyzabcdefghij"
   "lmnopqrstuvwxyzabcdefghijk"
   "mnopqrstuvwxyzabcdefghijkl"
   "nopqrstuvwxyzabcdefghijklm"
   "opqrstuvwxyzabcdefghijklmn"
   "pqrstuvwxyzabcdefghijklmno"
   "qrstuvwxyzabcdefghijklmnop"
   "rstuvwxyzabcdefghijklmnopq"
   "stuvwxyzabcdefghijklmnopqr"
   "tuvwxyzabcdefghijklmnopqrs"
   "uvwxyzabcdefghijklmnopqrst"
   "vwxyzabcdefghijklmnopqrstu"
   "wxyzabcdefghijklmnopqrstuv"
   "xyzabcdefghijklmnopqrstuvw"
   "yzabcdefghijklmnopqrstuvwx"
   "zabcdefghijklmnopqrstuvwxy"])

(defn extend-keyword [keyword msg]
  (apply str (take (count msg) (apply str (repeat (inc (/ (count msg) (count keyword))) keyword)))))

(defn getXY [x y]
  (nth (nth map-table (mod x 26)) (mod y 26)))

(defn get-repeat [in]
  (loop [n 1]
    (if (not-every? true? (map = in (drop n in)))
      (recur (inc n))
      (take n in))))

;; (defn get-repeat [in]
;;   (loop [shifted (rest in)
;;          n 1]
;;     (if (not-every? true? (map = in shifted))
;;       (recur (rest shifted) (inc n))
;;       (take n in))))

(defn encode [keyword message]
  (apply str
         (map #(apply getXY (list (- (int (first %)) (int \a)) (- (int (second %)) (int \a))))
              (partition 2 (interleave (extend-keyword keyword message) message)))))

(defn decode [keyword message]
  (apply str
         (map #(apply getXY (list (- (int (first %)) (int \a))
                                  (- (- (int (second %)) (int \a)))))
              (partition 2 (interleave message (extend-keyword keyword message))))))

(defn decipher [cipher message]
  (->>
   (map - (map int cipher) (map int message))
   (map #(if (< % 0) (+ % 26) %))
   get-repeat
   (map #(+ % (int \a)))
   (map char)
   (apply str)))

(defn char-to-int [c]
  (- (int c) (int \a)))

(defn int-to-char [i]
  (if (and (<= i (char-to-int \z))
           (>= i (char-to-int \a)))
    (char (+ i (int \a)))
    (char (+ (mod i 26) (int \a)))))

;; (char (+ -1 (mod i (char-to-int \z)) (int \a))))) ; wrong, should mode 26

(defn encode [keyword message]
  (->>
   (extend-keyword keyword message)
   (map #(+ (char-to-int %1) (char-to-int %2)) message)
   (map int-to-char)
   (apply str)))

(defn decode [keyword message]
  (->>
   (extend-keyword keyword message)
   (map #(- (char-to-int %1) (char-to-int %2)) message)
   (map int-to-char)
   (apply str)))

(defn decipher [cipher message]
  (->>
   (map #(- (char-to-int %1) (char-to-int %2)) cipher message)
   (map int-to-char)
   get-repeat
   (apply str)))
