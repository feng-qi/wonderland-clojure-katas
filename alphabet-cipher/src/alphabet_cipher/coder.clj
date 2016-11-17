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
  (loop [shifted (rest in)
         n 1]
    ;; (if (and (not= shifted '()) (not-every? true? (map = in shifted)))
    (if (not-every? true? (map = in shifted))
      (recur (rest shifted) (inc n))
      (take n in))))

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
