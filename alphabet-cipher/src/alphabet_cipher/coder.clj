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
  (apply str (take (count msg) (apply str (repeat (count msg) keyword)))))

(defn getXY [x y]
  (nth (nth map-table x) y))

(defn encode [keyword message]
  (apply str
         (map #(apply getXY (list (- (int (first %)) (int \a)) (- (int (second %)) (int \a))))
              (partition 2 (interleave (extend-keyword keyword message) message)))))

(defn decode [keyword message]
  "decodeme")

(defn decipher [cipher message]
  "decypherme")

