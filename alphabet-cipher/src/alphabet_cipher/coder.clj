(ns alphabet-cipher.coder)

(defn extend-keyword [keyword msg]
  (apply str (take (count msg) (apply str (repeat (inc (/ (count msg) (count keyword))) keyword)))))

(defn get-repeat [in]
  (loop [n 1]
    (if (not-every? true? (map = in (drop n in)))
      (recur (inc n))
      (take n in))))

(defn char-to-int [c]
  (- (int c) (int \a)))

(defn int-to-char [i]
  (if (and (<= i (char-to-int \z))
           (>= i (char-to-int \a)))
    (char (+ i (int \a)))
    (char (+ (mod i 26) (int \a)))))

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
