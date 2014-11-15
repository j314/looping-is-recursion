(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [acc base exp]
                 (if (zero? exp)
                   acc
                   (recur (* acc base) base (dec exp))))]
    (helper 1 base exp)))

(defn last-element [a-seq]
  (if (empty? (rest a-seq))
    (first a-seq)
    (recur (rest a-seq))))

(defn seq= [seq1 seq2]
  (cond
   (and (empty? seq1) (empty? seq2)) true
   (or (empty? seq1) (empty? seq2)) false
   (not (= (first seq1) (first seq2))) false
   :else (recur (rest seq1) (rest seq2))))

(defn find-first-index [pred a-seq]
  (loop [index 0
         seq a-seq]
    (cond
     (empty? seq) nil
     (pred (first seq)) index
     :else (recur (inc index) (rest seq)))))

(defn avg [a-seq]
  (loop [sum 0
         n 0
         seq a-seq]
    (if (empty? seq)
      (/ sum n)
      (recur (+ sum (first seq)) (inc n) (rest seq)))))

(defn parity [a-seq]
  (let [toggle (fn [set elem]
                 (if (contains? set elem)
                   (disj set elem)
                   (conj set elem)))]
    (loop [set #{}
           seq a-seq]
      (if (empty? seq)
        set
        (recur (toggle set (first seq)) (rest seq))))))

(defn fast-fibo [n]
  (loop [k 0
         fk 0
         fk-1 1]
    (if (>= k n)
      fk
      (recur (inc k) (+ fk fk-1) fk))))

(defn cut-at-repetition [a-seq]
  (loop [head []
         tail a-seq]
    (let [first-of-tail (first tail)]
      (cond
       (empty? tail) head
       (contains? (set head) first-of-tail) head
       :else (recur (conj head first-of-tail) (rest tail))))))

