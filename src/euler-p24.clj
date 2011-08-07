(defn swap [vect src-idx target-idx]
  "Swap the elements in vector on specified positions"
  (vec (for [i (range 0 (count vect))]
    (cond
      (= i src-idx) (vect target-idx)
      (= i target-idx) (vect src-idx)
      :else (vect i)))))

(defn find-k
  ([vect] (find-k (first vect) vect -1 -1))
  ([init vect idx result]
  (if (empty? vect) result
  (let [cur (first vect)]
    (recur cur (rest vect) (inc idx) (if (> cur init) idx result))))))

(defn find-l
  ([k vect] (find-l (vect k) vect -1 -1))
  ([maxi vect idx result]
  (if (empty? vect) result
  (let [cur (first vect)
        newidx (inc idx)]
    (recur maxi (rest vect) newidx (if (> cur maxi) newidx result))))))

(defn nth-perm
  "Generate the lexicographic permutations recursively until the nth permutation using the algorithm in
  http://en.wikipedia.org/wiki/Permutation#Systematic_generation_of_all_permutations"
  ([vect n] (nth-perm vect (find-k vect) 1 n))
  ([vect k count n]
  (if (or (= count n) (= -1 k)) vect
  (let [l (find-l k vect)
        swapped (swap vect k l)
        parts (split-at (inc k) swapped)
        nextperm (vec (concat (parts 0) (reverse (parts 1))))]
    (recur nextperm (find-k nextperm) (inc count) n)))))

(time (println (nth-perm [0 1 2 3 4 5 6 7 8 9] 1000000)))


