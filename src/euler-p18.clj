(defn to-num [^String ch] (Integer/parseInt ch))

(def graph (vec (reverse (map #(vec (map to-num (. % split "\\s+"))) (.
"75
95 64
17 47 82
18 35 87 10
20 04 82 47 65
19 01 23 75 03 34
88 02 77 73 07 63 67
99 65 04 28 06 16 70 92
41 41 26 56 83 40 80 70 33
41 48 72 33 47 32 37 16 94 29
53 71 44 65 25 43 91 52 97 51 14
70 11 33 28 77 73 17 78 39 68 17 57
91 71 52 38 17 14 91 43 58 50 27 29 48
63 66 04 68 89 53 67 30 73 16 69 87 40 31
04 62 98 27 23 09 70 98 73 93 38 53 60 04 23"
  split "\n")))))

(defn find-path [[rowidx newrow graph] row]
  "The result of this function is a vector containing [RowIndex NewRow Graph]
  where NewRow is constructed with list of elements of which the first element is
  current with the rest of list as left or right of the graph whichever has the
  higher sum e.g. if current element is 63 left -> (63,4) and right -> (63,62)
  since the right is higher the newrow will contain only ((63,62), (66,98)....)
  Thus the reduction is applied with growing list of lists which will contain one
  final list that is the maximum path"
  (let [nrow
        (vec (for [colidx (range 0 (count row))]
          (let [cur (row colidx)
                lt  (cons cur (newrow colidx))
                rt  (cons cur (newrow (inc colidx)))
                ltsum (reduce + lt)
                rtsum (reduce + rt)
                ]
            (if (> ltsum rtsum) lt rt))))]
    [(inc rowidx) nrow graph]))

(defn euler-p18 [graph]
  "Create the seed row with each element converted to a list containing the same and
  reduce the list with find-path function defined i.e. the result of find-path function
  is applied to the next row on top the current row ((4),(62),(98),(27).....) the
  individual lists will grow with each reduction whereas the number of lists will get
  reduced by 1 for each reduction operation"
  (let [seed (vec (map list (first graph)))
        final (((reduce find-path [0 seed graph] (rest graph)) 1) 0)]
    [final (reduce + final)]))

(time (println (euler-p18 graph)))



