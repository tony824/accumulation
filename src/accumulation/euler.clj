(ns accumulation.euler
  (:require [clojure.math.combinatorics :as combo])
  (:import [java.util GregorianCalendar]))

(comment
  Problem 1
  If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23.
  Find the sum of all the multiples of 3 or 5 below 1000.
  )

(defn sum [n]
  (reduce + (filter #(or (zero? (rem % 3))
                         (zero? (rem % 5)))
                    (range 1 n))))

(sum 1000)

(comment
  Problem 2
  By considering the terms in the Fibonacci sequence whose values do not exceed four million, find the sum of the even-valued terms.)
(defn sum-fibonacci
  [n]
  (->> (iterate (fn [[a b]] [(+ a b) a]) [1 1])
       (take-while #(< (first %) n))
       (map first)
       (filter even?)
       (reduce +)))

(sum-fibonacci 4000000)

;;note: `take-while' stops traversing the collection when the predicate is false, as is different from `filter'.
(take-while neg? [1 2 3 4 -5 -6 0 1])
;;=>'()

(take-while neg? [-2 1 -2 -6 -7 1 2 3 4 -5 -6 0 1])
;;=>'(-2)
(drop-while neg? [1 -2 -6 -7 1 2 3 4 -5 -6 0 1])
;;=>'(1 -2 -6 -7 1 2 3 4 -5 -6 0 1)

;; not-any? for all elements in seq  , pred will return false
;; every? for all elements in seq  pred will return true
;; not-every? for some elements in seq  pred will return false
;; some
(comment
  Problem 3
  The prime factors of 13195 are 5, 7, 13 and 29.
  What is the largest prime factor of the number 600851475143)

(defn is-prime? [n]
  (or (= 2 n)
      (not-any? #(= 0 (mod n %)) (cons 2 (range 3 (inc (Math/sqrt n)) 2)))))

(defn max-prime-factor [num]
  (loop [div 1]
    (if (and (zero? (rem num div))
             (is-prime? (quot num div)))
        (quot num div)
        (recur (inc div)))))

(comment
  Problem 4
  A palindromic number reads the same both ways. The largest palindrome made from the product of two 2 digit numbers is 9009 = 91 × 99.
  Find the largest palindrome made from the product of two 3 digit numbers.)

(defn l
  [m n]
  (loop [x m y n]
    (let [v (* x y)]
      (cond
        (= (seq (str v)) (reverse (seq (str v)))) [v x y]
        (= 1 y) (recur (dec x) n)
        :else (recur x (dec y))))))

(l 1000 1000)

;; when while in for
;; filter drop-while in for
(for [x [1 2 3]
      y [1 2 3]
      :while (<= x y)]
  [x y ])


(for [x [1 2 3]
      y [1 2 3]
      :when (<= x y)]
  [x y ])


(comment
  Problem 5
  2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder.
  What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20)

(defn smallest [n]
  (loop [x 1]
    (let [s (filter #(is-prime? %) (range 1 n))
          m (reduce * x s)]
      (if (every? #(= 0 (mod m %)) (range 1 n))
        [m x]
        (recur (inc x))))))

(smallest 20)

(defn factorial
  [n]
  (loop [x n acc 1N]
    (if (= 1 x)
      acc
      (recur (dec x) (bigint (* acc x))))))

(comment
  Problem 6
  Find the difference between the sum of the squares of the first one hundred natural numbers and the square of the sum)

(defn diff [w]
  (let [s (range 1 (inc w))
        m (reduce + s)
        n (reduce (fn [acc v] (+ (* v v) acc)) 0 s)]
    (- (* m m) n)))

(comment
  Problem 7 What is the 10001 st prime number)

(defn prime
  [n]
  (loop [v 2 cnt 1]
    (if (is-prime? v)
      (if (= cnt n) v (recur (inc v) (inc cnt)))
      (recur (inc v) cnt))))

(prime 10001)

(comment
  Problem 8
  Find the thirteen adjacent digits in the 1000 digit number that have the greatest product. What is the value of this product
  (clojure.string/replace "The color is red" #"\n" ""))
(def lstr "7316717653133062491922511967442657474235534919493496983520312774506326239578318016984801869478851843858615607891129494954595017379583319528532088055111254069874715852386305071569329096329522744304355766896648950445244523161731856403098711121722383113622298934233803081353362766142828064444866452387493035890729629049156044077239071381051585930796086670172427121883998797908792274921901699720888093776657273330010533678812202354218097512545405947522435258490771167055601360483958644670632441572215539753697817977846174064955149290862569321978468622482839722413756570560574902614079729686524145351004748216637048440319989000889524345065854122758866688116427171479924442928230863465674813919123162824586178664583591245665294765456828489128831426076900422421902267105562632111110937054421750694165896040807198403850962455444362981230987879927244284909188845801561660979191338754992005240636899125607176060588611646710940507754100225698315520005593572972571636269561882670428252483600823257530420752963450")

(defn product
  [n]
  (let [s (map #(Integer/parseInt (str %)) lstr)]
    (loop [v 1 s2 s m {}]
      (let [p  (reduce * (take n s2))]
        (if (seq s2)
          (if (> p v) (recur p (drop 1 s2) (assoc m :num (take n s2))) (recur v (drop 1 s2) m))
          [v m])))))
(comment
  Problem 9
  There exists exactly one Pythagorean triplet for which a + b + c = 1000.
  Find the product abc.)
(defn pythagorean
  [n]
  (for [x (range 1 n) y (range 1 n)
        :let [z (- n (+ x y))]
        :when (and (> y x)
                   (= (* z z) (+ (* x x) (* y y))) )] [x y z]))
(pythagorean 1000)

(comment
  Problem 10
  Find the sum of all the primes below two million)

(defn sum-prime [n]
  (let [s (filter #(is-prime? %) (range 2 n))]
    (reduce + s)))

(def v "08 02 22 97 38 15 00 40 00 75 04 05 07 78 52 12 50 77 91 08
  49 49 99 40 17 81 18 57 60 87 17 40 98 43 69 48 04 56 62 00
81 49 31 73 55 79 14 29 93 71 40 67 53 88 30 03 49 13 36 65
52 70 95 23 04 60 11 42 69 24 68 56 01 32 56 71 37 02 36 91
22 31 16 71 51 67 63 89 41 92 36 54 22 40 40 28 66 33 13 80
24 47 32 60 99 03 45 02 44 75 33 53 78 36 84 20 35 17 12 50
32 98 81 28 64 23 67 10 26 38 40 67 59 54 70 66 18 38 64 70
67 26 20 68 02 62 12 20 95 63 94 39 63 08 40 91 66 49 94 21
24 55 58 05 66 73 99 26 97 17 78 78 96 83 14 88 34 89 63 72
21 36 23 09 75 00 76 44 20 45 35 14 00 61 33 97 34 31 33 95
78 17 53 28 22 75 31 67 15 94 03 80 04 62 16 14 09 53 56 92
16 39 05 42 96 35 31 47 55 58 88 24 00 17 54 24 36 29 85 57
86 56 00 48 35 71 89 07 05 44 44 37 44 60 21 58 51 54 17 58
19 80 81 68 05 94 47 69 28 73 92 13 86 52 17 77 04 89 55 40
04 52 08 83 97 35 99 16 07 97 57 32 16 26 26 79 33 27 98 66
88 36 68 87 57 62 20 72 03 46 33 67 46 55 12 32 63 93 53 69
04 42 16 73 38 25 39 11 24 94 72 18 08 46 29 32 40 62 76 36
20 69 36 41 72 30 23 88 34 62 99 69 82 67 59 85 74 04 36 16
20 73 35 29 78 31 90 01 74 31 49 71 48 86 81 16 23 57 05 54
01 70 54 71 83 51 54 69 16 92 33 48 61 43 52 01 89 19 67 48")

(defn get-at [i j matrix]
  (if (and (>= i 0) (< i 20) (>= j 0) (< j 20))
    (nth matrix (+ j (* i 20))) 0))

(defn euler-011 []
  (let [matrix (->> (re-seq #"[^\n]+" v)
                    (map #(re-seq #"[^ ]+" %))
                    (map #(map (fn [v] (Integer/parseInt v)) %))
                    (apply concat))
        ways (for [i (range 20) j (range 20)]
               [(map #(get-at i (+ % j) matrix) (range 4))
                (map #(get-at (+ % i) j matrix) (range 4))
                (map #(get-at (+ % i) (+ % j) matrix) (range 4))
                (map #(get-at (+ % i) (- j %) matrix) (range 4))])]
    (reduce max (map #(reduce * %) (reduce concat ways)))))


(comment
  for m*n matrix
  use vector v to save this matrix  (=  (* m n) (count v))
  use i j (and (>= i 0) (< i m)
               (>= j 0) (< j n)) to transverse matrix
  (+ j (* i m)))

(defn get-matrix  [i j s m n]
  (let [index (+ j (* i m))]
    (if (and (>= i 0)
             (>= j 0)
             (< i m)
             (< j n))
      (nth s index)
      0)))

(comment
  for a m* n matrix
  get setp numbers (4 directions E S SE SW)
  (range n) starts from 0 (included) to n (excluded))
(defn calculate
  [m n s step]
  (for [i (range m)
        j (range n)]
    [(mapv #(get-matrix i (+ % j) s m n) (range step))
     (mapv #(get-matrix (+ % i) j s m n) (range step))
     (mapv #(get-matrix (+ % i) (+ % j) s m n) (range step))
     (mapv #(get-matrix (+ % i) (- j %) s m n) (range step))]))

(comment
  Problem 12
  What is the value of the first triangle number to have over five hundred divisors)

(defn factors
  [n]
  (let [s (filter #(zero? (mod n %)) (range 1 (inc (Math/sqrt n)) (if (even? n) 1 2)))]
    (set (reduce (fn [acc v] (conj acc (quot n v) v)) [] s))))

(defn triangle
  [fnum]
  (loop [n 1]
    (let [m (reduce + (range 1 (inc n)))
          s (seq (factors m))]
      (if (> (count s) fnum)
        m
        (recur (inc n))))))

(triangle 500)

(comment
  (bigint "37107287533902102798797998220837590246510135740250")
  (BigInteger. "37107287533902102798797998220837590246510135740250")
  Problem 13)
(def l-sr  "37107287533902102798797998220837590246510135740250
46376937677490009712648124896970078050417018260538
74324986199524741059474233309513058123726617309629
91942213363574161572522430563301811072406154908250
23067588207539346171171980310421047513778063246676
89261670696623633820136378418383684178734361726757
28112879812849979408065481931592621691275889832738
44274228917432520321923589422876796487670272189318
47451445736001306439091167216856844588711603153276
70386486105843025439939619828917593665686757934951
62176457141856560629502157223196586755079324193331
64906352462741904929101432445813822663347944758178
92575867718337217661963751590579239728245598838407
58203565325359399008402633568948830189458628227828
80181199384826282014278194139940567587151170094390
35398664372827112653829987240784473053190104293586
86515506006295864861532075273371959191420517255829
71693888707715466499115593487603532921714970056938
54370070576826684624621495650076471787294438377604
53282654108756828443191190634694037855217779295145
36123272525000296071075082563815656710885258350721
45876576172410976447339110607218265236877223636045
17423706905851860660448207621209813287860733969412
81142660418086830619328460811191061556940512689692
51934325451728388641918047049293215058642563049483
62467221648435076201727918039944693004732956340691
15732444386908125794514089057706229429197107928209
55037687525678773091862540744969844508330393682126
18336384825330154686196124348767681297534375946515
80386287592878490201521685554828717201219257766954
78182833757993103614740356856449095527097864797581
16726320100436897842553539920931837441497806860984
48403098129077791799088218795327364475675590848030
87086987551392711854517078544161852424320693150332
59959406895756536782107074926966537676326235447210
69793950679652694742597709739166693763042633987085
41052684708299085211399427365734116182760315001271
65378607361501080857009149939512557028198746004375
35829035317434717326932123578154982629742552737307
94953759765105305946966067683156574377167401875275
88902802571733229619176668713819931811048770190271
25267680276078003013678680992525463401061632866526
36270218540497705585629946580636237993140746255962
24074486908231174977792365466257246923322810917141
91430288197103288597806669760892938638285025333403
34413065578016127815921815005561868836468420090470
23053081172816430487623791969842487255036638784583
11487696932154902810424020138335124462181441773470
63783299490636259666498587618221225225512486764533
67720186971698544312419572409913959008952310058822
95548255300263520781532296796249481641953868218774
76085327132285723110424803456124867697064507995236
37774242535411291684276865538926205024910326572967
23701913275725675285653248258265463092207058596522
29798860272258331913126375147341994889534765745501
18495701454879288984856827726077713721403798879715
38298203783031473527721580348144513491373226651381
34829543829199918180278916522431027392251122869539
40957953066405232632538044100059654939159879593635
29746152185502371307642255121183693803580388584903
41698116222072977186158236678424689157993532961922
62467957194401269043877107275048102390895523597457
23189706772547915061505504953922979530901129967519
86188088225875314529584099251203829009407770775672
11306739708304724483816533873502340845647058077308
82959174767140363198008187129011875491310547126581
97623331044818386269515456334926366572897563400500
42846280183517070527831839425882145521227251250327
55121603546981200581762165212827652751691296897789
32238195734329339946437501907836945765883352399886
75506164965184775180738168837861091527357929701337
62177842752192623401942399639168044983993173312731
32924185707147349566916674687634660915035914677504
99518671430235219628894890102423325116913619626622
73267460800591547471830798392868535206946944540724
76841822524674417161514036427982273348055556214818
97142617910342598647204516893989422179826088076852
87783646182799346313767754307809363333018982642090
10848802521674670883215120185883543223812876952786
71329612474782464538636993009049310363619763878039
62184073572399794223406235393808339651327408011116
66627891981488087797941876876144230030984490851411
60661826293682836764744779239180335110989069790714
85786944089552990653640447425576083659976645795096
66024396409905389607120198219976047599490197230297
64913982680032973156037120041377903785566085089252
16730939319872750275468906903707539413042652315011
94809377245048795150954100921645863754710598436791
78639167021187492431995700641917969777599028300699
15368713711936614952811305876380278410754449733078
40789923115535562561142322423255033685442488917353
44889911501440648020369068063960672322193204149535
41503128880339536053299340368006977710650566631954
81234880673210146739058568557934581403627822703280
82616570773948327592232845941706525094512325230608
22918802058777319719839450180888072429661980811197
77158542502016545090413245809786882778948721859617
72107838435069186155435662884062257473692284509516
20849603980134001723930671666823555245252804609722
53503534226472524250874054075591789781264330331690")

(defn big-sum
  []
  (let [s (->> (re-seq #"[^\n]+" l-sr)
               (map #(read-string %)))]
    (reduce + s)))

(comment
  Problem 14)

(defn longest
  [v acc]
  (cond
    (= 1 v) (conj acc 1)
    (even? v) (recur (/ v 2) (conj acc v))
    (odd? v) (recur (+ 1 (* v 3)) (conj acc v))))

(defn find-longest
  [m]
  (loop [n m f {}]
    (let [v (longest n [])]
      (if (= 1 n)
        f
        (if ( > (count v) (count (:m f)))
          (recur (dec n) (assoc f :m v :v n))
          (recur (dec n) f))))))

(defn combination [n k]
  (cond (zero? n) 0
        (zero? k) 1
        :else (/ (factorial n) (* (factorial (- n k)) (factorial k)))))

(combination (+ 2 2) 2)

(comment
  Problem 15 pascal's triangle)
(defn routes-extend [lst]
  (let [size (count lst)]
    (for [i (range (inc size))]
      (if (or (= 0 i) (= size i))
        1
        (+ (nth lst (dec i)) (nth lst i))))))

;; Elapsed time: 14.809846 msecs
(defn euler-015 []
  (let [n 20 d (inc (* n 2))]
    (nth (last (take d (iterate routes-extend [1]))) n)))

(comment
  Problem 16)

(defn my-pow
  [n m]
  (loop [y m acc 1N]
    (if (= 1 y)
      (* acc n)
      (recur (dec y) (bigint (* acc n))))))

(defn convert
  [n acc]
  (let [d (quot n 10)
        r (int (mod n 10))]
    (if (zero? d)
      (cons r acc)
      (recur d (cons r acc)))))

;;(reduce + (map #(Integer/parseInt (str %)) (seq (str  (my-pow 2 1000)))))
;; 2 1000
(defn  euler-016 [m n]
  (reduce + (convert (my-pow m n) [])))

(def t "75\n95 64\n17 47 82\n18 35 87 10\n20 04 82 47 65\n19 01 23 75 03 34\n88 02 77 73 07 63 67\n99 65 04 28 06 16 70 92\n41 41 26 56 83 40 80 70 33\n41 48 72 33 47 32 37 16 94 29\n53 71 44 65 25 43 91 52 97 51 14\n70 11 33 28 77 73 17 78 39 68 17 57\n91 71 52 38 17 14 91 43 58 50 27 29 48\n63 66 04 68 89 53 67 30 73 16 69 87 40 31\n04 62 98 27 23 09 70 98 73 93 38 53 60 04 23")

(defn euler-018-f []
  (->> (re-seq #"[^\n]+" t)
       (map #(re-seq #"[^ ]+" %))
       (map #(map (fn [v] (Integer/parseInt v)) %))
       (map #(apply max %))
       (reduce +)))

(defn pos
  [s acc n]
  (if (seq s)
    (let [s1 (first s)
          a (nth s1 n) 
          b (nth s1 (inc n) -1 )
          pos (if (> a b) n (inc n))]
      (recur (rest s) (conj acc (max a b)) pos))
    acc))

(defn palindromic
  [n]
  (letfn [(first-p [n]
            (if (= (seq (str n)) (reverse (seq (str n))))
              n
              (recur (inc n))))
          (find-p [n]
            (if (zero? n)
              1
              (first-p (inc n))))]
    (iterate find-p
             (first-p n))))

(defn max-row [lst]
  (map #(reduce max %) (partition 2 1 lst)))

(defn step-max [lst1 lst2]
  (map + (max-row lst1) lst2))

;; Elapsed time: 0.421325 msecs
(defn euler-018 []
  (let [s (->> (re-seq #"[^\n]+" t)
               (map #(re-seq #"[^ ]+" %))
               (map #(map (fn [v] (Integer/parseInt v)) %)))]
    
    (reduce step-max (reverse s))))

(defn calendar-for [year month]
  (doto (GregorianCalendar.)
    (.set GregorianCalendar/YEAR year)
    (.set GregorianCalendar/MONTH month)
    (.set GregorianCalendar/DAY_OF_MONTH 1)))

;; Elapsed time: 30.138531 msecs
(defn euler-019 []
  (reduce +
          (for [year (range 1901 (inc 2000)) month (range 1 (inc 12))]
            (let [c (calendar-for year month)]
              (if (= GregorianCalendar/SUNDAY 
                     (.get c GregorianCalendar/DAY_OF_WEEK)) 1 0)))))

(defn euler-020 [n]
  (reduce + (convert (factorial n) [])))

(defn euler-021 [n]
  (let [s (for [v (range  n)
                :let [sum1 (reduce + (disj (factors v) v))
                      sum2 (reduce + (disj (factors sum1) sum1))]
                :when (and  (= sum2 v )
                            (not= sum1 sum2))]
            [v sum1])]
    (reduce  + (map first s))))

(defn euler-022 []
  (let [s (->>(slurp "euler/p022_names.txt")
              (re-seq #"\w+" )
              sort
              (map-indexed #(* (inc %)
                               (reduce + (map (fn [v]
                                                (- (int v)  64))
                                              (seq %2))))))]
    (reduce + s)))

(defn redundant? [n ] (> (reduce + (disj (factors n) n)) n))

(defn euler-023 []
  (reduce + (filter
             (fn  [n] (not-any?  #(and (redundant? %)
                                       (redundant? (- n %))) (range 1 (inc (quot n 2)))))
             
             (range 1 28123))))

(defn euler-024 [n]
  (combo/nth-permutation [0 1 2 3 4 5 6 7 8 9 ] (dec n)))

(defn euler-025
  [n]
  (-> (fn [item] (if (< (count (str item)) n) true false))
      (take-while
       (map first (iterate (fn [[a b]] [b (bigint (+ a b))]) [1 1])))
      (count)
      (inc)))
