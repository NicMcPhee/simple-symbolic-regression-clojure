(ns simple-symbolic-regression-clojure.interpreter-test
  (:use midje.sweet)
  (:use [simple-symbolic-regression-clojure.interpreter])
  )

(fact "it gets a empty program and an empty binding it returns nil with an empty stack"
      (interpret [] {}) => {:result nil, :stack []})

(fact "the interpreter gets script [7] and no binding it returns 7 with stack [7]"
      (interpret [7] {}) => {:result 7, :stack [7]})

(fact "process-token puts the token onto the stack if its a literal"
      (process-token [] 8) => [8]
      (peek (process-token [1 2 3] 4)) => 4
      (process-token [1 2 3] 4) => [1 2 3 4]
      )

(fact "run-script should process a list of literals by pushing them into the stack"
      (run-script [] {}) => []
      (run-script [1 2 3] {}) => [1 2 3])

(facts "about process-token with :+"
       (fact "process-token with :+ and an empty stack"
             (process-token [] :+) => [])
       (fact "process-token with :+ and only one number"
             (process-token [7] :+) => [7])
       (fact "process-token with :+ and two numbers"
             (process-token [3 5] :+) => [8]
             (process-token [1.2 -3.4] :+) => [-2.2]
             (process-token [2/3 5/6] :+) => [3/2]
             (process-token [0 0] :+) => [0]
             )
       (fact "process-token with :+ and many numbers"
             (process-token [5 8 9 6 3 2] :+) => [5 8 9 6 5])
       )

(facts "about process-token with :-"
       (fact "process-token with :- and an empty stack"
             (process-token [] :-) => [])
       (fact "process-token with :- and only one number"
             (process-token [7] :-) => [7])
       (fact "process-token with :- and two numbers"
             (process-token [3 5] :-) => [-2]
             (process-token [1.2 -3.4] :-) => [4.6]
             (process-token [2/3 5/6] :-) => [-1/6]
             (process-token [0 0] :-) => [0]
             )
       (fact "process-token with :- and many numbers"
             (process-token [5 8 9 6 3 2] :-) => [5 8 9 6 1])
       )

(facts "about process-token with :÷"
       (fact "process-token with :÷ and an empty stack"
             (process-token [] :÷) => [])
       (fact "process-token with :÷ and only one number"
             (process-token [7] :÷) => [7])
       (fact "process-token with :÷ and two numbers"
             (process-token [3 5] :÷) => [3/5]
             (process-token [1.5 -0.5] :÷) => [-3.0]
             (process-token [2/3 5/6] :÷) => [4/5]
             )
       (fact "process-token with :÷ and 0 as second argument"
             (process-token [3 0] :÷) => [3 0]
             (process-token [3 0.0] :÷) => [3 0.0]
             (process-token [1/2 0/5] :÷) => [1/2 0]
             (process-token [0.0 0.0] :÷) => [0.0 0.0]
             )
       (fact "process-token with :÷ and many numbers"
             (process-token [5 8 9 6 3 2] :÷) => [5 8 9 6 3/2])
       )

(facts "about process-token with :*"
       (fact "process-token with :* and an empty stack"
             (process-token [] :*) => [])
       (fact "process-token with :* and only one number"
             (process-token [7] :*) => [7])
       (fact "process-token with :* and two numbers"
             (process-token [3 5] :*) => [15]
             (process-token [1.2 -3.4] :*) => [-4.08]
             (process-token [2/3 5/6] :*) => [5/9]
             (process-token [0 0] :*) => [0]
             )
       (fact "process-token with :* and really large arguments"
             (process-token [1111111111111 22222222222222222222222222222] :*)
               => [24691358024688888888888888888641975308642N]
             (process-token [111111 22222222222222222] :*)
               => [2469133333333333308642N]
             )
       (fact "process-token with :* and many numbers"
             (process-token [5 8 9 6 3 2] :*) => [5 8 9 6 6])
       )


(fact "swap gets a vector containing two arguments, returns a vector with same elements reversed"
      (swap 5 8) => [8 5])

(facts "about process-token with :swap"
       (fact "process-token with :swap and an empty stack"
             (process-token [] :swap) => [])
       (fact "process-token with :swap and only one number"
             (process-token [7] :swap) => [7])
       (fact "process-token with :swap and two numbers"
             (process-token [3 5] :swap) => [5 3])
       (fact "process-token with :swap and several numbers"
             (process-token [5 8 9 6 3 2 0] :swap) => [5 8 9 6 3 0 2]))

(fact "dup gets a vector containing one argument, returns a vector with that element repeated"
      (dup 5) => [5 5])

(facts "about process-token with :dup"
       (fact "process-token with :dup and an empty stack"
             (process-token [] :dup) => [])
       (fact "process-token with :dup and only one number"
             (process-token [7] :dup) => [7 7])
       (fact "process-token with :dup and two numbers"
             (process-token [3 5] :dup) => [3 5 5])
       (fact "process-token with :dup and several numbers on the stack"
             (process-token [5 8 9 6 3 2 0] :dup) => [5 8 9 6 3 2 0 0]))

(facts "about process-token with a variable"
       (fact "process-token with an undefined variable"
             (process-token [3 4 7] :x) => [3 4 7])
       (fact "process-token with a defined variable"
             (process-token {:x 33, :y 47} [3 4 7] :x) => [3 4 7 33])
       )

;; interpreter

(fact "the interpreter does addition"
      (interpret [1 2 :+] {}) => {:result 3, :stack [3]}
      (interpret [1 :+] {}) => {:result 1, :stack [1]}
      (interpret [:+] {}) => {:result nil, :stack []}
      (interpret [1 2 3 4 :+] {}) => {:result 7, :stack [1 2 7]}
      (interpret [1 2 3 :+ 4 5 7 :+] {}) => {:result 12, :stack [1 5 4 12]}
      (interpret [1 2 3 4 5 :+ :+ :+] {}) => {:result 14, :stack [1 14]}
      )

(fact "the interpreter does subtraction"
      (interpret [1 2 :-] {}) => {:result -1, :stack [-1]}
      (interpret [1 :-] {}) => {:result 1, :stack [1]}
      (interpret [:-] {}) => {:result nil, :stack []}
      (interpret [1 2 3 4 :-] {}) => {:result -1, :stack [1 2 -1]}
      (interpret [1 2 3 :- 4 5 7 :-] {}) => {:result -2, :stack [1 -1 4 -2]}
      (interpret [1 2 3 4 5 :- :- :-] {}) => {:result -2, :stack [1 -2]}
      )

(fact "the interpreter does division"
      (interpret [1 2 :÷] {}) => {:result 1/2, :stack [1/2]}
      (interpret [6.5 0.125 :÷] {}) => {:result 52.0, :stack [52.0]}
      (interpret [1 :÷] {}) => {:result 1, :stack [1]}
      (interpret [:÷] {}) => {:result nil, :stack []}
      (interpret [1 2 3 4 :÷] {}) => {:result 3/4, :stack [1 2 3/4]}
      (interpret [1 2 3 :÷ 4 5 7 :÷] {}) => {:result 5/7, :stack [1 2/3 4 5/7]}
      (interpret [1 2 3 4 5 :÷ :÷ :÷] {}) => {:result (/ 2 (/ 3 (/ 4 5))),
                                           :stack [1 (/ 2 (/ 3 (/ 4 5)))]}
      (interpret [1.0 2.0 3.0 4.0 5.0 :÷ :÷ :÷] {}) => {:result (/ 2.0 (/ 3.0 (/ 4.0 5.0))),
                                                     :stack [1.0 (/ 2.0 (/ 3.0 (/ 4.0 5.0)))]}
      )

(fact "the interpreter does multiplication"
      (interpret [3 2 :*] {}) => {:result 6, :stack [6]}
      (interpret [3 :*] {}) => {:result 3, :stack [3]}
      (interpret [:*] {}) => {:result nil, :stack []}
      (interpret [1 2 3 4 :*] {}) => {:result 12, :stack [1 2 12]}
      (interpret [1 2 3 :* 4 5 7 :*] {}) => {:result 35, :stack [1 6 4 35]}
      (interpret [1 2 3 4 5 :* :* :*] {}) => {:result 120, :stack [1 120]}
      )

(fact "the interpreter does mixed operation arithmetic"
      (interpret [1 2 3 4 5 :* :+ :÷ :-] {}) => {:result 21/23, :stack [21/23]}
      (interpret [1 2 :* 3 :+ 4 :÷ 5 :-] {}) => {:result -15/4, :stack [-15/4]}
      )

(fact "the interpreter handles bound variables"
      (interpret [1 :x :+] {:x 7, :y 9}) => {:result 8, :stack [8]}
      (interpret [:y :y :÷ 111 :x :+ :*] {:x 7/4, :y 9.5}) => {:result 112.75, :stack [112.75]}
      )

;;; rubrics

(fact "we can construct rubrics and access their components"
      (let [rubric (->Rubric 1 2)]
        (:input rubric) => 1
        (:output rubric) => 2
        ))

(facts "we can compute the score of a script on a rubric"
       (fact "when the script has a constant value"
             (let [script [2 4 :+]
                   rubric (->Rubric {} 5)]
               (score-on script rubric) => 1)
             (let [script [4 20 :-]
                   rubric (->Rubric {} 5)]
               (score-on script rubric) => 21)
             (let [script [1 2 3 :+ :+]
                   rubric (->Rubric {} 6)]
               (score-on script rubric) => 0)
             (let [script [:+ :+]
                   rubric (->Rubric {} 6)]
               (score-on script rubric) => score-penalty)
             )
       (fact "when the script has a single variable"
             (let [script [2 :x :+]
                   rubric (->Rubric {:x 7} 5)]
               (score-on script rubric) => 4)
             (let [script [2 :x :+]
                   rubric (->Rubric {:x 17} 5)]
               (score-on script rubric) => 14)
             (let [script [2 :x :+]
                   rubric (->Rubric {:x -7, :y 17} 5)]
               (score-on script rubric) => 10)
             )
       )

(facts "we can compute the score of a script on a collection of rubrics"
       (fact "when the script has a constant value"
             (let [script [2 4 :+]
                   rubrics [(->Rubric {} 0) (->Rubric {} 1)
                            (->Rubric {} -1) (->Rubric {} 4)]]
               (total-score-on script rubrics) => (+ 6 5 7 2)))
       (fact "when the script has a single variable"
             (let [script [2 :x :*]
                   rubrics [(->Rubric {:x 0} 0) (->Rubric {:x 1} 1)
                            (->Rubric {:x -1} -1) (->Rubric {:x 3} 4)]]
               (total-score-on script rubrics) => (+ 0 1 1 2)))
       (fact "when the script has a two variables"
             (let [script [:y :x :*]
                   rubrics [(->Rubric {:x 0, :y 0} 0) (->Rubric {:x 1, :y -1} 1)
                            (->Rubric {:x -1, :y 7} -1) (->Rubric {:x 3, :y -2} 4)]]
               (total-score-on script rubrics) => (+ 0 2 6 10)))
       )
