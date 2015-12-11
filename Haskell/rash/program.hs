
Sequence 
(Assign "x" (Number 1))
(
    While (Expr LessThan (Variable "x") (Number 3)) 
    (Assign "x" (Expr Add (Variable "x") (Number 1)))
)
