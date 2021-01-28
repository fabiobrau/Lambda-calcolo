m = Abs "x" (App (Var "x") (Var "y"))
n = Abs "z" (App (Var "z") (Var "x"))

is_free "x" m
is_bound "x" m

l2s (subs "x" m (Var "x"))
l2s (subs "y" m n)

alpha_eq m n

alpha_eq m (subs "x" n (Var "y"))
