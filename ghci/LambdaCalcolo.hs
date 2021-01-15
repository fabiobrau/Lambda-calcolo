-- Definition of lambda terms
data LamTerm = Var String | Abs String LamTerm | App LamTerm LamTerm

-- Function that check if a variable is free
is_free x (Var y)     = (x == y)
is_free x (Abs y e1)  = if (x==y) then False else is_free x e1
is_free x (App e1 e2) = is_free(e1) || is_free(e2) 

-- Function that check if a variable is bound
is_bound x (Var y)     = False
is_bound x (Abs y e1)  = if (x==y) then True else is_bound x e1
is_bound x (App e1 e2) = is_bound(e1) || is_bound(e2) 

-- Function that generate fresh Var
-- fresh a --> String, return a string the is not free and not bound in a 

-- Substitution function:
-- (subs x m n) substitutes the variable x in m with the lambda term n
--
-- Caso 1
subs x (Var y) n = if (x == y) then n else (Var y)

-- Caso 2
subs x (App m1 m2) n = App (subs x m1 n) (subs x m2 n)

-- Caso 3
subs x (Abs y m) n = if (x == y) 
                        then (Abs y m) 
                    else 
                        if not ((is_free x m) && (is free y n))
                           then Abs y (subs x m n)
                        else 
                            Abs (fresh (App m n)) (subs x (subs y m (Var (fresh (App m n)))) n)
                            
-- Check for Alpha equivalence
alpha_eq (Var x) (Var y) = (x == y)
alpha_eq (App m1 m2) (App n1 n2) = (alpha_eq m1 n1) && (alpha_eq m2 n2)
alpha_eq (Abs x m) (Abs y n) = if (x==y) 
                                  then alpha_eq m n
                               else
                                  (not ((is_free y m)&&(is_free x n))) && (alpha_eq m (subs y n (Var y)))
-- Spiurious cases
alpha_eq (Var x) (App m n) = False
alpha_eq (App m n) (Var x) = False
alpha_eq (Var x) (Abs y m) = False
alpha_eq (Abs x m) (Var y) = False
alpha_eq (App m1 m2) (Abs x n) = False
alpha_eq (Abs x n) (App m1 m2) = False

