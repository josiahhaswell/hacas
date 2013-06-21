module Hacas.Simplifications where 


import Hacas.Evaluation
import Hacas.Expression


simplify :: Expression -> Expression

{--
 - 1 * a = a
 - a * 1 = a
 --}

simplify (Arithmetic Multiply (Numeric 1) a@(Symbol _)) = a
simplify (Arithmetic Multiply a@(Symbol _) (Numeric 1)) = a


{--
 - 0 * a = 0
 - a * 0 = 0
 --}

simplify (Arithmetic Multiply b@(Numeric 0) _) = b
simplify (Arithmetic Multiply _ b@(Numeric 0)) = b

{--
 - -0 = 0
 --}
simplify (Negate (Numeric 0)) = (Numeric 0)

{--
 - -(-a) = a
 --}
simplify (Negate (Negate a)) = a

{--
 - c1:Num * c2:Num = Num(c1 * c2)
 --}

simplify (Arithmetic Multiply (Numeric c1) (Numeric c2)) = Numeric (c1 * c2)

{--
 - c1:Num + c2:Num = Num(c1 + c2)
 --}

simplify (Arithmetic Add (Numeric c1) (Numeric c2)) = (Numeric (c1 + c2))

simplify b@(Arithmetic Add (Symbol a1) (Symbol a2)) = 
  if (a1 == a2) then Arithmetic Multiply (Numeric 2) (Symbol a1) else b



{--
 - c1 * s1 + c2 * s1 = (c1 + c2) * s1
 --}

simplify e@(Arithmetic Add
  e1@(Arithmetic Multiply (Numeric c1) (Symbol s1))
  e2@(Arithmetic Multiply (Numeric c2) (Symbol s2))) = 
    if(s1 == s2) then simplify (Arithmetic Multiply (Numeric (c1 * c2)) (Symbol s1)) else e

simplify e@(Arithmetic Add
  e1@(Arithmetic Multiply (Symbol s1) (Numeric c1))
  e2@(Arithmetic Multiply (Numeric c2) (Symbol s2))) = 
    if(s1 == s2) then simplify (Arithmetic Multiply (Numeric (c1 * c2)) (Symbol s1)) else e

simplify e@(Arithmetic Add
  e1@(Arithmetic Multiply (Numeric c1) (Symbol s1)) 
  e2@(Arithmetic Multiply (Symbol s2) (Numeric c2))) = 
    if(s1 == s2) then simplify (Arithmetic Multiply (Numeric (c1 * c2)) (Symbol s1)) else e

simplify e@(Arithmetic Add
  e1@(Arithmetic Multiply (Symbol s1) (Numeric c1))
  e2@(Arithmetic Multiply (Symbol s2) (Numeric c2))) = 
    if(s1 == s2) then (Arithmetic Multiply (Numeric (c1 * c2)) (Symbol s1)) else e


{--
 - base-case:  None of the simplifiers matched
 --}
simplify expr = expr



simplifyAll :: Expression -> Expression 
simplifyAll expr = 
  let 
    result1 = simplify expr
    result2 = simplify result1
  in if (result1 /= result2) then (simplifyAll result2) else result2

