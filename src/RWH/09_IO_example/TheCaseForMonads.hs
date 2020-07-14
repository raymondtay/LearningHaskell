data Term = Con Int | Div Term Term

data M a = Raise Exception | Return a
type Exception = String

{-
eval :: Term -> M Int
eval (Con a) = Return a
eval (Div t u) = 
    case eval t of 
        Raise e -> Raise e
        Return a -> 
            case eval u of 
                Raise e -> Raise e
                Return b -> 
                    if b == 0 then Raise "divide by zero" else Return (a / b)

    Variation two: State
    
    Forgetting errors for the moment, say it is desired to count the number of divisions
    performed during evaluation. In an impure language, this is easily achieved
    by the use of state. Set a given variable to zero initially, and increment it by one
    each time a division occurs.

    In a pure language, state may be mimicked by introducing a type to represent computations
    that act on state.

    type M a = State -> (a, State)
    type State = Int

-}

type S a = State -> (a, State) -- the type 'S of a' now is a function that takes in a initial state and returns the computed pair (a, State)
type State = Int

