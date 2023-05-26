module Lang where

import Turtle

-- added new expressions, for example Eq which refers to a==b or similar
data Expr = Val Value
          | Point (Expr, Expr)
          | Var String
          | Add Expr Expr
          | Mul Expr Expr
          | Sub Expr Expr
          | Div Expr Expr
          | Greater Expr Expr
          | Eq Expr Expr
          | Less Expr Expr
          | GE Expr Expr
          | LE Expr Expr
          | And Expr Expr
          | Or Expr Expr
          | Not Expr

  deriving Show

data TurtleCmd = Fd Expr
               | Rt Expr
               | Lt Expr
               | PenDown
               | PenUp
               | ChangeCol String
               | MoveTo Expr
  deriving Show

data Program = T TurtleCmd
             | Call String [Expr]
             | Seq [Program]
             | If Expr Program Program
             | Assign Expr Expr
             | Repeat Expr Program
             | Loop Program Expr Program Program
             | Empty
             | Precision Int
  deriving Show

data Function = Fn { arguments :: [String],
                     definition :: Program }
                     deriving Show
type Definitions = [(String, Function)]

-- changed to value instead of float in the tuple
type Locals = [(String, Value)]




--creates a function which takes in locals and returns a function that turns expressions to floats
eval :: Locals -> Definitions -> Expr -> Value
eval locs defs (Val value) = value
eval locs defs (Var name) = case lookup name locs of
  Just val -> val
  Nothing -> error ("No such variable" ++ show name)
eval locs defs (Add expr1 expr2) = (eval locs defs expr1) + (eval locs defs expr2)
eval locs defs (Mul expr1 expr2) = (eval locs defs expr1) * (eval locs defs expr2)
eval locs defs (Sub expr1 expr2) = (eval locs defs expr1) - (eval locs defs expr2)
eval locs defs (Div expr1 expr2) = (eval locs defs expr1) / (eval locs defs expr2)
--The below evals are all corresponding to comparison operations. This is where
--the user defined precision comes in play. The float values are compared to a
--certain level of decimal places.
eval locs defs (Greater expr1 expr2) | (valToFloat defs (eval locs defs expr1)) > (valToFloat defs (eval locs defs expr2)) = Num 1
                                     | otherwise = Num 0
eval locs defs (Less expr1 expr2) | (valToFloat defs (eval locs defs expr1)) < (valToFloat defs (eval locs defs expr2)) = Num 1
                                  | otherwise = Num 0
eval locs defs (GE expr1 expr2) | (valToFloat defs (eval locs defs expr1)) >= (valToFloat defs (eval locs defs expr2)) = Num 1
                                | otherwise = Num 0
eval locs defs (LE expr1 expr2) | (valToFloat defs (eval locs defs expr1)) <= (valToFloat defs (eval locs defs expr2)) = Num 1
                                | otherwise = Num 0
eval locs defs (Eq expr1 expr2) | (valToFloat defs (eval locs defs expr1)) == (valToFloat defs (eval locs defs expr2)) = Num 1
                                | otherwise = Num 0
eval locs defs (And expr1 expr2) | ((valToFloat defs (eval locs defs expr1))/=0) && ((valToFloat defs (eval locs defs expr2))/=0) = Num 1
                                 | otherwise = Num 0
eval locs defs (Or expr1 expr2) | ((valToFloat defs (eval locs defs expr1))/=0) || ((valToFloat defs (eval locs defs expr2))/=0) = Num 1
                                | otherwise = Num 0
eval locs defs (Not expr)  | (valToFloat defs (eval locs defs expr)) == 0 = Num 1
                           | otherwise = Num 0

--the functions below are used when comparing two flots eg Num 0.2 and Num 0.222
--the user defines the amount of accuracy.
valToFloat :: Definitions -> Value -> Float
valToFloat defs (Num float) = (fromInteger ( round ( float * (10^(findPrec defs)) / (10.0^^(findPrec defs)))))

findPrec :: Definitions -> Int
findPrec defs = case lookup "!" defs of
  Just funct -> precision
                where
                  precision = getPrecision (definition funct)
  Nothing -> error ("This will never happen!")

getPrecision :: Program -> Int
getPrecision (Precision precision) = precision
--

--add to the locals list, if the variable was already existing update it
addToLocs :: Locals -> Value -> String -> Locals
addToLocs locs val name = [x | x<-locs, (fst x /= name)] ++ [(name,val)]

interp :: Definitions -> Locals -> Program -> Either [Command] String
--Sequencing is used to encapsulate the notion of variable assignment.

--flattening nested sequences, very important for variable assignment
interp defs locs (Seq ((Seq prog):progs))
  = interp defs locs (Seq (prog++progs))


--In this example, if variables are defined within an if statement, and there
--is code outside the if statement, then the programs are sequenced together
--so they share the same locals!
interp defs locs (Seq ((If expr prog1 prog2):progs))
  | valToFloat defs (eval locs defs expr) /= 0 = interp defs locs (Seq (prog1:progs))
  | otherwise = interp defs locs (Seq (prog2:progs))

--here a loop with assignment collapses to one without assignment and a newly
--updated locs
interp defs locs (Seq (Loop (Assign (Var name) expr) (condition) (update) (body):progs))
  = interp defs (addToLocs locs (eval locs defs expr) name) (Seq (Loop (Empty) (condition) (update) (body):progs))

--again, sequencing is used to both sequence the events in the loop and the events
--after the loop.
interp defs locs (Seq (Loop (Empty) (condition) (update) (body):progs))
  | valToFloat defs (eval locs defs condition) /= 0 = interp defs locs (Seq (body : [update, Loop (Empty) (condition) (update) (body)] ++ progs))
  | otherwise = interp defs locs (Seq progs) --only the last progs is taken

--assignment updates locs and interprets the next part of the program with the
--new locs
interp defs locs (Seq ((Assign (Var name) expr):progs))
  = interp defs (addToLocs locs (eval locs defs expr) name) (Seq progs)

--repeat statements are made into a sequence of programs, joining with the
--program proceeding it
interp defs locs (Seq ((Repeat (expr) (prog)):progs))
  | valToFloat defs (eval locs defs expr) == 0 = Left [nothing]
  | valToFloat defs (eval locs defs expr) == 1 = interp defs locs (Seq (prog:progs))
  | otherwise = interp defs locs (Seq ([prog,(Repeat (Sub (expr) (Val (Num 1))) prog)] ++ progs))


--interpreate a sequence of programs that do not impact each other
interp defs locs (Seq (prog:progs))
  = case (interp defs locs prog) of
      Left command -> case (interp defs locs (Seq progs)) of
                            Left command2 -> Left (command ++ command2)
                            Right string -> Right string
      Right message -> Right message

--standalone programs, have no program which they will impact after it.
interp defs locs (Repeat (expr) (prog))
  | valToFloat defs (eval locs defs expr) == 0 = Left [nothing]
  | otherwise = interp defs locs (Seq [prog, Repeat (Sub (expr) (Val (Num 1))) prog])

interp defs locs (Loop (Assign (Var name) expr) (condition) (update) (body))
  = interp defs (addToLocs locs (eval locs defs expr) name) (Loop (Empty) (condition) (update) (body))
interp defs locs (Loop (Empty) (condition) (update) (body))
  | valToFloat defs (eval locs defs condition) /= 0 = interp defs locs (Seq [body, update, Loop (Empty) (condition) (update) (body)])
  | otherwise = Left [nothing]

interp defs locs (If expr prog1 prog2)
  | valToFloat defs (eval locs defs expr) /= 0 = interp defs locs prog1
  | otherwise = interp defs locs prog2

-- the below code has not changed from the base practical, with the exception
-- adding constructors for the Either monad. Also change colour, pen up/down
-- were changed, and move to were added.
interp defs locs (Call f args)
    = case lookup f defs of
        Just (Fn pargs prog) ->
            interp defs (zip pargs (map (eval locs defs) args)) prog
        Nothing   -> Right ("No such function " ++ show f)
interp defs locs (T (Fd expr)) = Left [(fd (eval locs defs expr))]
interp defs locs (T (Rt expr)) = Left [(rt (eval locs defs expr))]
interp defs locs (T (Lt expr)) = Left [(rt (eval locs defs (Sub (Val (Num 0)) (expr))))]
interp defs locs (T (PenDown)) = Left [(penDown 1)]
interp defs locs (T (PenUp)) = Left [(penDown 0)]
interp defs locs (T (MoveTo (Point (expr1,expr2)))) = Left [(moveTo ((eval locs defs expr1),(eval locs defs expr2)))]
interp defs locs (T (ChangeCol colorString)) = case stringToCol colorString of
                                                   Just colour -> Left [(changeCol colour)]
                                                   Nothing   -> Right ("No such colour " ++ colorString)
interp defs locs _ = Left [nothing]
