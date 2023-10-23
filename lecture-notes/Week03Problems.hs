module Week03Problems where

import Data.Char

{------------------------------------------------------------------------------}
{- T	UTORIAL QUESTIONS                                                         -}
{------------------------------------------------------------------------------}

{- 1. Lambda notation.

   Rewrite the following functions using the '\x -> e' notation (the
   "lambda" notation), so that they are written as 'double =
   <something>', and so on. -}

mulBy2 :: Int -> Int
mulBy2 = \x -> 2*x

mul :: Int -> Int -> Int
mul = \x -> (\y -> y * x)


invert :: Bool -> Bool
invert = \b  -> if b then False else True
  {- HINT: use a 'case', or an 'if'. -}


{- 2. Partial Application

   The function 'mul' defined above has the type 'Int -> Int ->
   Int'. (a) What is the type of the Haskell expression:

       mul 10: Int -> Int

   (b) what is 'mul 10'? How can you use it to multiply a number? -}

		-- it just times a number by 10 
{- 3. Partial Application

   Write the 'mulBy2' function above using 'mul'. Can you make your
   function as short as possible? -}
   

double_v2 :: Int -> Int
double_v2 = \x -> mul x 2 -- fill this in

{- 4. Using 'map'.

   The function 'toUpper' takes a 'Char' and turns lower case
   characters into upper cases one. All other characters it returns
   unmodified. For example:

       > toUpper 'a'
       'A'
       > toUpper 'A'
       'A'

   Strings are lists of characters. 'map' is a function that applies a
   function to every character in a list and returns a new list.

   Write the function 'shout' that uppercases a string, so that:

      > shout "hello"
      "HELLO"
-}

shout :: String -> String    -- remember that String = [Char]
shout text = map toUpper text


{- 5. Using 'map' with another function.

   The function 'concat' concatenates a list of lists to make one
   list:

      > concat [[1,2],[3,4],[5,6]]
      [1,2,3,4,5,6]

   Using 'map', 'concat', and either a helper function or a function
   written using '\', write a function 'dupAll' that duplicates every
   element in a list. For example:

      > dupAll [1,2,3]
      [1,1,2,2,3,3]
      > dupAll "my precious"
      "mmyy  pprreecciioouuss"

   HINT: try writing a helper function that turns single data values
   into two element lists. -}

dupAll :: [a] -> [a]
dupAll = concatMap (\d -> [d,d])


{- 6. Using 'filter'

   (a) Use 'filter' to return a list consisting of only the 'E's in
       a 'String'.

   (b) Use 'onlyEs' and 'length' to count the number of 'E's in a string.

   (c) Write a single function that takes a character 'c' and a string
       's' and counts the number of 'c's in 's'. -}

onlyEs :: String -> String 
onlyEs a = filter (=='E') (shout a)


numberOfEs :: String -> Int
numberOfEs a = length(onlyEs a)

numberOf :: Char -> String -> Int
numberOf _ [] = 0 
numberOf c s = length(filter(==c) s)


{- 7. Rewriting 'filter'

   (a) Write a function that does the same thing as filter, using
      'map' and 'concat'.

   (b) Write a function that does a 'map' and a 'filter' at the same
       time, again using 'map' and 'concat'.
-}

filter_v2 :: (a -> Bool) -> [a] -> [a]
filter_v2 b = concatMap(\x -> if b x then [x] else []) 

--filterMap :: (a -> Maybe b) -> [a] -> [b]
--filterMap = undefinded


{- 8. Composition

   Write a function '>>>' that composes two functions. It takes two
   functions 'f' and 'g', and returns a function that first runs 'f'
   on its argument, and then runs 'g' on the result.

   HINT: this is similar to the function 'compose' in the notes for
   this week. -}

(>>>) :: (a -> b) -> (b -> c) -> a -> c
(>>>) f b c = b (f c)

{- Try rewriting the 'numberOfEs' function from above using this one. -}

{- 9. Backwards application

   Write a function of the following type that takes a value 'x' and a
   function 'f' and applies 'f' to 'x'. Note that this functions takes
   its arguments in reverse order to normal function application! -}
(|>) :: a -> (a -> b) -> b
(|>) x f = f x 


{- This function can be used between its arguments like so:

       "HELLO" |> map toLower

   and it is useful for chaining calls left-to-right instead of
   right-to-left as is usual in Haskell:

       "EIEIO" |> onlyEs |> length
-}

{- 10. Flipping

   Write a function that takes a two argument function as an input,
   and returns a function that does the same thing, but takes its
   arguments in reverse order: -}

flip :: (a -> b -> c) -> b -> a -> c
flip  f a b = f b a 

{- 11. Evaluating Formulas

   Here is a datatype describing formulas in propositional logic, as
   in CS208 last year. Atomic formulas are represented as 'String's. -}

data Formula
  = Atom String
  | And  Formula Formula
  | Or   Formula Formula
  | Not  Formula
  deriving Show
  

{- (a) Write a function that evaluates a 'Formula' to a 'Bool'ean value,
       assuming that all the atomic formulas are given the value
       'True'. Note that the following Haskell functions do the basic
       operations on 'Bool'eans:

           (&&) :: Bool -> Bool -> Bool    -- 'AND'
           (||) :: Bool -> Bool -> Bool    -- 'OR'
           not  :: Bool -> Bool            -- 'NOT'
-}

eval_v1 :: Formula -> Bool
eval_v1 (Atom _) = True
eval_v1 (And f1 f2) = eval_v1 f1 && eval_v1 f2
eval_v1 (Or f1 f2) = eval_v1 f1 || eval_v1 f2
eval_v1 (Not f) = not (eval_v1 f)



{- (b) Now write a new version of 'eval_v1' that, instead of evaluating
       every 'Atom a' to 'True', takes a function that gives a 'Bool'
       for each atomic proposition: -}

eval :: (String -> Bool) -> Formula -> Bool
eval s (Atom f) = if s f then True else False
eval s (And f1 f2) = (eval s f1) && (eval s f2)
eval s (Or f1 f2) = (eval s f1) || (eval s f2)
eval s (Not f) = not (eval s f)
{- For example:

     eval (\s -> s == "A") (Or (Atom "A") (Atom "B"))  == True
     eval (\s -> s == "A") (And (Atom "A") (Atom "B")) == False
-}

{- 12. Substituting Formulas

   Write a function that, given a function 's' that turns 'String's
   into 'Formula's (a "substitution"), replaces all the atomic
   formulas in a Formula with whatever 'f' tells it to: -}

subst :: (String -> Formula) -> Formula -> Formula
subst s (Atom f) = s f
subst s (And f1 f2) = And (subst s f1) (subst s f2)
subst s (Or f1 f2) =  Or (subst s f1) (subst s f2)
subst s (Not f) = Not (subst s f)

{- For example:

     subst (\s -> if s == "A" then Not (Atom "A") else Atom s) (And (Atom "A") (Atom "B")) == And (Not (Atom "A")) (Atom "B")
-}

{- 13. Evaluating with failure

   The 'eval' function in 8(b) assumed that every atom could be
   assigned a value. But what if it can't? Write a function of the
   following type that takes as input a function that may or may not
   give a 'Bool' for each atom, and correspondingly, may or may not
   give a 'Bool' for the whole formula. -}

evalMaybe :: (String -> Maybe Bool) -> Formula -> Maybe Bool

evalMaybe _ (Atom f) = Nothing
evalMaybe _ (And f1 f2) = Nothing
evalMaybe _ (Or f1 f2) = Nothing
evalMaybe _ (Not f) = Nothing
evalMaybe s (Atom f) = s f 

evalMaybe s (And f1 f2) = do
  result1 <- evalMaybe s f1
  result2 <- evalMaybe s f2
  return (result1 && result2)
evalMaybe s (Or f1 f2) = do
  result1 <- evalMaybe s f1
  result2 <- evalMaybe s f2
  return (result1 || result2)
evalMaybe s (Not f) = do
  result <- evalMaybe s f
  return (not result)

