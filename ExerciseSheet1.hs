
-- PROBLEM 1 (a)
data Nat = Zero | Succ Nat 
           deriving (Eq)

instance Num Nat where
 x + Zero = x
 x + (Succ n) = Succ (x + n)
 
 Zero - _ = Zero
 x - Zero = x
 (Succ u) - (Succ v) = u - v
 
 x * Zero = Zero
 x * (Succ n) = x * n + x
 
 signum Zero = 0
 signum _ = 1
 
 fromInteger n
  |n <= 0 = 0
  |otherwise = Succ (fromInteger (n-1))
  
-- PROBLEM 1 (b)
{-
instance Show Nat where
 show = show n toInteger
      where toInteger Zero = 0
            toInteger (Succ n) = 1 + toInteger n
-}

-- PROBLEM 2
class (Eq cfg) => Model cfg where
 initialise :: String -> cfg
 acceptState :: cfg -> Bool
 doNextMove :: cfg -> cfg
 runFrom :: cfg -> cfg
 runModel :: String -> cfg
 runModel = runFrom . initialise

type Transitions s = [(s, Char, Char, Char, s)]

class (Eq s) => PDA s where
 transitions :: Transitions s
 initialState :: s
 haltStates :: [s]
 
data PDAConfig s = PDAConfig {
 state :: s,
 stack :: String,
 input :: String
 } deriving (Eq, Show)
 
instance (PDA s) => Model (PDAConfig s) where
 -- initialise :: String -> PDAConfig s
 initialise str = PDAConfig initialState [] str
 
 -- acceptState :: PDAConfig s -> Bool
 acceptState (PDAConfig s stk i) = (elem s haltStates) && (null stk) && (null i)
 
 -- doNextMove :: PDAConfig s -> PDAConfig s
 doNextMove cfg@(PDAConfig s stk i)
  |null i = cfg
  |acceptState cfg = cfg
  |otherwise = (PDAConfig s' stk' i')
  where
   (s', stk', i') = if (null next3) then (s, stk, i)
                    else head next3
   next3 = [ (s2, adjust x y stk, tail i)
            | (s1, c, x, y, s2) <- transitions
            , s1 == s
            , enabled x stk
            , c == head i ]
   enabled x stk = (x == '0') || (if (null stk) then False else (x == head stk))
   adjust x y stk = case (x, y, stk) of
    ('0', '0', _ ) -> stk
    ('0', y, _ ) -> y : stk
    (x, '0', (_:ts)) -> ts
    (x, y, (_:ts)) -> y : ts
    
  -- runFrom :: PDAConfig s -> PDAConfig s
 runFrom cfg@(PDAConfig s stk i)
  |null i = cfg
  |acceptState cfg = cfg
  |isStuck cfg = cfg
  |otherwise = runFrom (doNextMove cfg)
  where
   isStuck cfg@(PDAConfig s stk i) = null moves
   moves = [ (s1, c, x, y, s2) | (s1, c, x, y, s2) <- transitions
                               , s1 == s, c == head i
                               , (x == '0' || (if (null stk) then False else (x == head stk))) ]

data MyStates = A | B
               deriving (Show, Eq)
               
instance PDA MyStates where
 transitions = [ (A, 'a', '0', 'u', B),
                 (B, 'b', 'u', '0', B)]
 initialState = A
 haltStates = [B]

-- so the strings it can recognise are only {ab}
recognises :: String -> Bool
recognises str = acceptState ( runModel str :: PDAConfig MyStates )
 
 
 
 
 
 
 
 
 