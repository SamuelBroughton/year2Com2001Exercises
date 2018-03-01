
-- PROBLEM 1

{- What an Abstract Data Type (ADT) is and why they are useful in Software Engineering
 An abstract data type is a type with associated operations but whose representation is hidden. 
 The data is abstract because is leaves some aspects if its structure undefined, to be provided by the user.
 
 ADT's use Sorts Syntax and Semantics.
 Sorts define all the other types the data type needs.
 Syntax defines what functions and constants are being used.
 Semantics define the functions in terms of one another.
 
 ADT's are useful in Software Engineering because they provide a specification as to what the type can do and is used for.
-}

-- PROBLEM 2 (i)

{-
 addEntry d (removeEntry (addEntry c (removeEntry (addEntry b (addEntry a (emptyQ))))))
-}

-- PROBLEM 2 (ii)

{-
 Above solution becomes:
   addEntry d (removeEntry (addEntry c (removeEntry (addEntry b (addEntry a (emptyQ))))))
  =addEntry d (removeEntry (addEntry c (addEntry b (removeEntry (addEntry a (emptyQ))))))
  =addEntry d (removeEntry (addEntry c (addEntry b (emptyQ))))
  =addEntry d (addEntry c (removeEntry (addEntry b (emptyQ))))
  =addEntry d (addEntry c (emptyQ))
 
 Front of queue:
   frontEntry (addEntry d (addEntry c (emptyQ)))
  =frontEntry addEntry c (emptyQ)
  =c 
-}

-- PROBLEM 3 (i)

module Q (Q, emptyQ, addEntry, removeEntry, frontEntry, isEmpty) where
 
 -- example queue = [x5, x4, x3, x2, x1, x0] <- front of queue
 
 data Q a = Q [a]
 

 type MsgQ a = Q a
 type MsgE a = a
 
 msgNoQueue :: MsgQ a
 msgNoQueue = error "Result is not a valid queue"
 
 msgNoEntry :: MsgE a
 msgNoEntry = error "Result is not a valid entry"
 
 emptyQ :: Q a
 emptyQ = Q []
 
 addEntry :: a -> Q a -> Q a
 addEntry x (Q xs) = Q (x : xs)
 
 removeEntry :: Q a -> Q a
 removeEntry (Q ys) = case ys of
  []     -> msgNoQueue
  [x]    -> emptyQ
  x : xs -> addEntry x (removeEntry (Q xs))
 
 frontEntry :: Q a -> a
 frontEntry (Q ys) = case ys of
  []     -> msgNoEntry
  [x]    -> x
  x : xs -> frontEntry (Q xs)
  
 isEmpty :: Q a -> Bool
 isEmpty (Q []) = True
 isEmpty _  = False
 
 -- PROBLEM 3 (ii)
 
 -- testing prop.7
 -- if (isEmpty q) then x
 -- Should return 5
 testProp7 :: Int
 testProp7 = frontEntry (addEntry 5 emptyQ)
 
 -- testing prop.8
 -- else frontEntry q
 -- Should return 2
 testProp8 :: Int
 testProp8 = frontEntry (addEntry 5 (Q [2,2]))
 