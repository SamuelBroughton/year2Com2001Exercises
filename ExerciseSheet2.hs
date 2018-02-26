
class (Ord gs) => GradingSystem gs where
 isPass :: gs -> Bool
 isFail :: gs -> Bool
 
 -- default
 isFail = not . isPass
 isPass = not . isFail
 
-- PROBLEM 2
data SchoolRes = A | B | C | D | E | U
                 deriving (Eq, Ord)
                
instance GradingSystem SchoolRes where
 isPass grade
  |grade <= C = True
  |otherwise = False
  
data UniRes = Percentage Int | NC
              deriving (Eq, Ord)
              
instance GradingSystem UniRes where
 isPass grade = case grade of
                 Percentage n -> (n >=40)
                 NC           -> False
                
data UniDeg = First | UpperSecond | LowerSecond |Third | Pass | Fail
              deriving (Eq, Ord)
              
data Hons = WithHons | WithoutHons
            deriving (Eq, Ord)

data Degree = Degree UniDeg Hons
              deriving (Eq, Ord)
              
instance GradingSystem Degree where
 isPass (Degree Fail _) = False
 isPass _ = True
 
 
-- PROBLEM 4
data UserInfo = UserInfo {
 name :: String,
 phoneNumber :: Integer
 }
 
instance Show UserInfo where
 show (UserInfo n p) = "name: " ++ n ++ ", tele: " ++ show p
 
bob :: UserInfo
bob = UserInfo "Bob" 1234567

-- PROBLEM 5
data PhoneNumber = Phone Int | Unlisted

data UserInfo5 = UserInfo5 {
 name5 :: String,
 phoneNumber5 :: PhoneNumber
 }
 
instance Show UserInfo5 where
 show (UserInfo5 n p) = "name: " ++ n ++ ", tele: " ++ show number
                        where
                         number = case p of
                          Phone n  -> show n
                          Unlisted -> "unlisted"
 
mary :: UserInfo5
mary = UserInfo5 "Mary" Unlisted
 