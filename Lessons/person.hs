data Person = Person { first :: String
                     , last :: String
                     , age :: Int
                     , phone :: String
                     , note :: String
                     } deriving (Show)

data Car a b c = Car { company :: a  
                     , model :: b  
                     , year :: c   
                     } deriving (Show) 