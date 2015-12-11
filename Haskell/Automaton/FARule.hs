
module FARule where

import Data.List

data Input a = Epsilon | Input a
    deriving (Eq, Show, Read)

data Rule s c = Rule s (Input c) s
    deriving (Eq, Show, Read)

data Rulebook s c = Rulebook [Rule s c]
    deriving (Eq, Show, Read)

follow :: Rule s c -> s 
follow (Rule s c n) = n

isApplies :: (Eq s, Eq c) => Rule s c -> s -> Input c -> Bool
isApplies (Rule s c _) s' c' = s == s' && c == c'  

alphabet :: Rulebook s c -> [Input c]
alphabet (Rulebook rules) = map (\(Rule _ c _)->c) rules
