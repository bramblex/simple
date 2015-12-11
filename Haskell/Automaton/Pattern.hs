
module Pattern where

import Unique
import FARule
import NFA

data Pattern = Empty
             | Literal Char
             | Concatenate Pattern Pattern
             | Choose Pattern Pattern
             | Repeat Pattern

class Precedence a where
    precedence :: a -> Int
    precedence a = 0

class (Precedence a, Show a) => Bracket a where
    bracket :: a -> a -> String
    bracket self outer = if (precedence self) < (precedence outer)
                         then "(" ++ show self ++ ")"
                         else show self

instance Precedence Pattern where
    precedence Empty = 3
    precedence (Literal _) = 3
    precedence (Concatenate _ _) = 1
    precedence (Choose _ _) = 0
    precedence (Repeat _) = 2

instance Bracket Pattern
    
instance Show Pattern where
    show Empty = ""
    show (Literal c) = [c]
    show self@(Concatenate first second) = 
        bracket first self ++ bracket second self
    show self@(Choose first second) =
        bracket first self ++ "|" ++ bracket second self
    show self@(Repeat pattern) = bracket pattern self ++ "*"

toNFADesign :: Pattern -> Unique (NFADesign Int Char)
toNFADesign Empty = do 
    start_state <- newUnique
    return (NFADesign start_state [start_state] (Rulebook []))

toNFADesign (Literal character) = do
    start_state <- newUnique
    accept_state <- newUnique
    let rulebook = Rulebook [Rule start_state (Input character) accept_state]
    return $ NFADesign start_state [accept_state] rulebook

toNFADesign (Concatenate first second) = do
    (NFADesign first_start_state first_accept_states (Rulebook first_rules)) 
        <- toNFADesign first
    (NFADesign second_start_state second_accept_states (Rulebook second_rules)) 
        <- toNFADesign second
    let start_state = first_start_state
    let accept_states = second_accept_states
    let rules = first_rules ++ second_rules
    let extra_rules = [(Rule s Epsilon n) | s <- first_accept_states, n <- [second_start_state]]
    let rulebook = Rulebook (rules ++ extra_rules)
    return $ NFADesign start_state accept_states rulebook
    
toNFADesign (Choose first second) = do
    (NFADesign first_start_state first_accept_states (Rulebook first_rules)) 
        <- toNFADesign first
    (NFADesign second_start_state second_accept_states (Rulebook second_rules)) 
        <- toNFADesign second
    start_state <- newUnique
    let accept_states = first_accept_states ++ second_accept_states
    let rules = first_rules ++ second_rules
    let extra_rules = [(Rule start_state Epsilon n) | n <- [first_start_state, second_start_state]]
    let rulebook = Rulebook (rules ++ extra_rules)
    return $ NFADesign start_state accept_states rulebook

toNFADesign (Repeat pattern) = do
    (NFADesign pattern_start_state pattern_accept_states (Rulebook pattern_rules)) 
        <- toNFADesign pattern
    start_state <- newUnique
    let accept_states = pattern_accept_states ++ [start_state]
    let rules = pattern_rules
    let extra_rules = [(Rule accept_state Epsilon pattern_start_state)| accept_state <- pattern_accept_states] ++ [Rule start_state Epsilon pattern_start_state]
    let rulebook = Rulebook (rules ++ extra_rules)
    return $ NFADesign start_state accept_states rulebook

matches :: Pattern -> [Char] -> Bool
matches pattern str = evalUnique $ do
    pattern_nfa_design <- toNFADesign pattern 
    return (accepts pattern_nfa_design str)
