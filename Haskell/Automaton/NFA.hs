
module NFA where

import Data.Set (Set)
import qualified Data.Set as Set

import FARule

data NFA s c = NFA (Set s) [s] (Rulebook s c)
    deriving(Eq, Show, Read)

currentStates :: (Eq s, Eq c, Ord s) => NFA s c -> Set s
currentStates (NFA css _ rb) = followFreeMoves rb css

followFreeMoves :: (Eq s, Eq c, Ord s) => Rulebook s c -> Set s -> Set s
followFreeMoves rb ss = 
    if Set.isSubsetOf ms ss then ss else followFreeMoves rb (Set.union ss ms)
        where ms = nextStates rb ss Epsilon

nextStates :: (Eq s, Eq c, Ord s) => Rulebook s c -> Set s -> Input c -> Set s
nextStates rb ss c = 
    Set.fromList $ concatMap (\s->followRulesFor rb s c) ss

followRulesFor :: (Eq s, Eq c) => Rulebook s c -> s -> Input c -> [s]
followRulesFor rb s c = map (follow) $ rulesFor rb s c

rulesFor :: (Eq s, Eq c) => Rulebook s c -> s -> Input c -> [Rule s c]
rulesFor (Rulebook rs) s c = filter (\r->isApplies r s c) rs

accepting :: (Eq s, Eq c, Ord s) => NFA s c -> Bool
accepting nfa@(NFA current_states accept_states _) =
    Set.intersection (currentStates nfa) (Set.fromList accept_states) /= Set.empty

readCharacter :: (Eq s, Eq c, Ord s) => NFA s c -> Input c -> NFA s c
readCharacter nfa@(NFA current_states accept_states rulebook) character =
    NFA (nextStates rulebook (currentStates nfa) character) accept_states rulebook

readString :: (Eq s, Eq c, Ord s) => NFA s c -> [Input c] -> NFA s c
readString nfa (c:cs) = readString (readCharacter nfa c) cs
readString nfa [] = nfa

data NFADesign s c = NFADesign s [s] (Rulebook s c)
    deriving(Eq, Show, Read)

toNFA :: (Eq s, Eq c, Ord s) => NFADesign s c -> NFA s c
toNFA (NFADesign start_state accept_states rulebook) = 
    NFA (followFreeMoves rulebook $ Set.fromList [start_state]) accept_states rulebook

toNFAWithState :: (Eq s, Eq c, Ord s) => NFADesign s c -> Set s -> NFA s c
toNFAWithState nfa_design states = 
    let (NFA _ accept_states rulebook) = toNFA nfa_design
    in (NFA states accept_states rulebook)

accepts ::(Eq s, Eq c, Ord s) => NFADesign s c -> [c] -> Bool 
accepts nfa_design inputs =  accepting $ 
    readString (toNFA nfa_design) . map Input $ inputs
