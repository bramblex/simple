
module NFASimulation where

import Data.Set (Set)
import qualified Data.Set as Set

import FARule
import NFA hiding (rulesFor)

import DFA (DFADesign)
import qualified DFA as DFA

data NFASimulation s c = NFASimulation (NFADesign s c)
    deriving(Eq, Show, Read)

nextState :: (Eq s, Eq c, Ord s) => NFASimulation s c -> Set s -> Input c -> Set s
nextState (NFASimulation nfa_design) states character = 
    currentStates (readCharacter nfa character)
        where nfa = toNFAWithState nfa_design states

rulesFor :: (Eq s, Eq c, Ord s) => NFASimulation s c -> Set s -> [Rule (Set s) c]
rulesFor nfa_simulation@(NFASimulation (NFADesign _ _ rulebook)) states = 
    map (\c->(Rule states c (nextState nfa_simulation states c))) (alphabet rulebook)

discoverStatesAndRules :: (Eq s, Eq c, Ord s) =>
        NFASimulation s c -> Set (Set s) -> (Set (Set s), [Rule (Set s) c])
discoverStatesAndRules nfa_simulation states =
    if Set.isSubsetOf more_states states 
    then (states, rules)
    else discoverStatesAndRules nfa_simulation (Set.union states more_states)
        where rules = concatMap (\s-> rulesFor nfa_simulation s) states
              more_states = Set.fromList $ map follow rules

toDFADesign :: (Eq s, Eq c, Ord s) => NFASimulation s c -> DFADesign (Set s) c
toDFADesign nfa_simulation@(NFASimulation nfa_design) = 
    DFA.DFADesign start_state accept_states (Rulebook rules)
        where start_state = currentStates . toNFA $ nfa_design
              (states, rules) = discoverStatesAndRules nfa_simulation $ 
                  Set.fromList [start_state]
              accept_states =  flip filter (Set.toList states) $ 
                  (\s-> accepting . flip toNFAWithState s $ nfa_design)
