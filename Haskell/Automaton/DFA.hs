
module DFA where

import Data.List
import FARule

data DFA s c = DFA s [s] (Rulebook s c)
    deriving(Eq, Show, Read)

nextState :: (Eq s, Eq c) => Rulebook s c -> s -> Input c -> s
nextState rb s c = follow $ ruleFor rb s c

ruleFor :: (Eq s, Eq c) => Rulebook s c -> s -> Input c -> Rule s c
ruleFor (Rulebook rs) s c = helper $ find (\r->isApplies r s c) rs
    where helper (Just n) = n
          helper (Nothing) = error "unexpected input"

accepting :: (Eq s) => DFA s c -> Bool
accepting (DFA current_state accept_states _) =
    any (== current_state) accept_states

readCharacter :: (Eq s, Eq c) => DFA s c -> Input c -> DFA s c
readCharacter (DFA current_state accept_states rulebook) character =
    DFA (nextState rulebook current_state character) accept_states rulebook

readString :: (Eq s, Eq c) => DFA s c -> [Input c] -> DFA s c
readString dfa (c:cs) = readString (readCharacter dfa c) cs
readString dfa [] = dfa

data DFADesign s c = DFADesign s [s] (Rulebook s c)
    deriving(Eq, Show, Read)

toDfa :: (Eq s, Eq c) => DFADesign s c -> DFA s c
toDfa (DFADesign start_state accept_states rulebook) = 
    DFA start_state accept_states rulebook

accepts ::(Eq s, Eq c) => DFADesign s c -> [c] -> Bool 
accepts dfa_design inputs =  accepting $
    readString (toDfa dfa_design) . map Input $ inputs
