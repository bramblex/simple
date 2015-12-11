
module Main where
import PatternParser

main :: IO ()
main = do 
    pattern <- getLine
    interact (unlines . map (show . matchStr pattern) . lines)
