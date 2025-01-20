module Lib
    ( helloFunc
    ) where

import PGF2
import qualified Data.Map as Map

helloFunc :: IO ()
helloFunc = do
    pgf <- readPGF "Hello.pgf"
    -- First let's see what languages are available
    putStrLn "Available languages:"
    print $ Map.keys (languages pgf)
    
    case Map.lookup "HelloEng" (languages pgf) of
        Nothing -> putStrLn "Could not find English language in PGF file"
        Just eng -> do
            -- Parsing example
            let res = parse eng (startCat pgf) "hello world"
            case res of
                ParseOk ((tree,prob):rest) -> do
                    putStrLn "Parse tree:"
                    print tree
                    
                    -- Try linearizing in Italian
                    case Map.lookup "HelloIta" (languages pgf) of
                        Nothing -> putStrLn "Could not find Italian language"
                        Just ita -> do
                            putStrLn "Italian translation:"
                            print $ linearize ita tree
                _ -> putStrLn "Could not parse the input"