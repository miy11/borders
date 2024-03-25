module Tabulate where

import Data.Char (isDigit, isSpace)
import Data.Function
import Data.List (dropWhileEnd, isInfixOf)
import Text.Regex

strip :: String -> String
strip  = dropWhileEnd isSpace . dropWhile isSpace

strContainsNum :: String -> Bool
strContainsNum = any (isDigit)

borders :: [String] -> [(String, [String])]
borders sl = 
  let sl' = concatMap (\line -> 
            if (strContainsNum line) then  --In the format, if there are these random numbers within the line, then it is the country/region row
              [line, splitRegex (mkRegex "[0-9].*?\t") line & last]
            else
              [line]
            ) sl &  -- My copy-paste resulted in the newline between regions and the borders they share to be missing, so this will fix that
          zip [0..]
  in filter (strContainsNum . snd) sl' &  --Check if the line contains a number. If so, then it is the "source" country/region
  map (\e -> (processRegion (snd e), regionBorders (map snd sl') (fst e))) & --Gets the correlation between the countries/regions and the borders
  map (\e -> (strip . fst $ e, map strip . snd $ e)) &  --Remove excess whitespace
  filter (not . null . snd)  --Remove countries/regions that do not have a land border
  --TODO: Format into CSV or something?

processRegion :: String -> String  --Gets rid of all the useless crap
processRegion t = 
  splitRegex (mkRegex "[0-9].*") t & head & --Get the first part of the string, which is the country/region name
  (\s -> subRegex (mkRegex "\\[.*\\]") s "")  --Removes Wikipedia's notes like [a] [b] etc.

processBorder :: String -> String  --Gets rid of all the useless crap
processBorder t = 
  subRegex (mkRegex "\\(L\\)") t "" & --Removes the marker (L) for land-only border
  (\s -> subRegex (mkRegex "\\[.*\\]") s "")  --Removes Wikipedia's notes like [a] [b] etc.

regionBorders :: [String] -> Int -> [String]  --Finds all the borders of a country/region
regionBorders sl i = 
  drop (i + 1) sl &  --Skips past the country/region name and everything before it
  let loop l = 
              if ((strContainsNum . head) l) then  --Skips past everything belonging to the the next country/region
                []
              else if (length . tail) l > 1 then
                (if head l & (\r -> isInfixOf "(M)" r || isInfixOf "None" r) then  --Is a maritime border, ignore it
                  loop (tail l)  
                else
                  [head l & processBorder] ++ loop (tail l))  --Adds the country/region that is bordering
              else
                [last l & processBorder]  --End of list
  in loop