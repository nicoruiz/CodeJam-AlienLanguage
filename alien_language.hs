import Char
import Prelude hiding (snd, fst)

main = do 
    fileName <- getLine
    content <- readFile fileName 
    let parsed = getParsed content
    writeFile "output.txt" (getOutput 1 ((fst parsed)!!0) (snd parsed) (thr parsed))

--------------------
--- SUB METHODS ----
--------------------
file = "3 5 4\nabc\nbca\ncbc\ncbc\ncbc\n(cb)ac\n(cb)a(ca)\n(ba)aa\n(ab)dc\n"
example = "3 5 4\nabc\nbca\ndac\ndbc\ncba\n(ab)(bc)(ca)\nabc\n(abc)(abc)(abc)\n(zyx)bc\n"

--Toma los numeros del input file
getParsed :: String -> ([Int],[String],[[String]]) 
getParsed content = let (inputsAsString,rest) = splitAt 1 (lines content)
                    in let inputs = toIntList inputsAsString
                        in let (words, patterns) = splitAt (inputs!!1) rest
                            in (inputs, words, combinePatterns patterns)

--Agrupa los patrones en lista de listas
combinePatterns :: [String] -> [[String]]
combinePatterns [] = []
combinePatterns (p:ps) = (sliceParenthesis p False "") : combinePatterns ps

--Elimina los parentesis de los patrones
sliceParenthesis :: String -> Bool -> String -> [String]
sliceParenthesis [] _ accum = filter (\x -> x /= "") [accum]
sliceParenthesis (s:ss) isInside accum = if isInside && s /= ')'
                                            then sliceParenthesis ss True (accum ++ [s])
                                            else if isInside && s == ')'
                                                then accum : (sliceParenthesis ss False "")
                                                else if s == '('
                                                    then sliceParenthesis ss True ""
                                                    else [s] : sliceParenthesis ss False ""

getOutput :: Int -> Int -> [String] -> [[String]] -> String
getOutput acc lengthWords ws [] = ""
getOutput acc lengthWords ws (p:ps) = ("Case #" ++ show acc ++ ": " ++ show (getMatches lengthWords ws p) ++ "\n") ++ (getOutput (acc+1) lengthWords ws ps)

getMatches :: Int -> [String] -> [String] -> Int
getMatches _ [] _ = 0
getMatches lengthWords (w:ws) ps = (if (hasOcurrence 0 lengthWords w ps) then 1  else 0) + getMatches lengthWords ws ps 

hasOcurrence:: Int -> Int -> String -> [String] -> Bool
hasOcurrence pos lengthWords w ps = if pos < lengthWords
    then elem (w!!pos) (ps!!pos) && hasOcurrence (pos+1) lengthWords w ps
    else True

--------------------
---- AUXILIARES ----
--------------------
split :: Eq a => a -> [a] -> [[a]]
split d [] = []
split d s = x : split d (drop 1 y) where (x,y) = span (/= d) s

toIntList :: [String] -> [Int]
toIntList inputs = map read (split ' ' (head inputs))

fst :: (a,b,c) -> a
fst (x,y,z) = x

snd :: (a,b,c) -> b
snd (x,y,z) = y

thr :: (a,b,c) -> c
thr (x,y,z) = z