module Huffman (encodeHuffman, decodeHuffman) where
    import Prelude
    import Data.Map (Map, fromList, (!), singleton, union)

    type CharMap = [(Char, Int)]
    type LeafQueue = [Tree Char Int]

    data Tree a b = Leaf a b | Node (Tree a b) (Tree a b) b
        deriving (Eq)

    instance (Show a, Show b) => Show (Tree a b) where
        show (Leaf x y)     = "<" ++ show x ++ " " ++ show y ++ ">"
        show (Node t1 t2 y) = "("++show t1++" + "++show t2++")" 

    get :: Tree a b -> b
    get (Leaf a b) = b
    get (Node _ _ b) = b

    leafKey :: Tree a b -> a 
    leafKey (Leaf x _) = x

    nodeLeft :: Tree a b -> Tree a b
    nodeLeft (Node t1 _ _) = t1

    nodeRight :: Tree a b -> Tree a b
    nodeRight (Node _ t2 _) = t2

    add :: CharMap -> Char -> CharMap
    add [] c = [(c, 1)]
    add ((x, y):xs) c
        | x == c = (x, y + 1):xs
        | otherwise = (x, y):(add xs c)

    insertLQ :: LeafQueue -> Tree Char Int -> LeafQueue
    insertLQ [] tree = [tree]
    insertLQ (t:lq) tree 
        | get tree < get t = tree:t:lq
        | otherwise = t:insertLQ lq tree

    charMapToQueue :: CharMap -> LeafQueue
    charMapToQueue cm = charMapToQueueHelp [] cm
        where
            charMapToQueueHelp lq [] = lq
            charMapToQueueHelp lq ((c, i):cm) = charMapToQueueHelp (insertLQ lq $ Leaf c i) cm

    mapChars :: String -> CharMap
    mapChars s = mapCharsHelp [] s
        where
            mapCharsHelp cm [] = cm
            mapCharsHelp cm (x:xs) = mapCharsHelp (add cm x) xs

    createLQ :: String -> LeafQueue
    createLQ = charMapToQueue . mapChars

    mergeLQ :: LeafQueue -> Tree Char Int
    mergeLQ [t] = t
    mergeLQ (t1:t2:ts) = mergeLQ $ insertLQ ts $ Node t1 t2 $ get t1 + get t2

    makeCode :: Tree Char Int  -> Map Char String
    makeCode t = makeCodeHelp t ""
        where
            makeCodeHelp (Leaf x _) s = singleton x s
            makeCodeHelp (Node t1 t2 _) s = union (makeCodeHelp t1 (s ++ "0")) (makeCodeHelp t2 (s ++ "1")) 

    stringToCode :: String -> Map Char String
    stringToCode = makeCode . mergeLQ . createLQ

    encodeHuffman :: String -> String
    encodeHuffman s = encodeHuffmanHelp (stringToCode s) s
        where 
            encodeHuffmanHelp code "" = ""
            encodeHuffmanHelp code (x:xs) = (code ! x) ++ encodeHuffmanHelp code xs 

    decodeHuffman :: String -> String
    decodeHuffman = id
