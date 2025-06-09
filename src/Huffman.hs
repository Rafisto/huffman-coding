module Huffman (encodeHuffman, decodeHuffman) where
    import Prelude
    import qualified Data.ByteString as BS
    import qualified Data.ByteString.Lazy as BL
    import qualified Data.ByteString.Internal as BSI
    import qualified Data.Map as Map
    import Data.Map (Map, (!), singleton, union, toList, fromList)
    import Data.Bits (shiftL, (.|.), testBit)
    import Data.Binary.Put (Put, runPut, putWord32be, putWord16be, putByteString)
    import Data.Binary.Get (Get, runGet, getWord32be, getWord16be, getByteString)
    import Foreign.ForeignPtr (withForeignPtr)
    import Foreign.Ptr (plusPtr)
    import Foreign.Storable (poke)
    import Data.Word (Word8)

    type CharMap = [(Char, Int)]
    type LeafQueue = [Tree Char Int]

    data Tree a b = Leaf a b | Node (Tree a b) (Tree a b) b
        deriving (Eq)

    instance (Show a, Show b) => Show (Tree a b) where
        show (Leaf x y)     = "<" ++ show x ++ " " ++ show y ++ ">"
        show (Node t1 t2 _) = "("++show t1++" + "++show t2++")"

    get :: Tree a b -> b
    get (Leaf _ b) = b
    get (Node _ _ b) = b

    add :: CharMap -> Char -> CharMap
    add [] c = [(c, 1)]
    add ((x, y):xs) c
        | x == c = (x, y + 1):xs
        | otherwise = (x, y):add xs c

    mapChars :: String -> CharMap
    mapChars = mapCharsHelp []
        where
            mapCharsHelp cm [] = cm
            mapCharsHelp cm (x:xs) = mapCharsHelp (add cm x) xs

    createLQ :: String -> LeafQueue
    createLQ = charMapToQueue . mapChars

    insertLQ :: LeafQueue -> Tree Char Int -> LeafQueue
    insertLQ [] tree = [tree]
    insertLQ (t:lq) tree
        | get tree < get t = tree:t:lq
        | otherwise = t:insertLQ lq tree

    mergeLQ :: LeafQueue -> Tree Char Int
    mergeLQ [] = error "Cannot merge an empty queue"
    mergeLQ [t] = t
    mergeLQ (t1:t2:ts) = mergeLQ $ insertLQ ts $ Node t1 t2 $ get t1 + get t2

    charMapToQueue :: CharMap -> LeafQueue
    charMapToQueue = charMapToQueueHelp []
        where
            charMapToQueueHelp lq [] = lq
            charMapToQueueHelp lq ((c, i):cn) = charMapToQueueHelp (insertLQ lq $ Leaf c i) cn

    makeCode :: Tree Char Int  -> Map Char String
    makeCode t = makeCodeHelp t ""
        where
            makeCodeHelp (Leaf x _) s = singleton x s
            makeCodeHelp (Node t1 t2 _) s = union (makeCodeHelp t1 (s ++ "0")) (makeCodeHelp t2 (s ++ "1"))

    stringToCode :: String -> Map Char String
    stringToCode = makeCode . mergeLQ . createLQ

    serializeCode :: Map Char String -> BS.ByteString
    serializeCode m = BL.toStrict $ runPut $ do
        putWord32be (fromIntegral $ Map.size m)
        mapM_ putEntry (Map.toList m)
        where
            putEntry :: (Char, String) -> Put
            putEntry (c, bits) = do
                putWord16be (fromIntegral $ fromEnum c)
                putWord16be (fromIntegral $ length bits)
                putByteString $ packBits bits

    deserializeCode :: BS.ByteString -> Map Char String
    deserializeCode bs = runGet getMap (BL.fromStrict bs)
        where
            getMap :: Get (Map Char String)
            getMap = do
                n <- getWord32be
                entries <- sequence $ replicate (fromIntegral n) getEntry
                return $ Map.fromList entries

            getEntry :: Get (Char, String)
            getEntry = do
                c <- getWord16be
                len <- getWord16be
                let byteLen = (fromIntegral len + 7) `div` 8
                bytes <- getByteString byteLen
                let bits = take (fromIntegral len) (unpackBits bytes)
                return (toEnum (fromIntegral c), bits)


    packBits :: String -> BS.ByteString
    packBits bits = BSI.unsafeCreate byteLen $ \ptr ->
        writeBits ptr (chunk8 bits)
        where
            len = length bits
            byteLen = (len + 7) `div` 8

            chunk8 [] = []
            chunk8 bs = let (a, b) = splitAt 8 bs in a : chunk8 b

            toByte = foldl (\acc b -> shiftL acc 1 .|. if b == '1' then 1 else 0) 0

            writeBits _ [] = return ()
            writeBits ptr (b:bs) = do
                poke ptr (toByte (padTo8 b))
                writeBits (ptr `plusPtr` 1) bs

            padTo8 xs = take 8 (xs ++ repeat '0')

    unpackBits :: BS.ByteString -> String
    unpackBits bs = concatMap byteToBits (BS.unpack bs)
        where
        byteToBits w = reverse $ take 8 $ reverse (go 7 w)
            where
                go (-1) _ = []
                go i x = let bit = if testBit x i then '1' else '0' in bit : go (i - 1) x


    encodeHuffman :: String -> BS.ByteString
    encodeHuffman s =
        let code = stringToCode s
            header = serializeCode code
            bitString = encodeBits code s
            body = packBits bitString
            headerLen = runPut (putWord32be (fromIntegral $ BS.length header))
        in BL.toStrict $ headerLen <> BL.fromStrict header <> BL.fromStrict body
        where
            encodeBits _ "" = ""
            encodeBits code (x:xs) = (code ! x) ++ encodeBits code xs


    decodeHuffman :: BS.ByteString -> String
    decodeHuffman bs =
        let (headerLenBS, rest) = BS.splitAt 4 bs
            headerLen = runGet getWord32be (BL.fromStrict headerLenBS)
            (headerBS, bodyBS) = BS.splitAt (fromIntegral headerLen) rest
            codeMap = deserializeCode headerBS
            invertedCode = invertMap codeMap
            bitString = unpackBits bodyBS
        in decodeBits invertedCode bitString
        where
            invertMap :: Map Char String -> Map String Char
            invertMap = fromList . map (\(k,v) -> (v,k)) . toList

            decodeBits _ "" = ""
            decodeBits m bits = go bits ""
                where
                    go [] _ = []
                    go xs acc =
                        case Map.lookup acc m of
                            Just c -> c : go xs ""
                            Nothing -> case xs of
                                (b:bss) -> go bss (acc ++ [b])

