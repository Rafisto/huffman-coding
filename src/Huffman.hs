module Huffman (encodeHuffman, decodeHuffman) where
    import Prelude hiding (length)
    import qualified Data.ByteString as BS
    import qualified Data.ByteString.Lazy as BL
    import qualified Data.ByteString.Internal as BSI
    import qualified Data.Map as Map
    import Data.Map (Map, (!), singleton, union)
    import Data.Bits (shiftL, (.|.), testBit)
    import Data.Binary.Put (Put, runPut, putWord32be, putWord16be, putByteString)
    import Data.Binary.Get (Get, runGet, getWord32be, getWord16be, getByteString)
    import Foreign.ForeignPtr (withForeignPtr)
    import Foreign.Ptr (plusPtr)
    import Foreign.Storable (poke)
    import Data.Word (Word8)
    import Data.List (foldl', length)
    
    data Tree a b = Leaf a b | Node (Tree a b) (Tree a b) b
        deriving (Eq, Show)
    
    getWeight :: Tree a b -> b
    getWeight (Leaf _ w) = w
    getWeight (Node _ _ w) = w

    
    type CharMap = [(Word8, Int)]
    type LeafQueue = [Tree Word8 Int]
    
    addFreq :: CharMap -> Word8 -> CharMap
    addFreq [] c = [(c, 1)]
    addFreq ((x,y):xs) c
        | x == c    = (x, y+1) : xs
        | otherwise = (x,y) : addFreq xs c
    
    buildFreqMap :: BS.ByteString -> CharMap
    buildFreqMap = BS.foldl' addFreq []
    
    
    insertLQ :: LeafQueue -> Tree Word8 Int -> LeafQueue
    insertLQ [] tree = [tree]
    insertLQ (t:ts) tree
        | getWeight tree < getWeight t = tree : t : ts
        | otherwise                    = t : insertLQ ts tree
    
    charMapToQueue :: CharMap -> LeafQueue
    charMapToQueue = foldl' insertLQ [] . map (uncurry Leaf)
    
    
    mergeLQ :: LeafQueue -> Tree Word8 Int
    mergeLQ []  = error "Empty queue"
    mergeLQ [t] = t
    mergeLQ (t1:t2:ts) = mergeLQ $ insertLQ ts (Node t1 t2 (getWeight t1 + getWeight t2))

    makeCode :: Tree Word8 Int -> Map Word8 [Word8]
    makeCode = go []
      where
        go prefix (Leaf c _) = singleton c prefix
        go prefix (Node l r _) = union (go (prefix ++ [0]) l) (go (prefix ++ [1]) r)
    
    bytesToCode :: BS.ByteString -> Map Word8 [Word8]
    bytesToCode = makeCode . mergeLQ . charMapToQueue . buildFreqMap
    
    packBits :: [Word8] -> BS.ByteString
    packBits bits = BSI.unsafeCreate byteLen $ \ptr -> write ptr (chunk8 bits)
      where
        len = length bits
        byteLen = (len + 7) `div` 8
        chunk8 [] = []
        chunk8 bs = let (a,b) = splitAt 8 bs in a : chunk8 b
        toByte :: [Word8] -> Word8
        toByte = foldl' (\acc b -> shiftL acc 1 .|. b) 0
        pad8 xs = take 8 (xs ++ repeat 0)
        write _ [] = return ()
        write p (b:bs) = do
          poke p (toByte (pad8 b))
          write (p `plusPtr` 1) bs
    
    unpackBits :: BS.ByteString -> [Word8]
    unpackBits bs = concatMap byteToBits (BS.unpack bs)
      where
        byteToBits w = [if testBit w i then 1 else 0 | i <- [7,6..0]]
    
    serializeCode :: Map Word8 [Word8] -> BS.ByteString
    serializeCode m = BL.toStrict $ runPut $ do
      putWord32be (fromIntegral $ Map.size m)
      mapM_ putEntry (Map.toList m)
      where
        putEntry (c, bits) = do
          putWord16be (fromIntegral c)
          putWord16be (fromIntegral $ length bits)
          putByteString $ packBits bits
    
    deserializeCode :: BS.ByteString -> Map Word8 [Word8]
    deserializeCode bs = runGet getMap (BL.fromStrict bs)
      where
        getMap = do
          n <- getWord32be
          entries <- sequence $ replicate (fromIntegral n) getEntry
          return $ Map.fromList entries
        getEntry = do
          c <- getWord16be
          len <- getWord16be
          let byteLen = (fromIntegral len + 7) `div` 8
          bytes <- getByteString byteLen
          let bits = take (fromIntegral len) (unpackBits bytes)
          return (fromIntegral c, bits)
    
    encodeHuffman :: BS.ByteString -> BS.ByteString
    encodeHuffman bs =
      if BS.null bs then BS.empty else
      let code = bytesToCode bs
          header = serializeCode code
          bitList = concatMap (code !) (BS.unpack bs)
          body = packBits bitList
          headerLen = runPut (putWord32be (fromIntegral $ BS.length header))
          bitsLen = runPut (putWord32be (fromIntegral $ length bitList))
      in BL.toStrict $ headerLen <> BL.fromStrict header <> bitsLen <> BL.fromStrict body
    
    decodeHuffman :: BS.ByteString -> BS.ByteString
    decodeHuffman bs =
      if BS.null bs then BS.empty else
      let (headerLenBS, rest) = BS.splitAt 4 bs
          headerLen = runGet getWord32be (BL.fromStrict headerLenBS)
          (headerBS, rest2) = BS.splitAt (fromIntegral headerLen) rest
          (bitsLenBS, bodyBS) = BS.splitAt 4 rest2
          bitsLen = runGet getWord32be (BL.fromStrict bitsLenBS)
          codeMap = deserializeCode headerBS
          inverted = Map.fromList [ (v, k) | (k,v) <- Map.toList codeMap ]
          bits = take (fromIntegral bitsLen) (unpackBits bodyBS)
      in decodeBits inverted bits
    
    decodeBits :: Map [Word8] Word8 -> [Word8] -> BS.ByteString
    decodeBits code bits = BS.pack $ go bits []
      where
        go [] acc = maybe acc (:acc) (Map.lookup (reverse acc) code)
        go (b:bs) acc =
          let acc' = b : acc
          in case Map.lookup (reverse acc') code of
               Just c  -> c : go bs []
               Nothing -> go bs acc'

