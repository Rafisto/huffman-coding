module Huffman (encodeHuffman, decodeHuffman) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as Map
import           Data.Map.Strict (Map)
import qualified Data.Heap as H
import           Data.Heap (MinHeap)
import           Data.Bits (shiftL, (.|.), testBit)
import           Data.Binary.Put (runPut, putWord32be, putWord8, putWord16be, putByteString)
import           Data.Binary.Get (runGet, getWord32be, getWord8, getWord16be, getByteString)
import           Data.Word (Word8)
import           Foreign.Ptr (plusPtr)
import           Foreign.Storable (poke)
import qualified Data.ByteString.Internal as BSI
import           Foreign.ForeignPtr (withForeignPtr)

data Tree = Leaf Word8 Int | Node Tree Tree Int deriving (Show)

instance Eq Tree where a == b = getWeight a == getWeight b
instance Ord Tree where compare a b = compare (getWeight a) (getWeight b)

getWeight :: Tree -> Int
getWeight (Leaf _ w)   = w
getWeight (Node _ _ w) = w

buildFreqMap :: BS.ByteString -> Map Word8 Int
buildFreqMap = BS.foldl' (\m b -> Map.insertWith (+) b 1 m) Map.empty

buildTree :: Map Word8 Int -> Tree
buildTree freqMap = mergeHeap $ H.fromList [Leaf b f | (b, f) <- Map.toList freqMap]
  where
    mergeHeap :: MinHeap Tree -> Tree
    mergeHeap h
        | H.size h == 1 = case H.viewHead h of
            Just t  -> t
            Nothing -> error "Heap is empty"
        | otherwise =
            case (H.view h, H.view (H.drop 1 h)) of
                (Just (t1, _), Just (t2, h2)) ->
                    let newNode = Node t1 t2 (getWeight t1 + getWeight t2)
                    in mergeHeap (H.insert newNode h2)
                _ -> error "Invalid heap state"

makeCodeMap :: Tree -> Map Word8 [Bool]
makeCodeMap = go
  where
    go (Leaf b _) = Map.singleton b [False]
    go (Node l r _) =
      let leftMap  = Map.map (False :) (go l)
          rightMap = Map.map (True  :) (go r)
      in Map.union leftMap rightMap

encodeBits :: Map Word8 [Bool] -> BS.ByteString -> [Bool]
encodeBits m = concatMap (m Map.!) . BS.unpack

decodeBits :: Map [Bool] Word8 -> [Bool] -> BS.ByteString
decodeBits m = BS.pack . go
  where
    go []    = []
    go input = decode input []
    decode [] acc = maybe [] (:[]) (Map.lookup acc m)
    decode (x:xs) acc =
        let acc' = acc ++ [x]
        in case Map.lookup acc' m of
            Just c  -> c : go xs
            Nothing -> decode xs acc'

packBits :: [Bool] -> BS.ByteString
packBits bits = BSI.unsafeCreate byteLen $ \ptr -> write ptr (chunk8 bits)
  where
    len = length bits
    byteLen = (len + 7) `div` 8
    chunk8 [] = []
    chunk8 bs = let (a, b) = splitAt 8 bs in a : chunk8 b
    toByte = foldl (\acc b -> shiftL acc 1 .|. if b then 1 else 0) 0
    pad8 xs = take 8 (xs ++ repeat False)
    write _ [] = return ()
    write p (b:bs) = do
        poke p (toByte (pad8 b))
        write (p `plusPtr` 1) bs

unpackBits :: BS.ByteString -> [Bool]
unpackBits = concatMap toBits . BS.unpack
  where
    toBits w = map (\i -> testBit w i) [7,6..0]

serializeCode :: Map Word8 [Bool] -> BS.ByteString
serializeCode m = BL.toStrict $ runPut $ do
    putWord16be (fromIntegral $ Map.size m)
    mapM_ putEntry (Map.toList m)
  where
    putEntry (w8, bits) = do
        putWord8 w8
        putWord16be (fromIntegral $ length bits)
        putByteString $ packBits bits

deserializeCode :: BS.ByteString -> Map Word8 [Bool]
deserializeCode bs = runGet getMap (BL.fromStrict bs)
  where
    getMap = do
      size <- getWord16be
      entries <- sequence (replicate (fromIntegral size) getEntry)
      pure $ Map.fromList entries
    getEntry = do
      w8 <- getWord8
      len <- getWord16be
      bitsBS <- getByteString (fromIntegral $ (len + 7) `div` 8)
      let bits = take (fromIntegral len) $ unpackBits bitsBS
      pure (w8, bits)

encodeHuffman :: BS.ByteString -> BS.ByteString
encodeHuffman input =
    if BS.null input then BS.empty else
    let freqMap = buildFreqMap input
        tree = buildTree freqMap
        codeMap = makeCodeMap tree
        header = serializeCode codeMap
        bits = encodeBits codeMap input
        body = packBits bits
        bitsLen = length bits
        headerLenBS = BL.toStrict $ runPut $ putWord32be (fromIntegral $ BS.length header)
        bitsLenBS = BL.toStrict $ runPut $ putWord32be (fromIntegral bitsLen)
    in BS.concat [headerLenBS, header, bitsLenBS, body]

decodeHuffman :: BS.ByteString -> BS.ByteString
decodeHuffman bs =
    if BS.null bs then BS.empty else
    let (headerLenBS, rest1) = BS.splitAt 4 bs
        headerLen = runGet getWord32be (BL.fromStrict headerLenBS)
        (headerBS, rest2) = BS.splitAt (fromIntegral headerLen) rest1
        (bitsLenBS, bodyBS) = BS.splitAt 4 rest2
        bitsLen = runGet getWord32be (BL.fromStrict bitsLenBS)
        codeMap = deserializeCode headerBS
        inverted = Map.fromList [ (v, k) | (k, v) <- Map.toList codeMap ]
        bits = take (fromIntegral bitsLen) (unpackBits bodyBS)
        decodedWord8s = decodeBits inverted bits
    in decodedWord8s
