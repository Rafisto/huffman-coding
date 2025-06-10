{-# LANGUAGE OverloadedStrings #-}

import Test.QuickCheck
import Data.Char (chr, isPrint)
import qualified Data.ByteString as BS
import qualified Huffman
import Control.Monad (when)
import Test.QuickCheck.Monadic (monadicIO, run, assert)

genSafeChar :: Gen Char
genSafeChar = suchThat arbitrary (\c -> isPrint c)

genSafeString :: Gen String
genSafeString = listOf genSafeChar 

newtype SafeString = SafeString { unwrapSafeString :: String }
    deriving Show

instance Arbitrary SafeString where
  arbitrary = SafeString <$> genSafeString

prop_encodeDecode :: SafeString -> Property
prop_encodeDecode (SafeString bs) = monadicIO $ do
  let encoded = Huffman.encodeHuffman bs
      decoded = Huffman.decodeHuffman encoded
  run $ putStrLn $ "Input: " ++ show bs
  -- run $ putStrLn $ "Encoded: " ++ show encoded
  run $ putStrLn $ "Decoded: " ++ show decoded
  assert (decoded == bs)

main :: IO ()
main = quickCheck prop_encodeDecode