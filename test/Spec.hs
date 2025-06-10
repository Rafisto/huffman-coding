{-# LANGUAGE OverloadedStrings #-}

import Test.QuickCheck
import Data.Char (chr, isPrint)
import qualified Data.ByteString as BS
import qualified Huffman
import Control.Monad (when)
import Test.QuickCheck.Monadic (monadicIO, run, assert)

newtype TestByteString = TestByteString BS.ByteString deriving (Show, Eq)

arbitraryPrintableBS :: Gen BS.ByteString
arbitraryPrintableBS = BS.pack <$> listOf (arbitrary `suchThat` (\b -> isPrint (chr (fromIntegral b))))

instance Arbitrary BS.ByteString where
  arbitrary = arbitraryPrintableBS

instance Arbitrary TestByteString where
  arbitrary = TestByteString <$> arbitraryPrintableBS

prop_encodeDecode :: TestByteString -> Property
prop_encodeDecode (TestByteString bs) = monadicIO $ do
  let encoded = Huffman.encodeHuffman bs
      decoded = Huffman.decodeHuffman encoded
  run $ putStrLn $ "Input: " ++ show bs
  run $ putStrLn $ "Encoded: " ++ show encoded
  run $ putStrLn $ "Decoded: " ++ show decoded
  assert (decoded == bs)

main :: IO ()
main = quickCheck prop_encodeDecode
