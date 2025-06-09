module Main (main) where

import System.Environment (getArgs)
import System.Exit (die)
import qualified Data.ByteString.Char8 as BS

import Huffman (decodeHuffman, encodeHuffman)

type CommandLineArgs = (FilePath, FilePath, Bool)

main :: IO ()
main = do
    args <- getArgs
    case parseArgs args of
        Left err -> die err
        Right (inputFile, outputFile, decode) -> do
            input <- BS.readFile inputFile
            let output = if decode
                    then BS.pack $ decodeHuffman input
                    else encodeHuffman (BS.unpack input)
            BS.writeFile outputFile output

            let inputSize = BS.length input
                outputSize = BS.length output
                ratio = if inputSize == 0 
                        then 0
                        else (fromIntegral outputSize / fromIntegral inputSize) * 100 :: Double

            putStrLn $ inputFile ++ " (" ++ show inputSize ++ " bytes) -> " 
                        ++ outputFile ++ " (" ++ show outputSize ++ " bytes) [" 
                        ++ show ((fromIntegral (round (ratio * 100) :: Int) :: Double) / 100) ++ "%]"

parseArgs :: [String] -> Either String CommandLineArgs
parseArgs args
    | length args < 3 = Left "Usage: huffman <input-file> [-d] -o <output-file>"
    | notElem "-o" args = Left "Missing -o flag for output file"
    | otherwise =
        case args of
            (inputFile:rest) ->
                let outputFile = getOutputFile rest
                    decode = elem "-d" rest
                in case outputFile of
                    Nothing -> Left "Missing output file after -o"
                    Just out -> Right (inputFile, out, decode)
            [] -> Left "Missing input file"

getOutputFile :: [String] -> Maybe FilePath
getOutputFile args =
    case dropWhile (/= "-o") args of
        (_:out:_) -> Just out
        _ -> Nothing
