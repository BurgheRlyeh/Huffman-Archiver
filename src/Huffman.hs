module Huffman where

import Code
import qualified Data.ByteString        as BS
import qualified Data.Vector.Unboxed    as VU

type Key = (Int, Encoding, String)

encode filename = do
    text <- BS.readFile filename
    guard text                                          -- check empty file
    let freq = textToFreq text                          -- list of frequencies
    let tree = freqToTree freq                          -- tree with weights
    let bintree = treeToBTree tree                      -- tree with bits
    let encoding = treeToEncoding bintree               -- list with encodings of each symbol
    let textEncodedBits = textToEncoded text encoding   -- bits of encoded text
    let textEncoded = bitsToText textEncodedBits        -- encoded text
    -- key (number of extra bits in the end of encoded text, encoding, extension of filename)
    let key = (8 - VU.length textEncodedBits `mod` 8 - 1, encoding, getExt filename)
    writeFile (getName filename ++ ".key") (show key)           -- write key
    BS.writeFile (getName filename ++ ".huff") textEncoded   -- write archive

decode fileHuff fileKey = do
    keyText <- readFile fileKey
    let key = read keyText :: Key
    code <- BS.readFile fileHuff
    let textDecodedBits = textToBits code
    let textDecoded = bitsToDecoded (snd key) (VU.take (VU.length textDecodedBits - fst key) textDecodedBits)
    BS.writeFile ("new." ++ getName fileHuff ++ trd key) textDecoded
    where
        fst (a,_,_) = a; snd (_,b,_) = b; trd (_,_,c) = c