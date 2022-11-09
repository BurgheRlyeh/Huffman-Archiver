module Code where

{-____________________IMPORTS____________________-}
import qualified Data.Map               as Map
import qualified Data.List              as List
import qualified Data.Vector            as Vec
import qualified Data.Vector.Unboxed    as VU
import qualified Data.Bit               as Bit
import qualified Data.ByteString        as BS
import qualified Data.Word              as Word
import qualified Data.Maybe             as Mb
import qualified Control.Monad          as Monad
import qualified System.FilePath.Posix  as FP
import qualified Data.ByteString.Lazy.UTF8 as BSL

type Node = (Word.Word8, Int)
type Freq = [Node]

data Tree a = Leaf (Word.Word8, a) | Branch (Tree a) a (Tree a)
type Forest a = [Tree a]

type Encoding  = [(Word.Word8, [Bit.Bit])]

guard text = Monad.guard (0 < BS.length text)

textToFreq :: BS.ByteString -> Freq
textToFreq text  = Map.toList $ textToFreq' (BS.head text) (BS.tail text) Map.empty
    where
        textToFreq' :: Word.Word8 -> BS.ByteString -> Map.Map Word.Word8 Int -> Map.Map Word.Word8 Int
        textToFreq' symbol string map
            | BS.null string = Map.insertWith (+) symbol 1 map
            | otherwise = textToFreq' (BS.head string) (BS.tail string) (Map.insertWith (+) symbol 1 map)

freqToTree :: Freq -> Tree Int
freqToTree freq = forestToTree $ freqToForest $ sortFreq freq
    where
        -- sort Freq by freqs of symbols
        sortFreq :: Freq -> Freq
        sortFreq = List.sortOn snd

        -- convert Freq to Forest
        -- [Node] to [Leaf Node]
        freqToForest :: Freq -> Forest Int
        freqToForest = map nodeToTree

        -- convert Node to Tree's Leaf
        nodeToTree :: Node -> Tree Int
        nodeToTree  = Leaf

        -- merge Forest's Trees to one Tree
        forestToTree :: Forest Int -> Tree Int
        forestToTree [tree]       = tree
        forestToTree (t1:t2:ts)   = forestToTree $ sortForest $ combine t1 t2 : ts
            where
                -- combine 2 trees to 1 with total weight
                combine :: Tree Int -> Tree Int -> Tree Int
                combine t1 t2   = Branch t1 w t2
                    where w = weightTree t1 + weightTree t2

                -- sort Forest by weight
                sortForest :: Forest Int -> Forest Int
                sortForest = List.sortOn weightTree
        
                -- weight from Tree
                weightTree :: Tree Int -> Int
                weightTree (Leaf (_, w))    = w
                weightTree (Branch _ w _)   = w

-- Tree with weights to binary Tree with bits
treeToBTree :: Tree Int -> Tree Bit.Bit
treeToBTree tree = treeToBTree' tree 0
    where
        treeToBTree' :: Tree Int -> Bit.Bit -> Tree Bit.Bit
        treeToBTree' (Leaf (s, w)) v      = Leaf (s, v)
        treeToBTree' (Branch t1 w t2) v   =
            Branch (treeToBTree' t1 0) v (treeToBTree' t2 1)

-- symbol's encodings from tree
treeToEncoding :: Tree Bit.Bit -> Encoding
treeToEncoding (Leaf (symbol, bit)) = [(symbol, [bit])]
treeToEncoding (Branch t1 _ t2) = Map.toList $ 
    treeToEncoding' t1 [] Map.empty `Map.union` treeToEncoding' t2 [] Map.empty
    where
        treeToEncoding' :: Tree Bit.Bit -> [Bit.Bit] -> Map.Map Word.Word8 [Bit.Bit] -> Map.Map Word.Word8 [Bit.Bit]
        treeToEncoding' (Leaf (symbol, bit)) bits map = 
            Map.insertWith (++) symbol (bits ++ [bit]) map
        treeToEncoding' (Branch t1 bit t2) bits map = 
            treeToEncoding' t1 (bits ++ [bit]) map `Map.union` treeToEncoding' t2 (bits ++ [bit]) map


textToEncoded :: BS.ByteString -> [(Word.Word8, [Bit.Bit])] -> Bit.Vector Bit.Bit
textToEncoded string encoding = VU.fromList $ textToEncoded' string
    where
        textToEncoded' :: BS.ByteString -> [Bit.Bit]
        textToEncoded' string
            | BS.length string == 1 = Map.findWithDefault [] (BS.head string) $ Map.fromList encoding
            | otherwise             = textToEncoded' (BS.take 1 string) ++ textToEncoded' (BS.tail string)

bitsToText :: Bit.Vector Bit.Bit -> BS.ByteString
bitsToText = Bit.cloneToByteString


{-____________________DECODING____________________-}
textToBits :: BS.ByteString -> Bit.Vector Bit.Bit
textToBits = Bit.cloneFromByteString

bitsToDecoded :: Encoding -> Bit.Vector Bit.Bit -> BS.ByteString
bitsToDecoded en bits = foo (Map.fromList (invert en)) [] (VU.toList bits)
    where
        invert :: [(a, b)] -> [(b, a)]
        invert = map (\(a,b) -> (b,a))
        
        foo :: Map.Map [Bit.Bit] Word.Word8 -> [Bit.Bit] -> [Bit.Bit] -> BS.ByteString
        foo map bits []    = BS.empty
        foo map bits (b:bs)
            | Mb.isJust (Map.lookup bits map) = Mb.fromJust (Map.lookup bits map) `BS.cons` foo map [] (b:bs)
            | otherwise = foo map (bits ++ [b]) bs

{-____________________OTHER____________________-}
getName :: FilePath -> String
getName filename = fst $ FP.splitExtension filename

getExt :: FilePath -> String
getExt filename = snd $ FP.splitExtension filename