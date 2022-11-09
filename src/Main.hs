import Huffman

main = do
    encode "WarAndPeaceTXT.txt"
    decode "WarAndPeaceTXT.huff" "WarAndPeaceTXT.key"
    encode "WarAndPeacePNG.png"
    decode "WarAndPeacePNG.huff" "WarAndPeacePNG.key"
    return ()