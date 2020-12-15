-- Base64 encode/decode strings or files.
import Data.List
import System.IO
import Text.Printf
import Data.Char
import Data.Bits
import System.Environment


encodeTable = (['A', 'B' .. 'Z'] ++ ['a', 'b' .. 'z'] ++ ['0', '1' .. '9'] ++ ['+', '/'])
pending = '='

programName = "base64"

authors = "Simon Josefsson"

helpPrinc = putStrLn ("\tUsage: " ++ programName ++ " [OPTION]... [FILE]\n\tBase64 encode or decode FILE, or standard input, to standard output.\n")

helpFlags = putStrLn "\t-d, --decode decode data \n\t-i, --ignore-garbage  when decoding, ignore non-alphabet characters\n\t-w, --wrap=COLS wrap encoded lines after COLS character (default 76).\n\t\t\t\t\t\tUse 0 to disable line wrapping"


helpDesc = putStrLn "\n\tWith no FILE, or when FILE is -, read standard input.\n\n\tThe data are encoded as described for the base64 alphabet in RFC 3548.\n\tWhen decoding, the input may contain newlines in addition to the bytes of\n\tthe formal base64 alphabet.  Use --ignore-garbage to attempt to recover\n\tfrom any other non-alphabet bytes in the encoded stream.\n"

help =  do {helpPrinc;helpFlags;helpDesc}

stringToListBlock :: [Char] -> [Int]
stringToListBlock [] = []
stringToListBlock a = stringToListBlockAux x 0
    where x = a ++ y
          y = take z (repeat '\0')
          z = 3 - (length a)

stringToListBlockAux :: [Char] -> Int -> [Int]
stringToListBlockAux x 0 = (((strVar .&. 0xfc) `shiftR` 2) : (stringToListBlockAux x 1))
    where strVar = ord (x !! 0)
stringToListBlockAux x 1 = ((((strVar1 .&. 0x3) `shiftL` 4) .|. ((strVar2 .&. 0xf0) `shiftR` 4)) : (stringToListBlockAux x 2))
    where strVar1 = ord (x !! 0)
          strVar2 = ord (x !! 1)
stringToListBlockAux x 2 = ((((strVar1 .&. 0x0f) `shiftL` 2) .|. ((strVar2 .&. 0xc0) `shiftR` 6)) : (stringToListBlockAux x 3))
    where strVar1 = ord (x !! 1)
          strVar2 = ord (x !! 2)
stringToListBlockAux x 3 = ((strVar .&. 0x3f) : (stringToListBlockAux x 4))
    where strVar = ord (x !! 2)
stringToListBlockAux x y = []

encodeBlockAux :: Int -> [Int] -> [Char]
encodeBlockAux x [] = (encodeTable !! x) : []
encodeBlockAux x xs = ((encodeTable !! x) : (encodeBlockAux (head xs) (tail xs)))

encodeBlock :: [Int] -> [Char]
encodeBlock [] = []
encodeBlock a = encodeBlockAux (head a) (tail a)

checkAppend :: [Char] -> Int -> [Char]
checkAppend xs 0 = if lastVar == 'A' then (checkAppend (init xs) 1) ++ "=" else xs
    where lastVar = last xs
checkAppend xs 1 = if lastVar == 'A' then (init xs) ++ "=" else xs
    where lastVar = last xs
checkAppend xs x = xs

encodeBase64Aux :: [Char] -> [Char]
encodeBase64Aux [] = []
encodeBase64Aux a = (encodeBlock block) ++ (encodeBase64 (drop x a))
    where x = if (length a) >= 3 then 3 else (length a)
          block = stringToListBlock (take x a)

encodeBase64 :: [Char] -> [Char]
encodeBase64 [] = []
encodeBase64 a = checkAppend encodedVar 0
    where encodedVar = encodeBase64Aux a


-- decodeBase64 :: [Char] -> [Char]
-- deencodeBase64 [] = []

listBlockToString :: [Char] -> [Int]
listBlockToString [] = []
listBlockToString a = listBlockToStringAux a 0


listBlockToStringAux :: [Char] -> Int -> [Int]
listBlockToStringAux x 0 = (blockI .|. blockJ) : listBlockToStringAux x 1
    where i = x !! 0
          blockI = (findList encodeTable i) `shiftL` 2
          j = x !! 1
          blockJ = ((findList encodeTable j) .&. 0x30) `shiftR` 4
listBlockToStringAux x 1 = if j == '=' then [] else (blockI .|. blockJ) : listBlockToStringAux x 2
    where i = x !! 1
          blockI = ((findList encodeTable i) .&. 0xf) `shiftL` 4
          j = x !! 2
          blockJ = ((findList encodeTable j) .&. 0x3c) `shiftR` 2
listBlockToStringAux x 2 = if j == '=' then [] else (blockI `shiftL` 6 .|. blockJ) : listBlockToStringAux x 3
    where i = x !! 2
          blockI = (findList encodeTable i) .&. 0x3
          j = x !! 3
          blockJ = findList encodeTable j
listBlockToStringAux x y = []

findList :: [Char] -> Char -> Int
findList [] x = -1
findList xs x = if i >= j then -1 else i
    where i = findListAux (head xs) (tail xs) x
          j = length xs

findListAux :: Char -> [Char] -> Char -> Int
findListAux x xs y = if x == y then 0 else 1 + findListAux (head xs) (tail xs) y
findListAux x [] y = if x == y then 0 else 1


decodeBlock :: [Int] -> [Char]
decodeBlock [] = []
decodeBlock xs = x : decodeBlock (tail xs)
    where x = chr y
          y = head xs

decodeBase64 :: [Char] -> [Char]
decodeBase64 [] = []
decodeBase64 xs = decodeBlock i ++ decodeBase64 (drop 4 xs)
    where i = listBlockToString j
          j = take 4 xs

main = do
    let elementX = [1,2,3] !! 1
    putStrLn (show elementX)
    -- show out
