module Utils.Random exposing (randomIdString)

import Random exposing (Generator)
import Char


randomIdString : Generator String
randomIdString =
    randomString 14 (char 0 127)


char : Int -> Int -> Generator Char
char start end =
    Random.map Char.fromCode (Random.int start end)


randomString : Int -> Generator Char -> Generator String
randomString length charGenerator =
    Random.map String.fromList (Random.list length charGenerator)
