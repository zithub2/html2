module Main(main) where

import Data.Char
import Data.List.Extra

tag = "html2"
obj = "html2"

main = interact html2

html2 = ("<!DOCTYPE html>"++) . blocks . wordsBy null . fmap trimEnd . lines
blocks (b0:bs) = concatMap firstBlockLine b0 ++  concatMap regularBlock bs
firstBlockLine s = toOpentag s ++ toClosetag (keyOf s)
regularBlock block@(l0:ls) = if isTagname l0 then tag_contents l0 ls else tag_contents tag block
tag_contents tagName lines = withTag tagName $ concatMap regularBlockLine lines
regularBlockLine line = let (key,content)=kcOf line in
  if head key == '.' then
     withTag "script" $ obj++key++"(\""++escapeJS content++"\")"
  else if isTagname key
  then withTag key $ trimStart content
  else line

escapeJS = backslash "\"\\"
backslash = escapeWith '\\'
escapeWith leader chars str = concatMap (\x->if x`elem`chars then [leader,x] else [x]) str

withTag tagName s = toOpentag tagName ++ s ++ toClosetag tagName
toOpentag s = "<"++s++">"
toClosetag s = "</"++s++">"

isTagname (c:cs) = checkFirstChar c && all checkRestChar cs
checkFirstChar x = isLatin1 x && isLetter x
checkRestChar x = isPrint x && not (isSpace x)

kcOf = fmap trimStart . span (not . isSpace)
keyOf = fst . kcOf
contentOf = snd . kcOf
