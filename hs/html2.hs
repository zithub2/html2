module Main(main) where

import Data.Char
import Data.List.Extra

tag2 = "html2"
obj2 = "html2"

main = interact html2

html2 = ("<!DOCTYPE html>"++) . blocks . wordsBy null . fmap trimEnd . lines
blocks (b0:bs) = concatMap firstBlockLine b0 ++  concatMap regularBlock bs
firstBlockLine s = toOpentag s ++ toClosetag (keyOf s)
regularBlock block@(l0:ls) =
  if head l0 == '>' then let (p:c:_)=words (tail l0) in pc (withTag c) p ls
  else if isTagname l0 then tag_contents l0 ls
  else tag_contents tag2 block
pc transLine p cs = withTag p $ concatMap (transLine . regularBlockLine) cs
tag_contents = pc id
regularBlockLine line = let (key,content)=kcOf line in
  if head key == '*' then
    concatMap (withTag $ tail key) $ words content
  else if head key == '.' then
     withTag "script" $ obj2++key++"(\""++escapeJS content++"\")"
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
checkRestChar x = isAlphaNum x || x `elem` "_-.:"

kcOf = fmap trimStart . span (not . isSpace)
keyOf = fst . kcOf
contentOf = snd . kcOf
