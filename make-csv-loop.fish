#!/usr/bin/fish
while read line
  echo $line,(runhaskell chapterize.hs $line)
  end < html-files.txt
