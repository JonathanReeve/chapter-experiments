#!/usr/bin/fish
while read line
  echo $line,(runhaskell chapterize.hs $line)
  end < pg-prose-fic-html-files.txt
