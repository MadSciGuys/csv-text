csv-text
========

This is a Haskell module for loading CSV files (and other similar things) in 
Haskell.  There isn't much complexity to it.  The main thing is that there were
a few edge cases inolving CSVs that I never considered.

For instance, the lines function in a lot of string libraries (including the
packate text) only split on newline characters.  They usually don't split on 
carriage returns which text files from Windows applications tend to use.

Another case is that Windows applications tend to encode files in Latin-1
(sometimes mis-referred to as ANSI).  Most Haskell libraries assume UTF-8
encoding (to which plain old ASCII conforms by default).  There are functions
in this csv-text module which call the necessary functions in the text module
to deal with Latin-1, which took me an emberassingly long time to find.

installation
------------

```
#while in directory of the cabal sandobx you want to use csv-text
git clone [TODO: specify Phabricator URL for this repository]
cabal install csv-text/
```
