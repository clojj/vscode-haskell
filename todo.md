- start HaskellLexerServerJson with realtive path

- in active editor: decorate positions from lexer/parser-response

- obsolete: `haskell-src/*Server*.*`

- optional lexing in ghc-engine

- find (Json-)Object utilities

- re-start server-process(es) automatically on process-errors, action, ...

- send '{{exit}}' from appropriate place ? otherwise just cancel the child-process


Github VisualizingHaskellAST
============================
Main.hs (like BasicParser, but multiple modules)


ghc-exactprint
==============
* AnnsViewer.hs
outputs ApiAnns: comments !

ghc-exactprint/src/Language/Haskell/GHC/ExactPrint/Parsers.hs

* todo: use String parameter instead of FilePath !
https://github.com/alanz/ghc-exactprint/blob/master/src%2FLanguage%2FHaskell%2FGHC%2FExactPrint%2FParsers.hs#L151

