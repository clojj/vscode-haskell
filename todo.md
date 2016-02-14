- return lexer errors as JSON in `ghc-engine/src/HaskellLexerParserServerJson.hs`
- obsolete: `haskell-src/*Server*.*`

- optional lexing in ghc-engine
- optional lexing and optional parsing in HaskellLexerParserServerJson

- in active editor: decorate positions from lexer/parser-response
- start server-process(es) automatically on activation
- send '{{exit}}' from appropriate place

- find (Json-)Object utilities


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

