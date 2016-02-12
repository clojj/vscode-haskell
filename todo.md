- git push, create NEW independent repo
- **refactor ghc-dump-tree (forever in zmq)**
  * return error message as JSON (line 277 in DumpTree.hs) 
  * add lexing

- decorate positions from parser-response
- start server-process(es) automatically on activation
- send '{{exit}}' from appropriate place

- find (Json-)Object utilities

- remove if obsolete:
  * lib/reader.js parser.js
  * tests/reader-test.js
  * tests/data
  * BasicParserServerJson.*


obsolete (remove!) BasicParserServerJson.hs
===========================================
- optimize output, see also https://hackage.haskell.org/package/ghc-syb-utils-0.2.3/docs/src/GHC-SYB-Utils.html#showData
  
  * produce JSON !
  
```javascript
  { "node" : "SymbolicName", // (L, ValD, etc.)
    "loc"  : "1:1" // or "12:2-6" or "(11,3)-(12,6)"
    "objects": [ 
      { "node" : "SymbolicName" }
      { "node" : "SymbolicName" }
      ...
    ]  
  }
```
OR

```javascript
  {
    "node": "L",
    "list": [
      {
        "srcSpan": "1:8-17"
      },
      {
        "ModuleName": "HelloWorld"
      }
    ]
  }
```

  * no ',' after the last object in array
  * JSONize like functions 'srcSpan' and 'moduleName' 
  
- use original GHC API directly / understand what ghc-parser package is doing



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

