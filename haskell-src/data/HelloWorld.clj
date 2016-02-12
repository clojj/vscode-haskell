  (L {<interactive>:1:1}
   (HsModule
    (Just
     (L {<interactive>:1:8-17} {ModuleName: HelloWorld}))
    (Nothing)
    []
    [
     (L {<interactive>:2:1-31}
      (ValD
       (FunBind
        (L {<interactive>:2:1-4}
         (Unqual {OccName: main}))
        (False)
        (MG
         [
          (L {<interactive>:2:1-31}
           (Match
            (Just
             ((,)
              (L {<interactive>:2:1-4}
               (Unqual {OccName: main}))
              (False)))
            []
            (Nothing)
            (GRHSs
             [
              (L {<interactive>:2:6-31}
               (GRHS
                []
                (L {<interactive>:2:8-31}
                 (HsApp
                  (L {<interactive>:2:8-15}
                   (HsVar
                    (Unqual {OccName: putStrLn})))
                  (L {<interactive>:2:17-31}
                   (HsLit
                    (HsString "\"Hello, World!\"" {FastString: "Hello, World!"})))))))]
             (EmptyLocalBinds))))]
         []
         (PlaceHolder)
         (FromSource))
        (WpHole)
        (PlaceHolder)
        [])))]
    (Nothing)
    (Nothing)))