module HelloWorld2 where

main = do
  putStrLn "Hello, World!"
  putStrLn "multiline1111\
  \line2222\
  \line333"
  let result = f 3
  print result

-- comment 2
f :: Int -> Int
f x = x + 5
