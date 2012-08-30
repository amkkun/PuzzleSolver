{-# OPTIONS -Wall #-}

echo :: IO ()
echo = do
  c <- getChar
  putChar '\n'
  putChar c
  putChar '\n'