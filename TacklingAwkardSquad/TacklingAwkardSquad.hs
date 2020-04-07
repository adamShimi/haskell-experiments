module TacklingAwkwardSquad () where

-- Reimplement putline

putline' :: [Char] -> IO ()
putline' [] = return ()
putline' (x:xs) = do
                  putChar x
                  putline' xs
