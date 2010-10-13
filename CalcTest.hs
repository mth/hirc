import Calculator

main = do
    putStrLn "Calculator."
    interact (unlines . map calc . lines)
