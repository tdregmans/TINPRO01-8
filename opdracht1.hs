{--
    opdracht1.hs
    TINPRO01-8 (Functional programming 2)
    Sep van der Biezen, Thijs Dregmans 
    Last edited: 2023-04-12
--}

module Main where
    import Data.Char
    
    import System.Environment

    main = do
        text <- readFile "test.txt"
        let processed = reverse $map toUpper text
        writeFile "test2.txt" processed
        putStrLn "file written"