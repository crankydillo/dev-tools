import Prelude hiding (lookup)
import Data.List hiding (lookup)
import Data.Map hiding (filter, map)
import System.Directory
import System.Environment

listDirs d = do 
              isDir <- doesDirectoryExist d
              if isDir
                  then do
                         dir <- getDirectoryContents d
                         print $ show $ listDirs' dir
                  else print "nope"

listDirs' :: [FilePath] -> [FilePath]
listDirs' d = ["a"]

main = do
       args <- getArgs
       let className = head args -- handle empty list
       classTemplate <- readFile "scala-template"
       writeFile "out" $ fillTemplate classTemplate className
       putStrLn className
       {-
       let m = case lookup arg myMap of
               Just a -> a
               Nothing -> "oh nos"
       putStrLn m
       -}

myMap = fromList [("{class}", msg)]

msg = "his"

fillTemplate :: String -> String -> String
fillTemplate t v = v ++ t
