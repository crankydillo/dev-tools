import Control.Monad
import Control.Monad.Trans
import Prelude hiding (lookup)
import Data.List hiding (lookup)
import System.Directory
import System.Environment

main = do 
        args <- getArgs
        res <- recurseDirs $ head args
        print res

recurseDirs :: FilePath -> IO [FilePath]
recurseDirs d = do
                 dirs <- listDirs d
                 subdirs <- mapM recurseDirs dirs
                 return $ dirs ++ foldl (++) [] subdirs

listDirs :: FilePath -> IO [FilePath]
listDirs d = do 
              files <- getDirectoryContents d
              let pruned = delete "." $ delete ".." files
              let paths = map ((d ++ "/") ++) pruned
              dirs <- filterM doesDirectoryExist paths
              return dirs

{-
main = do
       args <- getArgs
       let className = head args -- handle empty list
       classTemplate <- readFile "scala-template"
       writeFile "out" $ fillTemplate classTemplate className
       putStrLn className
-}
       {-
       let m = case lookup arg myMap of
               Just a -> a
               Nothing -> "oh nos"
       putStrLn m
       -}

--myMap = fromList [("{class}", msg)]

msg = "his"

fillTemplate :: String -> String -> String
fillTemplate t v = v ++ t
