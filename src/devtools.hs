import Control.Monad
import Control.Monad.Trans
import Prelude hiding (lookup)
import Data.List hiding (lookup)
import Data.Maybe
import System.Directory
import System.Environment

data Tree a = Node a | Fork a [Tree a] deriving (Show)

main = do 
        args <- getArgs
        res <- recurseDirs $ head args
        print res
        print $ srcMainJava res

extract (Node a)   = a
extract (Fork a _) = a

srcMainJava :: Tree String -> Maybe (Tree String)
srcMainJava smj@(Fork "src" [(Fork "main" ((Fork "java" [ts1]):ts2))]) = Just smj
srcMainJava (Fork _ [ts]) = case (catMaybes $ map srcMainJava [ts]) of
                                [] -> Nothing
                                (x:xs) -> Just x
srcMainJava _ = Nothing

recurseDirs :: FilePath -> IO (Tree FilePath)
recurseDirs p = do
                 dirs <- listDirs p
                 subtrees <- mapM recurseDirs dirs
                 case subtrees of
                   [] -> return $ Node p
                   _  -> return $ Fork p subtrees

pruneDirs = [".", "..", ".git"]

listDirs :: Tree FilePath -> IO [FilePath]
listDirs d = do 
              files <- getDirectoryContents d
              let pruned = filter (\x -> not (elem x pruneDirs)) files
              --let paths = map ((d ++ "/") ++) pruned
              let paths = pruned
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
