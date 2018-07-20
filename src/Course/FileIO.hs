{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}

module Course.FileIO where

import Course.Core
import Course.Applicative
import Course.Monad
import Course.Functor
import Course.List

{-

-- need to learn a new type: ":info IO"

Useful Functions --

  getArgs :: IO (List Chars)
  putStrLn :: Chars -> IO ()
  readFile :: FilePath -> IO Chars
  lines :: Chars -> List Chars
  void :: IO a -> IO ()
-- type FilePath = List Char       -- Defined at src/Course/List.hs:761:1

Abstractions --
  Applicative, Monad:

    <$>, <*>, >>=, =<<, pure

Tuple Functions that could help --

  fst :: (a, b) -> a
  snd :: (a, b) -> b
  (,) :: a -> b -> (a, b)

Problem --
  Given a single argument of a file name, read that file,
  each line of that file contains the name of another file,
  read the referenced file and print out its name and contents.

Consideration --
  Try to avoid repetition. Factor out any common expressions.
  
Example --
Given file files.txt, containing:
  a.txt
  b.txt
  c.txt

And a.txt, containing:
  the contents of a

And b.txt, containing:
  the contents of b

And c.txt, containing:
  the contents of c

To test this module, load ghci in the root of the project directory, and do
    >> :main "share/files.txt"

Example output:

$ ghci
GHCi, version ... 
Loading package...
Loading ...
[ 1 of 28] Compiling (etc...
...
Ok, modules loaded: Course, etc...
>> :main "share/files.txt"
============ share/a.txt
the contents of a

============ share/b.txt
the contents of b

============ share/c.txt
the contents of c

-}

-- Given the file name, and file contents, print them.
-- Use @putStrLn@.
printFile ::
  FilePath
  -> Chars
  -> IO ()
printFile n c =
   do putStrLn ("====== " ++ n ++ "\n" ++ c)
-- IO is a Monad ~


-- Given a list of (file name and file contents), print each.
-- Use @printFile@.
printFiles ::
  List (FilePath, Chars)
  -> IO ()
printFiles list =
   void (sequence ((\(name, contents) -> printFile name contents) <$> list))
-- here, void: IO (List()) --> IO ();
-- sequence :: Applicative k => List (k a) -> k (List a)

-- uncurry :: (a -> b -> c) -> (a, b) -> c
{-     to SIMPLIFY ~
  void . sequence . (<$>) (uncurry printFile)
-}


-- Given a file name, return (file name and file contents).
-- Use @readFile@.
getFile ::
  FilePath
  -> IO (FilePath, Chars)
getFile name = (<$>) (\c -> (name, c)) (readFile name)
-- readFile :: FilePath -> IO Chars

{-
  lift2 (<$>) (,) readFile
-}
-- \x -> f (g x) (h x)
-- lift2 f g h


-- Given a list of file names, return list of (file name and file contents).
-- Use @getFile@.
getFiles ::
  List FilePath
  -> IO (List (FilePath, Chars))
getFiles lst =
--    sequence ((\name -> getFile name) <$> lst)
   sequence ((<$>) getFile lst)
{-
  getFiles Nil = pure Nil
  getFiles (h:.t) =
      getFile h  >>= \pairOfStuff ->
      getFiles t >>= \listOfPairOfStuff ->
      pure (pairOfStuff :. listOfPairOfStuff)
-}


-- Given a file name, read it and for each line in that file, read and print contents of each.
-- Use @getFiles@ and @printFiles@.
run ::
  FilePath
  -> IO ()
run name = readFile name         >>= \line ->
           getFiles (lines line) >>= \file ->
           printFiles file
-- lines :: Chars -> List Chars


-- /Tip:/ use @getArgs@ and @run@
-- >>> :main
-- pass in args

main ::
  IO ()
main =
  do a <- getArgs
     case a of
       Nil  -> putStrLn "pass in args"
       h:._ -> run h
-- getArgs :: IO (List Chars)
-- run :: FilePath -> IO ()


----

-- Was there was some repetition in our solution?
-- ? `sequence . (<$>)`
-- ? `void . sequence . (<$>)`
-- Factor it out.
