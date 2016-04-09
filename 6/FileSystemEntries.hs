module FileSystemEntries where

data FSEntry = Folder String [FSEntry] | File String String

-- (a)

fFSE :: (String -> [a] -> a) -> (String -> String -> a) -> FSEntry -> a
fFSE _ g (File a b) = g a b
fFSE f g (Folder a b) = f a (map (fFSE f g) b)

-- (b)

-- i.

number :: FSEntry -> Int
number = fFSE (\_ x -> 1 + sum x) (\_ _ -> 1)

-- ii.

count :: Char -> FSEntry -> Int
count c = fFSE (const sum) (\_ x -> length (filter (== c) x))

-- (c)

paths :: FSEntry -> [String]
paths (File n _) = [n]
paths (Folder n e) = concatMap (map (combine n) . paths) e
  where combine a b = a ++ "/" ++ b


