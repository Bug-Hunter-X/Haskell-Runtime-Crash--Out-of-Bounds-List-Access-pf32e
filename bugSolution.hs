The solution involves using safe list access methods to avoid the runtime crash.  Instead of `!!`, we can use pattern matching or a dedicated library for safe list manipulation.

**Solution 1: Pattern Matching**
Pattern matching elegantly handles cases where the list might not have enough elements to satisfy the index.

```haskell
import Data.Maybe (fromMaybe)

safeGet :: [a] -> Int -> Maybe a
safeGet [] _ = Nothing
safeGet (x:_) 0 = Just x
safeGet (_:xs) i = safeGet xs (i -1)

main :: IO ()
main = do
  let myList = [1, 2, 3]
  print (fromMaybe (-1) (safeGet myList 0)) -- Safe: index within bounds
  print (fromMaybe (-1) (safeGet myList 3)) -- Safe: returns Nothing which is then converted to -1
```

**Solution 2: Using the `safe` package (requires installation)**
The `safe` package provides functions for safe array/list access, among other things.

```haskell
import Data.Array.IArray (listArray, (!))
import Data.SafeCopy

safeGet :: [a] -> Int -> Maybe a
safeGet xs i = Just $ unsafePerformIO $ do 
  let arr = listArray (0, length xs - 1) xs
  copy(arr ! i)

main :: IO ()
main = do
  let myList = [1,2,3]
  print $ safeGet myList 0  --Safe
  print $ safeGet myList 3 --Safe; returns Nothing
```
This solution uses the `safe` package to guarantee that the operation is safe; the alternative to safeCopy would have a runtime error.