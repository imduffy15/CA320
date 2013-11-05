shortest :: [[a]] -> [a]
shortest [] = []
shortest [soleList] = soleList
shortest (firstList : remainingLists) =
    shortestOfTwo firstList (shortest remainingLists)

shortestOfTwo firstList secondList =
    if length firstList < length secondList
      then firstList
      else secondList
