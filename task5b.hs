deletei2:: ([a],[a]) -> [a]
deletei2 twolists = (fst twolists) ++ (tail (snd twolists))
deletei:: Int -> [a] -> [a]
deletei i list = deletei2 (splitAt i list)

addToList:: a -> [a] -> [a]
addToList a list = a:list
addToLists:: (a, [[a]]) -> [[a]]
addToLists (a, lists) = map (addToList a) lists


dev_li:: [(a, [a])] -> Int -> [a] -> [(a, [a])]
dev_li result index slist =  if (length slist) > index
    then dev_li (result ++ [(slist !! index, deletei index slist)]) (index + 1) slist
    else result

dev_li2:: [a] -> [(a, [a])]
dev_li2 list = dev_li [] 0 list

add_devide:: (a, [a]) -> [[a]]
add_devide (a, list) = addToLists (a, perm list)

--main function: returns list of permutations for a list given
perm:: [a] -> [[a]]
perm [a] = [[a]]
perm list = foldl (++) [] (map (add_devide) (dev_li2 list))
