data Tagged_tree a = Empty | Leaf {tag::Int, elem::a} | Node {tag::Int, left::(Tagged_tree a), right::(Tagged_tree a)}

instance Show a => Show (Tagged_tree a) where
    show Empty = show "| |"
    show (Leaf x a) = show "|" ++ show x ++ show ":" ++ show a ++ show "|"
    show (Node x left right) = show x ++ show ":" ++ show "<" ++ show left ++ show "+" ++ show right ++ show ">"

find_pow number = aux number 0 where
    aux number cnt
     |number == 0 = 2^cnt
     |otherwise = aux (div number 2) (cnt + 1)

splitList list = aux list (div (find_pow (length list)) 4) (div (find_pow (length list)) 2) (length list) where
    aux list mini maxi length
     |length - mini < maxi = splitAt (length-mini) list--(length - mini, mini)
     |otherwise = splitAt maxi list--(maxi, length - maxi)

addTags list = aux list [] 0 where
    aux [] res ind = res
    aux (x:xs) res ind = aux xs (res ++ [(ind, x)]) (ind + 1)


list_to_tree list = fun1 (addTags list) where
    fun1 [] = Empty
    fun1 [x] = Leaf (fst x) (snd x)
    fun1 list = aux (splitList list) where
        aux tuple = Node (fst (last (fst tuple))) (fun1 (fst tuple)) (fun1 (snd tuple))

get_elem index tree = aux tree index where
    aux Empty ind = error "no such element"
    aux (Leaf tag elem) ind = elem
    aux tree index
     |index <= (tag tree) = aux (left tree) index
     |otherwise = aux (right tree) index
