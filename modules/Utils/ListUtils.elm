module ListUtils
    ( uniq, contains, getAt
    ) where

{-| A library for some additional list utilities -}
import List exposing ( isEmpty, head, tail )

{-| Remove all duplicate elements from a list using the given equality function, keeping the last occurence of equal items.

    a = { id= 1, occ= 1 }
    b = { id= 2, occ= 1 }
    c = { id= 2, occ= 2 }
    d = { id= 2, occ= 3 }

    uniq (\x y -> x.id == y.id) [a,b,c,d] == [a,d]
    uniq (==) [a,b,c,d] == [a,b,c,d]
-}
uniq : (a -> a -> Bool) -> List a -> List a
uniq eq list =
  if isEmpty list then []
  else let  (Just h) = head list
            (Just t) = tail list
       in if contains eq h t then uniq eq t
          else h :: uniq eq t

{-| Checks if a list contains an element based on the given equality function.

    a = { id= 1, occ= 1 }
    b = { id= 2, occ= 1 }
    c = { id= 2, occ= 2 }
    d = { id= 2, occ= 3 }

    contains (\x y -> x.id == y.id) b [a,b,c,d] == True
    contains (\x y -> x.id == y.id) b [a,d] == True
    contains (==) b [a,d] == False
-}
contains : (a -> a -> Bool) -> a -> List a -> Bool
contains eq elem list =
  if isEmpty list then False
  else let (Just h) = head list in
    if elem `eq` h then True
    else let (Just t) = tail list in
      contains eq elem t

{-| Get an element of a list at the specified index.

    getAt -1 [1,2,3] == Nothing
    getAt 0 [1,2,3] == Just 1
-}
getAt : List a -> Int -> Maybe a
getAt list index  =
  if isEmpty list || index < 0 then Nothing
  else
    let h = head list
    in if index == 0 then h else
      case tail list of
        Just t -> getAt t (index-1)
        _ -> Nothing
