module Utils exposing (..)

splitList : Int -> List a -> List (List a)
splitList len l = case l of
    [] -> []
    list -> List.take len list :: splitList len (List.drop len list)

at :  Int -> List a -> Maybe a
at n xs = List.head (List.drop n xs)

catMaybes : List (Maybe a) -> List a
catMaybes ms =
    case ms of
        [] -> []
        (m :: xs) -> case m of
           Nothing -> catMaybes xs
           Just x -> x :: catMaybes xs
