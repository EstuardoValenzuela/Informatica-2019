module Main exposing (..)


iFilter: Int -> List Int -> List Int
iFilter x xs = case xs of 
               []->[]
               b::bs-> if modBy x b == 0 then iFilter x bs else b::iFilter x bs 

filter: (Int->Bool)->List Int->List Int
filter f xs =case xs of 
             []->[]
             b::bs ->if f b == True then b:: filter f bs else filter f bs 

iZipWith: List Int -> List Int -> List Int
iZipWith xs ys = case (xs, ys) of
                 ([], hs)-> []
                 (hs, [])-> []
                 (b::bs,c::cs)-> (b+c)::iZipWith bs cs

zipWith: (Int->Int->Int)->List Int-> List Int-> List Int
zipWith p xs ys = case (xs,ys) of
                  ([], zs)-> []
                  (hs, [])-> []
                  (b::bs,c::cs) -> (p b c)::(zipWith p bs cs) 
