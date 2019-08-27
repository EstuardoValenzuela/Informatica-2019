module Main exposing(..)
esPrimo: Int -> Bool
esPrimo n =  esPrimoN 2 n 
esPrimoN  cont n =
    if n == 2 
    then True 
    else if modBy   cont  n  == 0 
    then False 
    else if cont == n-1
    then True
    else esPrimoN (cont + 1) n

fibonacci:  number -> number1
fibonacci n = 
    if n == 0 
    then 0
    else if n == 1
    then 1 
    else if n > 1
    then fibonacci(n-1) + fibonacci(n-2)
    else 5

primos: Int -> List Int
primos n = 
    if n < 2
    then []
    else if esPrimo n == False 
    then primos (n - 1)
    else n :: primos (n - 1)

nPrimos: Int -> List Int
nPrimos s = conta (s , 2) 
conta (s,y) = 
    if s == 0 
    then []
    else if esPrimo y == False
    then conta (s,y + 1)
    else y :: conta (s-1, y+1)
