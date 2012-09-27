module Minimal where

import Expression
import Setting

entry, row, column, subGrid :: Expr (Int, Int, Int)
entry = allAnd [ allOr [Var (x, y, z) | z <- nums]
               | x <- nums
               , y <- nums
               ]           
row = allAnd [ Or (Not (Var (x, y, z))) (Not (Var (i, y, z)))
             | y <- nums 
             , z <- nums
             , x <- init nums                             
             , i <- nums
             , i > x
             ]
column = allAnd [ Or (Not (Var (x, y, z))) (Not (Var (x, i, z)))
                | x <- nums 
                , z <- nums
                , y <- init nums                             
                , i <- nums
                , i > y
                ]
subGrid = And (allAnd [ Or (Not (Var (subGridSize * i + x, subGridSize * j + y, z)))
                           (Not (Var (subGridSize * i + x, subGridSize * j + k, z)))
                      | z <- nums
                      , i <- map pred subNums
                      , j <- map pred subNums
                      , x <- subNums
                      , y <- subNums
                      , k <- subNums
                      , k > y
                      ])
              (allAnd [ Or (Not (Var (subGridSize * i + x, subGridSize * j + y, z)))
                           (Not (Var (subGridSize * i + k, subGridSize * j + l, z)))
                      | z <- nums
                      , i <- map pred subNums
                      , j <- map pred subNums
                      , x <- subNums
                      , y <- subNums
                      , k <- subNums
                      , k > x
                      , l <- subNums
                      ])
    
-- 変数の数
numVar :: Int
numVar = gridSize * gridSize * gridSize

-- Clauseの数
numClause :: Int -> Int
numClause n = gridSize * gridSize + 3 * gridSize * (combination gridSize 2) * gridSize + n
