module Extended where

import Expression
import Setting

entry, row, column, subGrid :: Expr (Int, Int, Int)
entry = allAnd [ Or (Not (Var (x, y, z))) (Not (Var (x, y, i)))
               | x <- nums
               , y <- nums
               , z <- init nums
               , i <- nums
               , i > z
               ]
row = allAnd [ allOr [ Var (x, y, z)
                     | x <- nums
                     ]
             | y <- nums
             , z <- nums
             ]
column = allAnd [ allOr [ Var (x, y, z)
                        | y <- nums
                        ]
                | x <- nums
                , z <- nums
                ]
subGrid = allAnd [ allOr [ Var (subGridSize * i + x, subGridSize * j + y, z)
                         | x <- subNums
                         , y <- subNums
                         ]
                 | z <- nums
                 , i <- map pred subNums
                 , j <- map pred subNums
                 ]

-- 変数の数
numVar :: Int
numVar = gridSize * gridSize * gridSize

-- Clauseの数
numClause :: Int -> Int
numClause n = 3 * gridSize * gridSize + gridSize * (combination gridSize 2) * gridSize + n
