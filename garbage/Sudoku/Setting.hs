module Setting where

-- User Settings

subGridSize :: Int
subGridSize = 3

-- end

gridSize :: Int
gridSize = subGridSize * subGridSize

nums :: [Int]
nums = [1.. gridSize]

subNums :: [Int]
subNums = [1.. subGridSize]
