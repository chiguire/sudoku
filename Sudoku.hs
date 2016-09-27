module Sudoku where

data SquarePart = UL | UC | UR | CL | CC | CR | DL | DC | DR deriving (Show, Eq, Read)
data SudokuPart = Square SquarePart | Column Int | Row Int deriving (Show, Eq, Read)

removeAll :: (Eq a) => a -> [a] -> [a]
removeAll _ [] = []
removeAll e (x:xs)
    | e == x = removeAll e xs
    | otherwise = (x:removeAll e xs)

whichNonUnique :: (Eq a) => [a] -> [a]
whichNonUnique [] = []
whichNonUnique (x:xs)
    | x `elem` xs = (x: (whichNonUnique $ removeAll x xs))
    | otherwise = whichNonUnique xs

allDifferent :: (Eq a) => [a] -> Bool
allDifferent [] = True
allDifferent (x:xs) = x `notElem` xs && allDifferent xs

isValidSquare :: (Eq a) => [a] -> Bool
isValidSquare [ul, uc, ur, cl, cc, cr, dl, dc, dr] = allDifferent [ul, uc, ur, cl, cc, cr, dl, dc, dr]

boardData (l, _) = l
boardMetadata (_, m) = m

errorReport :: (Eq a) => ([a], SudokuPart) -> Maybe (SudokuPart, [a])
errorReport (square, metadata)
    | allDifferent square = Nothing
    | otherwise           = Just (metadata, whichNonUnique square)

decomposeBoard [ul@[ul_ul, ul_uc, ul_ur, ul_cl, ul_cc, ul_cr, ul_dl, ul_dc, ul_dr]
               ,uc@[uc_ul, uc_uc, uc_ur, uc_cl, uc_cc, uc_cr, uc_dl, uc_dc, uc_dr]
               ,ur@[ur_ul, ur_uc, ur_ur, ur_cl, ur_cc, ur_cr, ur_dl, ur_dc, ur_dr]
               ,cl@[cl_ul, cl_uc, cl_ur, cl_cl, cl_cc, cl_cr, cl_dl, cl_dc, cl_dr]
               ,cc@[cc_ul, cc_uc, cc_ur, cc_cl, cc_cc, cc_cr, cc_dl, cc_dc, cc_dr]
               ,cr@[cr_ul, cr_uc, cr_ur, cr_cl, cr_cc, cr_cr, cr_dl, cr_dc, cr_dr]
               ,dl@[dl_ul, dl_uc, dl_ur, dl_cl, dl_cc, dl_cr, dl_dl, dl_dc, dl_dr]
               ,dc@[dc_ul, dc_uc, dc_ur, dc_cl, dc_cc, dc_cr, dc_dl, dc_dc, dc_dr]
               ,dr@[dr_ul, dr_uc, dr_ur, dr_cl, dr_cc, dr_cr, dr_dl, dr_dc, dr_dr]
               ]
               = 
               [ (ul, Square UL)
               , (uc, Square UC)
               , (ur, Square UR)
               , (cl, Square CL)
               , (cc, Square CC)
               , (cr, Square CR)
               , (dl, Square DL)
               , (dc, Square DC)
               , (dr, Square DR)
               , ([ul_ul, ul_uc, ul_ur, uc_ul, uc_uc, uc_ur, ur_ul, ur_uc, ur_ur], Row 1)
               , ([ul_cl, ul_cc, ul_cr, uc_cl, uc_cc, uc_cr, ur_cl, ur_cc, ur_cr], Row 2)
               , ([ul_dl, ul_dc, ul_dr, uc_dl, uc_dc, uc_dr, ur_dl, ur_dc, ur_dr], Row 3)
               , ([cl_ul, cl_uc, cl_ur, cc_ul, cc_uc, cc_ur, cr_ul, cr_uc, cr_ur], Row 4)
               , ([cl_cl, cl_cc, cl_cr, cc_cl, cc_cc, cc_cr, cr_cl, cr_cc, cr_cr], Row 5)
               , ([cl_dl, cl_dc, cl_dr, cc_dl, cc_dc, cc_dr, cr_dl, cr_dc, cr_dr], Row 6)
               , ([dl_ul, dl_uc, dl_ur, dc_ul, dc_uc, dc_ur, dr_ul, dr_uc, dr_ur], Row 7)
               , ([dl_cl, dl_cc, dl_cr, dc_cl, dc_cc, dc_cr, dr_cl, dr_cc, dr_cr], Row 8)
               , ([dl_dl, dl_dc, dl_dr, dc_dl, dc_dc, dc_dr, dr_dl, dr_dc, dr_dr], Row 9)
               , ([ul_ul, ul_cl, ul_dl, cl_ul, cl_cl, cl_dl, dl_ul, dl_cl, dl_dl], Column 1)
               , ([ul_uc, ul_cc, ul_dc, cl_uc, cl_cc, cl_dc, dl_uc, dl_cc, dl_dc], Column 2)
               , ([ul_ur, ul_cr, ul_dr, cl_ur, cl_cr, cl_dr, dl_ur, dl_cr, dl_dr], Column 3)
               , ([uc_ul, uc_cl, uc_dl, cc_ul, cc_cl, cc_dl, dc_ul, dc_cl, dc_dl], Column 4)
               , ([uc_uc, uc_cc, uc_dc, cc_uc, cc_cc, cc_dc, dc_uc, dc_cc, dc_dc], Column 5)
               , ([uc_ur, uc_cr, uc_dr, cc_ur, cc_cr, cc_dr, dc_ur, dc_cr, dc_dr], Column 6)
               , ([ur_ul, ur_cl, ur_dl, cr_ul, cr_cl, cr_dl, dr_ul, dr_cl, dr_dl], Column 7)
               , ([ur_uc, ur_cc, ur_dc, cr_uc, cr_cc, cr_dc, dr_uc, dr_cc, dr_dc], Column 8)
               , ([ur_ur, ur_cr, ur_dr, cr_ur, cr_cr, cr_dr, dr_ur, dr_cr, dr_dr], Column 9)
               ]

isValidBoard :: (Eq a) => [[a]] -> (Bool, [Maybe (SudokuPart, [a])])
isValidBoard board = (foldr (&&) True $ map (isValidSquare . boardData) $ decomposeBoard board
                     ,map (errorReport) $ decomposeBoard board
                     )

-- findSolution :: (Eq a) => [[a]] -> [[a]] -- Specify 0 for empty spaces
-- findSolution [ul@[ul_ul, ul_uc, ul_ur, ul_cl, ul_cc, ul_cr, ul_dl, ul_dc, ul_dr]
--              ,uc@[uc_ul, uc_uc, uc_ur, uc_cl, uc_cc, uc_cr, uc_dl, uc_dc, uc_dr]
--              ,ur@[ur_ul, ur_uc, ur_ur, ur_cl, ur_cc, ur_cr, ur_dl, ur_dc, ur_dr]
--              ,cl@[cl_ul, cl_uc, cl_ur, cl_cl, cl_cc, cl_cr, cl_dl, cl_dc, cl_dr]
--              ,cc@[cc_ul, cc_uc, cc_ur, cc_cl, cc_cc, cc_cr, cc_dl, cc_dc, cc_dr]
--              ,cr@[cr_ul, cr_uc, cr_ur, cr_cl, cr_cc, cr_cr, cr_dl, cr_dc, cr_dr]
--              ,dl@[dl_ul, dl_uc, dl_ur, dl_cl, dl_cc, dl_cr, dl_dl, dl_dc, dl_dr]
--              ,dc@[dc_ul, dc_uc, dc_ur, dc_cl, dc_cc, dc_cr, dc_dl, dc_dc, dc_dr]
--              ,dr@[dr_ul, dr_uc, dr_ur, dr_cl, dr_cc, dr_cr, dr_dl, dr_dc, dr_dr]
--              ]

-- example1 is deliberately a wrong solution to the board
example1 = isValidBoard [[5,6,8,4,2,3,1,7,9]
                        ,[2,1,7,6,9,5,4,3,8]
                        ,[4,9,3,7,1,8,6,5,2]
                        ,[7,3,1,9,4,2,8,5,6]
                        ,[8,5,4,7,6,1,9,2,3]
                        ,[9,2,6,5,8,3,4,7,1]
                        ,[3,1,7,2,8,5,6,9,4]
                        ,[5,8,9,3,4,6,1,7,2]
                        ,[2,6,4,1,9,7,8,3,5]
                        ]

-- example2 is also wrong
example2 = isValidBoard [[1,2,8,9,3,4,2,6,9]
                        ,[9,3,4,5,6,2,8,1,7]
                        ,[9,5,6,8,1,7,2,3,4]
                        ,[9,8,7,2,3,6,5,4,1]
                        ,[6,2,3,7,9,5,1,8,4]
                        ,[1,7,5,4,6,8,3,2,9]
                        ,[6,5,2,9,1,8,4,7,3]
                        ,[4,5,8,3,7,2,6,5,1]
                        ,[7,9,1,5,4,6,3,8,2]
                        ]

-- example3 should be fine
example3 = isValidBoard [[4,9,1,7,6,5,8,3,2]
                        ,[2,5,7,8,3,4,1,6,9]
                        ,[8,6,3,9,2,1,4,5,7]
                        ,[3,5,8,9,2,7,1,4,6]
                        ,[7,2,6,5,4,1,9,8,3]
                        ,[1,9,4,6,3,8,5,7,2]
                        ,[2,1,4,6,7,9,5,8,3]
                        ,[6,7,5,3,1,8,4,9,2]
                        ,[3,8,9,2,4,5,7,1,6]
                        ]