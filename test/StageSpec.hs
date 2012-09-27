module StageSpec where

import Test.Hspec
import Stage
import Core
import Data.List

stage = mkStage (10,20) 

s1 = mkState [Block (0,0) TKind]
s2 = mkState [Block (3,17) TKind]

spec = do
    describe "Moving to the left the current piece should" $ do
        it "change blocks in the view" left1
        it "as long as it doesn't hit the wall" leftWall1
        it "or another block in the grid" leftHit1


    describe "Moving to the right the current piece should" $ do
        it "change the blocks in view" right1
        
    describe "Rotating the current piece should" $ do
        it "change the blocks in the view" rotate1
         
    describe "Ticking the current pieces should" $ do
        it "change the blocks in the view" True

left1 = let blks = blocksGS . moveLeft $ s1
        in  (sort $ map posBlock blks) == (sort $ [(0, 0), (3, 17), (4, 17), (5, 17), (4, 18)])

leftWall1 = let blks = blocksGS . moveLeft . moveLeft. moveLeft. moveLeft . moveLeft $ s1
            in (sort $ map posBlock blks) == (sort $ [(0, 0), (0, 17), (1, 17), (2, 17), (1, 18)])

leftHit1 = let blks = blocksGS . moveLeft $ s2
           in (sort $ map posBlock blks) == (sort $ [(3, 17), (4, 17), (5, 17), (6, 17), (5, 18)]) 

right1 = let blks = blocksGV . viewGS . moveRight $ s1
         in  (sort $ map posBlock blks) == (sort $ [(0, 0),(5, 17), (6, 17), (7, 17), (6, 18)]) 
 
rotate1 = let blks = blocksGV . viewGS . rotateCW $ s1
          in (sort $ map posBlock blks) == (sort $ [(0, 0), (5, 18), (5, 17), (5, 16), (6, 17)])

tick1 = let blks = blocksGS $ tick s1
        in (sort $ map posBlock blks) == (sort $ [(0, 0), (4, 16), (5, 16), (6, 16), (5, 17)])
