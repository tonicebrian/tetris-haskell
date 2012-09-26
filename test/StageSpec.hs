module StageSpec where

import Test.Hspec
import Stage
import Core
import Data.List

stage = mkStage (10,20) 

s1 = mkState [Block (0,0) TKind]

spec = do
    describe "Moving to the left the current piece should" $ do
        it "change blocks in the view" $
            let blks = blocksGV . viewGS . moveLeft $ s1
            in  (sort $ map posBlock blks) == (sort $ [(0, 0), (3, 17), (4, 17), (5, 17), (4, 18)])
        it "as long as it doesn't hit the wall" $
            let blks = blocksGV . viewGS . moveLeft . moveLeft. moveLeft. moveLeft . moveLeft $ s1
            in (sort $ map posBlock blks) == (sort $ [(0, 0), (0, 17), (1, 17), (2, 17), (1, 18)])


    describe "Moving to the right the current piece should" $ do
        it "change the blocks in view" $
            let blks = blocksGV . viewGS . moveRight $ s1
            in  (sort $ map posBlock blks) == (sort $ [(0, 0),(5, 17), (6, 17), (7, 17), (6, 18)]) 

    describe "Rotating the current piece should" $ do
        it "change the blocks in the view" $
            let blks = blocksGV . viewGS . rotateCW $ s1
            in (sort $ map posBlock blks) == (sort $ [(0, 0), (5, 18), (5, 17), (5, 16), (6, 17)])

    --describe "Ticking the current pieces should" $ do
    --    it "change the blocks in the view" $ 
    --        let blks = blocksGS $ tick s1
    --        in (sort $ map posBlock blks) == (sort $ [(0, 0), (4, 16), (5, 16), (6, 16), (5, 17)])


        
