module StageSpec where

import Test.Hspec
import Stage
import Core
import Data.List

stage = mkStage (10,20) 

spec = do
    describe "Moving to the left the current piece should" $ do
        it "change blocks in the view" $
            let blks = blocks . view . moveLeft $ stage
            in  (sort $ map posBlock blks) == (sort $ [(0, 0), (3, 17), (4, 17), (5, 17), (4, 18)])
        it "as long as it doesn't hit the wall" $
            let blks = blocks . view . moveLeft . moveLeft. moveLeft. moveLeft . moveLeft $ stage
            in (sort $ map posBlock blks) == (sort $ [(0, 0), (0, 17), (1, 17), (2, 17), (1, 18)])


    describe "Moving to the right the current piece should" $ do
        it "change the blocks in view" $
            let blks = blocks . view . moveRight $ stage
            in  (sort $ map posBlock blks) == (sort $ [(0, 0),(5, 17), (6, 17), (7, 17), (6, 18)]) 
        
