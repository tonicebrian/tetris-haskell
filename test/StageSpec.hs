module StageSpec where

import Test.Hspec
import Stage
import Core
import Core.Game
import Data.List

ttt = [TKind,TKind,TKind]
s1 = mkState [Block (0,0) TKind] (repeat TKind)
s2 = mkState [Block (3,17) TKind] ttt 
s3 = mkState (map (\x -> Block x TKind) $ [(0, 0), (1, 0), (2, 0), (3, 0), (7, 0), (8, 0), (9, 0)]) ttt
s4 = mkState [] [OKind,OKind]
stateAboutToFillLastLine = mkState (map (\x -> Block (x,0) TKind) [0..8]) ttt 

spec = do
    describe "Moving to the left the current piece should" $ do
        it "change blocks in the view" left1
        it "as long as it doesn't hit the wall" leftWall1
        it "or another block in the grid" leftHit1


    describe "Moving to the right the current piece should" $ do
        it "change the blocks in view" right1
        it "as long as it doesn't hit the wall" rightWall1
        
    describe "Rotating the current piece should" $ do
        it "change the blocks in the view" rotate1
         
    describe "Ticking the current pieces should" $ do
        it "change the blocks in the view" tick1
        it "or spawn a new piece when it hits something" tick2
        it "it should also clear out of full rows" tick3

    describe "The current piece should" $ do
        it "be initialized to the first element in the state" init1

    describe "Dropping the current piece should" $ do
        it "tick the piece until it hits something" drop1

    -- Bug
    describe "An incomplete line should not" $ do
        it "be marked as full" fullLine
        it "disappear" disappear1

left1 = let blks = blocksGS . moveLeft $ s1
        in  (sort $ map posBlock blks) `shouldBe` (sort $ [(0, 0), (3, 17), (4, 17), (5, 17), (4, 18)])

leftWall1 = let blks = blocksGS . moveLeft . moveLeft. moveLeft. moveLeft . moveLeft $ s1
            in (sort $ map posBlock blks) `shouldBe` (sort $ [(0, 0), (0, 17), (1, 17), (2, 17), (1, 18)])

leftHit1 = let blks = blocksGS . moveLeft $ s2
           in (sort $ map posBlock blks) `shouldBe` (sort $ [(3, 17), (4, 17), (5, 17), (6, 17), (5, 18)]) 

right1 = let blks = blocksGV . viewGS . moveRight $ s1
         in  (sort $ map posBlock blks) `shouldBe` (sort $ [(0, 0),(5, 17), (6, 17), (7, 17), (6, 18)]) 

rightWall1 = let blks = blocksGS . moveRight . moveRight. moveRight. moveRight . moveRight $ s1
            in (sort $ map posBlock blks) `shouldBe` (sort $ [(0, 0), (7, 17), (8, 17), (9, 17), (8, 18)])

rotate1 = let blks = blocksGV . viewGS . rotateCW $ s1
          in (sort $ map posBlock blks) `shouldBe` (sort $ [(0, 0), (5, 18), (5, 17), (5, 16), (6, 17)])

tick1 = let blks = blocksGS $ tick s1
        in (sort $ map posBlock blks) `shouldBe` (sort $ [(0, 0), (4, 16), (5, 16), (6, 16), (5, 17)])

tick2 = let moves = foldr (.) id (take 18 $ repeat tick)
            blks = map posBlock $ blocksGS $ moves s1
        in sort blks `shouldBe` sort [(0, 0), (4, 0), (5, 0), (6, 0), (5, 1), (4, 17), (5, 17), (6, 17), (5, 18)]

tick3 = let moves = foldr (.) id (take 18 $ repeat tick)
            blks = map posBlock $ blocksGS $ moves s3
        in sort blks `shouldBe`sort [(5, 0), (4, 17), (5, 17), (6, 17), (5, 18)]

init1 = let k = (kindPiece . currentPieceGS) s4
            blks = map posBlock $ (blocksGS . moveLeft) s4
        in do (k `shouldBe` OKind) 
              (sort blks `shouldBe` sort [(3, 17), (4, 17), (3, 16), (4, 16)])

drop1 = let blks = map posBlock $ (blocksGS . dropPiece) s1
        in sort blks `shouldBe` sort [(0,0),(4,0),(5,0),(6,0),(5,1),(4,17),(5,17),(6,17),(5,18)]

disappear1 = let blks = map posBlock $ (blocksGS . moveLeft . moveLeft . moveLeft . dropPiece . dropPiece . moveRight . moveRight . moveRight ) s1
             in (length . nub . map (snd)) blks `shouldBe` 4 -- Two lines and the spawned figure

blocksInLine i gs = filter (\(a,b) -> b == i) $ map posBlock $ blocksGS gs

fullLine = blocksInLine 0 (tick stateAboutToFillLastLine) `shouldBe` blocksInLine 0 stateAboutToFillLastLine
