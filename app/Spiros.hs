{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}


import Prelude hiding (writeFile, (.=))
import System.Random
import Control.Monad
import Data.List.Split
import Data.List (intercalate)
import System.Directory
import Data.ByteString.Lazy (writeFile)
import Data.Aeson

import Diagrams.Prelude hiding ((.=))
import qualified Graphics.SVGFonts as SF
import Diagrams.Backend.SVG
import Diagrams.Backend.SVG.CmdLine
import Diagrams.TwoD.Layout.Grid

import qualified Data.UUID.V4 as UUID4


normalizeRange :: [Double] -> [Double]
normalizeRange xs = map (flip (/) d . flip (-) xmin) xs
    where
        xmax = foldr1 max xs
        xmin = foldr1 min xs
        d = xmax - xmin


spiro :: [Double] -> Int -> ([P2 Double], [Double])
spiro turnSpeeds n =
        (map pathVertex (range n),
         map pathSpeed (range n))
    where
        range n = [0,(1/fromIntegral n)..1]
        handles' t = zipWith
            (\i sp -> (1/(2^i), sp * t @@ turn) ^. from r2PolarIso)
            [0..] turnSpeeds
        handles t = map
            (\sp -> (1, sp * t @@ turn) ^. from r2PolarIso)
            turnSpeeds
        pathVertex t = t & handles & sum & (.+^) origin
        pathSpeed t = sqrt $ (xdot t)^2 + (ydot t)^2
        xdot t = foldr1 (+) $ zipWith
            (\i sp -> - sin(sp * 2 * pi * t) * sp * 2 * pi * 1/(2^i))
            [0..] turnSpeeds
        ydot t = foldr1 (+) $ zipWith
            (\i sp -> cos(sp * 2 * pi * t) * sp * 2 * pi * 1/(2^i))
            [0..] turnSpeeds


spiroDiag :: [Double] -> Diagram B
spiroDiag turnSpeeds =
    fromVertices vertices
              # rotate (90 @@ deg)
              # strokeLine
              # lwL 0.03
    where (vertices, pathSpeeds) = (spiro turnSpeeds 1000)


colors = cycle [aqua, orange, deeppink, blueviolet, crimson, darkgreen]


spiroDiag2 :: [Double] -> Diagram B
spiroDiag2 turnSpeeds =
        path # explodeTrail # zipWith lw linewidths # mconcat
    where (vertices, pathSpeeds) = (spiro turnSpeeds 10000)
          path = fromVertices vertices
                  # rotate (90 @@ deg)
          linewidths =  pathSpeeds
                        # map (\v -> 0.3 * pow (1/v) 0.5) 
                        # map local


pow a b = exp $ (log a) * b


logistic x = 0.5 + 0.5 * tanh(0.5 * x)


spiroWithSub :: [Double] -> Diagram B
spiroWithSub turnSpeeds =
    spiroDiag turnSpeeds === ((text subtitle) <> strutY 1) # fontSizeL 0.5
    where subtitle = intercalate ", " (map (show . round) turnSpeeds)


text''' :: String -> IO (Diagram B)
text''' t = do
    print "huhu"
    font <- SF.loadFont "fonts/JosefinSans-fix1.svg"
    print "loaded"
    return $ stroke (SF.textSVG' (SF.TextOpts font SF.INSIDE_H SF.KERN False 1 1) t)


randomSpeeds :: Integer -> Integer -> IO [Double]
randomSpeeds nhandles nmax = do
        abs <- replicateM (fromIntegral nhandles) randomAbs
        signs <- replicateM (fromIntegral nhandles) randomSign
        return $ map fromIntegral $ zipWith (*) signs abs
    where
        randomAbs = randomRIO (1, nmax)
        randomSign = do
            x <- randomRIO (0, 1)
            return $ 2*x - 1


randomSpiro :: Integer -> Integer -> IO (Diagram B)
randomSpiro nhandles nmax = do
    turnSpeeds <- randomSpeeds nhandles nmax
    return $ spiroWithSub turnSpeeds


writeMeta :: [Double] -> String -> IO ()
writeMeta turnSpeeds directory = 
    writeFile (directory ++ "/meta.json" ) bs
    where bs = encode $ object ["turnSpeeds" .= turnSpeeds]


saveSpiro :: FilePath -> [Double] -> (Diagram B) -> IO ()
saveSpiro basedir turnSpeeds diagram = do
    writeMeta turnSpeeds basedir
    renderSVG (basedir ++ "/diagram.svg") (dims (r2 (800, 600))) diagram


likes1 :: [[Double]]
likes1 = [[12, 6, 10],
 [-2, -7, -12],
 [-4, -9, 1],
 [-3, 8, -3],
 [6, 1, -14],
 [-7, -13, 6],
 [-7, -9, 9],
 [-13, 14, -12],
 [-9, 4, -13],
 [7, 5, 2],
 [-3, -8, -13],
 [-11, 13, 1],
 [11, 15, -15],
 [-4, 12, -1],
 [14, 1, 13],
 [11, -1, 12],
 [-7, -13, 2],
 [5, 1, 9],
 [-13, 9, -9],
 [8, -6, 15],
 [10, 5, 12],
 [9, 4, -14],
 [7, -11, -5],
 [-5, -12, 2],
 [-4, 5, 11],
 [-9, 4, -13],
 [9, 3, -3],
 [-2, -7, 13],
 [-6, 7, -11],
 [11, -10, -3]]


likes2 :: [[Double]]
likes2 = [[-2, -15, 11],
 [-13, -15, 15],
 [-5, 9, 9],
 [-4, -1, -13],
 [7, 3, -15],
 [4, -14, -14],
 [10, -6, 14],
 [-5, 9, -12],
 [5, -7, -11],
 [6, -3, -14],
 [-4, -13, -7],
 [-4, -7, -13],
 [-5, 9, -12],
 [-1, -7, 5],
 [6, -12, 15],
 [-9, -5, -9],
 [-11, 5, 5],
 [4, -8, 10],
 [8, -10, -1],
 [11, -13, 13],
 [14, 11, 11],
 [4, -6, 9],
 [-4, 12, 6],
 [15, -8, 8],
 [5, -3, -13],
 [-2, -2, 7],
 [5, -7, 12],
 [9, 8, 1],
 [-3, -8, -13],
 [-3, 9, 13]]

likesSL :: [[Double]]
likesSL = [[3, -13, 4],
 [6, 10, 1],
 [-7, 11, -1],
 [10, -1, 6],
 [14, 5, 2],
 [-3, 9, -15],
 [-9, 12, -3],
 [5, 9, 7],
 [13, 13, 15],
 [-11, -8, 1],
 [3, 11, 15],
 [-4, 10, 12],
 [9, 7, 11],
 [8, 10, 6],
 [15, 11, 9],
 [-11, -15, -3],
 [10, -14, 2],
 [7, -10, 3],
 [-3, 12, -5],
 [15, 14, 12],
 [-10, -9, -4],
 [-11, -7, 9],
 [-6, -7, 13],
 [-13, -11, -15],
 [-5, -8, -7],
 [4, 5, 5],
 [11, 13, 8],
 [-15, -13, -3],
 [8, 10, 5],
 [8, -6, 15],
 [12, 5, 9],
 [14, -11, -6],
 [5, 9, 4],
 [8, 8, 7],
 [-14, -8, -8],
 [-13, 12, 2],
 [5, 8, 7],
 [-15, -2, -13],
 [-7, -3, -9],
 [12, 4, 11],
 [15, 13, 5],
 [2, -1, 11]]


mainPoster turnSpeedsList filename =
    renderSVG filename (dims (r2 (800, 600))) example
    where diagrams = map spiroWithSub turnSpeedsList
          example = gridCat diagrams

renderGrid :: [[Double]] -> FilePath -> IO ()
renderGrid lturnSpeeds outfile =
    renderSVG outfile (dims (r2 (210 * s, 297 * s))) $ gridCat' 3 (sameBoundingRect diags)
    where
        diags = map spiroWithSub lturnSpeeds
        s = 1.2 * (1 / 0.39)

mainMultiPoster = 
    zipWithM_ (\ts i -> renderGrid ts ("posters/postersl_" ++ (show i) ++ ".svg"))
              (chunksOf 12 likesSL)
              [0..]

mainRandomFiles = 
    replicateM 500 $ f
    where
        outdir = "output"
        f = do
           id <- UUID4.nextRandom
           turnSpeeds <- randomSpeeds 3 15
           let basedir = outdir ++ "/" ++ (show id)
               diagram = spiroDiag turnSpeeds
           createDirectoryIfMissing True basedir
           saveSpiro basedir turnSpeeds diagram

main' = do
    spiros <- replicateM 20 $ randomSpiro 3 15
    let example = gridCat spiros
    -- let example = spiroDiag2 [-5, 7, -7]
    renderSVG "bar.svg" (dims (r2 (800, 600))) example
    
