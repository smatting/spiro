{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE AllowAmbiguousTypes #-}


import Diagrams.Prelude
import qualified Graphics.SVGFonts as SF
import Diagrams.Backend.SVG
import Diagrams.Backend.SVG.CmdLine
import Data.List (intercalate)
import Diagrams.TwoD.Layout.Grid
import System.Random
import Control.Monad


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
        handles t = zipWith
            (\i sp -> (1/(2^i), sp * t @@ turn) ^. from r2PolarIso)
            [0..] turnSpeeds
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


main = do
    spiros <- replicateM 20 $ randomSpiro 3 9
    let example = gridCat spiros
    -- let example = spiroDiag2 [-5, 7, -7]
    renderSVG "bar.svg" (dims (r2 (800, 600))) example
    
