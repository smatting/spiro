{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

{-# LANGUAGE AllowAmbiguousTypes #-}



import Diagrams.Prelude
import qualified Graphics.SVGFonts as SF
import Diagrams.Backend.SVG
import Diagrams.Backend.SVG.CmdLine
-- or:
-- import Diagrams.Backend.xxx.CmdLine
-- where xxx is the backend you would like to use.
--
--
--
-- text' d s = (strokeP $ SF.textSVG' (SF.TextOpts SF.lin2 SF.INSIDE_H SF.KERN False d d) s)
--           # lw none
-- example :: Diagram B
-- example = text' 5 "Hello" # fc blue ||| text' 3 "world" # fc green

--
-- example :: Diagram B
-- example = text "Hello world!" <> rect 8 1


spiroPoints :: [Double] -> Int -> [P2 Double]
spiroPoints speeds n = map spiro (range n)
    where
        range n = [0,(1/fromIntegral n)..1]
        handles t = zipWith
            (\i speed ->  (1/(2^i), speed * t @@ turn) ^. from r2PolarIso)
            [0..] speeds
        spiro t = t & handles & sum & (.+^) origin

example :: Diagram B
example = fromVertices (spiroPoints [5, 7, -7] 1000)
          # rotate (90 @@ deg)
          # strokeLine


text''' :: String -> IO (Diagram B)
text''' t = do
    print "huhu"
    font <- SF.loadFont "fonts/JosefinSans-fix1.svg"
    print "loaded"
    return $ stroke (SF.textSVG' (SF.TextOpts font SF.INSIDE_H SF.KERN False 1 1) t)


-- zz = (dims (r2 (300, 170)))

-- main = mainWith example

main = do
    example <- (text''' "huhu, clara")
    renderSVG "bar.svg" (dims (r2 (200, 170))) example
    
