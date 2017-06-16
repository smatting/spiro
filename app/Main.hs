{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
-- or:
-- import Diagrams.Backend.xxx.CmdLine
-- where xxx is the backend you would like to use.

myCircle :: Diagram B
myCircle = circle 200

example :: Diagram B
example = hsep 1 $ map showOrigin
         $ [ d, mempty ||| d, d ||| mempty ]
   where d = square 1


main = mainWith example
