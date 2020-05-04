module Lib where

import Text.LaTeX.Base
import Text.LaTeX.Base.Class


-- section without numbering
section_ :: LaTeXC l => l -> l
section_ = comm1 "section*"

subsection_ :: LaTeXC l => l -> l
subsection_ = comm1 "subsection*"

class ShowLaTeX a where
    -- For monoid style
    toLaTeX :: a -> LaTeX
    toLaTeX = execLaTeXM . doLaTeX    
    
    -- For monad style
    doLaTeX :: a -> LaTeXM ()
    doLaTeX a = textell $ toLaTeX a
