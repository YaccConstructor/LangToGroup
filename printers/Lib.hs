module Lib where

import Text.LaTeX.Base
import Text.LaTeX.Base.Class
import Text.LaTeX.Base.Writer
import Text.LaTeX.Packages.Inputenc


section_ :: LaTeXC l => l -> l
section_ = comm1 "section*"


class ShowLaTeX a where
    toLaTeX :: a -> LaTeX
    toLaTeX = execLaTeXM . doLaTeX    

    doLaTeX :: a -> LaTeXM ()
    doLaTeX a = textell $ toLaTeX a
