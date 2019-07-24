{-# LANGUAGE OverloadedStrings #-}

module SMPrinter where
    import Text.LaTeX.Base
    import Text.LaTeX.Base.Class
    import Text.LaTeX.Base.Commands
    import Text.LaTeX.Packages.AMSMath
    import Text.LaTeX.Packages.Inputenc
    import qualified Data.Set as Set
    import Text.LaTeX.Base.Types
    import Data.Matrix
    import Data.Maybe
    import Debug.Trace

    import SMType
    import Lib
    import Tm1Printer

    instance ShowLaTeX Y where 
        doLaTeX (Y y) = doLaTeX y 

    instance ShowLaTeX SMTag where
        doLaTeX tag = raw $ fromString $ show tag

    instance ShowLaTeX StateName where
        doLaTeX name = raw $ fromString $ show name

    instance ShowLaTeX State where 
        doLaTeX (State name idx tags val) = do
            let tagsList = Set.toList tags
            let setTag q tag =
                    case tag of
                        Quote -> q <> (raw "^{'}")
                        Hat -> (raw "\\hat{") <> q <> (raw "}") 
                        Dash -> (raw "\\bar{") <> q <> (raw "}")

            let setTags = foldl setTag
            let setVal Nothing q = q
            let setVal (Just value) q  = 
                    case (tmCommand value, smTag value) of
                        (Nothing, Nothing) -> q <> "(" <> (fromString $ show $ tape value) <> ")"
                        (Just cmd, Just smtag) -> q <> "(" <> (fromString $ show $ tape value) <> ", " <> (showBCommand cmd) <> ", " <> (doLaTeX smtag) <> ")"
            setVal val $ (setTags (doLaTeX name) tagsList) <> (raw "_{") <> (raw $ fromString idx) <> (raw "}")

    showSMStates :: [State] -> LaTeXM ()
    showSMStates = helper where
        helper [state]    = doLaTeX state
        helper (state:ss) = do { doLaTeX state; ", "; showSMStates ss }


    showYs :: [Y] -> LaTeXM ()
    showYs ys = showAlphabet squares
        where squares = map (\(Y y) -> y) ys

    instance ShowLaTeX Smb where
        toLaTeX (SmbY y) = toLaTeX y
        toLaTeX (SmbY' y) = toLaTeX y <> raw "^{-1}"
        toLaTeX (SmbQ q) = toLaTeX q  

    instance ShowLaTeX SMType.Word where
        doLaTeX (Word w) = mapM_ (\s -> do { doLaTeX s ; " " }) w

    instance ShowLaTeX SRule where
        doLaTeX (SRule s) = math $ do { "[" ; (foldr (\(w1,w2) acc -> do { doLaTeX w1 ; to ; " " ; doLaTeX w2 ; ";" ; acc }) "" s) ; "]\n" } 

    instance ShowLaTeX SM where
        doLaTeX sm = do
            subsection_ "Alphabet"
            enumerate $ mapM_ (\ys -> do { item Nothing; showYs ys}) $ yn sm
            subsection_ "States"
            enumerate $ mapM_ (\states -> do { item Nothing; math $ showSMStates states}) $ qn sm
            subsection_ "Rules"
            enumerate $ mapM_ (\rule -> do { item Nothing; doLaTeX rule}) $ srs sm