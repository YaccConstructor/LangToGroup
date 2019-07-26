{-# LANGUAGE OverloadedStrings #-}

module SMPrinter where
    import Text.LaTeX.Base
    import Text.LaTeX.Base.Class
    import Text.LaTeX.Base.Commands
    import Text.LaTeX.Packages.AMSMath
    import Text.LaTeX.Packages.Inputenc
    import qualified Data.Set as Set
    import Text.LaTeX.Base.Types
    import Data.Maybe
    import Debug.Trace (trace)
    import Data.List

    import SMType
    import Lib
    import Tm1Printer

    instance ShowLaTeX Y where 
        doLaTeX (Y y) = doLaTeX y 

    instance ShowLaTeX SMTag where
        doLaTeX tag = raw $ fromString $ show tag

    instance ShowLaTeX StateName where
        doLaTeX name = raw $ fromString $ show name

    instance ShowLaTeX TMCMD where
        doLaTeX (CommandAlias c) = raw $ fromString $ c 
        doLaTeX (Command cmd) = showBCommand cmd

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
                        (Just cmd, Just smtag) -> q <> "(" <> (fromString $ show $ tape value) <> ", " <> (doLaTeX cmd) <> ", " <> (doLaTeX smtag) <> ")"
            setVal val $ (setTags (doLaTeX name) tagsList) <> (raw "_{") <> (raw $ fromString idx) <> (raw "}")

    showSMStates :: [State] -> LaTeXM ()
    showSMStates = helper where
        helper [state]    = math $ doLaTeX state
        helper (state:ss) = do { math $ doLaTeX state; ", "; showSMStates ss }


    showYs :: [Y] -> LaTeXM ()
    showYs ys = showAlphabet squares
        where squares = map (\(Y y) -> y) ys

    instance ShowLaTeX Smb where
        toLaTeX (SmbY y) = toLaTeX y
        toLaTeX (SmbY' y) = toLaTeX y <> raw "^{-1}"
        toLaTeX (SmbQ q) = toLaTeX q  

    instance ShowLaTeX SMType.Word where
        doLaTeX (Word w) = mapM_ (\s -> do { math $ doLaTeX s ; " " }) w

    instance ShowLaTeX SRule where
        doLaTeX (SRule s) = do {(foldr (\(w1,w2) acc -> do { doLaTeX w1 ; math to ; " " ; doLaTeX w2 ; lnbk ; acc }) "" s) } 

    substituteCommands rules = do
        let tau i = "\\tau_{" ++ (show i) ++ "}"
        
        let substituteWord i w acc accNames =
                case w of
                    [] -> (i, reverse acc, accNames)
                    smbq@(SmbQ s) : t 
                        | isJust cmd && isNothing cmdInAcc -> substituteWord (i + 1) t (newSmbQ : acc) ((name, command) : accNames)
                        | isJust cmd -> substituteWord i t (newSmbQ : acc) accNames
                        | otherwise -> substituteWord i t (smbq : acc) accNames
                            where 
                                (Just sval) = s_val s 
                                cmd = tmCommand sval
                                (Just command) = cmd 
                                cmdInAcc = find (\(_, c1) -> command == c1) accNames
                                name = CommandAlias $ tau i
                                newSmbQ = case cmdInAcc of 
                                            Nothing -> SmbQ $ s {s_val = Just $ sval {tmCommand = Just name}}
                                            (Just (n, c)) -> SmbQ $ s {s_val = Just $ sval {tmCommand = Just n}}

                    s : t -> substituteWord i t (s : acc) accNames
        let substituteRule i rule acc accNames =
                case rule of
                    (Word w1, Word w2) : t -> substituteRule newI2 t ((Word newWord1, Word newWord2) : acc) newNames2
                        where
                            (newI1, newWord1, newNames1) = substituteWord i w1 [] accNames 
                            (newI2, newWord2, newNames2) = substituteWord newI1 w2 [] newNames1 
                    [] -> (i, reverse acc, accNames)
        let internal i rules acc accNames = 
                case rules of
                    SRule s : t -> internal newI t ((SRule newRule) : acc) newNames
                        where 
                            (newI, newRule, newNames) = substituteRule i s [] accNames
                    [] -> (reverse acc, accNames)
        internal 0 rules [] []

    instance ShowLaTeX SM where
        doLaTeX sm = do
            let (rules, commandsName) = substituteCommands $ srs $ trace ("I'm here!") sm
            subsection_ "Alphabet"
            enumerate $ mapM_ (\ys -> do { item Nothing; showYs ys}) $ yn sm
            subsection_ "States"
            enumerate $ mapM_ (\states -> do { item Nothing; showSMStates states}) . map Set.toList $ qn sm
            subsection_ "Commands"
            enumerate $ mapM_ (\(name, cmd) -> do { item Nothing ; math $ doLaTeX name ; " = " ; math $ doLaTeX cmd }) commandsName
            subsection_ "Rules"
            enumerate $ mapM_ (\rule -> do { item Nothing; doLaTeX rule}) $ rules