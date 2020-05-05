module DotGraphWriter where

import SMPrinter
import SMType
import Prelude hiding (Word)
import Text.LaTeX.Base.Render
import Text.LaTeX.Base
import Lib
import Data.Tuple.Utils
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.Text.Lazy (fromStrict, replace, pack)
import Data.GraphViz
import Data.GraphViz.Printing
import Data.GraphViz.Attributes.Complete
import Data.Text (Text)
import qualified Data.ByteString.Lazy as B
import Data.Text.Lazy.Encoding

tex2text :: LaTeXM a -> Data.Text.Text
tex2text = render . execLaTeXM

substCommandsInWord :: Word -> Word
substCommandsInWord (Word word) = Word $ snd3 $ substituteWord 0 word [] []

instance Labellable Word where
        toLabelValue = StrLabel . fromStrict . tex2text . doLaTeX . substCommandsInWord 

writeGraph :: FilePath -> Gr Word Int -> IO ()
writeGraph f = B.writeFile f . encodeUtf8 . replace (pack "\\\\") (pack "\\") . renderDot . toDot . graphToDot quickParams