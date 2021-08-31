module Lens (
    Getter,
    Getting,
    Setter,
    ASetter,
    Traversal',
    (^.),
    to,
    view,
    views,
    use,
    uses,
    (%~),
    (%=),
    (.~),
    (.=),
    _1,
    _2,
    both,
    (&),
    makeLenses,
    on,
  ) where

import Control.Lens (
    Getter, Getting, Setter, ASetter, Traversal', (^.), to, view, views, use,
    uses, (%~), (%=), (.~), (.=), _1, _2, both, (&), makeLenses
  )
import Data.Function (on)
