\subsection{CollectDef formatter}

> module CollectDef ( defPrefix, defs, mangling ) where

> import Data.Char ( ord )
> import Data.List ( isPrefixOf, words )
> import Numeric ( showHex )
> import Auxiliaries

> defPrefix :: String
> defPrefix = "^^^"

> defs :: String -> String
> defs = concat . map ((++ "\n") . drop (length defPrefix)) . filter (defPrefix `isPrefixOf`) . words 

> mangling :: String -> String
> mangling = concat . map (flip showHex "" . ord)

