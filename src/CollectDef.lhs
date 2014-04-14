> module CollectDef ( defPrefix, defs, hypTarget, hypLink ) where

> import Data.Char ( ord )
> import Data.List ( isPrefixOf, words )
> import Numeric ( showHex )

> import Document
> import Auxiliaries

> defPrefix :: String
> defPrefix = "^^^"

> defs :: String -> String
> defs = concat . map ((++ "\n") . drop (length defPrefix)) . filter (defPrefix `isPrefixOf`) . words 

> mangling :: String -> String
> mangling = concat . map (flip showHex "" . ord)

> hypTarget :: Doc -> String -> Doc
> hypTarget d s = let s' = mangling s
>                 in  Text ("\\raisebox{\\ht\\strutbox}{\\hypertarget{" ++ s' ++ "}{}}") :^: d

> hypLink :: Doc -> String -> Doc
> hypLink d s = Text ("\\protect\\hyperlink{" ++ mangling s ++ "}{") :^: d :^: Text "}"

