> module CollectDef ( defPrefix, defs, hypTarget, hypLink ) where

> import Data.Char ( ord, toUpper, isAlpha )
> import Data.List ( isPrefixOf, words )
> import Numeric ( showHex )

> import Document
> import Auxiliaries

> defPrefix :: String
> defPrefix = "^^^"

> defs :: String -> String
> defs s = let sss = filter (not . null) (map (take 2 . filter (defPrefix `isPrefixOf`) . words) (lines s))
>          in  concat (map (\ss -> if null ss then "" else foldr1 (\x y -> x ++ " " ++ y) (map (drop (length defPrefix)) ss) ++ "\n") sss)

> mangling :: String -> String
> mangling s = let c = head s in (if isAlpha c then toUpper (head s) else '_') : concat (map (flip showHex "" . ord) s)

> hypTarget :: Doc -> Doc -> String -> Doc
> hypTarget d d' s = let s' = mangling s
>                    in  Text ("\\raisebox{\\ht\\strutbox}{\\hypertarget{" ++ s' ++ "}{}}") <> d <>
>                        Text ("\\index{" ++ s' ++ "@\\ensuremath{") <> d' <> Text "}}"

> hypLink :: Doc -> String -> Doc
> hypLink d s = Text ("\\protect\\hyperlink{" ++ mangling s ++ "}{") <> d <> Text "}"

