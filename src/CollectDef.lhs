\subsection{CollectDef formatter}

> module CollectDef ( isDef, defId, defs ) where

> import Data.List ( isPrefixOf, words )

> isDef :: String -> Bool
> isDef = ("^^^" `isPrefixOf`)

> defId :: String -> String
> defId = drop 3

> defs :: String -> String
> defs = concat . map ((++ "\n") . defId) . filter isDef . words 

