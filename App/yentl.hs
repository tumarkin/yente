import App.Yente.CLI
import App.Yente.RunMatch
import App.Yente.Prelude

main = yentl =<< parseCLI

-- yentl yo@YenteOptions{..} = do
--     (fromNameTL, toNameTL) <- readAndPreprocessNames yo
--     yenteG yo compareNameListToNameLev fromNameTL toNameTL


-- compareNameListToNameLev ∷ Traversable t
--                          ⇒ (t NameTokenList, NameTokenList)
--                          → t NameComparison
-- compareNameListToNameLev (ns, n) = map (levenshtein n) ns

