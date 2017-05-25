module App.Yente.Types (
    module X
  , computeWeightsFromNames
  ) where


import App.Yente.Types.Mode           as X
import App.Yente.Types.Name           as X
import App.Yente.Types.NameComparison as X
import App.Yente.Types.TokenWeightMap as X

import App.Yente.Prelude



-- -- SHOULD THE COMPUTE WEIGHTS USE `NUB`?

computeWeightsFromNames :: [Name] -- ^ List of names
                -> TokenWeightMap
computeWeightsFromNames ns =
  TokenWeightMap{ tokenWeights     = tw
                , rarestTokenValue = rtv
                }
  where
    score wds = (head wds, log(corpsize/(fromIntegral . length $ wds)))
    allTokens = map tokens ns -- [[String]]
    tkns      = App.Yente.Prelude.group . sort . concatMap nub $ allTokens
    corpsize  = fromIntegral . length $ allTokens
    tw        = fromList $ map score tkns
    rtv       = maximum . elems $ tw --- This value is stored with the token weight map for efficiency

