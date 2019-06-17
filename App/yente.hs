
import           App.Yente.Match
import           App.Yente.CLI
import           App.Yente.Core



main ∷ IO ()
main = do
    yo@YenteOptions{..} <- parseCLI

    (fromNamesWts, toNamesWts) <- tokenizeNames preprocessingConfig
        <$> readNamesFile fromFile
        <*> readNamesFile toFile

    let comparer = compareNameListToNameCosine (misspellingMethod matchConfig)

    yenteG yo comparer fromNamesWts toNamesWts




compareNameListToNameCosine ∷ Maybe MisspellingMethod
                            → Name NormWeights
                            → Name NormWeights
                            → NameComparison
compareNameListToNameCosine Nothing = cosine
compareNameListToNameCosine (Just mm) = cosineWithMispellings mm




