import           App.Yente.CLI
import           App.Yente.Core
import           App.Yente.Match
import           App.Yente.Match.Compare.Levenshtein

main = yentl =<< parseCLI


yentl ∷ YenteOptions → IO ()
yentl yo@YenteOptions{..} = do
    fromNameTL <- fmap (nameEncoder preprocessingConfig) <$> readNamesFile fromFile
    toNameTL   <- fmap (nameEncoder preprocessingConfig) <$> readNamesFile toFile

    yenteG yo levenshtein fromNameTL toNameTL



