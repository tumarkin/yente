import Options.Applicative

cmd :: String -> Parser a -> String -> Mod CommandFields a
cmd s p d =
   command s (info p (progDesc d))

parser1 :: Parser Int
parser1 =
  subparser $
  metavar "INT" <>
  cmd "one" (pure 1) "Got 1" <>
  cmd "two" (pure 2) "Got 2"

parser2 :: Parser Char
parser2 =
  subparser $
  metavar "CHAR" <>
  cmd "lol" (pure 'a') "Got a" <>
  cmd "hel" (pure 'b') "Got b"

parser3 :: Parser Bool
parser3 =
  subparser $
  cmd "no" (pure True) "Got true" <>
  cmd "yes" (pure False) "Got false"

parser4 :: Parser (Maybe Bool)
parser4 =
  subparser $
  cmd "just yes" (pure (Just True)) "Got Just true" <>
  cmd "nothing" (pure Nothing) "Got nothing"

test :: ParserInfo (Int, Char, Bool, Maybe Bool)
test = info (helper <*> ((,,,) <$> parser1 <*> parser2 <*> parser3 <*> parser4)) idm
