--- https://github.com/NaturalNode/natural/blob/master/lib/natural/phonetics/metaphone.js
-- Figure out what to do about single initial w and y
module Text.PhoneticCode.Metaphone 
    ( metaphone
    ) where


import Data.Text ( Text
                 , replace
                 , pack
                 , cons
                  ) 

import qualified Data.Text as T
import Data.List
import Data.Char (toUpper, isLetter)
import Debug.Trace



metaphone :: String -> String
metaphone "" = ""
metaphone s =  --undefined "Needs to handle empty strings at each point in the process" -- traceShow s $ 
    T.unpack -- dropVowels 
   . dropVowels
   . transformV
   . transformZ
   . dropWandY
   . dropH
   . transformPH
   . transformG
   . transformD
   . transformToK
   . transformC 
   . transformToX
   . transformTH
   . transformSCH
   $ T.pack
   . dropBafterMAtEnd 
   . changeWordStarts 
   . removeDuplicateLetters 
   . removeNonLetters
   . upperCase $ s

metaphonePhrase :: String -> String
metaphonePhrase = intercalate " " . map metaphone . words


upperCase :: String -> String
upperCase = map toUpper

removeNonLetters :: String -> String
removeNonLetters = filter isLetter

--[ /([bcdfhjklmnpqrstvwxyz])\1+/, '\1' ],  # Remove doubled consonants except g.
removeDuplicateLetters :: String -> String
removeDuplicateLetters = concat . map eraseDupsButG . group
  where
    eraseDupsButG t = if (head t /= 'G') then [head t] --0-T.singleton (T.head t)
                      else t


changeWordStarts :: String -> String
changeWordStarts s 
    |  ("KN" `isPrefixOf` s) 
    || ("GN" `isPrefixOf` s) 
    || ("PN" `isPrefixOf` s) 
    || ("AE" `isPrefixOf` s) 
    || ("WR" `isPrefixOf` s) = tail s
    |  ("X"  `isPrefixOf` s) = "S" ++ tail s
    |  ("WH" `isPrefixOf` s) = "W" ++ drop 2 s 
    | otherwise            = s


{-[ /mb$/,            'M' ],  # [PHP] remove $ from regexp.-}
dropBafterMAtEnd :: String -> String
dropBafterMAtEnd s | "MB" `isSuffixOf` s = init s
                   | otherwise           = s


{-[ /(?!^)sch/,      'SK' ],-}
transformSCH :: Text -> Text
transformSCH = dropLettersWithRule changeSCH 
  where
    changeSCH = DropReplaceRule
                    { letterToDrop = pack "SCH"
                    , replacement  = pack "SK"
                    , preRule      = not . T.null
                    , postRule     = alwaysTrueRule 
                    }


{-[ /th/,             '0' ],-}
transformTH :: Text -> Text
transformTH = replace (pack "TH") (pack "0")



{-[ /t?ch|sh/,        'X' ],-}
{-[ /c(?=ia)/,        'X' ],-}
{-[ /[st](?=i[ao])/,  'X' ],-}
transformToX :: Text -> Text
transformToX = id
             . dropLettersWithRule changeSTI
             . dropLettersWithRule changeCIA
             . replace (pack "SH") x
             . replace (pack "CH") x
             . replace (pack "TCH") x
  where
    x = pack "X"
    changeCIA = basicDropReplaceRule{ letterToDrop = pack "C"
                                    , replacement  = pack "X"
                                    , postRule     = T.isPrefixOf (pack "IA")
                                    }
    changeSTI = basicDropReplaceRule{ letterToDrop = pack "ST"
                                    , replacement  = pack "X"
                                    , postRule     = \x -> T.isPrefixOf (pack "IA") x || T.isPrefixOf (pack "IO") x
                                    }

{-[ /s?c(?=[iey])/,   'S' ],-}
transformC :: Text -> Text
transformC = dropLettersWithRule dropCRule2 
           . dropLettersWithRule dropCRule1  
  where
    dropCRule1 = basicDropReplaceRule
                    { letterToDrop = pack "SC"
                    , replacement  = pack "S"
                    , postRule     = notNullAndRule (\x -> T.head x `elem` ['I', 'E', 'Y']) 
                    }

    dropCRule2 = basicDropReplaceRule
                    { letterToDrop = pack "C"
                    , replacement  = pack "S"
                    , postRule     = notNullAndRule (\x -> T.head x `elem` ['I', 'E', 'Y']) 
                    }


{-[ /(ck?|q)/,        'K' ],-}
transformToK:: Text -> Text
transformToK = replace (pack "C") k
             . replace (pack "CK") k
             . replace (pack "Q") k
  where
    k = pack "K"


{-[ /dg(?=[iey])/,    'J' ],-}
{-[ /d/,              'T' ],-}
transformD :: Text -> Text
transformD = replace (pack "D")   (pack "T") 
           . dropLettersWithRule changeDG
  where 
    changeDG = basicDropReplaceRule
                    { letterToDrop = pack "DG"
                    , replacement  = pack "J"
                    , postRule     = notNullAndRule ( \x -> T.head x `elem` ['I', 'E', 'Y'] )  
                    }

               
{-[ /g(?=h[^aeiou])/, ''  ],-}
{-[ /gn(ed)?/,        'N' ],-}
--[ /([^g]|^)g(?=[iey])/, '\1j' ],
{-[ /g+/,             'K' ],-}
transformG :: Text -> Text
transformG = replace g k 
           . replace gg g
           . dropLettersWithRule dropGRule3 
           . replace gn n 
           . dropLettersWithRule dropGRule1
  where
    dropGRule1 = basicDropReplaceRule 
        { letterToDrop = gh
        , replacement  = h 
        , postRule     = isNullOrRule (\x -> isCons . T.head $ x)
        }
    dropGRule3 = DropReplaceRule
        { letterToDrop = g
        , replacement  = pack "J"
        , preRule      = isNullOrRule (\x -> not (T.last x == 'G'))
        , postRule     = notNullAndRule (\x -> T.head x `elem` ['I', 'E', 'Y' ])
        }
    g  = pack "G"
    gg = pack "GG"
    gn = pack "GN"
    gh = pack "GH"
    k  = pack "K"
    h  = pack "H"
    ed = pack "ED"
    n  = pack "N"


{-[ /ph/,             'F' ],-}
transformPH :: Text -> Text
transformPH t = replace ( pack "PH" ) ( pack "F" ) t       


--[ /([aeiou])h(?=\b|[^aeiou])/, '\1' ],
dropH :: Text -> Text
dropH = dropLettersWithRule dropHRule 
  where
    dropHRule = basicDropReplaceRule 
        { letterToDrop = pack "H"
        , preRule      = notNullAndRule (\x -> isVowel . T.last $ x ) 
        , postRule     = isNullOrRule   (\x -> isCons  . T.head $ x ) 
        }


{-[ /[wy](?![aeiou])/, '' ],-}
dropWandY :: Text -> Text
dropWandY t 
    |  t == pack "W" 
    || t == pack "Y" = t -- Need to preserve single letter initials (not clear what metaphone sounds should be)
    | otherwise      = dropLettersWithRule dropWRule
                     . dropLettersWithRule dropYRule $ t
  where
    dropWRule = basicDropReplaceRule 
            { letterToDrop = pack "W"
            , postRule     = isNullOrRule (\x -> isCons  . T.head $ x ) 
            }
    dropYRule = basicDropReplaceRule 
            { letterToDrop = pack "Y"
            , postRule     = isNullOrRule (\x -> isCons  . T.head $ x ) 
            }



{-[ /z/,              'S' ],-}
transformZ :: Text -> Text
transformZ  = replace (pack "Z") (pack "S")

{-[ /v/,              'F' ],-}
transformV :: Text -> Text
transformV = replace (pack "V") (pack "F")

--[ /(?!^)[aeiou]+/,  ''  ],
dropVowels :: Text -> Text
dropVowels wrd = cons (T.head wrd) ( T.filter isCons . T.tail $ wrd )




isVowel :: Char -> Bool
isVowel v = v `elem` ['A','E','I','O','U']


isCons :: Char -> Bool
isCons = not . isVowel














-- Drop letter functions
data DropReplaceRule = DropReplaceRule 
    { letterToDrop :: !Text
    --, replacement  :: !( (Text, Text) -> Text )
    , replacement  :: !Text
    , preRule      :: !(Text -> Bool)
    , postRule     :: !(Text -> Bool)
    } 

basicDropReplaceRule = DropReplaceRule  T.empty T.empty alwaysTrueRule alwaysTrueRule

dropLettersWithRule :: DropReplaceRule 
                    -> Text 
                    -> Text
dropLettersWithRule drr t  
    | pre == t        =  t -- No matching letters in this word
    | preRuleSatisfied && postRuleSatisfied = pre `T.append` replacement drr `T.append` dropLettersWithRule drr post -- Letter matching conditions
    | otherwise             = pre `T.append` letterToDrop drr `T.append` dropLettersWithRule drr post -- Fails to match condition 
  where
    broken            = breakOnPrePost (letterToDrop drr) t
    preRuleSatisfied  = (preRule drr) pre
    postRuleSatisfied = (postRule drr) post
    pre               = fst broken
    post              = snd broken
    -- | preRuleSatisfied && postRuleSatisfied = pre `T.append` (replacement drr) broken `T.append` dropLettersWithRule drr post



breakOnPrePost :: Text -> Text -> (Text, Text)
breakOnPrePost brk txt | T.null postWithBrk = brokenText
                       | otherwise          = (pre, T.drop (T.length brk) postWithBrk)
  where
    brokenText  = T.breakOn brk txt
    pre         = fst brokenText
    postWithBrk = snd brokenText


alwaysTrueRule :: Text -> Bool
alwaysTrueRule _ = True

notNullAndRule :: (Text->Bool) -> Text -> Bool
notNullAndRule r t = (not . T.null $ t) && (r t)

isNullOrRule :: (Text->Bool) -> Text -> Bool
isNullOrRule r t = (T.null $ t) || (r t)


