import Questionnaire
import Data.Aeson
import Data.Maybe
import Data.List
import Data.Char

getNames :: [Questionnaire] -> [String]
getNames = (name <$>)

colors :: [String]
colors = cycle [ "dark-red"
               , "dark-green"
               , "light-purple"
               , "gold"
               , "purple"
               , "orange"
               , "blue"
               ]

lowercase :: String -> String
lowercase = (toLower <$>)

indexHeading :: String
indexHeading = "<p class=\"f1 dark-red b\">Christmas Family Questionnaire 2021!</p>"

genIndex :: [String] -> String
genIndex = foldr eachLink indexHeading . zip colors
  where eachLink (col, p) c = c ++
                       "<p class=\"f2\"><a class=\"" ++ col ++
                       "\" href=\"./" ++
                       lowercase p ++
                       ".html\">" ++ p ++ "</a></p>\n"

genSection :: String -> Section -> String
genSection c s
  | items s == [] = ""
  | otherwise = "<p class=\"f2 b " ++ c ++ "\">" ++ header s ++ "</p>\n" ++
                unlines (eachItem <$> items s)
  where eachItem s = "<p class=\"f4\">" ++ s ++ "</p>"

genWishes :: Questionnaire -> String
genWishes qn = "<p class=\"f1 dark-red b\">" ++ name qn ++ "'s Wish List</p>" ++
               unlines (uncurry genSection <$> zip (drop 2 colors) (sections qn))

buildPage :: String -> String -> String -> String
buildPage head foot content = unlines [head, foot, content]

makeWishFile :: (String -> String) -> Questionnaire -> IO ()
makeWishFile pb qn = writeFile ((lowercase $ name qn) ++ ".html") (pb $ genWishes qn)

main :: IO ()
main = do
  contents <- decodeFileStrict "questionnaires.json" :: IO (Maybe [Questionnaire])
  let questionnaires = fromMaybe [] contents
  header <- readFile "partials/head.html"
  footer <- readFile "partials/foot.html"
  let pageBuilder = buildPage header footer
  writeFile "index.html" (pageBuilder (genIndex (reverse . sort $ getNames questionnaires)))
  mapM_ (makeWishFile pageBuilder) questionnaires
