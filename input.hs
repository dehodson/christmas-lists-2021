import qualified Data.ByteString.Lazy as B
import Questionnaire
import GHC.Generics
import Data.Maybe
import Data.Aeson
import System.IO
import Data.List

sectionNames :: [String]
sectionNames = [ "Favorite Drinks"
               , "Favorite Snacks Salty &amp; Sweet"
               , "Current Hobbies &amp; Interests"
               , "Gifts For Your Pets"
               , "Favorite Media"
               , "Book / Magazine Wishlist"
               , "Gift Card Ideas"
               , "Services You'd Use"
               , "Gadgets / Tools / Tech"
               , "Household Items and Personal Care"
               , "Extra Gift Info"
               , "Miscellaneous Wants and Wishes"
               ]

promptFor :: String -> IO String
promptFor s = putStr s >> hFlush stdout >> getLine

getSection :: String -> IO Section
getSection name = do
  putStrLn name
  let loop = do {
    item <- getLine ;
    case item of
      "" -> return []
      _ -> (item :) <$> loop
  }
  items <- loop
  return $ Section name items

getQuestionnaire :: String -> IO Questionnaire
getQuestionnaire name = do
  hFlush stdout
  sections <- sequenceA $ getSection <$> sectionNames
  return $ Questionnaire name sections

insertQuestionnaire :: Questionnaire -> [Questionnaire] -> [Questionnaire]
insertQuestionnaire q qs = do
  let i = findIndex (((name q) ==) . name) qs
  case i of
    Just index -> take index qs ++ [q] ++ drop (index + 1) qs
    Nothing -> q : qs

mergeQuestionnaires :: [Questionnaire] -> [Questionnaire] -> [Questionnaire]
mergeQuestionnaires = foldr insertQuestionnaire

main :: IO ()
main = do
  contents <- decodeFileStrict "questionnaires.json" :: IO (Maybe [Questionnaire])
  let initialQuestionnaires = fromMaybe [] contents
  let loop = do {
    name <- promptFor "Person's name: " ;
    case name of
      "" -> return []
      _ -> do {
        q <- getQuestionnaire name ;
        (q :) <$> loop
      }
  }
  questionnaires <- loop
  encodeFile "questionnaires.json" (mergeQuestionnaires initialQuestionnaires questionnaires)
  putStrLn "Thank you! :)"
