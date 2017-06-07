{-#LANGUAGE TemplateHaskell#-}
{-#LANGUAGE OverloadedStrings#-}
module EmonicTutor.SearchCard.SearchCriteria
  ( SearchCriteria(..)
  , getSearchParams
  ) where

import           Control.Lens (makeLenses, set)
import           Data.Bifunctor (first)
import qualified Data.ByteString.Char8 as BSC
import           Data.Maybe (fromMaybe)
import           EmonicTutor.Data.Card (Set(Set), CardName(CardName))
import           EmonicTutor.Types (Tutor)
import           Snap.Core (getParam)
import           Text.Parsec

type Parser = Parsec BSC.ByteString SearchCriteria

data SearchCriteria = SearchCriteria
  { _searchName :: Maybe CardName
  , _searchSet :: Maybe Set
  }
  deriving Show

makeLenses ''SearchCriteria

emptySearch :: SearchCriteria
emptySearch = SearchCriteria Nothing Nothing

getSearchParams :: Tutor (Either BSC.ByteString SearchCriteria)
getSearchParams = do
  text <- fromMaybe "" <$> getParam "text"
  pure . (first $ BSC.pack . show) $ runParser parseSearchCriteria emptySearch "" text

parseSearchCriteria :: Parser SearchCriteria
parseSearchCriteria = do
  _ <- manyTill parseOption eof
  getState

parseOption :: Parser ()
parseOption = choice [ parseCardName 
                     , parseSet
                     ]

parseCardName :: Parser ()
parseCardName = try $ do
  spaces
  _ <-string "--name"
  spaces
  cardName <- CardName <$> parseToken
  modifyState (set searchName (Just cardName))


parseSet :: Parser ()
parseSet = try $ do
  spaces
  _ <- string "--set"
  spaces
  cardSet <- Set <$> parseToken
  modifyState (set searchSet (Just cardSet))

parseToken :: Parser BSC.ByteString
parseToken =
  BSC.pack <$> (   between ((:[]) <$> char '\"')
                           ((:[]) <$> char '\"')
                           (many $ noneOf ['\"'])
               <|> many alphaNum
               )
