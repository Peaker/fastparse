{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExistentialQuantification #-}
import           Control.Applicative (Alternative(..))
import qualified Control.Lens as Lens
import           Control.Lens.Operators
import           Control.Monad.State (State, state, get, put)
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8
import           Data.Char (ord)
import           Data.IntMap (IntMap)
import qualified Data.IntMap as IM

data Err = MissingInput
    deriving (Show)

alphabetLength :: Int
alphabetLength = 256

data Parser a = Parser
    { _parserCurrentResult :: !(Maybe a)
    , _parserHandlers :: !(IntMap (Parser a))
    } deriving (Show, Functor)
Lens.makeLenses ''Parser

allowsEmpty :: Parser a -> Bool
allowsEmpty = Lens.has (parserCurrentResult . Lens._Just)

char :: Char -> Parser Char
char c = empty & parserHandlers %~ IM.insert (ord c) (pure c)

string :: String -> Parser String
string [] = pure []
string (c:cs) = (:) <$> char c <*> string cs

instance Applicative Parser where
    pure x = empty & parserCurrentResult .~ Just x
    Parser mf hf <*> px =
        Parser Nothing (fmap (<*> px) hf)
        & case mf of
          Nothing -> id
          Just f -> ((f <$> px) <|>)

instance Alternative Parser where
    empty = Parser Nothing mempty
    Parser mx hx <|> Parser my hy =
        Parser mr (IM.unionWith (<|>) hx hy)
        where
            mr = case (mx, my) of
                (Nothing, Nothing) -> Nothing
                (Nothing, Just x) -> Just x
                (Just x, Nothing) -> Just x
                (Just _, Just _) -> error "Ambiguous parser!"
    some v = (:) <$> v <*> many v
    many v
        | allowsEmpty v = error "many/some used on parser that accepts empty"
        | otherwise = some v <|> pure []

nextChar :: State ByteString (Maybe Char)
nextChar = state $ \bs -> case BS8.uncons bs of
    Nothing -> (Nothing, bs)
    Just (c, bs') -> (Just c, bs')

parseLongest :: Parser a -> State ByteString (Either Err a)
parseLongest rootParser =
    do
        bs <- get
        go (put bs >> return (Left MissingInput)) rootParser
    where
        go retLongest (Parser mx handlers) =
            do
                oldBs <- get
                let retLongest' =
                        case mx of
                        Nothing -> retLongest
                        Just x -> put oldBs >> return (Right x)
                mc <- nextChar
                case (mc, mx) of
                    (Nothing, Nothing) -> retLongest'
                    (Nothing, Just x) -> return (Right x)
                    (Just c, _) ->
                        case IM.lookup (ord c) handlers of
                        Nothing -> retLongest'
                        Just p -> go retLongest' p
