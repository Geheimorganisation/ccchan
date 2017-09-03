{-# LANGUAGE OverloadedStrings #-}
module Markup where

import Types

import Data.Char
import Data.Foldable
import Data.Monoid ((<>))
import Data.Text (Text ())
import qualified Data.Text as T

data PrimMarkup a
  = Literal a
  | Reference Id
  deriving (Show, Eq, Ord)

type Markup a = [PrimMarkup a]

parseMarkup :: Text -> Markup Text
parseMarkup "" = []
parseMarkup t
  | T.take 2 t == ">>" =  let (a,b) = T.span (not . isSpace) (T.drop 2 t) in Reference a : parseMarkup b
  | otherwise = let (a,b) = T.span (/= '>') t in Literal a : parseMarkup b

class ToText a where
  toText :: a -> Text

instance ToText Text where
  toText = id

instance ToText a => ToText [a] where
  toText = foldl' (\acc e -> acc <> toText e) mempty

instance ToText a => ToText (PrimMarkup a) where
  toText (Literal a) = toText a
  toText (Reference id) = ">>" <> id
