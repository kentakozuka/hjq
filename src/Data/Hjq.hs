{-# LANGUAGE OverloadedStrings #-}
module Data.Hjq where

import Control.Error.Util
import Data.Aeson
import Data.Text as T
import Data.ByteString.Lazy as B
import Data.Hjq.Parser
import Data.Hjq.Query
import Data.Aeson.Encode.Pretty

{-
-- hjqコマンドの処理本体
-- Json文字列とクエリの文字列を受け取って処理
-}
hjq :: ByteString -> T.Text -> Either T.Text ByteString
hjq jsonString queryString = do
    -- decode :: FromJSON a => ByteString -> Maybe a
    -- Control.Error.Util note :: a -> Maybe b -> Either a b
    value <- note "invlid json format." $ decode jsonString
    -- クエリ文字列をパースする
    query <- parseJqQuery queryString
    executeQuery query value >>= return . encodePretty
