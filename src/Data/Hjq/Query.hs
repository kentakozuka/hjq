{-# LANGUAGE OverloadedStrings #-}
module Data.Hjq.Query where
import Control.Lens
import Control.Monad
import Data.Monoid
import Data.Hjq.Parser
import Data.Aeson
import Data.Aeson.Lens
import Data.Text as T
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as H

{-
-- フィルタの実行
-- フィールド、インデックス、無効な入力でパターンマッチ
-}
applyFilter :: JqFilter -> Value -> Either T.Text Value
applyFilter (JqField fieldName n) obj@(Object _)
    = join $ noteNotFoundError fieldName (fmap (applyFilter n) (obj ^? key fieldName))
applyFilter (JqIndex index n) array@(Array _)
    = join $ noteOutOfRangeError index (fmap (applyFilter n) (array ^? nth index))
applyFilter JqNil v = Right v
applyFilter f o = Left $ "unexpected pattern : " <> tshow f <> " : " <> tshow o

-- フィールド名見つからない場合のエラー
noteNotFoundError :: T.Text -> Maybe a -> Either T.Text a
noteNotFoundError _ (Just x) = Right x
noteNotFoundError s Nothing = Left $ "field name not found " <> s

-- インデックスが存在しない場合のエラー
noteOutOfRangeError :: Int -> Maybe a -> Either T.Text a
noteOutOfRangeError _ (Just x) = Right x
noteOutOfRangeError s Nothing = Left $ "out of range : " <> tshow s

-- Show型クラスのインスタンスをText型に変換
tshow :: Show a => a -> T.Text
tshow = T.pack . show

{-
- クエリ実行関数
-}
executeQuery :: JqQuery -> Value -> Either T.Text Value
-- オブジェクト
executeQuery (JqQueryObject o) v
    = fmap (Object . H.fromList) . sequence . fmap sequence $ fmap (fmap $ flip executeQuery v) o
-- 関数
executeQuery (JqQueryArray l) v
    = fmap (Array . V.fromList) . sequence $ fmap (flip executeQuery v) l
-- その他フィルタ文字列
executeQuery (JqQueryFilter f) v = applyFilter f v
