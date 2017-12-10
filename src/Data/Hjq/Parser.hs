
-- 入力されたJSONからデータを取得する

{-# LANGUAGE OverloadedStrings #-}
module Data.Hjq.Parser where
import Control.Applicative
import Data.Attoparsec.Text
import Data.Text


{-
- ユーザーが入力する文字列用の構文木の型定義
--
--jqFilterParserで呼ばれる
- 再帰的に値コンストラクタを呼び出し、構文木を構築する
-
- 値コンストラクタはそれぞれ
-   フィールド名    JqField Text JqFilter
-   インデックス    JqIndex Int JqFilter
-   何もしない入力  JqNil
-
-}
data JqFilter
    = JqField Text JqFilter
    | JqIndex Int JqFilter
    | JqNil
    deriving (Show, Read, Eq)

{-
-- パースを実行する関数
-- パースに失敗する可能性があるため、返り値をEither型にして、
-- エラーメッセージを返却できるようにする
--
-- parse関数はParser a というパーサを表す型の値と、そのパーサに入力するText型の文字列を入力すると、
   パースした結果を表すResult a 型の値を返す
   最後にResult空文字列を与えることによって、パース終了を明示する
-}
parseJqFilter :: Text -> Either Text JqFilter
parseJqFilter s = showParseResult
    $ parse (jqFilterParser <* endOfInput) s `feed` ""

{-
-- スペース対策のため、char関数をラップする
-- char関数は引数の文字とマッチした場合のみパースを成功させる
--  char :: Char -> Parser Char
-}
schar :: Char -> Parser Char
schar c = skipSpace *> char c <* skipSpace

{-
-- パーサ本体
-- 再帰的に呼び出して、JqFilter型の値を返す
--
-- attoparsecを使用してフィルタの文字列をJqFilter型にパース
--
-- >>は左項を評価し、その結果を捨てて、右項を評価する
-- <|>は左項でパースが失敗した場合に、右項のパースを試みる
-}
jqFilterParser :: Parser JqFilter
jqFilterParser = schar '.' >> (jqField <|> jqIndex <|> pure JqNil)
  where
    jqFilter :: Parser JqFilter
    jqFilter = (schar '.' >> jqField) <|> jqIndex <|> pure JqNil

    -- フィールド名
    jqField :: Parser JqFilter
    jqField = JqField <$> word <*> jqFilter

    -- インデックス
    jqIndex :: Parser JqFilter
    jqIndex = JqIndex <$> (schar '[' *> decimal <* schar ']') <*> jqFilter

-- パース結果の表示
showParseResult :: Show a => Result a -> Either Text a
showParseResult (Done _ r) = Right r
showParseResult r = Left . pack $ show r

-- フィールド名などの識別子をパースするパーサ
word :: Parser Text
word = fmap pack $ many1 (letter <|> schar '-' <|> digit)


{-
- クエリデータ構造
-}
data JqQuery
    -- オブジェクト
    = JqQueryObject [(Text, JqQuery)]
    -- 配列
    | JqQueryArray [JqQuery]
    -- 構文木
    | JqQueryFilter JqFilter
    deriving (Show, Read, Eq)

{-
- クエリ文字列のパースの実行
--
-- parse関数はParser a というパーサを表す型の値と、そのパーサに入力するText型の文字列を入力すると、
-- パースした結果を表すResult a 型の値を返す
-- 最後にResult空文字列を与えることによって、パース終了を明示する
-- parse :: Parser a -> Text -> Result a
-}
parseJqQuery :: Text -> Either Text JqQuery
parseJqQuery s = showParseResult $ parse (jqQueryParser <* endOfInput) s `feed` ""

{-
-- パーサ本体
-- 再帰的に呼び出して、JqQuery型の値を返す
-- <|>は左項でパースが失敗した場合に、右項のパースを試みる
--
-- <$>はfmapの中置演算子。
-}
jqQueryParser :: Parser JqQuery
jqQueryParser = queraArray <|> queryFilter <|> queryObject
    where
        -- 配列
        queraArray :: Parser JqQuery
        queraArray = JqQueryArray <$> (schar '[' *> jqQueryParser `sepBy` (schar ',') <* schar ']')

        -- オブジェクト
        queryObject :: Parser JqQuery
        queryObject = JqQueryObject <$> (schar '{' *> (qObj `sepBy` schar ',') <* schar '}')

        qObj :: Parser (Text, JqQuery)
        qObj = (,) <$> (schar '"' *> word <* schar '"') <*> (schar ':' *> jqQueryParser)

        -- 構文木
        queryFilter :: Parser JqQuery
        queryFilter = JqQueryFilter <$> jqFilterParser


