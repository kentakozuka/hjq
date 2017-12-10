{-# LANGUAGE OverloadedStrings #-}
module Main where
import Data.Hjq.Parser
import Data.Hjq.Query
import Test.HUnit

import Control.Lens
import Data.Monoid
import Data.Text
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as H
import Data.Aeson
import Data.Aeson.Lens

main :: IO ()
main = do
    runTestTT $ TestList
        [
              jqFilterParserTest
            , jqQueryParserTest
            , applyFilterTest
            , executeQueryTest
        ]
    return ()

jqFilterParserTest :: Test
jqFilterParserTest = TestList
    [
      "jqFilterParser test 1" ~:
        parseJqFilter " . " ~?= Right JqNil

    , "jqFilterParser test 2" ~:
        parseJqFilter " . [0] " ~?= Right (JqIndex 0 JqNil)

    , "jqFilterParser test 3" ~:
        parseJqFilter " . fieldName" ~?= Right (JqField "fieldName" JqNil)
    , "jqFilterParser test 4" ~:
        parseJqFilter " . [0] . fieldName" ~?= Right (JqIndex 0 (JqField "fieldName" JqNil))

    , "jqFilterParser test 5" ~:
        parseJqFilter " . fieldName [ 0 ] " ~?= Right (JqField "fieldName" (JqIndex 0 JqNil))
    ]

jqQueryParserTest :: Test
jqQueryParserTest = TestList
    [
      "jqQueryParserTest test 1" ~:
        parseJqQuery " [ ] "
            ~?= Right (JqQueryArray [])

    , "jqQueryParserTest test 2" ~:
        parseJqQuery " [ . hoge , . piyo ] "
            ~?= Right (JqQueryArray [JqQueryFilter (JqField "hoge" JqNil), JqQueryFilter (JqField "piyo" JqNil)])

    , "jqQueryParserTest test 3" ~:
        parseJqQuery " { \" hoge \" : [ ] , \" piyo \" : [ ] } "
            ~?= Right (JqQueryObject [("hoge", JqQueryArray []), ("piyo", JqQueryArray [])])
    ]

testData :: Value
testData = Object $ H.fromList
    [
        ("string-field", String "string value")
    ,   ("nested-field", Object $ H.fromList
            [
                ("inner-string", String "inner value")
            ,   ("inner-number", Number 100)
            ]
        )
    ,   ("array-field", Array $ V.fromList
            [
                String "first field"
            ,   String "next field"
            ,   Object (H.fromList
                    [ ("object-in-array", String "string value in object-in-array") ] )
            ]
        )
    ]

applyFilterTest :: Test
applyFilterTest = TestList
    [
      "applyFilter test 1" ~:
        applyFilter (unsafeParseFilter ".") testData
            ~?= Right testData

    , "applyFilter test 2" ~:
        (Just $ applyFilter (unsafeParseFilter ".string-field") testData)
            ~?= fmap Right (testData^?key "string-field")

    , "applyFilter test 3" ~:
        (Just $ applyFilter (unsafeParseFilter ".nested-field.inner-string") testData)
            ~?= fmap Right (testData ^? key "nested-field" . key "inner-string")
    
    , "applyFilter test 4" ~:
        (Just $ applyFilter (unsafeParseFilter ".nested-field.inner-number") testData)
            ~?= fmap Right (testData ^? key "nested-field" . key "inner-number")
    
    , "applyFilter test 5" ~:
        (Just $ applyFilter (unsafeParseFilter ".array-field[0]") testData)
            ~?= fmap Right (testData ^? key "array-field" . nth 0)
    
    , "applyFilter test 6" ~:
        (Just $ applyFilter (unsafeParseFilter ".array-field[1]") testData)
            ~?= fmap Right (testData ^? key "array-field" . nth 1)
    
    , "applyFilter test 7" ~:
        (Just $ applyFilter (unsafeParseFilter ".array-field[2].object-in-array") testData)
            ~?= fmap Right (testData ^? key "array-field" . nth 2 . key "object-in-array")
    ]

-- パースを簡易的に呼び出すためにparseJqFilterをラップした関数
unsafeParseFilter :: Text -> JqFilter
unsafeParseFilter t = case parseJqFilter t of
    Right f -> f
    Left s -> error $ "PARSE FAILURE IN A TEST * " ++ unpack s


executeQueryTest :: Test
executeQueryTest = TestList
    [
      "executeQueryTest test 1" ~:
        executeQuery (unsafeParseQuery "{}") testData
            ~?= Right (Object $ H.fromList [])

    , "executeQueryTest test 2" ~:
        executeQuery (unsafeParseQuery "{ \"field1\": . , \"field2\": .string-field}") testData
            ~?= Right (Object $ H.fromList [("field1", testData), ("field2", String "string value")])

    , "executeQueryTest test 3" ~:
        executeQuery (unsafeParseQuery "[ .string-field, .nested-field.inner-string]") testData
            ~?= Right (Array $ V.fromList [String "string value", String "inner value"])
    ]

-- パースを簡易的に呼び出すためにparseJqQueryをラップした関数
unsafeParseQuery :: Text -> JqQuery
unsafeParseQuery t = case parseJqQuery t of
    Right q -> q
    Left s -> error $ "PARSE FAILURE IN A TEST * " ++ unpack s


