{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module Main (main) where

import Data.Aeson.Shim (bwd)
import Data.Aeson (FromJSON (..), ToJSON (..), Value (Number, Object), decode, encode, object, pairs, (.:), (.=))
import qualified Data.HashMap.Strict as HM
import Data.Text (Text)
import qualified Data.Text as T

import Control.Applicative (empty)
import qualified Data.ByteString.Lazy.Char8 as BL

data Coord = Coord {x :: Double, y :: Double}
    deriving (Show)

instance ToJSON Coord where
    toJSON (Coord xV yV) =
        object
            [ "x" .= xV
            , "y" .= yV
            ]

    toEncoding Coord{..} =
        pairs $
            "x" .= x
                <> "y" .= y

instance FromJSON Coord where
    parseJSON (Object v) =
        Coord
            <$> v .: "x"
            <*> v .: "y"
    parseJSON _ = empty

keyValue :: (Show a) => Text -> a -> String
keyValue k v = T.unpack k ++ " = " ++ show v

wrongKey :: (Show k) => Text -> HM.HashMap k v -> String
wrongKey k (HM.keys -> ks) = "No \"" ++ T.unpack k ++ "\" field, fields are " ++ show ks ++ "."

echo :: Coord -> String
echo c@(toJSON -> Object s) = "As object: " ++ show s ++ "\nAs coord: " ++ show c
echo c = "As coord: " ++ show c

-- | Using @-XViewPatterns@ at the top level. Imagine if the implementation is
-- more way more complex (instead of just calling `keyValue`, `wrongKey`, and
-- `echo`), you're making an upgrade to aeson-2 and this is a breaking change.
-- Rather than use CPP and conditional compilation, with a shim and top-level
-- view patterns the implementation can stay the same except for conversion of
-- an input argument.
echoTopViewPatterns :: Text -> Coord -> String
echoTopViewPatterns k (toJSON -> Object (HM.lookup k . bwd -> Just (Number v))) = keyValue k v
echoTopViewPatterns k (toJSON -> Object (bwd -> x)) = wrongKey k x
echoTopViewPatterns _ x = echo x

-- | Using @-XViewPatterns@ in a case expression. Like with
-- `echoTopViewPatterns`, if the implementation doesn't compile with the
-- breaking aeson-2 change, this change can be isolated to the case expression
-- pattern match by adding a shim there.
echoCaseViewPatterns :: Text -> Coord -> String
echoCaseViewPatterns k c =
    case toJSON c of
        Object (HM.lookup k . bwd -> Just (Number v)) -> keyValue k v
        Object (bwd -> x) -> wrongKey k x
        _ -> echo c

-- | Not using @-XViewPatterns@ but using @-XPatternGuards@ instead, the
-- shimming is done in the implementation. With a more complex implementation,
-- this might have to be done in multiple places, increasing the risk of errors.
echoPatternGuards :: Text -> Coord -> String
echoPatternGuards k c
    | Object x <- toJSON c
    , Just (Number v) <- HM.lookup k (bwd x) =
        keyValue k v
    | Object x <- toJSON c = wrongKey k (bwd x)
    | otherwise = echo c

-- | Not using @-XViewPatterns@ or @-XPatternGuards@, only case expressions and
-- again, the shimming is done in the implementation.
echoCase :: Text -> Coord -> String
echoCase k c =
    case toJSON c of
        Object x -> case HM.lookup k (bwd x) of
            Just (Number v) -> keyValue k v
            _ -> wrongKey k (bwd x)
        _ -> echo c

main :: IO ()
main = do
    let req = decode "{\"x\":3.0,\"y\":-1.0}" :: Maybe Coord
    print req
    let reply = Coord 123.4 20
    BL.putStrLn (encode reply)

    putStrLn "\nEcho"
    putStrLn $ echo reply

    putStrLn "\nTop View Patterns"
    putStrLn $ echoTopViewPatterns "x" reply
    putStrLn $ echoTopViewPatterns "y" reply
    putStrLn $ echoTopViewPatterns "z" reply

    putStrLn "\nCase View Patterns"
    putStrLn $ echoCaseViewPatterns "x" reply
    putStrLn $ echoCaseViewPatterns "y" reply
    putStrLn $ echoCaseViewPatterns "z" reply

    putStrLn "\nPattern Guards"
    putStrLn $ echoPatternGuards "x" reply
    putStrLn $ echoPatternGuards "y" reply
    putStrLn $ echoPatternGuards "z" reply

    putStrLn "\nOnly Case Expressions"
    putStrLn $ echoCase "x" reply
    putStrLn $ echoCase "y" reply
    putStrLn $ echoCase "z" reply
