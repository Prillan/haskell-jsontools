{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
module Program where

import Data.Json.Path (PathElement(..))

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.State
import Data.Aeson
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Bifunctor (bimap)
import qualified Data.ByteString.Lazy as L
import Data.Default
import qualified Data.HashMap.Strict as HM
import Data.List (elemIndex, sortOn)
import Data.Maybe
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Data.Scientific
import qualified Data.Vector as V
import Graphics.Vty

import Prelude hiding (lines)

data J = JObj [(Text, J)]
       | JArr [J]
       | JStr !Text
       | JNum !Scientific
       | JBool !Bool
       | JNull
  deriving Show


instance FromJSON J where
   parseJSON (Object obj) =
     JObj . sortOn fst <$> mapM (\(k, v) -> (k,) <$> parseJSON v) (HM.toList obj)
   parseJSON (Array arr) =
     JArr <$> mapM parseJSON (V.toList arr)
   parseJSON (String s) = pure $ JStr s
   parseJSON (Number n) = pure $ JNum n
   parseJSON (Bool b)   = pure $ JBool b
   parseJSON Null       = pure $ JNull

instance ToJSON J where
  toJSON (JObj obj) = Object (HM.fromList $ map (\(k, v) -> (k, toJSON v)) obj)
  toJSON (JArr arr) = Array (V.fromList $ map toJSON arr)
  toJSON (JStr s)   = String s
  toJSON (JNum n)   = Number n
  toJSON (JBool b)  = Bool b
  toJSON (JNull)    = Null

data JZipper = JZipper { jzCurrentObj  :: J
                       , jzPointer     :: Maybe (Zipper (PathElement, J))
                       , jzHistory     :: [(PathElement, J)] }
  deriving Show

testObj = JObj [("a", JArr [JBool True,JBool False]),
                ("b", JObj [("c",JNum 1.0),("d",JNum 2.0)])]

loadRoot :: J -> JZipper
loadRoot o@(JObj vals) = JZipper o (zipProps o) []
loadRoot a@(JArr vals) = JZipper a (zipProps a) []
loadRoot _ = error "Invalid root"

zipProps :: J -> Maybe (Zipper (PathElement, J))
zipProps (JObj vals) = zipperFromList $ map (\(k, v) -> (Property k, v)) vals
zipProps (JArr vals) = zipperFromList $ zip (map (Index . fromIntegral) [0..length vals - 1]) vals
zipProps _ = Nothing

moveIn :: JZipper -> Maybe JZipper
moveIn jz = case (current <$> jzPointer jz) of
              Nothing -> Nothing
              Just (p, v) ->
                Just $ JZipper v
                               (zipProps v)
                               ((p, jzCurrentObj jz):jzHistory jz)
moveOut :: JZipper -> Maybe JZipper
moveOut jz = case (jzHistory jz) of
               []     -> Nothing
               ((p, v):xs) ->
                 Just $ JZipper v
                                (zipProps v >>= seekToValueBy fst p)
                                xs

moveDown :: JZipper -> JZipper
moveDown jz = jz { jzPointer = moveRight <$> jzPointer jz }

moveUp :: JZipper -> JZipper
moveUp jz = jz { jzPointer = moveLeft <$> jzPointer jz }

data Zipper a = Zipper [a] a [a]
  deriving Show

instance Functor Zipper where
  fmap f (Zipper left c right) = Zipper (fmap f left) (f c) (fmap f right)

moveRight z@(Zipper _    _ [])  = z
moveRight (Zipper left c (r:right)) = Zipper (c:left) r right

moveLeft z@(Zipper [] _ _) = z
moveLeft (Zipper (l:left) c right) = Zipper left l (c:right)

left (Zipper l _ _) = l
current (Zipper _ c _) = c
right (Zipper _ _ r) = r

seekToValueBy :: Eq b => (a -> b) -> b -> Zipper a -> Maybe (Zipper a)
seekToValueBy f v z = conv <$> i
  where zl = zipperToList z
        i = elemIndex v (map f zl)
        conv i = Zipper (reverse h) c t
          where (h, c:t) = splitAt i zl

zipperFromList [] = Nothing
zipperFromList (x:xs) = Just $ Zipper [] x xs

zipperToList (Zipper l c r) = reverse l ++ [c] ++ r

exampleLines = fromJust . zipperFromList . map (\i -> "Line " <> show i) $ [1..10]

data ProgramState = ProgramState { obj :: JZipper, vty :: Vty }
type ProgramM = StateT ProgramState IO

lineDown :: ProgramState -> ProgramState
lineDown st = st { obj = moveDown (obj st) }

lineUp :: ProgramState -> ProgramState
lineUp st = st { obj = moveUp (obj st) }

lineIn st =
  case moveIn (obj st) of
    Just m  -> st { obj = m }
    Nothing -> st
lineOut st =
  case moveOut (obj st) of
    Just m  -> st { obj = m }
    Nothing -> st

updateState :: Event -> ProgramM ()
updateState (EvKey KDown _) = modify lineDown
updateState (EvKey KUp _) = modify lineUp
updateState (EvKey KRight _) = modify lineIn
updateState (EvKey KLeft _) = modify lineOut
updateState _ = pure ()

toVtyLine color = string (defAttr `withForeColor` color) . T.unpack

jsonToTextLines = T.lines . decodeUtf8 . L.toStrict . encodePretty

appendComma :: [T.Text] -> [T.Text]
appendComma [] = []
appendComma [x] = [x <> ","]
appendComma (x:y:xs) = x:appendComma (y:xs)

appendCommas :: [[T.Text]] -> [[T.Text]]
appendCommas [] = []
appendCommas [x] = [x]
appendCommas (x:y:xs) = appendComma x:appendCommas (y:xs)

modHead :: (a -> a) -> [a] -> [a]
modHead _ [] = []
modHead f (x:xs) = f x:xs

drawJZipper :: JZipper -> Image
drawJZipper jz =
  case jzCurrentObj jz of
    JNum n -> toVtyLine blue . T.pack . show $ n
    JStr s -> toVtyLine blue s
    JBool b -> toVtyLine blue (if b then "true" else "false")
    JNull -> toVtyLine blue "null"
    JArr _ -> toVtyLine green "[" <-> drawArrayZipper (jzPointer jz)
                                  <-> toVtyLine green "]"
    JObj _ -> toVtyLine green "{" <-> drawObjectZipper (jzPointer jz)
                                  <-> toVtyLine green "}"
drawArrayZipper Nothing = emptyImage
drawArrayZipper (Just z)
  | null (zipperToList z) = emptyImage
  | otherwise = foldl1 (<->) $ top
                            ++ mid
                            ++ bot
  where fmt c = toVtyLine c
        fmtMany c l commaAfter = map (fmt c)
                               . (if commaAfter then appendComma else id)
                               . map ("  " <>)
                               . concat
                               . appendCommas
                               . map jsonToTextLines
                               . map snd $ l
        top = fmtMany green (reverse $ left z) (not . null . left $ z)
        mid = fmtMany blue [current z] (not . null . right $ z)
        bot = fmtMany green (right z) False
drawObjectZipper Nothing = emptyImage
drawObjectZipper (Just z)
  | null (zipperToList z) = emptyImage
  | otherwise = foldl1 (<->) $ top
                            ++ mid
                            ++ bot
  where fmtMany c l commaAfter = map (toVtyLine c)
                               . (if commaAfter then appendComma else id)
                               . map ("  " <>)
                               . concat
                               . appendCommas
                               . map (\(Property k, v) -> modHead (("\"" <> k <> "\": ") <> ) v)
                               . map (bimap id jsonToTextLines) $ l
        top = fmtMany green (reverse $ left z) (not . null . left $ z)
        mid = fmtMany blue [current z] (not . null . right $ z)
        bot = fmtMany green (right z) False


drawState :: ProgramM ()
drawState = do
   jz <- obj <$> get
   let  outputLines = drawJZipper jz
        pic = picForImage outputLines
   v <- vty <$> get
   liftIO $ update v pic

loop = do
  v <- vty <$> get
  e <- liftIO (nextEvent v)
  case e of
    (EvKey (KChar 'q') _) -> liftIO $ shutdown v
    _ -> do
      updateState e
      drawState
      loop

runProgram o = do
     vty' <- mkVty def
     runStateT (drawState *> loop) (ProgramState (loadRoot o) vty')
     -- let line0 = string (defAttr ` withForeColor ` green) (show exampleLines)
     --     line1 = string (defAttr ` withBackColor ` blue) "second line"
     --     img = line0 <-> line1
     --     pic = picForImage img
     -- update vty pic
     -- e <- nextEvent vty
     -- shutdown vty
     -- print ("Last event was: " ++ show e)
