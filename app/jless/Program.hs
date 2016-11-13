{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
module Program where

import Data.Json.Path (PathElement(..), fromList, rfc6901pointer)

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Data.Aeson
import Data.Aeson.Encode.Pretty (Config(..), encodePretty', defConfig, )
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
import System.Hclip

import UI.NCurses

import Prelude hiding (lines)

data ObjState = StateExpanded | StateCollapsed
  deriving (Eq, Show, Read)

data J = JObj [(Text, J)]
       | JArr [J]
       | JStr !Text
       | JNum !Scientific
       | JBool !Bool
       | JNull
  deriving Show


instance FromJSON J where
   parseJSON (Object obj) =
     JObj . sortOn fst <$> mapM (\(k, v) ->
                                   (k,) <$> parseJSON v
                                     )
                                (HM.toList obj)
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

loadRoot :: J -> JZipper
loadRoot o@(JObj _) = JZipper o (zipProps o) []
loadRoot a@(JArr _) = JZipper a (zipProps a) []
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
  fmap f (Zipper l c r) = Zipper (fmap f l) (f c) (fmap f r)

moveRight :: Zipper a -> Zipper a
moveRight z@(Zipper _    _ [])  = z
moveRight (Zipper l c (r:rs)) = Zipper (c:l) r rs

moveLeft :: Zipper a -> Zipper a
moveLeft z@(Zipper [] _ _) = z
moveLeft (Zipper (l:ls) c r) = Zipper ls l (c:r)

left :: Zipper a -> [a]
left (Zipper l _ _) = l
current :: Zipper a -> a
current (Zipper _ c _) = c
right :: Zipper a -> [a]
right (Zipper _ _ r) = r

seekToValueBy :: Eq b => (a -> b) -> b -> Zipper a -> Maybe (Zipper a)
seekToValueBy f v z = conv <$> i
  where zl = zipperToList z
        i = elemIndex v (map f zl)
        conv i = Zipper (reverse h) c t
          where (h, c:t) = splitAt i zl

zipperFromList :: [a] -> Maybe (Zipper a)
zipperFromList [] = Nothing
zipperFromList (x:xs) = Just $ Zipper [] x xs

zipperToList :: Zipper a -> [a]
zipperToList (Zipper l c r) = reverse l ++ [c] ++ r

data ProgramState = ProgramState { obj :: JZipper
                                 , pad :: Pad
                                 , botBar :: Window
                                 , scroll :: (Integer, Integer)
                                 , color :: Color -> ColorID
                                 , redraw :: Bool }
type ProgramM = StateT ProgramState Curses

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

scrollPad f step st = st { scroll = clamp . f step . scroll $ st }
  where clamp (y, x) = (max 0 y, max 0 x)

smallStep = 1
bigStep = 5
hugeStep = 25

scrollUp step = \(y, x) -> (y - step, x)
scrollDown step = \(y, x) -> (y + step, x)
scrollLeft step = \(y, x) -> (y, x - step)
scrollRight step = \(y, x) -> (y, x + step)

dirty = modify (\st -> st { redraw = True })
clean = modify (\st -> st { redraw = False })

updateState :: Event -> ProgramM ()
updateState (EventSpecialKey KeyDownArrow) = modify (scrollPad scrollDown smallStep)
updateState (EventSpecialKey KeyUpArrow) = modify (scrollPad scrollUp smallStep)
updateState (EventSpecialKey KeyPreviousPage) = modify (scrollPad scrollUp hugeStep)
updateState (EventSpecialKey KeyNextPage) = modify (scrollPad scrollDown hugeStep)
updateState (EventSpecialKey KeyRightArrow) = modify (scrollPad scrollRight smallStep)
updateState (EventSpecialKey KeyLeftArrow) = modify (scrollPad scrollLeft smallStep)
updateState (EventCharacter 'a') = modify lineOut *> dirty
updateState (EventCharacter 'd') = modify lineIn *> dirty
updateState (EventCharacter 'w') = modify lineUp *> dirty
updateState (EventCharacter 's') = modify lineDown *> dirty
updateState (EventCharacter 'c') = toClipboard
updateState x = uw $ moveCursor 0 0 >> drawString (show x)

pretty = encodePretty' defConfig { confCompare = compare }
jsonToText = decodeUtf8 . L.toStrict . pretty
jsonToTextLines = T.lines . jsonToText

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

uw :: Update () -> ProgramM ()
uw x = do
  p <- gets pad
  (screenHeight, screenWidth) <- lift screenSize
  (scrollY, scrollX) <- gets scroll
  lift (updatePad p
                  scrollY scrollX
                  0 0 (screenHeight - 3) (screenWidth - 1)
                  x)

drawInitial :: ProgramM ()
drawInitial = do
  b <- gets botBar
  (screenHeight, screenWidth) <- lift screenSize
  jz <- gets (jzCurrentObj . obj)
  uw $ do
    clear
    moveCursor 0 0
    forM_ (jsonToTextLines jz) $ \line -> do
      drawText line
      (y', _) <- cursorPosition
      moveCursor (y'+1) 0
  lift $ updateWindow b $ do
    resizeWindow 2 screenWidth
    moveWindow (screenHeight - 2) 0
    moveCursor 0 0
    drawLineH Nothing screenWidth
    moveCursor 1 0

drawState :: ProgramM ()
drawState = do
   jz <- gets obj
   d <- gets redraw
   c <- gets color
   if d
     then drawInitial
     else uw $ setTouched True
   clean
   b <- gets botBar
   lift $ updateWindow b $ do
     moveCursor 1 0
     clearLine
     moveCursor 1 0
     drawText . rfc6901pointer . fromList . reverse . map fst $ jzHistory jz
     setColor (c ColorBlue)
     case (current <$> jzPointer jz) of
       Just (el, _) -> drawText . rfc6901pointer . fromList . pure $ el
       Nothing -> pure ()
     setColor (c ColorDefault)
   lift render

loop :: ProgramM ()
loop = do
  w <- lift defaultWindow
  e <- lift $ getEvent w Nothing
  case e of
    Just (EventCharacter 'q') -> pure ()
    Just e' -> do
      updateState e'
      drawState
      loop
    Nothing -> loop

setupColors :: Curses (Color -> ColorID)
setupColors = do
  let colors = [ColorRed, ColorBlue, ColorDefault]
  ids <- forM (zip colors [1..]) $ \(c, i) ->
    (,) <$> pure c <*> newColorID c ColorDefault i
  pure $ \c -> case (filter ((== c) . fst) ids) of
    [(_, i)] -> i
    _ -> defaultColorID

toClipboard :: ProgramM ()
toClipboard = do
  t <- gets (jsonToText . jzCurrentObj . obj)
  liftIO $ setClipboard . T.unpack $ t

runProgram :: J -> IO ()
runProgram o = runCurses $ do
  _ <- setCursorMode CursorInvisible
  w <- defaultWindow
  updateWindow w clear
  render
  setEcho False
  setKeypad w True
  setRaw True

  colors <- setupColors
  let root = (loadRoot o)
      lines = jsonToTextLines (jzCurrentObj root)
      w = fromIntegral $ maximum . map T.length $ lines
      h = fromIntegral $ length lines

  let (padH, padW) = (h + 10, w + 10)
  p <- newPad padH padW

  b <- newWindow 0 0 2 10

  _ <- runStateT (drawInitial *> drawState *> loop)
                 (ProgramState root p b (0, 0) colors True)

  pure ()
