module Gui where

import AutoCorrect
import Brick
import Brick.AttrMap
import Brick.Widgets.Border
import Brick.Widgets.Center
import Brick.Widgets.Core
import ColorMapper
import Control.DeepSeq
import Cursor.Brick.Text
import Cursor.Brick.TextField
import Cursor.List.NonEmpty
import Cursor.Text
import Cursor.TextField
import Cursor.Types
import Data.Char
import Data.List.Split (splitOn)
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.IO hiding (getContents)
import Formatter
import qualified Graphics.Vty as V
import Graphics.Vty.Attributes
import Graphics.Vty.Input.Events
import Lens.Micro (set, (%~), (&), (.~), (^.))
import LuParser
import LuSyntax hiding (Name)
import Path
import Path.IO
import Prelude hiding (getContents)
import System.Environment
import System.Exit
import Text.Wrap

textCursorSelectEndWord :: TextCursor -> TextCursor
textCursorSelectEndWord tc =
  let goRight = maybe tc textCursorSelectEndWord (textCursorSelectNext tc)
   in case textCursorNextChar tc of
        Nothing -> tc
        Just p
          | isSpace p -> case textCursorPrevChar tc of
            Nothing -> goRight
            Just n
              | isSpace n -> goRight
              | otherwise -> tc
          | otherwise -> goRight

textCursorSelectBeginWord :: TextCursor -> TextCursor
textCursorSelectBeginWord tc =
  let goLeft = maybe tc textCursorSelectBeginWord (textCursorSelectPrev tc)
   in case textCursorPrevChar tc of
        Nothing -> tc
        Just p
          | isSpace p -> case textCursorNextChar tc of
            Nothing -> goLeft
            Just n
              | isSpace n -> goLeft
              | otherwise -> tc
          | otherwise -> goLeft

textFieldCursorSelectEndWord :: TextFieldCursor -> TextFieldCursor
textFieldCursorSelectEndWord = textFieldCursorSelectedL %~ textCursorSelectEndWord

textFieldCursorSelectBeginWord :: TextFieldCursor -> TextFieldCursor
textFieldCursorSelectBeginWord = textFieldCursorSelectedL %~ textCursorSelectBeginWord

{--
-- This text editor is heavily inspired by Tom Sydney Kerckhove's tutorial, available at
-- <https://www.youtube.com/watch?v=Kmf3lnln1BI>
--}

-- | Data model for a text editor, including the string, and dictionary of current words
data GUI = GUI
  { cursor :: TextFieldCursor,
    dictionary :: [String],
    previous :: Maybe GUI
  }

-- | Resource Name data type
data Name
  = Text
  | Viewport
  deriving (Show, Ord, Eq)

-- | Application
app :: App GUI e Name
app =
  App
    { appDraw = drawGUI,
      appChooseCursor = showFirstCursor,
      appHandleEvent = handleEvent,
      appStartEvent = pure,
      appAttrMap =
        const $
          attrMap
            mempty
            [ (attrName "text", fg white),
              (attrName "bg", fg black),
              (attrName "keyword", fg blue),
              (attrName "whitespace", fg black),
              (attrName "operator", fg red),
              (attrName "string", fg green),
              (attrName "border", fg white)
            ]
    }

-- handles terminal input. Should take a single argument,
-- and open that as a file
getContents :: Path Abs File -> IO Text
getContents fpath = do
  maybeContents <- forgivingAbsence $ Data.Text.IO.readFile (Path.fromAbsFile fpath)
  return $ fromMaybe T.empty maybeContents



terminalInit :: IO ()
terminalInit = do
  args <- getArgs
  case args of
    [x, y] -> do
      -- get the dictionary contents
      fpath <- resolveFile' x
      dictPath <- resolveFile' y
      textContents <- getContents fpath
      dictContents <- getContents dictPath
      initialState <- openFile textContents dictContents
      endState <- defaultMain app initialState
      let contents' = rebuildTextFieldCursor (cursor endState)
      saveFile contents' fpath

    -- no arguments passed or more than one argument passed
    _ -> die "Error: Usage ludicrous [text filename] [dictionary filename]"

-- | saves file and formats it, if it is well-formed
saveFile :: Text -> Path Abs File -> IO ()
saveFile txt path = do
  let trimmed = T.dropWhile (\x -> x == ' ' || x == '\n' || x == '\t') txt
   in case parseFromText trimmed of
        Left pe -> do
          Data.Text.IO.writeFile (Path.fromAbsFile path) txt
        Right block -> Data.Text.IO.writeFile (Path.fromAbsFile path) (formatBlock block)

-- initial state, from opening a file to get text
openFile :: Text -> Text -> IO GUI
openFile text dictText = do
  return
    GUI
      { cursor = makeTextFieldCursor text,
        dictionary = initDict dictText,
        previous = Nothing
      }

-- formats text contained within a GUI, returns a new, formatted GUI
format :: GUI -> GUI
format s =
  let text = rebuildTextFieldCursor (cursor s)
   in case parseFromText text of
        Left pe -> s
        Right block ->
          let formatted = formatBlock block
           in s
                { cursor = makeTextFieldCursor formatted,
                  previous = Just s
                }

-- handles a Brick Event
handleEvent :: GUI -> BrickEvent Name e -> EventM Name (Next GUI)
handleEvent s e =
  case e of
    VtyEvent vtye ->
      let mDo ::
            (TextFieldCursor -> Maybe TextFieldCursor) ->
            EventM n (Next GUI)
          mDo f = do
            let curr = cursor s
            let next = fromMaybe curr $ f curr
            let s' = s {cursor = next, previous = Just s}
            continue s'
       in case vtye of
            EvKey (KChar c) [] -> mDo $ textFieldCursorInsertChar c . Just
            EvKey KUp [] -> mDo textFieldCursorSelectPrevLine
            EvKey KDown [] -> mDo textFieldCursorSelectNextLine
            EvKey KRight [] -> mDo textFieldCursorSelectNextChar
            EvKey KLeft [] -> mDo textFieldCursorSelectPrevChar
            EvKey KBS [] -> mDo $ dullMDelete . textFieldCursorRemove
            EvKey KDel [] -> mDo $ dullMDelete . textFieldCursorDelete
            EvKey KEnter [] -> mDo $ Just . textFieldCursorInsertNewline . Just
            EvKey KEsc [] -> halt s -- save and exit
            EvKey (KChar c) [MCtrl] -> case c of
              -- undo
              'z' -> continue $ fromMaybe s (previous s)
              -- format
              'f' -> continue $ format s
              -- go to end of line
              'e' -> mDo $ Just . textFieldCursorSelectEndOfLine
              -- go to beginning of line
              'b' -> mDo $ Just . textFieldCursorSelectStartOfLine
              -- use AutoComplete
              't' -> continue $ correctWord s
              _ -> continue s
            _ -> continue s
    _ -> continue s

-- modified widget creator to wrap lines
createTfcWidget :: n -> TextFieldCursor -> Widget n
createTfcWidget n (TextFieldCursor tfc) =
  flip foldNonEmptyCursor tfc $ \befores current afters ->
    vBox $
      Prelude.concat
        [ Prelude.map coloredTxtWrap befores,
          [visible $ coloredTxtWrapCursor n current],
          Prelude.map coloredTxtWrap afters
        ]

-- Brick function that finds the width of a text
safeTextWidth :: Text -> Int
safeTextWidth = V.safeWcswidth . T.unpack

--  helper function for length of colored text
colorLength :: [(Text, a)] -> Int
colorLength = foldr (\(x, _) acc -> T.length x + acc) 0

-- reconstructs a colored line
reconstruct :: [(Text, a)] -> Text
reconstruct = foldr (\(x, _) acc -> x <> acc) T.empty

-- wraps and colors text
wrapAndColorText :: Text -> Int -> [[(Text, ColorMapper.Color)]]
wrapAndColorText t width = let colored = colorMap t in createLines colored width

-- create the lines from colored text
createLines :: [(Text, ColorMapper.Color)] -> Int -> [[(Text, ColorMapper.Color)]]
createLines [] _ = []
createLines colored width =
  let (x, y) = createLine colored width []
   in x : createLines y width
  where
    createLine [] _ curr = (curr, [])
    createLine ((t, c) : ys) width curr =
      if colorLength curr + T.length t < width
        then createLine ys width (curr ++ [(t, c)])
        else
          let remainingLength = width - colorLength curr
              (first, next) = T.splitAt remainingLength t
           in (curr ++ [(first, c)], (next, c) : ys)

-- image for an empty line
emptyLine :: Attr -> Result n
emptyLine attr = emptyResult & imageL .~ V.text' attr (T.pack " ")

-- creates the images that will form the wrapped text lines
createLineImages :: [[(Text, ColorMapper.Color)]] -> AttrMap -> Attr -> Int -> Result n
createLineImages lines attrMap attr width = case lines of
  [] -> emptyLine attr
  multiple ->
    let lineImgs = lineImg <$> multiple
        createImage (text, attr) =
          V.text'
            (attrMapLookup (attrName (attribute attr)) attrMap)
            (sanitiseText text)
        lineImg lStr =
          let vals = map createImage lStr
              endspaces =
                V.text'
                  attr
                  (T.replicate (width - safeTextWidth (reconstruct lStr)) (T.pack " "))
           in foldr (V.<|>) endspaces vals
     in emptyResult & imageL .~ V.horizCat [V.vertCat lineImgs]

-- text wrap with color
coloredTxtWrap :: T.Text -> Widget n
coloredTxtWrap s =
  Widget Greedy Fixed $ do
    c <- getContext
    let lines = wrapAndColorText s (c ^. availWidthL)
     in return $ createLineImages lines (ctxAttrMap c) (c ^. attrL) (c ^. availWidthL)

-- text wraps a textCursor with color and cursor location
coloredTxtWrapCursor :: n -> TextCursor -> Widget n
coloredTxtWrapCursor n tc =
  Widget Greedy Fixed $ do
    c <- getContext
    let s = rebuildTextCursor tc
        lines = wrapAndColorText s (c ^. availWidthL)
        cursorLoc = getCursorLoc tc lines n
        lineWidget = createLineImages lines (ctxAttrMap c) (c ^. attrL) (c ^. availWidthL)
     in return $ lineWidget & cursorsL .~ cursorLoc

-- Given a cursor and a screen width, returns a Brick Location in order to place the cursor
findPhysicalLocation :: TextCursor -> [Int] -> Brick.Location
findPhysicalLocation tc y = go (textCursorIndex tc) y 0
  where
    go i [] h = Brick.Location (i, h)
    go i [x] h = Brick.Location (i, h)
    go i (x : xs) h = if i > x then go (i - x) xs (h + 1) else Brick.Location (i, h)

-- gets the cursor location given a set of lines
getCursorLoc :: TextCursor -> [[(Text, ColorMapper.Color)]] -> n -> [CursorLocation n]
getCursorLoc tc lines n = case lines of
  [] -> [CursorLocation (Brick.Location (0, 0)) (Just n)]
  multiple ->
    let loc = findPhysicalLocation tc (safeTextWidth <$> map reconstruct multiple)
     in [CursorLocation loc (Just n)]

-- draws a GUI
drawGUI :: GUI -> [Widget Name]
drawGUI gui =
  [ (border $ padLeftRight 1 $ viewport Viewport Vertical $ createTfcWidget Text (cursor gui))
    <=>
    createAutoCompleteWidget gui
  ]


-- | Creates the widget for the autocomplete box 
createAutoCompleteWidget :: GUI -> Widget n
createAutoCompleteWidget gui = str $ "Autocomplete suggestion: " ++ (getSuggestedWord gui)


-- | returns a new GUI with the text edited based on autocorrect, if successful 
correctWord :: GUI -> GUI 
correctWord s = case getCorrectedTextCursor s of
  Nothing -> s
  Just c ->
    s {
      cursor = c,
      previous = Just s
      }

-- | Given a gui, attempts to return a textFieldCursor with the current word corrected
getCorrectedTextCursor :: GUI -> Maybe TextFieldCursor
getCorrectedTextCursor gui = do
  let curr = getCurrentWord gui
  let sug = getSuggestedWord gui
  if sug == "" then return (cursor gui) else do
    cursor' <- removeN (length curr) (textFieldCursorSelectEndWord $ cursor gui) -- delete current word
    cursor'' <- foldr textFieldCursorInsertChar (Just $ cursor') (reverse sug) -- insert corrected word
    return $ textFieldCursorSelectEndWord cursor''


-- | deletes n characters from the cursor
removeN :: Int -> TextFieldCursor -> Maybe TextFieldCursor
removeN 0 c = Just c
removeN n c = do
  c' <- (textFieldCursorRemove c)
  c'' <- dullDelete c'
  removeN (n-1) c''

-- | Given a gui gets the suggested word
getSuggestedWord :: GUI -> String 
getSuggestedWord gui = let curr = getCurrentWord gui in
  if length curr < 10 then fromMaybe "" (bestSuggestion curr (dictionary gui)) else ""

-- | returns the current word we are in
getCurrentWord :: GUI -> String
getCurrentWord s =
  let text = rebuildTextFieldCursor (cursor s) in
  let (_, startCol) = textFieldCursorSelection $ textFieldCursorSelectBeginWord (cursor s) in
  let (_, endCol) = textFieldCursorSelection $ textFieldCursorSelectEndWord (cursor s) in
  T.unpack $ T.take (endCol - startCol) (T.drop startCol text)
