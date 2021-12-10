module Gui where

import AutoComplete
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
              (attrName "string", fg green)
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
  case parseFromText txt of
    Left pe -> do
      Prelude.putStr ("Error parsing file: " ++ pe)
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
              't' -> continue $ completeWord s

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

-- private function from Brick, does safe text width
safeTextWidth :: Text -> Int
safeTextWidth = V.safeWcswidth . T.unpack

-- colored text wrapping widget function
coloredTxtWrap :: Text -> Widget n
coloredTxtWrap = txtWrapWith' defaultWrapSettings

-- modified text wrappers to also take in color
txtWrapWith' :: WrapSettings -> T.Text -> Widget n
txtWrapWith' settings s =
  Widget Greedy Fixed $ do
    c <- getContext
    let theLines = fixEmpty <$> wrapTextToLines settings (c ^. availWidthL) s
        fixEmpty l
          | T.null l = T.pack " "
          | otherwise = l
    case force theLines of
      [] -> return $ emptyResult & imageL .~ V.text' (c ^. attrL) (T.pack " ")
      multiple ->
        let maxLength = maximum $ safeTextWidth <$> multiple
            padding = V.charFill (c ^. attrL) ' ' (c ^. availWidthL - maxLength) (length lineImgs)
            lineImgs = lineImg <$> multiple
            createImage (text, attr) =
              V.text'
                (attrMapLookup (attrName (attribute attr)) (ctxAttrMap c))
                (sanitiseText text)
            lineImg lStr =
              let vals = colorMap lStr
                  vals' = map createImage vals
                  endspaces =
                    V.text'
                      (c ^. attrL)
                      (T.replicate (maxLength - safeTextWidth lStr) (T.pack " "))
               in foldr (V.<|>) endspaces vals'
         in return $ emptyResult & imageL .~ V.horizCat [V.vertCat lineImgs, padding]

coloredTxtWrapCursor :: n -> TextCursor -> Widget n
coloredTxtWrapCursor = txtWrapWithCursor defaultWrapSettings

txtWrapWithCursor :: WrapSettings -> n -> TextCursor -> Widget n
txtWrapWithCursor settings n tc =
  Widget Greedy Fixed $ do
    c <- getContext
    let s = rebuildTextCursor tc
        theLines = fixEmpty <$> wrapTextToLines settings (c ^. availWidthL) s
        fixEmpty l
          | T.null l = T.pack " "
          | otherwise = l
    case force theLines of
      [] ->
        let cursorLoc = [CursorLocation (Brick.Location (0, 0)) (Just n)]
         in return $ set cursorsL cursorLoc (emptyResult & imageL .~ V.text' (c ^. attrL) (T.pack " "))
      multiple ->
        let maxLength = maximum $ safeTextWidth <$> multiple
            loc = findPhysicalLocation tc (safeTextWidth <$> multiple)
            cursorLoc = [CursorLocation loc (Just n)]
            padding = V.charFill (c ^. attrL) ' ' (c ^. availWidthL - maxLength) (length lineImgs)
            lineImgs = lineImg <$> multiple
            createImage (text, attr) =
              V.text'
                (attrMapLookup (attrName (attribute attr)) (ctxAttrMap c))
                (sanitiseText text)
            lineImg lStr =
              let vals = colorMap lStr
                  vals' = map createImage vals
                  endspaces =
                    V.text'
                      (c ^. attrL)
                      (T.replicate (maxLength - safeTextWidth lStr) (T.pack " "))
               in foldr (V.<|>) endspaces vals'
         in return $ set cursorsL cursorLoc (emptyResult & imageL .~ V.horizCat [V.vertCat lineImgs, padding])

findPhysicalLocation :: TextCursor -> [Int] -> Brick.Location
findPhysicalLocation tc y = go (textCursorIndex tc) y 0
  where
    go i [] h = Brick.Location (i, h)
    go i [x] h = Brick.Location (i, h)
    go i (x : xs) h = if i > x then go (i - x - 1) xs (h + 1) else Brick.Location (i, h)

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


-- | returns a new GUI with the text edited based on autocomplete
completeWord :: GUI -> GUI 
completeWord s = let newText = getCompletedText s in
  s {
    cursor = makeTextFieldCursor newText,
    previous = Just s
  }

-- | Given a gui, gets the suggested word and appends it to text using autocomplete modoule
getCompletedText :: GUI -> Text
getCompletedText gui = let (rest, curr) = getCurrentWord gui in
  let completedCurr = fromMaybe "" (bestSuggestion curr (dictionary gui)) in
    T.pack $ (rest ++ " " ++ completedCurr)

-- | Given a gui gets the suggested word
getSuggestedWord :: GUI -> String 
getSuggestedWord gui = let (rest, curr) = getCurrentWord gui in
  fromMaybe "" (bestSuggestion curr (dictionary gui))

-- | returns a tuple (rest, currentWord)
getCurrentWord :: GUI -> (String, String)
getCurrentWord s = let text = rebuildTextFieldCursor (cursor s) in
  case words (T.unpack text) of
    [] -> ("", "")
    l -> let curr = last l in splitAt (length (T.unpack text) - length curr - 1) (T.unpack text)
-- ^^^ Right now this just works for the last word.... how do I get the current word based on cursor position?
-- TextFieldCursorSelectNextWord ?? 
