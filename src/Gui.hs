module Gui   where

import Brick
import Brick.AttrMap
import Brick.Widgets.Border
import Brick.Widgets.Center
import Brick.Widgets.Core
import Cursor.TextField
import Cursor.List.NonEmpty
import Cursor.Brick.TextField
import Cursor.Brick.Text
import Cursor.Text
import Cursor.Types
import LuSyntax hiding (Name)
import Data.Maybe
import Formatter
import System.Environment
import ColorMapper
import AutoComplete
import qualified Graphics.Vty as V
import Graphics.Vty.Attributes
import Graphics.Vty.Input.Events 
import Text.Wrap
import Path
import Path.IO
import Lens.Micro ((^.), (.~), (&))
import Data.Text
import qualified Data.Text as T
import Data.Text.IO

{--
-- This text editor is heavily inspired by Tom Sydney Kerckhove's tutorial, available at 
-- <https://www.youtube.com/watch?v=Kmf3lnln1BI>
--}

-- | Data model for a text editor, including the string, and dictionary of current words
data GUI = GUI {
    cursor :: TextFieldCursor, 
    dictionary :: [String],
    previous :: Maybe GUI
}

-- | Resource Name data type
data Name = 
    Text |
    Viewport
    deriving (Show, Ord, Eq)

-- | Application
app :: App GUI e Name
app = App {
    appDraw = drawGUI,
    appChooseCursor = showFirstCursor,
    appHandleEvent = handleEvent,
    appStartEvent = pure,
    appAttrMap = const $ attrMap mempty [(attrName "text", fg white), (attrName "bg", fg black)]
}

-- handles terminal input. Should take a single argument, 
-- and open that as a file
terminalInit :: IO ()
terminalInit = do
    args <- getArgs
    case args of
        [x] -> do
            -- gets the path for a file from the string x
            path <- resolveFile' x
            maybeContents <- forgivingAbsence $ Data.Text.IO.readFile (Path.fromAbsFile path)
            let contents = fromMaybe Data.Text.empty maybeContents
            initialState <- openFile contents
            endState <- defaultMain app initialState
            let contents' = rebuildTextFieldCursor (cursor endState)
            saveFile contents' path
        -- no arguments passed or more than one argument passed
        _ -> error "Error: Usage - project-cis552-exe [filename]"

saveFile :: Text -> Path Abs File -> IO ()
saveFile txt path = Data.Text.IO.writeFile (Path.fromAbsFile path) txt

-- initial state, from opening a file to get text
openFile :: Text -> IO GUI
openFile text = do
  return GUI {
      cursor = makeTextFieldCursor text,
      dictionary = [],
      previous = Nothing
    }


-- formats text contained within a GUI, returns a new, formatted GUI
format :: GUI -> GUI
format = undefined

-- handles a Brick Event
handleEvent :: GUI -> BrickEvent Name e -> EventM Name (Next GUI)
handleEvent s e =
  case e of
    VtyEvent vtye ->
      let mDo ::
               (TextFieldCursor -> Maybe TextFieldCursor)
            -> EventM n (Next GUI)
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
              _ -> continue s
            _ -> continue s
    _ -> continue s

selectedTextCursorWidget' :: n -> TextCursor -> Widget n
selectedTextCursorWidget' n tc = let wgt = create_tc_widget tc in
  Widget Greedy Fixed $ do
    ctx <- getContext
    let distance = textCursorIndex tc
        loc = Brick.Location (distance `mod` ctx^.availWidthL, distance `div` ctx^.availWidthL) in
      render $ Brick.showCursor n loc wgt

-- modified widget helper to wrap lines
create_tc_widget :: TextCursor -> Widget n
create_tc_widget tc =
  txtWrap $
    let t = sanitiseText $ rebuildTextCursor tc
     in if T.null t
          then T.pack " "
          else t

-- modified widget creator to wrap lines
create_tfc_widget :: n -> TextFieldCursor -> Widget n
create_tfc_widget n (TextFieldCursor tfc) = 
  flip foldNonEmptyCursor tfc $ \befores current afters ->
    vBox $
      Prelude.concat
        [ Prelude.map textWidgetWrap befores,
          [visible $ selectedTextCursorWidget' n current],
          Prelude.map textWidgetWrap afters
        ]

-- draws a GUI
drawGUI :: GUI -> [Widget Name]
drawGUI gui = [
    border $
    padLeftRight 1 $ viewport Viewport Vertical $ create_tfc_widget Text (cursor gui)
  ]

-- gets current (partial) word
getCurrentWord :: GUI -> String
getCurrentWord = undefined

-- gets dictionary of used words
getDictionary :: GUI -> [String]
getDictionary = dictionary
