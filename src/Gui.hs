module Gui   where

import Brick
import Brick.AttrMap
import Brick.Widgets.Border
import Brick.Widgets.Center
import Brick.Widgets.Core
import Cursor.TextField
import Cursor.Brick.TextField
import Cursor.Types
import LuSyntax hiding (Name)
import Data.Maybe
import Formatter
import System.Environment
import ColorMapper
import AutoComplete
import Graphics.Vty.Attributes
import Graphics.Vty.Input.Events
import Data.Tree
import Path
import Path.IO
import Data.Text
import Data.Text.IO

{--
-- This text editor is heavily inspired by Tom Sydney Kerckhove's tutorial, available at 
-- <https://www.youtube.com/watch?v=Kmf3lnln1BI>
--}

-- | Data model for a text editor, including the string, and dictionary of current words
data GUI = GUI {
    cursor :: TextFieldCursor, 
    dictionary :: [String],
    path :: Path Abs File
}

-- | Resource Name data type
data Name = 
    Name 
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
            initialState <- openFile contents path
            endState <- defaultMain app initialState
            let contents' = rebuildTextFieldCursor (cursor endState)
            Data.Text.IO.writeFile (Path.fromAbsFile path) contents'
        -- no arguments passed or more than one argument passed
        _ -> error "Error: Usage - project-cis552-exe [filename]"
    

-- initial state, from opening a file to get text
openFile :: Text -> Path Abs File -> IO GUI
openFile text path = do
  return GUI {
      cursor = makeTextFieldCursor text,
      dictionary = [],
      path = path
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
            let s' = s {cursor = next}
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
            EvKey KEsc [] -> halt s
            _ -> continue s
    _ -> continue s

-- draws a GUI
drawGUI :: GUI -> [Widget Name]
drawGUI gui = [ forceAttr (attrName "text") $
    centerLayer $
    border $
    padLeftRight 1 $ selectedTextFieldCursorWidget Name (cursor gui),
    forceAttr (attrName "bg") $ fill '@'
  ]

-- gets current (partial) word
getCurrentWord :: GUI -> String
getCurrentWord = undefined

-- gets dictionary of used words
getDictionary :: GUI -> [String]
getDictionary = undefined
