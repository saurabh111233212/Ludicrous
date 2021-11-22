module Gui   where

import Brick
import Cursor.TextField
import Cursor.Brick.TextField
import LuSyntax hiding (Name)
import Data.Maybe
import Formatter
import ColorMapper
import AutoComplete
import Data.Tree
import Data.Text (Text)

-- | Data model for a text editor, including the string, and dictionary of current words
data GUI = GUI {
    cursor :: TextFieldCursor, 
    dictionary :: [String]
}

-- | Resource Name data type
data Name = 
    Name 
    deriving (Show, Ord, Eq)

-- | Application
app :: App GUI e Name
app = App {
    appDraw = drawGUI,
    appChooseCursor = undefined,
    appHandleEvent = undefined,
    appStartEvent = undefined,
    appAttrMap = undefined
}

-- handles terminal input
terminal_init :: IO ()
terminal_init = undefined

-- initial state, from opening a file to get text
openFile :: Text -> IO GUI
openFile = undefined 

-- formats text contained within a GUI, returns a new, formatted GUI
format :: GUI -> GUI
format = undefined

-- handles a Brick Event
handleEvent :: GUI -> BrickEvent Name e -> EventM Name (Next GUI)
handleEvent g e = undefined

-- draws a GUI
drawGUI :: GUI -> [Widget Name]
drawGUI = undefined

-- gets current (partial) word
getCurrentWord :: GUI -> String
getCurrentWord = undefined

-- gets dictionary of used words
getDictionary :: GUI -> [String]
getDictionary = undefined
