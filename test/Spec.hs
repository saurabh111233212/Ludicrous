import ColorMapperTests
import ParserTests
import AutoCorrectTests


main :: IO ()
main = do
  runAutoCorrectTests
  runParserTests
  runColorMapperTests
  return ()




