
import Text.XML.HXT.Parser.XmlParsec
import Text.XML.HXT.XPath.XPathEval

import qualified Network.HTTP

main = do
    xml <- Network.HTTP.simpleHttp (getRequest "http://www.theinsider.com/partners/index.xml")
    -- let xml = "<foo><a>A</a><c>C</c></foo>"
    let xmltree = head $ xread xml
    -- let result = getXPath "//a" xmltree
    print $ xmltree
    -- print $ xread "<foo>abc<bar/>def</foo>"
    print "HI"
