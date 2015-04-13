module Main where

import Haste
import Haste.HPlay.View hiding (head)

import Binary.Application
import Control.Monad.IO.Class
import Prelude hiding (div, id)
import Binary.Util

main :: IO (Maybe ())
main = do
  addCss "./bootstrap.min.css"
  addCss "./bootstrap-theme.min.css"
  embedCss myCss
  addJs  "./jquery-1.11.2.min.js"
  addJs  "./bootstrap.min.js"
  embedJs myJs
  runBody $ at "main-content" Insert $ timeout 1000 (runApplication initialState)

addCss :: String -> IO ()
addCss s = addHeader $ 
  link ! atr "rel" "stylesheet" 
       ! href s

embedCss :: String -> IO ()
embedCss s = addHeader $ styleBlock s

addJs :: String -> IO ()
addJs s = addHeader $ script noHtml ! src s

embedJs :: String -> IO ()
embedJs s = addHeader $ script s 

myCss :: String
myCss = ".vertical-align {\n" ++
        "  display: flex;\n" ++
        "  flex-direction: row;\n" ++
        "}\n" ++

        ".vertical-align > [class^=\"col-\"],\n" ++
        ".vertical-align > [class*=\" col-\"] {\n" ++
        "  display: flex;\n" ++
        "  align-items: center;\n" ++
        "  justify-content: center; \n" ++
        "}"

myJs :: String
myJs = "var mouse = {x: 0, y: 0};\n" ++

      "document.addEventListener('mousemove', function(e){\n" ++
      "mouse.x = e.clientX || e.pageX;\n" ++
      "mouse.y = e.clientY || e.pageY\n" ++
      "}, false);"
