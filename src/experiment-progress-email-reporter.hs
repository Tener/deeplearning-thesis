{-# LANGUAGE OverloadedStrings #-}
module Main where

import Network.Mail.Mime
import System.Process
import System.Environment
import Control.Applicative
import qualified Data.Text as T
import qualified Data.Text.Lazy.IO as TIO
import qualified Data.Text.Lazy as TL
import qualified Text.Blaze.Html5 as B
import Text.Blaze.Html5 ((!))

import qualified Text.Blaze.Html5.Attributes as BA
import qualified Text.Blaze.Html.Renderer.Text as BR

import Data.List (intersperse)

main = do
  [addrEmail, addrName, fromEmail, fromName] <- getArgs
  raw <- readProcess "tmux-capture-all-panes-ggexp.sh" [] ""
  let plain = TL.pack raw
      html = BR.renderHtml $ B.docTypeHtml $ B.body $
             B.p $ B.toMarkup $ (B.code . (B.div ! BA.class_ "list") .  B.toMarkup) <$> (lines raw)
      addr = Address (Just $ T.pack addrName) (T.pack addrEmail)
      from = Address (Just $ T.pack fromName) (T.pack fromEmail)
  TIO.writeFile "/tmp/log.html" html
  mail <- simpleMail addr from "Experiment progress report" plain html [("Content-Type: text/plain; charset=utf-8", "/tmp/log.html")]

  renderSendMail mail
