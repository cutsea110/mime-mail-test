{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad (forM)
import qualified Data.ByteString.Lazy as L
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LT
import Network.Mail.Mime hiding (simpleMail)
import System.FilePath (takeFileName)

simpleMail :: Text -> [Text] -> [Text] -> [Text] -> Text -> LT.Text -> LT.Text -> [(Text, FilePath)] -> IO Mail
simpleMail from tos ccs bccs subject textBody htmlBody attachments = do
  as <- forM attachments $ \(ct, fn) -> do
    content <- L.readFile fn
    return (ct, fn, content)
  return Mail { mailFrom = Address Nothing from
              , mailTo = map (Address Nothing) tos
              , mailCc = map (Address Nothing) ccs
              , mailBcc = map (Address Nothing) bccs
              , mailHeaders = [ ("Subject", subject) ]
              , mailParts = [ Part "text/plain; charset=utf-8" QuotedPrintableText Nothing [] $ LT.encodeUtf8 textBody
                            , Part "text/html; charset=utf-8" QuotedPrintableText Nothing [] $ LT.encodeUtf8 htmlBody
                            ]
                            : (map (\(ct, fn, content) -> [Part ct Base64 (Just $ T.pack (takeFileName fn)) [] content]) as)
              }

testMail :: IO Mail
testMail = simpleMail
           "cutsea110@gmail.com" 
           []
           []
           [ "cutsea110@gmail.com"
           ]
           "全部BCCで投げてみた"
           "journalに対するテストを兼ねてる."
           "<p><a href='http://www.google.co.jp?q=journal+exchange'>journal</a>に対するテストを兼ねている.<br /></p>"
           [ ("text/plain", "/home/cutsea110/devel/haskell/mime-mail-test/attached.txt")
           , ("image/png", "/home/cutsea110/devel/haskell/mime-mail-test/screen_2013-08-07-162617.png")
           ]

main :: IO ()
main = renderSendMail =<< testMail
