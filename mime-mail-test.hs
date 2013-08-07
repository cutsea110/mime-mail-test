{-# LANGUAGE OverloadedStrings #-}
module Main where

import Network.Mail.Mime

testMail :: IO Mail
testMail = simpleMail addr addr
           "てすとでふー"
           "文字化けについてのテストだよ."
           "<p><a href='http://www.google.co.jp'>文字化け</a>についてのテストだよ.<br /></p>"
           []
  where
    addr = Address Nothing "your@example.com"

main :: IO ()
main = renderSendMail =<< testMail
