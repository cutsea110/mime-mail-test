{-# LANGUAGE OverloadedStrings #-}
module Main where

import Network.Mail.Mime

testMail :: IO Mail
testMail = simpleMail addr addr
           "てすとでふー"
           "文字化けについてのテスト."
           "<p><a href='http://www.google.co.jp'>文字化け</a>についてのテスト.</p>"
           []
  where
    addr = Address Nothing "cutsea110@gmail.com"

main :: IO ()
main = renderSendMail =<< testMail
