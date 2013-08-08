{-# LANGUAGE OverloadedStrings #-}
module Main where

import Network.Mail.Mime

testMail :: IO Mail
testMail = simpleMail addr addr
           "テスト"
           "journalに対するテストを兼ねてる."
           "<p><a href='http://www.google.co.jp'>文字化け</a>についてのテスト.</p>"
           []
  where
    addr = Address Nothing "cutsea110@gmail.com"

main :: IO ()
main = renderSendMail =<< testMail
