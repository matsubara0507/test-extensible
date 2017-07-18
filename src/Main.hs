{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE TypeOperators    #-}

module Main where

import           Control.Lens
import           Data.Extensible
import           Sample.Aeson
import           Sample.Csv

main :: IO ()
main = putStrLn "hello world"

type Book = Record '[ "name" >: String
                    , "author" >: [String]
                    , "date" >: String
                    , "isbm" >: String
                    , "price" >: Float
                    ]

book1 :: Book
book1 = #name @= "Type and Programming Language"
     <: #author @= ["Benjamin C. Pierce"]
     <: #date @= "January 2002"
     <: #isbm @= "9780262162098"
     <: #price @= 95.00
     <: emptyRecord

book2 :: Book
book2 = #name @= "Structure and Interpretation of Computer Programs"
     <: #author @= ["Harold Abelson", "Gerald Jay Sussman", "Julie Sussman"]
     <: #date @= "July 1996"
     <: #isbm @= "9780262510875"
     <: #price @= 55.00
     <: emptyRecord

{- no good
book2 :: Book
book2 = #name @= "Structure and Interpretation of Computer Programs"
     <: #author @= ["Harold Abelson", "Gerald Jay Sussman", "Julie Sussman"]
     <: #date @= "July 1996"
     <: #price @= 55.00
     <: #isbm @= "9780262510875"
     <: emptyRecord
-}
