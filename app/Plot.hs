{-# LANGUAGE OverloadedStrings #-}
module Plot where

import Data.Csv (FromNamedRecord (..), Header, (.:), decodeByName)
import qualified RIO.ByteString.Lazy as BS
import qualified Data.Vector as V

import GHC.Generics (Generic)
import qualified RIO.Text as T
import Graphics.Vega.VegaLite
import Prelude hiding (filter, lookup, repeat)


data PriceInfoT = PriceInfo
  { date :: T.Text
  , amazon :: Double
  , dpz :: Double
  , bitcoin :: Double
  , netflix :: Double
  } deriving (Generic, Show)

instance FromNamedRecord PriceInfoT where
  parseNamedRecord v =
      PriceInfo
        <$> v .: "Date"
        <*> v .: "AMZN"
        <*> v .: "DPZ"
        <*> v .: "BTC"
        <*> v .: "NFLX"

readFromFile :: IO ()
readFromFile = do
  csvFile <- BS.readFile "portfolio_data.csv"
  let decoded = decodeByName csvFile :: Either String (Header, V.Vector PriceInfoT)
  case decoded of
    Left e -> print e
    Right (_, result) ->
        let transformed = transformPriceInfo result
         in toHtmlFile "prices.html" $ plotSomePrice transformed

plotSomething :: IO ()
plotSomething = do
    let cars = dataFromUrl "https://vega.github.io/vega-datasets/data/cars.json" []
        enc = encoding
                . position X [PName "Horsepower", PmType Quantitative]
                . position Y [PName "Miles_per_Gallon", PmType Quantitative, PTitle "Miles per Gallon"]
                . color [MName "Origin"]
        bkg = background "rgba(0, 0, 0, 0.05)"
     in toHtmlFile "result.html" $ toVegaLite [bkg, cars, mark Circle [MTooltip TTEncoding], enc [], height 800, width 600]

transformPriceInfo :: V.Vector PriceInfoT -> [(T.Text, T.Text, Double)]
transformPriceInfo ps = concatMap toRows (V.toList ps)
    where
        toRows pinfo =
            [ (date pinfo, "AMZN", amazon pinfo)
            , (date pinfo, "DPZ", dpz pinfo)
            , (date pinfo, "BTC", bitcoin pinfo)
            , (date pinfo, "NFLX", netflix pinfo)
            ]

plotSomePrice :: [(T.Text, T.Text, Double)] -> VegaLite
plotSomePrice ps =
    let dataValues = dataFromRows [Parse [("Date", FoDate "%m/%d/%Y")]]
                    . foldr mapToPriceInfo id ps
        enc = encoding
                . position X [PName "Date", PmType Temporal]
                . position Y [PName "Price", PmType Quantitative]
                . color [MName "Stock"]
        bkg = background "rgba(0, 0, 0, 0.05)"
    in toVegaLite [bkg, dataValues [], mark Line [], enc [], height 500, width 800]

    where
        mapToPriceInfo (d, s, p) acc =
            acc . dataRow [("Date", Str d), ("Stock", Str s), ("Price", Number p)]
