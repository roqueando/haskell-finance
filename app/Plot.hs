{-# LANGUAGE OverloadedStrings #-}
module Plot where

import Data.Csv (FromNamedRecord (..), Header, (.:), decodeByName)
import qualified RIO.ByteString.Lazy as BS
import qualified Data.Vector as V

import GHC.Generics (Generic)
import qualified RIO.Text as T
import Graphics.Vega.VegaLite
import Prelude hiding (filter, lookup)
import Data.List (tails)

data Stock = AMZN
           | DPZ
           | BTC
           | NFLX

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

-- | read data from CSV and plot to a html file
readDataAndPlot :: IO ()
readDataAndPlot = do
  csvFile <- BS.readFile "portfolio_data.csv"
  let decoded = decodeByName csvFile :: Either String (Header, V.Vector PriceInfoT)
  case decoded of
    Left e -> print e
    Right (_, result) ->
        let transformed = transformPriceInfoByStock result AMZN
         in toHtmlFile "prices.html" $ plotStockPrices transformed

-- | transform a vector of price info into a tuple data with date, name and price
transformPriceInfo :: V.Vector PriceInfoT -> [(T.Text, T.Text, Double)]
transformPriceInfo ps = concatMap toRows (V.toList ps)
    where
        toRows pinfo =
            [ (date pinfo, "AMZN", amazon pinfo)
            , (date pinfo, "DPZ", dpz pinfo)
            , (date pinfo, "BTC", bitcoin pinfo)
            , (date pinfo, "NFLX", netflix pinfo)
            ]

-- | same as `transformPriceInfo` but filtering by one stock
transformPriceInfoByStock :: V.Vector PriceInfoT -> Stock -> [(T.Text, T.Text, Double)]
transformPriceInfoByStock ps st = concatMap (toRows st) (V.toList ps)
    where
        toRows AMZN pinfo = [(date pinfo, "AMZN", amazon pinfo)]
        toRows DPZ pinfo = [(date pinfo, "DPZ", dpz pinfo)]
        toRows BTC pinfo = [(date pinfo, "BTC", bitcoin pinfo)]
        toRows NFLX pinfo = [(date pinfo, "NFLX", netflix pinfo)]

-- | plot all stock prices by color
plotStockPrices :: [(T.Text, T.Text, Double)] -> VegaLite
plotStockPrices ps =
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


-- | exponential weighted moving average
movingAvg :: [Double] -> Double
movingAvg [] = 0.0
movingAvg (x:xs) = a * x + (1.0 - a) * movingAvg xs
    where
        a = 0.95

-- | arithmetic mean
mean :: [Double] -> Double
mean xs = sum xs / fromIntegral (length xs)

mAvg :: [Double] -> Int -> [Double]
mAvg [] _ = [0.0]
mAvg xs n = map ((\a -> a / fromIntegral n) . sum) (window' n xs)
    where
        window' wn = foldr (zipWith (:)) (Prelude.repeat []) . take wn . tails
