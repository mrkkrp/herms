module UnitConversions where

import Data.Maybe (mapMaybe)
import Data.Text (pack, replace, unpack)
import Text.Read (readMaybe)
import Text.Regex.TDFA ((=~))
import Types

-- NOTE: Here, "imperial" means "U.S. Customary". Conversion to British,
-- Australian, Canadian, etc. imperial units is not yet implemented.

data Conversion = Metric | Imperial | None deriving (Show, Read, Eq)

convertRecipeUnits :: Conversion -> Recipe -> Recipe
convertRecipeUnits unit recp =
  case unit of
    None -> recp
    Metric ->
      recp
        { ingredients = map convertIngredientToMetric (ingredients recp),
          directions = map convertTemperatureToMetric (directions recp)
        }
    Imperial ->
      recp
        { ingredients = map convertIngredientToImperial (ingredients recp),
          directions = map convertTemperatureToImperial (directions recp)
        }

convertIngredientToMetric :: Ingredient -> Ingredient
convertIngredientToMetric ingr =
  case unit ingr of
    Tsp -> ingr {quantity = quantity ingr * 5, unit = Ml}
    Tbsp -> ingr {quantity = quantity ingr * 15, unit = Ml}
    Oz -> ingr {quantity = quantity ingr * 30, unit = Ml}
    FlOz -> ingr {quantity = quantity ingr * 28, unit = G}
    Cup -> ingr {quantity = quantity ingr * 237, unit = Ml}
    Lb -> ingr {quantity = quantity ingr * 454, unit = G}
    Pint -> ingr {quantity = quantity ingr * 473, unit = Ml}
    Quart -> ingr {quantity = quantity ingr * 946, unit = Ml}
    Gallon -> ingr {quantity = quantity ingr * 3.785, unit = L}
    -- These cases are here so that if we add other units, the compiler will force us
    -- to add appropriate cases here.
    Ml -> ingr
    L -> ingr
    G -> ingr
    Other _ -> ingr

convertIngredientToImperial :: Ingredient -> Ingredient
convertIngredientToImperial ingr =
  case unit ingr of
    Ml
      | quantity ingr < 15 -> ingr {quantity = quantity ingr / 5, unit = Tsp}
      | quantity ingr < 250 -> ingr {quantity = quantity ingr / 15, unit = Tbsp}
      | otherwise -> ingr {quantity = quantity ingr / 250, unit = Cup}
    L
      | quantity ingr < 4 -> ingr {quantity = quantity ingr * 4.23, unit = Cup}
      | otherwise -> ingr {quantity = quantity ingr * 0.26, unit = Gallon}
    G -> ingr {quantity = quantity ingr / 28, unit = Oz}
    -- These cases are here so that if we add other units, the compiler will force us
    -- to add appropriate cases here.
    Tsp -> ingr
    Tbsp -> ingr
    Cup -> ingr
    Oz -> ingr
    FlOz -> ingr
    Lb -> ingr
    Pint -> ingr
    Quart -> ingr
    Gallon -> ingr
    Other _ -> ingr

convertTemperatureToMetric = convertTemperature C

convertTemperatureToImperial = convertTemperature F

convertTemperature :: TempUnit -> String -> String
convertTemperature u s =
  unpack $ foldl replaceTemperature (pack s) (packText . convertReplacement <$> findReplacements s)
  where
    packText (s1, s2) = (pack s1, pack s2)
    replaceTemperature text (old, new) = replace old new text
    convertReplacement = fmap $ show . toTempUnit u

findReplacements :: String -> [(String, Temperature)]
findReplacements = mapMaybe parseRegexResult . findTemperatures
  where
    parseRegexResult r = to3Tuple r >>= parseTemperature

to3Tuple :: [a] -> Maybe (a, a, a)
to3Tuple (a : b : c : _) = Just (a, b, c)
to3Tuple _ = Nothing

parseTemperature :: (String, String, String) -> Maybe (String, Temperature)
parseTemperature (s, v, u) = case (readMaybe v, parseTempUnit u) of
  (Just value, Just unit) -> Just (s, Temperature value unit)
  _ -> Nothing

-- returns a list of matches, where every match is a list of the regex groups
findTemperatures :: String -> [[String]]
findTemperatures s = s =~ "(-?[0-9]{1,3}) ?°?(C|F)([ .!?]|$)"

parseTempUnit :: String -> Maybe TempUnit
parseTempUnit "C" = Just C
parseTempUnit "F" = Just F
parseTempUnit _ = Nothing

data Temperature = Temperature Int TempUnit deriving (Eq)

instance Show Temperature where
  show (Temperature value unit) = show value ++ show unit

data TempUnit = C | F deriving (Eq)

instance Show TempUnit where
  show C = "°C"
  show F = "°F"

toTempUnit :: TempUnit -> Temperature -> Temperature
toTempUnit C (Temperature x F) = Temperature (fahrenheitToCelsius x) C
toTempUnit F (Temperature x C) = Temperature (celsiusToFahrenheit x) F
toTempUnit _ t = t

fahrenheitToCelsius :: Int -> Int
fahrenheitToCelsius = round . (/ 1.8) . (+ (-32)) . fromIntegral

celsiusToFahrenheit :: Int -> Int
celsiusToFahrenheit = round . (+ 32) . (* 1.8) . fromIntegral
