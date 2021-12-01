module Lib
  ( validPass
  , validPass2
  ) where

import qualified Data.HashMap as HashMap
import Data.Maybe
import qualified Data.Set as Set
import Text.Regex

type Passport = HashMap.Map String String

validPass :: Passport -> Bool
validPass p = Set.isSubsetOf requiredFields (HashMap.keysSet p)
  where
    requiredFields =
      Set.fromList ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

-- part 2
lk = flip HashMap.lookup

inRange :: Int -> Int -> String -> Bool
inRange l h s = l <= x && x <= h
  where
    x = read s :: Int

byr :: Passport -> Bool
byr p = Just True == (inRange 1920 2002 <$> lk p "byr")

iyr :: Passport -> Bool
iyr p = Just True == (inRange 2010 2020 <$> lk p "iyr")

eyr :: Passport -> Bool
eyr p = Just True == (inRange 2020 2030 <$> lk p "eyr")

hgtregex = mkRegex "^([0-9]+)(cm|in)$"

hgt :: Passport -> Bool
hgt p =
  case lk p "hgt" >>= matchRegex hgtregex of
    Just (n:"cm":_) -> inRange 150 193 n
    Just (n:"in":_) -> inRange 59 76 n
    _ -> False

hcl :: Passport -> Bool
hcl p = isJust $ lk p "hcl" >>= matchRegex re
  where
    re = mkRegex "^#[0-9a-f]{6}$"

ecl :: Passport -> Bool
ecl p = Just True == (isValid <$> lk p "ecl")
  where
    colorset = Set.fromList ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
    isValid x = Set.member x colorset

pid :: Passport -> Bool
pid p = isJust $ lk p "pid" >>= matchRegex re
  where
    re = mkRegex "^[0-9]{9}$"

validPass2 :: Passport -> Bool
validPass2 p = foldl1 (&&) $ fieldChecks <*> pure p
  where
    fieldChecks = [byr, iyr, eyr, hgt, hcl, ecl, pid]
