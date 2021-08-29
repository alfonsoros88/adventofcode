module Lib
  ( readIngredients
  , ingredientsCount
  , separateIngredients
  ) where

import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import           Data.List           (sort)
import           Data.List.Split     (splitOn)
import           Data.Set            (Set)
import qualified Data.Set            as Set

type Ingredient = String

type Allergen = String

readIngredients :: String -> ([Ingredient], [Allergen])
readIngredients ls = (words ingredients, allergens)
  where
    (ingredients, aller) = span (/= '(') ls
    allergens = tail $ words $ filter (/= ',') $ tail $ init aller

indexByAllergen :: ([Ingredient], [Allergen]) -> [(Allergen, Set Ingredient)]
indexByAllergen (ingredients, allergens) =
  ($ ingredientSet) <$> ((,) <$> allergens)
  where
    ingredientSet = Set.fromList ingredients

allergensMap ::
     [([Ingredient], [Allergen])] -> HashMap Allergen (Set Ingredient)
allergensMap ls =
  HashMap.fromListWith Set.intersection (concat $ indexByAllergen <$> ls)

ingredientsCount :: [[Ingredient]] -> HashMap Ingredient Int
ingredientsCount ls = HashMap.fromListWith (+) $ concatMap (fmap (, 1)) ls

removeSimpleAllergens ::
     HashMap Allergen (Set Ingredient) -> HashMap Allergen (Set Ingredient)
removeSimpleAllergens m = HashMap.fromList $ removeSolved <$> HashMap.toList m
  where
    solved =
      foldr1 Set.union (HashMap.elems (HashMap.filter ((== 1) . Set.size) m))
    removeSolved :: (Allergen, Set Ingredient) -> (Allergen, Set Ingredient)
    removeSolved =
      \(a, ing) ->
        if Set.size ing == 1
          then (a, ing)
          else (a, Set.difference ing solved)

isSolved :: HashMap Allergen (Set Ingredient) -> Bool
isSolved m = all ((== 1) . Set.size) $ HashMap.elems m

bruteReduce ::
     HashMap Allergen (Set Ingredient) -> HashMap Allergen (Set Ingredient)
bruteReduce m =
  if g m' < g m
    then bruteReduce m'
    else m
  where
    g x = sum $ Set.size <$> HashMap.elems x
    m' = removeSimpleAllergens m

separateIngredients ::
     [([Ingredient], [Allergen])] -> ([Ingredient], [Ingredient])
separateIngredients input =
  if isSolved m'
    then (ing, sortedByAllergen)
    else error "Could not solve"
  where
    m = allergensMap input
    m' = bruteReduce m
    allI = foldr1 Set.union (Set.fromList . fst <$> input)
    withAller = foldr1 Set.union (HashMap.elems m')
    ing = Set.toList $ allI `Set.difference` withAller
    sortedByAllergen = concat $ Set.toList . snd <$> sort (HashMap.toList m')
