module Game.OrionsOutlaws.UI.ScoresUI
  ( scoresUI
  ) where

import Game.OrionsOutlaws.Rendering.UI    (UI, ui, text, Justification (JustCentered), UIElement)
import Game.OrionsOutlaws.Model           (Score (..))
import Game.OrionsOutlaws.Assets          (pixeboyFont)
import Game.OrionsOutlaws.Util.Util       (enumerateFrom, rpad)
import Game.OrionsOutlaws.Rendering.Font  (Font(..))
import Data.Char                          (toUpper)

scoresUI :: [Score] -> UI
scoresUI scores = ui $ text "High Scores" JustCentered pixeboyFont 1 (0, 200) : concatMap renderScore (enumerateFrom 1 $ rpad 5 Nothing $ map Just scores)
  where
    scoreScale :: Float
    scoreScale = 0.3

    renderScore :: (Int, Maybe Score) -> [UIElement]
    renderScore (i, s) = let yOffset = 100 + (fromIntegral (-(i - 1) * fontLineHeight pixeboyFont) * scoreScale)
      -- Using a case instead of pattern matching so we can reuse the yOffset
      in case s of
        Just (Score n s' d) -> [text (show i ++ ". " ++ map toUpper (n ++ " " ++ show s' ++ " " ++ show d)) JustCentered pixeboyFont scoreScale (0, yOffset)]
        Nothing             -> [text (show i ++ ". -") JustCentered pixeboyFont scoreScale (0, yOffset)]
