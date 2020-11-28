module Utils where

import Data.Maybe (fromMaybe)

(?:) :: Maybe c -> c -> c
(?:) = flip fromMaybe

(=:) = (,)