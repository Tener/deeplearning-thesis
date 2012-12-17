-- general utilities not really connected with the main focus of this project

module Utils where

import Text.Printf

data Hidden a = Hidden { unhide :: a }

class TypeTag a where
    getTypeTag :: a -> String -- get 'name' of a type

instance (TypeTag a) => Show (Hidden a) where
    show (Hidden foo) = (printf "Hidden {<%s>}" (getTypeTag foo) :: String)

instance Eq (Hidden a) where
    _ == _ = True