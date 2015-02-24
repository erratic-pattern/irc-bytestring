module Network.IRC.ByteString.Utils where

import Control.Applicative
import Data.ByteString.Char8 as BS

infixr 5 <:>, <++>

-- |Utility function
-- @
--   infixr 5 <:> 
--   x <:> xs = cons <$> x <*> xs
-- @
(<:>) :: Applicative f => f Char -> f ByteString -> f ByteString          
x <:> xs = BS.cons <$> x <*> xs

-- |Utility function
-- @
--   infixr 5 <++> 
--   x <++> y = append <$> x <*> y
-- @
(<++>) :: Applicative f => f ByteString -> f ByteString -> f ByteString
x <++> y = BS.append <$> x <*> y
