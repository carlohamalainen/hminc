{-# LANGUAGE TypeFamilies, FlexibleInstances #-}

module Data.Minc.Utils where

import Data.Minc.Types

import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Either

type MincIO a = IO (Either MincError a)

type Access a = ReaderT (String, FilePath) (EitherT MincError IO) a

runAccess :: String -> String -> Access a -> MincIO a
runAccess f p = runEitherT . flip runReaderT (f, p)

class Checkable a where
    type OutType a :: *
    status :: a -> Int
    proj :: a -> OutType a

instance Checkable Int where
    type OutType Int = ()
    status s = s
    proj _ = ()

instance Checkable (Int, a) where
    type OutType (Int, a) = a
    status (s, _) = s
    proj (_, a) = a

instance Checkable (Int, a, b) where
    type OutType (Int, a, b) = (a, b)
    status (s, _, _) = s
    proj (_, a, b) = (a, b)

instance Checkable (Int, a, b, c) where
    type OutType (Int, a, b, c) = (a, b, c)
    status (s, _, _, _) = s
    proj (_, a, b, c) = (a, b, c)

instance Checkable (Int, a, b, c, d) where
    type OutType (Int, a, b, c, d) = (a, b, c, d)
    status (s, _, _, _, _) = s
    proj (_, a, b, c, d) = (a, b, c, d)

instance Checkable (Int, a, b, c, d, e) where
    type OutType (Int, a, b, c, d, e) = (a, b, c, d, e)
    status (s, _, _, _, _, _) = s
    proj (_, a, b, c, d, e) = (a, b, c, d, e)

chk :: Checkable a => IO a -> Access (OutType a)
chk act = do
    res <- lift $ liftIO $ act
    let st  = status res
        val = proj   res
    (f, p) <- ask
    lift $ if st == 0
      then right val
      else left $ MincError f st "FIXME - look up error" p
