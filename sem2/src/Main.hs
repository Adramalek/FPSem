module Main where

import Prelude hiding (readLn)

import Types
import Solution

import Control.Monad
import Data.List

{- transformers -}
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Writer
import Control.Monad.Trans.State
import Control.Monad.Trans.Class

{-
:: f a -> f (a -> b) -> f b
:: m a -> (a -> m b) -> m b
-}

(<**>) :: Monad m => m (a -> b) -> m a -> m b
(<**>) mf ma = mf >>= (\f -> ma >>= \a -> return $ f a)

type OrError = Either String

funny :: Int -> OrError Double
funny x = if x == 0
          then Left "Can't divide by zero"
          else Right (1 / fromIntegral x)
funny2 :: Double -> OrError Double
funny2 x = if x > 0 then Right $ sqrt x else Left "Sqrt from negative number"
funny3 :: Double -> OrError String
funny3 = Right . show

f x = do
  y1 <- funny x
  y2 <- funny2 y1
  funny3 y2

f2 x = funny x >>= (\y1 -> funny2 y1 >>= (\y2 -> funny3 y2))

g listOfLists = do
  petrov <- lookup "Petrov" listOfLists
  let abc = length petrov
  good <- find (>abc) petrov
  return good

twice :: Monad m => m a -> m a
twice act = act >> act

readerFun :: Reader (Int, Double) Double
readerFun = do
  (x,y) <- ask
  z <- asks fst
  return $ y^z

writerFun :: Writer String Int
writerFun = do
  tell "start"
  tell "end"
  return 42

statefulFibo :: State (Int, (Integer, Integer)) Integer
statefulFibo = do
  (n, (x,y)) <- get
  case n of
    1 -> return x
    2 -> return y
    _ -> do
      put (n-1, (y, x+y))
      statefulFibo

statefulFibo2 :: Int -> State (Integer, Integer) ()
statefulFibo2 n = replicateM_ n $ do
  (x,y) <- get
  put (y, x+y)

fibo2 n = fst $ execState (statefulFibo2 n) (0,1)


{- RWS r w s
   RWST r w s m
e.g. RWST r w s IO
e.g. RWST (Int,Double) () Int IO
-}
{-
monadStack :: ReaderT (Int, Double) (StateT Int IO) ()
monadStack = do
  (x,y) <- ask
  z <- lift get
  lift $ put $ x + truncate y + z
  lift $ lift $ print "Ololo!"


newtype MySt s a = MySt { runMySt :: s -> (a, s) }
newtype MyStT m s a = MyStT { runMyStT :: s -> m (a,s) }

instance Functor (MySt s) where
  fmap f (MySt g) = MySt $ \s -> let (a, s') = g s in (f a, s')

instance Applicative (MySt s) where
  pure x = MySt $ \s -> (x, s)
  MySt sf <*> MySt sx = MySt $ \s -> let
    (f, s') = sf s
    (x, s'')= sx s'
    in (f x, s'')

instance Monad (MySt s) where
  MySt sa >>= f = MySt $ \s -> let
    (a, s') = sa s
    MySt sb = f a
    in sb s'

get :: MySt s s
get = MySt $ \s -> (s, s)

put :: s -> MySt s ()
put s = MySt $ \_ -> ((), s)
-}



data Free f a = Free (f (Free f a))
              | Pure a

instance Functor f => Functor (Free f) where
  fmap f (Pure a) = Pure $ f a
  fmap f (Free ffree) = Free $ fmap f <$> ffree

instance Functor f => Applicative (Free f) where
  pure = Pure

  Pure a <*> Pure b = Pure $ a b
  Pure a <*> Free mb = Free $ fmap a <$> mb
  Free ma <*> b = Free $ (<*> b) <$> ma

instance Functor f => Monad (Free f) where
  Pure a >>= f = f a
  Free m >>= f = Free ((>>= f) <$> m)



data CmdF next
  = ReadLn (String -> next)
  | WriteLn String next

instance Functor CmdF where
  fmap f (ReadLn next) = ReadLn $ f . next
  fmap f (WriteLn s next) = WriteLn s $ f next


type Cmd = Free CmdF

cmd1 :: Cmd ()
cmd1 = Free (ReadLn (\s -> Free (WriteLn s (Pure ()))))

readLn = Free (ReadLn (\s -> Pure s))

writeLn s = Free (WriteLn s (Pure ()))

identity = readLn >>= writeLn

cmd2 = do
  x <- readLn
  writeLn x
  pure ()

cmd3 = do
  x <- readLn
  identity
  writeLn x

interp1 :: Show a => Cmd a -> [String]
interp1 (Pure a) = pure $ "Return: " ++ show a
interp1 (Free (ReadLn ffree)) = "readLn abc" : interp1 (ffree "abc")
interp1 (Free (WriteLn s next)) = ("writeLn " ++ s) : interp1 next

interp2 :: Cmd () -> IO ()
interp2 (Pure ()) = pure ()
interp2 (Free (ReadLn ffree)) = getLine >>= interp2 . ffree
interp2 (Free (WriteLn s next)) = putStrLn s >> interp2 next





main :: IO ()
main = do
  putStr "x" >> putStrLn "y"
  putStrLn "hello world"
  print $ runReader readerFun (5,6)
  print $ runState statefulFibo (5, (0,1))
  -- runStateT (runReaderT monadStack (5,6)) 2 >>= print

