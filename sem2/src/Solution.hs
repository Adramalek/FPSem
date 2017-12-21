module Solution where

import Types
import Data.Unique
import Data.List (elemIndex)
import System.IO.Unsafe (unsafePerformIO)

type Result = Either String Type
type Context = [(Symbol, Type)]

getUniqueSymbol :: Symbol -> Symbol
getUniqueSymbol sym = let getSym x = do
                                       unique <- newUnique
				       let newSym = (x++) . show . hashUnique $ unique
				       return newSym
		      in unsafePerformIO . getSym $ sym


isOK :: Result -> Maybe Type
isOK (Left _)  = Nothing
isOK (Right t) = Just t


ifCorrect :: Context -> Term -> (Type -> Result) -> Result
ifCorrect ctx tm act = let mtm = isOK $ typeOf' ctx tm
		       in case mtm of
		         Nothing   -> Left "..."
		         (Just tp) -> act tp

ifBothCorrect :: Context -> Term -> Term -> (Type -> Type -> Result) -> Result
ifBothCorrect ctx tm1 tm2 act = ifCorrect ctx tm1 $ \tp1 -> ifCorrect ctx tm2 $ \tp2 -> act tp1 tp2


equalTypes' :: Context -> Term -> Term -> Result
equalTypes' ctx tm1 tm2 = ifBothCorrect ctx tm1 tm2 $ \tp1 tp2 -> if tp1 == tp2 then Right tp2 else Left "..."

equalTypes :: Context -> Type -> Term -> Term -> Result
equalTypes ctx tp tm1 tm2 = ifBothCorrect ctx tm1 tm2 $ \tp1 tp2 -> let tps = [tp1, tp2]
							             in if all (==tp) tps
						                       then Right tp
							               else Left "..."


typeOf' :: Context -> Term -> Result
typeOf' ctx term = case term of
  (Sym x)           -> let ind = elemIndex x $ map fst ctx
		       in case ind of
		         Nothing  -> Left "..."
			 (Just i) -> Right . snd $ ctx!!i
  (Natural n)       -> Right Nat 
  (Boolean b)       -> Right Bool
  (Nil tp)          -> Right . List $ tp
  (Add tm1 tm2)     -> equalTypes ctx Nat tm1 tm2
  (Mult tm1 tm2)    -> equalTypes ctx Nat tm1 tm2
  (And tm1 tm2)     -> equalTypes ctx Bool tm1 tm2 
  (Or tm1 tm2)      -> equalTypes ctx Bool tm1 tm2
  (Not tm)          -> ifCorrect ctx tm $ \tp -> if tp == Bool then Right tp else Left "..."
  (Iff tm1 tm2 tm3) -> let func tp = if tp == Bool
					   then equalTypes' ctx tm2 tm3
					   else Left "..."
			   in ifCorrect ctx tm1 func
  (Pair tm1 tm2)    -> ifBothCorrect ctx tm1 tm2 $ \tp1 tp2 -> Right $ PairT tp1 tp2
  (Fst tm)          -> typeOf' ctx tm
  (Snd tm)          -> typeOf' ctx tm
  (IsNil tm)        -> typeOf' ctx tm
  (Head tm)         -> typeOf' ctx tm
  (Tail tm)         -> ifCorrect ctx tm $ \tp -> Right . List $ tp
  (Cons tm1 tm2)    -> ifBothCorrect ctx tm1 tm2 $ \tp1 tp2 -> if tp1 == tp2
  					  		         then case tp1 of 
							           (List t) -> Right . List $ t
							 	   _        -> Right . List $ tp1
							         else case tp1 of
							           (List t1) -> if t1 == tp2
								                  then Right . List $ t1
								  	          else case tp2 of
								 	            (List t2) -> if t1 == t2
									 	                 then Right . List $ t1
										  	         else Left "..."
									  	    _         -> Left "..."
							           _         -> case tp2 of
								                   (List t2) -> if tp1 == t2
									                          then Right . List $ t2
									 		          else Left "..."
									           _         -> Left "..."
  (App tm1 tm2)    -> ifCorrect ctx tm1 $ \tp -> case tp of
                                                  (Fun atp rtp) -> if atp == tp
					                             then Right rtp
					  		  	     else Left "..."
						  _             -> Left "..."
  (Lam sym tp tm)  -> ifCorrect ((sym,tp):ctx) tm $ \tp0 -> Right $ Fun tp tp0


typeOf :: Term -> Result
typeOf = typeOf' []

-- > typeOf $ Lam "x" Nat $ Add (Sym "x") (Natural 5)
-- Right (Fun Nat Nat)

-- > typeOf $ Lam "x" Bool $ Sym "x"
-- Right (Fun Bool Bool)

-- > typeOf $ Add (Natural 5) (Boolean False)
-- Left "..."

-- > typeOf $ App (Lam "x" Nat $ Sym "x") (Natural 5)
-- Right Nat

-- > typeOf $ App (Lam "x" Nat $ Boolean False) (Natural 5)
-- Right Bool

-- > typeOf $ App (Lam "x" Bool $ Boolean False) (Natural 5)
-- Left "..."

-- > typeOf $ Nil Nat
-- Right (List Nat)

-- > typeOf $ Cons (Natural 5) $ Cons (Boolean False) $ Nil Nat
-- Left "..."

