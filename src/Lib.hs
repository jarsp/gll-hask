{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Lib
    ( module Lib
    ) where

import qualified Data.HashMap.Strict as H
import Data.HashMap.Strict ((!))
import qualified Data.Hashable as HS
import qualified Data.Set as S
import qualified Data.List as L
import qualified Data.Ord as O
import qualified Debug.Trace as TR

------ Type Declarations, Constants ------
class Display a where
    display :: a -> String

pretty :: State -> IO()
pretty s = putStr $ display s

data Symbol = NT String
            | T String
    deriving (Show, Eq, Ord)

instance Display Symbol where
    display (NT s) = s
    display (T s) = s

instance HS.Hashable Symbol where
    hashWithSalt s n =
        case n of
          NT x -> s `HS.hashWithSalt` x
          T x -> s `HS.hashWithSalt` x

type Item = [Symbol]

instance Display Item where
    display l = concatMap (display) l

type Grammar = H.HashMap Symbol (S.Set Item)

instance Display a => Display (S.Set a) where
    display s = S.foldr (\i a -> a ++ " " ++ display i) "" s

instance (Display a, Display b) => Display (H.HashMap a b) where
    display h = H.foldrWithKey (\k v a -> a ++ "\n" ++ display k ++ ": " ++ display v) "" h

data SPPFNode = Node Symbol Integer Integer
              | Inter Symbol Item Item Integer Integer
              | Pack Symbol Item Item Integer SPPFNode SPPFNode
              | None
    deriving (Show, Eq, Ord)

instance Display SPPFNode where
    display (Node s i j) = "(" ++ display s ++ ", " ++ show i ++ ", " ++ show j ++ ")"
    display (Inter s a b i j) = "{" ++ display s ++ " ::= " ++ display a ++ "." ++ display b ++ ", " ++ show i ++ ", " ++ show j ++ "}"
    display (Pack s a b i l r) = "[(" ++ display s ++ " ::= "  ++ display a ++ "." ++ display b ++ ", " ++ show i ++ ") " ++ display l ++ " " ++ display r ++ "]"
    display None = "($)"

instance HS.Hashable SPPFNode where
    hashWithSalt s n =
        case n of
          Node x i j -> s `HS.hashWithSalt` x `HS.hashWithSalt` 
                        i `HS.hashWithSalt` j
          Inter x a b i j -> s `HS.hashWithSalt` x `HS.hashWithSalt` 
                             a `HS.hashWithSalt` b `HS.hashWithSalt` 
                             i `HS.hashWithSalt` j
          Pack x a b i l r -> if l /= None then 
                                 s `HS.hashWithSalt` x `HS.hashWithSalt` 
                                 a `HS.hashWithSalt` b `HS.hashWithSalt`
                                 i `HS.hashWithSalt` l `HS.hashWithSalt` r
                              else
                                 s `HS.hashWithSalt` x `HS.hashWithSalt` 
                                 a `HS.hashWithSalt` b `HS.hashWithSalt`
                                 i `HS.hashWithSalt` r

type SPPF = H.HashMap SPPFNode (S.Set SPPFNode)

type GSSNode = (String, Integer)

instance Display GSSNode where
    display n = show n

type GSSEdge = (GSSNode, SPPFNode) -- should be a regular node

instance Display GSSEdge where
    display (n, s) = "(" ++ display n ++ ", " ++ display s ++ ")"

type GSS = H.HashMap GSSNode (S.Set GSSEdge)

type LabelMap = H.HashMap String (Symbol, Item, Item)

type Desc = (String, GSSNode, Integer, SPPFNode)

instance Display Desc where
    display (l, t, i, w) = "(" ++ l ++ ", " ++ display t ++ ", " ++ show i ++ ", " ++ display w ++ ")"

type RSet = [Desc]

instance Display RSet where
    display = show

type USet = S.Set Desc

type PSet = H.HashMap GSSNode (S.Set SPPFNode)

type State = (Desc, RSet, USet, PSet, GSS, SPPF)

instance Display State where
    display (d, r, u, p, g, s) = "Descriptor:\n" ++ display d ++ "\n\n"
                              ++ "Queue:\n " ++ display r ++ "\n\n"
                              ++ "Added:\n" ++ display u ++ "\n\n"
                              ++ "Popped:\n" ++ display p ++ "\n\n"
                              ++ "GSS:\n" ++ display g ++ "\n\n"
                              ++ "SPPF:\n" ++ display s ++ "\n\n"

type Aux = (Grammar, LabelMap)

type ParseFunc = String -> Aux -> State -> State

epsilon = [] :: Item
base = ("$", 0) :: GSSNode
initGSS = H.singleton base S.empty
initState = (("L0", base, 0, None), [], S.empty, H.empty, initGSS, H.empty) :: State

------ Auxilliary Functions/Data ------

nullable :: Symbol -> Grammar -> Bool
nullable n@(NT x) g = S.member epsilon $ g ! n
nullable (T x) _ = error $ "not a nonterminal: " ++ x

isTerminal :: Symbol -> Bool
isTerminal (T _) = True
isTerminal _ = False

add :: Desc -> State -> State
add d' (d, r, u, p, g, s) = (d, r', u', p, g, s) 
    where isNew = not $ S.member d' u
          u' = S.insert d' u
          r' = if isNew then (d':r) else r

pop :: GSSNode -> Integer -> SPPFNode -> Aux -> State -> State
pop ("$", 0) _ _ _ st = st
pop t@(l, k) i z ax@(gr, lm) (d, r, u, p, g, s) = st'
    where p' = H.insertWith f t (S.singleton z) p
          f _ old = S.insert z old
          (x, a, b) = lm ! l
          st' = S.foldr f' (d, r, u, p', g, s) $ g ! t
          f' (v, w) sx = add (l, v, i, y) sx'
              where (sx', y) = getNodeP x a b w z ax sx

create :: Desc -> Aux -> State -> (State, GSSNode)
create (l, t, i, w) ax@(gr, lm) st@(d, r, u, p, g, s) = (st', v)
    where v = (l, i)
          edge = (t, w)
          g' = H.insertWith f v (S.singleton edge) g
          f _ old = S.insert edge old
          isNew = case H.lookup v g of
                    Just es -> not $ S.member edge es
                    Nothing -> True
          (x, a, b) = lm ! l
          st' = if isNew then
                    case H.lookup v p of
                      Just popped -> S.foldr f' (d, r, u, p, g', s) popped
                      _ -> (d, r, u, p, g', s)
                else
                    (d, r, u, p, g', s)
          f' z@(Node _ _ h) sx = add (l, t, h, y) sx'
            where (sx', y) = getNodeP x a b w z ax sx

getNodeT :: Symbol -> Integer -> State -> (State, SPPFNode)
getNodeT t@(T x) i (d, r, u, p, g, s) = ((d, r, u, p, g, s'), n)
    where s' = H.insertWith f n S.empty s
          f _ old = old
          n = Node t i h 
          h = if x == "" then i else i + 1

getNodeP :: Symbol -> Item -> Item -> SPPFNode -> SPPFNode -> Aux -> State -> (State, SPPFNode)
-- Right child z is never an intermediate or packed node
-- Left child w is never a packed node
getNodeP x@(NT _) a b w z@(Node _ k i) (gr, lm) st@(d, r, u, p, g, s) =
    if (length a == 1)
       && (isTerminal (a !! 0) || (not $ nullable (a !! 0) gr))
       && (b /= epsilon)
    then 
        (st, z)
    else
        let t = if b == epsilon then (Node x) else (Inter x a b)
            y = if w /= None then 
                  case w of
                    Node _ j k -> t j i
                    Inter _ _ _ j k -> t j i
                    _ -> error $ "could not match y in: getNodeP " ++ show x ++ " "
                                 ++ show a ++ " " ++ show b ++ " " ++ show w ++ " "
                                 ++ show z
                else
                  t k i
            pack = Pack x a b k w z -- w may be None, but not z
            s' = H.insertWith f y (S.singleton pack) s
            f _ old = S.insert pack old
         in ((d, r, u, p, g, s'), y)
getNodeP x a b w z _ _ = error $ show x ++ " " ++ show a ++ " " ++ show b ++ " " ++ show w ++ " " ++ show z ++ "\n"

------ Parsing Helpers  ------
getFunc :: String -> H.HashMap String a -> a
getFunc s h = h ! s

getSym :: Integer -> String -> Char
getSym i s = s !! (fromIntegral i)

isSuccessful :: String -> SPPF -> Bool
isSuccessful inp s =
    if H.member (Node (NT "S") 0 $ toInteger $ length inp - 1) s then
        True
    else 
        False

getDesc :: State -> Desc
getDesc (d, _, _, _, _, _) = d

getLabel :: Desc -> String
getLabel (l, _, _, _) = l

setLabel :: String -> Desc -> Desc
setLabel l' (_, t, i, w) = (l', t, i, w)

getCU :: State -> GSSNode
getCU ((_, t, _, _), _, _, _, _, _) = t

setCU :: GSSNode -> State -> State
setCU t' ((l, t, i, w), r, u, p, g, s) = ((l, t', i, w), r, u, p, g, s)

getCI :: State -> Integer
getCI ((_, _, i, _), _, _, _, _, _) = i

setCI :: Integer -> State -> State
setCI i' ((l, t, i, w), r, u, p, g, s) = ((l, t, i', w), r, u, p, g, s)

getCN :: State -> SPPFNode
getCN ((_, _, _, w), _, _, _, _, _) = w

setCN :: SPPFNode -> State -> State
setCN w' ((l, t, i, w), r, u, p, g, s) = ((l, t, i, w'), r, u, p, g, s)

popQueue :: State -> (String, State)
popQueue (d, r:rs, u, p, g, s) = (getLabel r, (r, rs, u, p, g, s))
popQueue st = ("", st)
