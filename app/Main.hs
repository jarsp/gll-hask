module Main where

import qualified Data.HashMap.Strict as H
import Data.HashMap.Strict ((!))
import qualified Data.Hashable as HS
import qualified Data.Set as S
import qualified Data.List as L
import qualified Data.Ord as O
import qualified Debug.Trace as TR

import qualified Text.Dot as D

import Lib

----- Test Grammars -----

--- G1 ---
g1 = H.fromList [(NT "S", S.fromList [[T "a", NT "S", T "b"],
                                      [T "d"],
                                      [T "a", T "d", T "b"]])
                ]

g1LMap = H.fromList [("RS1", (NT "S", [T "a", NT "S"], [T "b"]))
                    ]

g1Aux = (g1, g1LMap)

getFuncG1 = flip getFunc g1Func

g1Func = H.fromList [("L0", (g1L0)),
                     ("LS", (g1LS)),
                     ("LS1", (g1LS1)),
                     ("RS1", (g1RS1)),
                     ("LS2", (g1LS2)),
                     ("LS3", (g1LS3))
                    ]

g1L0 :: ParseFunc
g1L0 inp ax (d, (r@(l, t, i, w)):rs, u, p, g, s) =
    (getFuncG1 l) inp ax (r, rs, u, p, g, s)
g1L0 _ _ s = s

g1LS :: ParseFunc
g1LS inp ax st = g1L0 inp ax st1
    where t = getCU st
          i = getCI st
          w = getCN st
          c = getSym i inp
          st1 | c == 'a' = add ("LS3", t, i, None) $ add ("LS1", t, i, None) st
              | c == 'd' = add ("LS2", t, i, None) st
              | otherwise = st

g1LS1 :: ParseFunc
g1LS1 inp ax st = 
    let i = getCI st 
        (st1, w1) = getNodeT (T "a") i st
        st2 = setCN w1 $ setCI (i + 1) st1
        (st3, t1) = create (setLabel "RS1" $ getDesc st2) ax st2
        st4 = setCU t1 st3
        match = [getSym (i + 1) inp] `L.isInfixOf` "ad"
        stfinal = if match then st4 else st2
        next = if match then (g1LS) else (g1L0)
     in next inp ax stfinal

g1RS1 :: ParseFunc
g1RS1 inp ax st =
    let i = getCI st
        (st1, cr) = getNodeT (T "b") i st
        (st2, w1) = getNodeP (NT "S") [T "a", NT "S", T "b"] [] (getCN st1) cr ax st1
        st3 = setCN w1 $ setCI (i + 1) st2
        st4 = pop (getCU st3) (getCI st3) (getCN st3) ax st3
        match = getSym i inp == 'b'
        stfinal = if match then st4 else st
     in g1L0 inp ax stfinal

g1LS2 :: ParseFunc
g1LS2 inp ax st =
    let i = getCI st
        (st1, cr) = getNodeT (T "d") i st
        (st2, w1) = getNodeP (NT "S") [T "d"] [] (getCN st2) cr ax st1
        st3 = setCN w1 $ setCI (i + 1) st2
        stfinal = pop (getCU st3) (getCI st3) (getCN st3) ax st3
     in g1L0 inp ax stfinal

g1LS3 :: ParseFunc
g1LS3 inp ax st =
    let i = getCI st
        (st1, w1) = getNodeT (T "a") i st
        st2 = setCI (i + 1) $ setCN w1 st1
        (st3, cr1) = getNodeT (T "d") (i + 1) st2
        (st4, w2) = getNodeP (NT "S") [T "a", T "d"] [T "b"] w1 cr1 ax st3
        st5 = setCI (i + 2) $ setCN w2 st4
        (st6, cr2) = getNodeT (T "b") (i + 2) st5
        (st7, w3) = getNodeP (NT "S") [T "a", T "d", T "b"] [] w2 cr2 ax st6
        st8 = setCI (i + 3) $ setCN w3 st7
        st9 = pop (getCU st8) (getCI st8) (getCN st8) ax st8
        matchd = getSym (i + 1) inp == 'd'
        matchb = getSym (i + 2) inp == 'b'
        stfinal = if matchd && matchb then st9 else (if matchd then st5 else st2)
     in g1L0 inp ax stfinal

--- G2 --

g2 = H.fromList [(NT "S", S.fromList [[NT "S", T "+", NT "S"],
                                      [NT "S", T "*", NT "S"],
                                      [T "x"]])
                ]

g2LMap = H.fromList [("RS1", (NT "S", [NT "S", T "+", NT "S"], [])),
                     ("RS2", (NT "S", [NT "S", T "*", NT "S"], [])),
                     ("L1", (NT "S", [NT "S"], [T "+", NT "S"])),
                     ("L2", (NT "S", [NT "S"], [T "*", NT "S"]))
                    ]

g2Aux = (g2, g2LMap)

getFuncG2 = flip getFunc g2Func

g2Func = H.fromList [("L0", (g2L0)),
                     ("LS", (g2LS)),
                     ("LS1", (g2LS1)),
                     ("L1", (g2L1)),
                     ("RS1", (g2RS1)),
                     ("LS2", (g2LS2)),
                     ("L2", (g2L2)),
                     ("RS2", (g2RS2)),
                     ("LS3", (g2LS3))
                    ]

g2L0 :: ParseFunc
g2L0 inp ax (d, (r@(l, t, i, w)):rs, u, p, g, s) =
    (getFuncG2 l) inp ax (r, rs, u, p, g, s)
g2L0 _ _ s = s

g2LS :: ParseFunc
g2LS inp ax st = g2L0 inp ax st1
    where t = getCU st
          i = getCI st 
          w = getCN st
          c = getSym i inp
          st1 | c == 'x' = add ("LS3", t, i, None) $ add ("LS2", t, i, None) $ add ("LS1", t, i, None) st
              | otherwise = st

g2LS1 :: ParseFunc
g2LS1 inp ax st =
    let (st1, t1) = create (setLabel "L1" $ getDesc st) ax st
        st2 = setCU t1 st1
     in g2LS inp ax st2

g2L1 :: ParseFunc
g2L1 inp ax st = 
    let i = getCI st
        (st1, cr) = getNodeT (T "+") i st
        (st2, w1) = getNodeP (NT "S") [NT "S", T "+"] [NT "S"] (getCN st1) cr ax st1
        st3 = setCN w1 $ setCI (i + 1) st2
        (st4, t1) = create (setLabel "RS1" $ getDesc st3) ax st3 
        st5 = setCU t1 st4
        matchp = getSym i inp == '+'
        matchx = getSym (i + 1) inp == 'x'
        (stfinal, next) = if matchp && matchx then
                             (st5, (g2LS))
                          else if matchp then
                                  (st3, (g2L0))
                               else 
                                  (st, (g2L0))
     in next inp ax stfinal

g2RS1 :: ParseFunc
g2RS1 inp ax st =
    let st1 = pop (getCU st) (getCI st) (getCN st) ax st
     in g2L0 inp ax st1

g2LS2 :: ParseFunc
g2LS2 inp ax st =
    let (st1, t1) = create (setLabel "L2" $ getDesc st) ax st
        st2 = setCU t1 st1
     in g2LS inp ax st2

g2L2 :: ParseFunc
g2L2 inp ax st = 
    let i = getCI st
        (st1, cr) = getNodeT (T "*") i st
        (st2, w1) = getNodeP (NT "S") [NT "S", T "*"] [NT "S"] (getCN st1) cr ax st1
        st3 = setCN w1 $ setCI (i + 1) st2
        (st4, t1) = create (setLabel "RS2" $ getDesc st3) ax st3 
        st5 = setCU t1 st4
        matchp = getSym i inp == '*'
        matchx = getSym (i + 1) inp == 'x'
        (stfinal, next) = if matchp && matchx then
                             (st5, (g2LS))
                          else if matchp then
                                  (st3, (g2L0))
                               else
                                  (st, (g2L0))
     in next inp ax stfinal

g2RS2 :: ParseFunc
g2RS2 inp ax st =
    let st1 = pop (getCU st) (getCI st) (getCN st) ax st
     in g2L0 inp ax st1

g2LS3 :: ParseFunc
g2LS3 inp ax st =
    let i = getCI st 
        (st1, cr) = getNodeT (T "x") i st
        (st2, w1) = getNodeP (NT "S") [T "x"] [] (getCN st1) cr ax st1
        st3 = setCN w1 $ setCI (i + 1) st2
        stfinal = pop (getCU st3) (getCI st3) (getCN st3) ax st3
     in g2L0 inp ax stfinal

--- G3 --

g3 = H.fromList [(NT "S", S.fromList [[NT "S", T "+", NT "S"],
                                      [NT "S", T "*", NT "S"],
                                      [T "x"]])
                ]

g3LMap = H.fromList [("RS1", (NT "S", [NT "S", T "+", NT "S"], [])),
                     ("RS2", (NT "S", [NT "S", T "*", NT "S"], [])),
                     ("L1", (NT "S", [NT "S"], [T "+", NT "S"])),
                     ("L2", (NT "S", [NT "S"], [T "*", NT "S"]))
                    ]

g3Aux = (g3, g3LMap)

getFuncG3 = flip getFunc g3Func

g3Func = H.fromList [("L0", (g3L0)),
                     ("LS", (g3LS)),
                     ("LS1", (g3LS1)),
                     ("L1", (g3L1)),
                     ("RS1", (g3RS1)),
                     ("LS2", (g3LS2)),
                     ("L2", (g3L2)),
                     ("RS2", (g3RS2)),
                     ("LS3", (g3LS3))
                    ]

g3L0 :: String -> ParseFunc
g3L0 _ inp ax st =
    let (l, st') = popQueue st
     in if l /= "" then (getFuncG3 l) "" inp ax st' else st'

g3LS :: String -> ParseFunc
g3LS msg inp ax st = g3L0 "" inp ax st1
    where t = getCU st
          i = getCI st 
          w = getCN st
          c = getSym i inp
          st1 | c == 'x' = 
                    case msg of
                      "*_l" ->
                          add ("LS3", t, i, None) $ add ("LS2", t, i, None) st
                      "*_r" -> 
                          add ("LS3", t, i, None) st
                      "+_r" ->
                          add ("LS3", t, i, None) $ add ("LS2", t, i, None) st
                      otherwise -> 
                          add ("LS3", t, i, None) $ add ("LS2", t, i, None) $ add ("LS1", t, i, None) st
              | otherwise = st

g3LS1 :: String -> ParseFunc
g3LS1 _ inp ax st =
    let (st1, t1) = create (setLabel "L1" $ getDesc st) ax st
        st2 = setCU t1 st1
     in g3LS "+_l" inp ax st2

g3L1 :: String -> ParseFunc
g3L1 _ inp ax st = 
    let i = getCI st
        (st1, cr) = getNodeT (T "+") i st
        (st2, w1) = getNodeP (NT "S") [NT "S", T "+"] [NT "S"] (getCN st1) cr ax st1
        st3 = setCN w1 $ setCI (i + 1) st2
        (st4, t1) = create (setLabel "RS1" $ getDesc st3) ax st3 
        st5 = setCU t1 st4
        matchp = getSym i inp == '+'
        matchx = getSym (i + 1) inp == 'x'
        (stfinal, next) = if matchp && matchx then
                             (st5, (g3LS))
                          else if matchp then
                                  (st3, (g3L0))
                               else 
                                  (st, (g3L0))
     in next "+_r" inp ax stfinal

g3RS1 :: String -> ParseFunc
g3RS1 _ inp ax st =
    let st1 = pop (getCU st) (getCI st) (getCN st) ax st
     in g3L0 "" inp ax st1

g3LS2 :: String -> ParseFunc
g3LS2 _ inp ax st =
    let (st1, t1) = create (setLabel "L2" $ getDesc st) ax st
        st2 = setCU t1 st1
     in g3LS "*_l" inp ax st2

g3L2 :: String -> ParseFunc
g3L2 _ inp ax st = 
    let i = getCI st
        (st1, cr) = getNodeT (T "*") i st
        (st2, w1) = getNodeP (NT "S") [NT "S", T "*"] [NT "S"] (getCN st1) cr ax st1
        st3 = setCN w1 $ setCI (i + 1) st2
        (st4, t1) = create (setLabel "RS2" $ getDesc st3) ax st3 
        st5 = setCU t1 st4
        matchp = getSym i inp == '*'
        matchx = getSym (i + 1) inp == 'x'
        (stfinal, next) = if matchp && matchx then
                             (st5, (g3LS))
                          else if matchp then
                                  (st3, (g3L0))
                               else
                                  (st, (g3L0))
     in next "*_r" inp ax stfinal

g3RS2 :: String -> ParseFunc
g3RS2 _ inp ax st =
    let st1 = pop (getCU st) (getCI st) (getCN st) ax st
     in g3L0 "" inp ax st1

g3LS3 :: String -> ParseFunc
g3LS3 _ inp ax st =
    let i = getCI st 
        (st1, cr) = getNodeT (T "x") i st
        (st2, w1) = getNodeP (NT "S") [T "x"] [] (getCN st1) cr ax st1
        st3 = setCN w1 $ setCI (i + 1) st2
        stfinal = pop (getCU st3) (getCI st3) (getCN st3) ax st3
     in g3L0 "" inp ax stfinal

------ Dotfile Generation ------

main :: IO ()
main = undefined
