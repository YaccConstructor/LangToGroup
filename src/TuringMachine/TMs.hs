module TuringMachine.TMs (
    testingSetTMs,
    module TuringMachine,
    module ShowInfo,
  ) where

import TuringMachine
import ShowInfo

import TuringMachine.Constructors
import TuringMachine.Optimization

-- here '.' == blankChar (from TuringMachine.Symbol)
-- I use '.' for more readable code

simple :: String -> TuringMachine
simple s = foldr1 (++>) $ move toRight <$> pure <$> s

star :: String -> TuringMachine
star s = loop (simple s) ||> check "."

plus :: String -> TuringMachine
plus s = simple s ++> star s

opt :: String -> TuringMachine
opt s = simple s ||> check "."

just :: TuringMachine -> TuringMachine
just = (++> check ".")

dyck_v1 :: TuringMachine
dyck_v1 =
    check "." ||>
    die "*" ||>
    loop (
        (
                rewriteAndMove "(" '.' toRight ++>
                (moveInf toRight "()*" ||> rewrite "." '*')
              ||>
                rewriteAndMove ")" '.' toRight ++>
                (moveInf toRight "()*" ||> move toLeft ".") ++>
                (rewriteAndMove "*" '.' toLeft ||> die ".()")
          ) ++> (
            moveInf toLeft "()*" ||> move toRight "."
          )
      )

dyck_v2 :: TuringMachine
dyck_v2 = optimize maxO $
    check "." ||>
    die ")" ||>
    loop (
        rewriteAndMove "(" '.' toRight ++>
        (rewrite "(" '{' ||> rewrite ")" '}' ||> die ".")
      ) ||>
    loop (
        rewriteAndMove "{" '.' toRight ++>
        (
            moveInf toRight "{}" ||>
            rewriteAndMove "(" '{' toRight ||>
            rewriteAndMove ")" '}' toRight ||>
            die "."
          ) ++>
        (
            rewrite "(" '{' ||>
            rewrite ")" '}' ||>
            die "."
          ) ++>
        (
            moveInf toLeft "{}" ||>
            move toRight "."
          )
      ) ||>
    loop (rewriteAndMove "}" '.' toRight)

dyck_v3 :: TuringMachine
dyck_v3 = optimize maxO $
    check "." ||>
    die ")" ||>
    loop (
        rewriteAndMove "(" '.' toRight @@>
        rewriteAndMove ")" '.' toRight ++>
        (
            die "." ||>
            (
                rewriteAndMove "(" '.' toRight ++>
                (
                    moveInf toRight "(" ||>
                    rewrite ")" '(' ||>
                    die "."
                  ) ++>
                (
                    moveInf toLeft "(" ||>
                    move toRight "."
                  )
              )
          )
      )

badEvenPalindrome :: String -> TuringMachine
badEvenPalindrome a =
    check "." ||>
    loop (
        foldr1 (||>) [
            rewriteAndMove [c] '.' toRight ++>
            (moveInf toRight a ||> move toLeft ".") ++>
            (rewrite [c] '.' ||> die ('.':a)) ++>
            move toLeft "." ++>
            (moveInf toLeft a ||> move toRight ".")
          | c <- a
          ]
      )

evenPalindrome :: String -> TuringMachine
evenPalindrome a =
    check "." ||>
    loop (
        foldr1 (||>) [
            rewriteAndMove [c] '.' toRight ++>
            (moveInf toRight a ||> move toLeft ".") ++>
            (rewrite [c] '.' ||> die ('.':a))
          | c <- a
          ] ++>
        move toLeft "." ++>
        (moveInf toLeft a ||> move toRight ".")
      )

testingSetTMs :: [WithTitle TuringMachine]
testingSetTMs = uncurry withTitle <$> [
    ("a*", secure $ star "a"),
    ("a+", secure $ plus "a"),
    ("a?", secure $ just $ opt "a"),
    ("a|b", secure $ just $ simple "a" ||> simple "b"),
    ("abc", secure $ just $ simple "abc"),
    ("ababa", secure $ just $ simple "ababa"),
    ("a(bc)*ba v.1",
        secure $ just $ simple "ab" ++> (die "." ||> star "cb" ||> simple "a")
      ),
    ("a(bc)*ba v.2",
        secure $ just $
            move toRight "a" ++>
            (move toRight "b" @@> move toRight "c") ++>
            move toRight "a"
      ),
    ("Dyck v.1", dyck_v1),
    ("Dyck v.2", dyck_v2),
    ("Dyck v.3", dyck_v3),
    ("wwR 2s v.1", badEvenPalindrome "ab"),
    ("wwR 2s v.2", evenPalindrome "ab"),
    ("wwR 4s v.1", badEvenPalindrome "abcd"),
    ("wwR 4s v.2", evenPalindrome "abcd"),
    ("wwR 8s", evenPalindrome "abcdefgh"),
    ("wwR 16s", evenPalindrome "abcdefghijklmnop"),
    ("wwR 32s", evenPalindrome "abcdefghijklmnopqrstuvwxyz012345"),
    ("wwR 64s",
        evenPalindrome
            "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789^_"
      )
  ]
