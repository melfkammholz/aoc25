#import "@preview/cetz:0.4.2"
#import "@preview/touying:0.6.1": *
#import themes.simple: *


#show: simple-theme.with(
  aspect-ratio: "16-9",
  header: none,
  config-methods(
    init: (self: none, body) => {
      set text(lang: "de", fill: self.colors.neutral-darkest, size: 25pt)
      show footnote.entry: set text(size: .6em)
      show strong: self.methods.alert.with(self: self)
      show heading.where(level: self.slide-level + 1): set text(1.4em)
      show raw: set text(font: "CaskaydiaCove NF", 0.67em)

      body
    },
  )
)


#title-slide[
  = Happy With Haskell
  == Advent of Code 2025
]

== Tag 1

#align(center)[
  ```hs
  data Rot = RotL Int | RotR Int
    deriving Show

  dist :: Rot -> Int
  dist (RotL n) = -n
  dist (RotR n) = n

  pattern Rot :: Int -> Rot
  pattern Rot n <- (dist -> n)

  password :: [Rot] -> Int
  -- lame: password = lengthOn (== 0) . scanl (\x (Rot n) -> (x + n) `mod` 100) 50
  password = lengthOn (== 0) . scanl (\x (Rot n) -> x + modInt n) (modInt @100 50)


  -- scanl f x0 [x1, x2, ...] = x0 : f x0 x1 : f (f x0 x1) x2 : ...
  ```
]

== ModInt

#align(center)[
  ```hs
  data ModInt n = MI !Int
    deriving Eq

  type family Modulus (m :: Nat) :: Constraint where
    Modulus m = 
      (KnownNat m, NonZero m ('Text "Modulus cannot be zero."))

  modInt :: forall m. Modulus m => Int -> ModInt m
  modInt x = MI (x `smod` fromInteger (natVal (Proxy @m)))

  smod :: Integral a => a -> a -> a
  smod x m = (x `mod` m + m) `mod` m

  instance Modulus m => Num (ModInt m) where
    MI x + MI y = modInt (x + y)
    MI x - MI y = modInt (x - y)
    MI x * MI y = modInt (x * y)
    -- ...
  ```
]

== Tag 2

Ein Wort $w in Sigma^*$ ist genau dann eine Wiederholung, wenn ein
$i in { 2, 3, ..., abs(w) }$ existiert, sodass
$ i - 1 + "LCP"_i (w) = abs(w) and gcd(i - 1, "LCP"_i (w)) = i - 1. $

#text(1.1em, align(center)[

  ```hs
  isRep :: String -> Bool
  isRep w =
    let n = length w
        lcp = zalg w
     in Seq.foldrWithIndex 
          (\i lcpi b -> b || i + lcpi == n && gcd i lcpi == i) 
          False 
          lcp
  ```
])

== Nutze $"LCP"_k (w), k < i$ für $"LCP"_i (w)$

#align(center)[
  #cetz.canvas({
    import cetz.draw: *
    
    rect((8, 0), (15, -1.5), stroke: green.darken(25%), fill: green.lighten(50%), name: "abox1")
    content("abox1", [$alpha$])

    rect((13, 1.5), (15, 0), stroke: red.darken(25%), fill: red.lighten(50%), name: "bbox1")
    content("bbox1", [$beta$])

    rect((0, -2.25), (7, -3.75), stroke: green.darken(25%), fill: green.lighten(50%), name: "abox2")
    content("abox2", [$alpha$])
    
    rect((5, -0.75), (7, -2.25), stroke: red.darken(25%), fill: red.lighten(50%), name: "bbox2")
    content("bbox2", [$beta$])

    set-style(mark: (symbol: "|"))
    line((0, 0), (20, 0), name: "w", stroke: 1.25pt)
    content("w.start", [$w$], anchor: "east", padding: .4)

    set-style(mark: (symbol: none))
    line((8, .25), (8, -.25), name: "l")
    content("l.start", [$l$], anchor: "south", padding: .2)
    line((13, .25), (13, -.25), name: "i")
    content("i.start", [$i$], anchor: "south-east", padding: .2)
    line((15, .25), (15, -.25), name: "r")
    content("r.start", [$r$], anchor: "south-west", padding: .2)

    line((5, .25), (5, -3.5), stroke: (dash: "dotted"))
    line((5, -3.5), (5, -4), name: "i-l+1")
    content("i-l+1.start", [$i-l+1$], anchor: "north", padding: 0.8)

  })
]

#text(0.9em)[
  $"LCP"_(i-l+1)(w)$ ist bereits berechnet, kann aber länger als $abs(beta)$.
  Danach können verschiedene Buchstaben folgen 
  $=> "LCP"_i (w) >= min("LCP"_(i-l+1)(w), abs(beta))$ mit anschließenden expliziten matching.
]

== Z-Algorithmus

Berechnung der längsten gemeinsamen Präfixe der Suffixe eines Wortes.

#align(center)[
```hs
  zalg :: String -> Seq Int
  zalg s = go 1 0 0 (Seq.singleton n)
    where
      n = length s
      s' = Array.listArray (0, n - 1) s

      match i c = if ok then 1 + match i (c + 1) else 0
        where ok = i + c < n && s' Array.! c == s' Array.! (i + c)

      go i _ _ z | i == n = z
      go i l r z =
        let c = if i < r then min (z `Seq.index` (i - l)) (r - i) else 0
            !zi = match i c
            (l', r') = if i + zi > r then (i, i + zi) else (l, r)
         in go (i + 1) l' r' (z Seq.|> zi)
  ```
]
