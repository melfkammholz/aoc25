#import "@preview/cetz:0.4.2"
#import "@preview/touying:0.6.1": *
#import themes.simple: *


#show: simple-theme.with(
  aspect-ratio: "16-9",
  header: none,
  config-methods(
    init: (self: none, body) => {
      set text(lang: "de", fill: self.colors.neutral-darkest, size: 24pt)
      show footnote.entry: set text(size: .6em)
      show strong: self.methods.alert.with(self: self)
      show heading.where(level: self.slide-level + 1): set text(1.4em)
      show raw: set text(font: "CaskaydiaCove NF", 1em)
      show math.equation.where(block: false): box
      show raw.where(block: false): box

      body
    },
  )
)


#title-slide[
  = Happy With Haskell
  == Advent of Code 2025
]


== Tag 1 (Drehknopf-Klicks eines Safes)

#text(0.8em, align(center + horizon)[
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
])


== ModInt

#text(0.71em, align(center + horizon)[
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
])


== Tag 2 (Lustige Produkt-IDs mit Wiederholungen)

Ein Wort $w in Sigma^*$ ist genau dann eine Wiederholung, wenn ein
$i in { 2, 3, ..., abs(w) }$ existiert, sodass
$ i - 1 + "LCP"_i (w) = abs(w) and gcd(i - 1, "LCP"_i (w)) = i - 1, $
wobei $"LCP"_i (w) = max { k in {i, i + 1, ..., abs(w)} | w[i..k] in "Pref"(w) }$ ist, falls ein $k$ existiert, sonst ist $"LCP"_i (w) = 0$.

_Beispiel_
$
w = upright("10101") ~> "LCP"(w) = mat(5, 0, 3, 0, 1) ~> "ohne ggT" i in {3, 5} \
w = upright("101101") ~> "LCP"(w) = mat(6, 0, 1, 3, 0, 1) ~> i in {4}
$


== Implementierung

#align(center + horizon)[
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
]


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

#text(0.79em, align(center + horizon)[
```hs
  zalg :: String -> Seq Int
  zalg s = go 1 0 0 [n]
    where
      n = length s
      s' = fromList s :: Array Int Int

      go i _ _ z | i == n = z
      go i l r z = go (i + 1) l' r' (z |> zi)
        where
          c = if i < r then min (z ! (i - l)) (r - i) else 0
          zi = match c
          match j = if ok then 1 + match (j + 1) else 0
            where ok = i + j < n && s' ! j == s' ! (i + j)
          (l', r') = if i + zi > r then (i, i + zi) else (l, r)
  ```
])

== Tag 3 (Batteriebank)

Finde $i in {1, ..., abs(a)}^12$ so, dass $i_k < i_(k + 1)$
und $sum_(k=1)^12 10^(12 - k) a_(i_k)$ maximal ist.

#text(0.75em, align(center)[
  ```hs
  joltage :: Bank -> Int
  joltage (Bank a) = table ! (n - 1, 12)
    where
      n = length a
      table = listArray ((0, 1), (n - 1, 12)) [go i j | i <- [0..n - 1], j <- [1..12]]

      go 0 1              = a ! 0
      go i 1              = max (table ! (i - 1, 1)) (a ! i)
      go i k | i == k - 1 = table ! (i - 1, k - 1) * 10 + a ! i
      go i k              = max (table ! (i - 1, k)) (table ! (i - 1, k - 1) * 10 + a ! i)
  ```
])

Ist mir aufgefallen, dass man das Problem greedy lösen kann? Nö. Wollte ich
ein DP sehen und es mithilfe eines DP lösen? Ja.
#emoji.cocktail.tropical #h(-.6em) #emoji.face.cool

== Tag 4 (Gabelstapler und Wände durchbrechen)

#align(center + horizon)[
  Nicht weiter interessant für mich. Deshalb ist hier ein meme.
  // https://www.reddit.com/r/adventofcode/comments/1pdv05i/2025_day_4_part_12_surely_there_must_be_a_better/
  #image("images/forklift.jpg", height: 73%)
]

== Just kidding #emoji.face.tongue.squint Etwas später an Tag 4

Fasse die Papierrollen als Subgraph des Gitter-Graphen auf. Lösche iterativ
alle Knoten deren Eingangsgrad kleiner als 4 ist. In Haskell mit
```hs Graph``` (Adjazenzlisten) aus ```hs Data.Graph```.

```hs
access :: Graph -> Int
access g = m + if m > 0 then access g' else 0
  where
    vs = fmap (< 4) (indegree g)
    m = lengthOn id vs
    g' = deleteVertices vs g
```

== Löschen von Knoten mit Reindexierung

#align(center + horizon)[
  #text(0.96em)[
    ```hs
    deleteVertices :: Array Int Bool -> Graph -> Graph
    deleteVertices vs g =
      let bs@(m, n) = Array.bounds g
          rem = filter (\v -> not (vs ! v))
          vs' = rem (Array.indices g)
          ixs = Array.array bs (zip vs' [m..])
       in Array.array (m, n - lengthOn id vs)
            [(ixs ! v, fmap (ixs !) (rem ws)) | (v, ws) <- Array.assocs g
                                              , not (vs ! v)
                                              ]
    ```
  ]
]

== Tag 5 (Keine Lebenmittelsvergiftung am Nordpol)

#align(horizon)[
  Gegeben sind $A_i = [a_i, b_i] = {a_i, a_i + 1, ..., b_i}, a_i <= b_i$.
  Wir sollen $abs(union.big_(i=1)^n [a_i, b_i])$ berechnen.
  #footnote[
    Entschuldigung, wir sollen das Integral der Indikatorfunktion $chi_A$ unter
    dem Zählmaß des Messraums $(ZZ, cal(P)(ZZ))$ berechnen, wobei
    $A = union.big_(i=1)^n [a_i, b_i]$ ist.
  ]


  Wenn $a_(i+1) <= b_i$ gilt, dann $A_i$ und $A_(i + 1)$ nicht disjunkt und wir
  vereinigen sie als $[a_i, max(b_i, b_(i + 1))]$. Solange so ein $i$ existiert,
  wiederholen wieder diesen Schritt.
]

== Implementierung

#align(center + horizon)[
  ```hs
  fresh :: Database -> Int
  fresh (Database rs _) = go rs
    where
      go [r] = size r
      go (r1@(Range a1 b1) : r2@(Range a2 b2) : rs)
        | a2 <= b1  = go (insert (max b1 b2) r1 : rs)
        | otherwise = size r1 + go (r2 : rs)
  ```
]

