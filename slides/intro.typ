#import "@preview/cetz:0.4.2"
#import "@preview/touying:0.6.1": *
#import themes.simple: *

#let primary = rgb("#386641")

#show: simple-theme.with(
  aspect-ratio: "16-9",
  header: none,
  primary: primary,
  config-methods(
    init: (self: none, body) => {
      set text(lang: "de", fill: self.colors.neutral-darkest, size: 24pt)
      show footnote.entry: set text(size: .6em)
      show strong: self.methods.alert.with(self: self)
      show heading.where(level: self.slide-level + 1): set text(1.4em)
      show raw: set text(font: "CaskaydiaCove NF", 1em)

      body
    },
  )
)


#title-slide(config: config-page(fill: primary))[
  #set text(1.5em, fill: white)
  = Advent of Code 2025
  == Die Dekoration des Nordpols
]


== Unser Abenteuer führte uns ...

+ zur Basis des Nordpols, dessen Eingang durch ein Passwort geschützt war, das
  in einem Safe versteckt war.
+ Im Souvenir-Geschäfts mussten wir die hauseigene Produkt-Datenbank von
  unsinnigen Produkt-IDs bereinigen, die durch einen Angestellten eingegeben
  wurden.
+ In der Lobby angekommen, fanden wir defekte Fahrstühle und Rolltreppen.
  Letztere konnten wir durch das Anschalten von Batterien, die die maximal
  mögliche Spannung erzeugten, wieder funktionsfähig machen.
+ Wir haben Papierrollen aus den Weg geräumt, damit wir eine Wand durchbrechen
  konnten.
+ Die Weihnachtshelfenden des Nordpols ist einer Lebensmittelvergiftung
  entkommen, da wir frische Zutaten identifizieren konnten.
+ Durch einen Müllschlucker aus der Cafetaria in eine Müllpresse traffen wir
  auf eine junge Kopffüßerin und halfen ihr bei ihren Mathematik-Hausaufgaben.


#focus-slide[
  #set text(1.5em)
  Bühne frei für euch!
]

