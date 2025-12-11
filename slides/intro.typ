#import "@preview/cades:0.3.1": qr-code
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

  #v(1em)

  #line(stroke: white, length: 3em)

  #v(.33em)

  #grid(
    columns: (1fr, auto),
    gutter: 1em,
    text(0.73em, align(left)[
      Wir bestellen Pizza (oder anderes) bei Farina di Nonna auf Selbstkosten.
      Bestellschluss ist $17^30$ Uhr. Meldet euch bitte mit eurer Bestellung
      bei Kairelius Prottus.
    ]),
    box(
      fill: white,
      outset: .125em,
      qr-code("https://farinadinonna.pizza/speisekarte/", color: primary, width: 3em)
    )
  )
]


== Unser Abenteuer führte uns ...

#text(0.76em)[
  #align(horizon)[
    #grid(
      columns: (1fr, 1fr),
      align: (horizon, horizon),
      gutter: 1em,
      [
        + zur Basis des Nordpols, dessen Eingang durch ein Passwort geschützt war, das
          in einem Safe versteckt war.
        + Im Souvenir-Geschäfts mussten wir die hauseigene Produkt-Datenbank von
          unsinnigen Produkt-IDs bereinigen, die durch einen Angestellten eingegeben
          wurden.
        + In der Lobby angekommen, fanden wir defekte Fahrstühle und Rolltreppen.
          Letztere konnten wir durch das Anschalten von Batterien, die die maximal
          mögliche Spannung erzeugten, wieder funktionsfähig machen.
      ],
      align(horizon, image("images/escalator.png"))
    )
  ]


  #align(horizon)[
    #grid(
      columns: (1fr, 1fr),
      align: (horizon, horizon),
      gutter: 1em,
      align(horizon, image("images/hole.png")),
      [
        #set enum(start: 4)
        + Wir haben Papierrollen aus den Weg geräumt, damit wir eine Wand durchbrechen
          konnten.
        + Die Weihnachtshelfenden des Nordpols ist einer Lebensmittelvergiftung
          entkommen, da wir frische Zutaten identifizieren konnten.
        + Durch einen Müllschlucker aus der Cafetaria in eine Müllpresse traffen wir
          auf eine junge Kopffüßerin und halfen ihr bei ihren Mathematik-Hausaufgaben.
      ]
    )
  ]

  #grid(
    columns: (1fr, 1fr),
    gutter: 1em,
    align(horizon, image("images/hole.png")),
    [
      #set enum(start: 7)
      + Unter zur Hilfenahme der Viele-Welten-Interpretation mussten wir die vielen
        Zeitachsen berechnen, durch die ein Tachyon reisen kann, um die
        Quanten-Tachyonen-Mannigfaltigkeit zu reparieren.
      + Wir haben so wenig Lichterketten wie möglich zwischen Abzweigdosen aufgehangen.
      + Wir haben das größte Rechteck bestehend aus roten und grünen Fliesen eines
        Kinosaals berechnet, sodass dieses neu dekoriert werden konnte.
    ]
  )

  #grid(
    columns: (1fr, 1fr),
    gutter: 1em,
    align(horizon, image("images/hole.png")),
    [
      #set enum(start: 7)
      + Es wurden viele Knöpfkombinationen gedrückt, um die Indikatorlichter
        korrekt einzustellen und um die Joltage-Anforderungen zu erfüllen.
      + Durch unsere Kabelsalat-Analyse konnten wir helfen, einen Ringkernreaktor
        wieder in Gang zu kriegen.
    ]
  )
]


#focus-slide[
  #set text(1.5em)
  Bühne frei für euch!
]

