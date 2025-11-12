function aoc {
  # install dependencies before running any program
  # cabal exec runghc does not install them automatically
  cabal build --only-dependencies

  day=$(printf "%02d" $1)
  input="Day$day/input.txt"
  if [ -f $input ]; then
    cat $input | cabal exec runghc "Day$day/$2.hs"
  else
    cabal exec runghc "Day$day/$2.hs"
  fi
}

