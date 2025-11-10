function aoc {
  day=$(printf "%02d" $1)
  input="Day$day/input.txt"
  if [ -e $input ]; then
    cat $input | cabal exec runghc "Day$day/$2.hs"
  else
    cabal exec runghc "Day$day/$2.hs"
  fi
}

