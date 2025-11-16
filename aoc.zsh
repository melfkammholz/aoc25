aoc() {
  local day=$(printf "%02d" $1)
  local part="${2:u}"

  local input="Day$day/input.txt"
  local src="Day$day/$part.hs"
  local prog="Day$day/$part"

  # install dependencies before running any program
  # cabal exec runghc does not install them automatically
  cabal build --only-dependencies

  if cabal exec ghc -- -O2 -main-is "Day$day.$part.main" -o "$prog" "$src"; then
    if [ -f "$input" ]; then
      "$prog" < "$input"
    else
      "$prog"
    fi
  else
    return 1
  fi
}

