aoc() {
  if [[ $# -lt 2 ]]; then
    echo "Usage: aoc <day> <part> [--no-input | --input file]"
    return 1
  fi

  local day part input src prog noinp=0
  day=$(printf "%02d" "$1")
  part=$(echo "$2" | tr "[:lower:]" "[:upper:]")
  shift 2

  input="Day$day/input.txt"
  src="Day$day/$part.hs"
  prog="Day$day/$part"

  while [[ $# -gt 0 ]]; do
    case $1 in
      --no-input)
        noinp=1
        ;;
      --input)
        shift
        if [[ -z "$1" ]]; then
          echo "Error: --input requires a file argument"
          return 1
        fi
        noinp=0
        input="$1"
        ;;
      -*|--*)
        echo "Error: Unknown option $1" >%2
        return 1
      ;;
    esac
    shift
  done

  if [[ ! -f "$src" ]]; then
    echo "Error: Source file \"$src\" not found" >&2
    return 1
  fi

  # install dependencies before running any program
  # cabal exec runghc does not install them automatically
  cabal build --only-dependencies

  if cabal exec ghc -- -O2 -main-is "Day$day.$part.main" -o "$prog" "$src"; then
    if [[ -f "$input" && $noinp -eq 0 ]]; then
      "$prog" < "$input"
    else
      "$prog"
    fi
  else
    return 1
  fi
}

