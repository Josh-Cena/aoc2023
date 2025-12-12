# Advent of Code 2023

Language: ![R](https://img.shields.io/badge/r-%23276DC3.svg?style=for-the-badge&logo=r&logoColor=white) (4.3.2)
Package Manager: R itself, just run `installer.R`!

This repo uses my standard AoC setup. Inputs are stored as `inputs/day{n}/{name}.txt`. By default `name` is `real` (the real question). To run a specific day's solution, use the following command:

```bash
Rscript src/main.r {day} {part} {name}
```

For example, to run the solution for day 1, part 2 with the example input:

```bash
Rscript src/main.r 1 2 ex
```

(And make sure that `inputs/day1/ex.txt` exists.)

Note: to successfully install `gmp`, I had to do the following:

```bash
cat <<EOF > ~/.R/Makevars
CPPFLAGS += -I/opt/homebrew/opt/gmp/include
LDFLAGS  += -L/opt/homebrew/opt/gmp/lib
EOF
```
