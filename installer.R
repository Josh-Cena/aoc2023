install.packages("gsubfn", repos = "https://cran.r-project.org")

prefix <- system("brew --prefix gmp", intern = TRUE)
install.packages(
  "gmp",
  type = "source",
  repos = "https://cran.r-project.org"
)
