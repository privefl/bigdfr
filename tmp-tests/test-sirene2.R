csv2sqlite <- function(file,
                       every_nlines,
                       table_name,
                       backingfile = sub("\\.csv$", ".sqlite", file),
                       ...) {

  begin <- proc.time()[3]

  # Prepare reading
  con <- RSQLite::dbConnect(RSQLite::SQLite(), backingfile)
  init <- TRUE
  fill_sqlite <- function(df) {

    if (init) {
      RSQLite::dbCreateTable(con, table_name, df)
      init <<- FALSE
    }

    res <- RSQLite::dbAppendTable(con, table_name, df)
    reset <- proc.time()[3]
    print(round(reset - begin, 1))
    begin <<- reset

    res
  }

  # Read and fill by parts
  bigreadr::big_fread1(file, every_nlines = every_nlines,
                       .transform = fill_sqlite,
                       .combine = unlist,
                       ... = ...)

  # Returns
  con
}

csv <- "../data/sirc-17804_9075_61173_201807_L_M_20180801_015133163.csv"
unlink(sub("\\.csv$", ".sqlite", csv))
# nlines(csv)  ## 11081571
library(RSQLite)
con <- csv2sqlite(csv, every_nlines = 1e6, table_name = "sirene",
                  encoding = "Latin-1")

df <- dplyr::tbl(con, "sirene")
dplyr::pull(df, 1)  ## 11081570
