library(lubridate)
class(today())
typeof(today())
str(today())
attributes(today())

structure(-1, class = "Date")
?today
class(now())
typeof(now())
attributes(now())
tmp <- structure(1, class = c("POSIXct", "POSIXt"), tzone = "")
attributes(as.POSIXlt(tmp))
