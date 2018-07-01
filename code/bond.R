bond <- read_excel(path = "data/bond/Chinabond.xlsx",
                   sheet = "yield", 
                   skip = 22, 
                   col_names = F, 
                   col_types = c("date", rep("numeric", 15))) %>% as.data.frame
names(bond) <- c("Date","TRCHZ12", "TRCHZ15", "TRCHZ20", "TRCHZ25", "TRCHZ2Y", "TRCHZ30", "TRCHZ3Y", "TRCHZ4Y", "TRCHZ5Y", "TRCHZ6Y", "TRCHZ7Y", "TRCHZ8Y", "TRCHZ9Y", "TRCHZ10", "TRCHZ1Y")
bond$Date <- substr(bond$Date, 1, 10)
