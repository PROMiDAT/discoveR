datos <- read.table("diccionario.csv", header = T, sep = ";", encoding = "utf8")
translation <- dlply(datos ,.(key), function(s) key = as.list(s))
save(translation, file = "translation.bin")
