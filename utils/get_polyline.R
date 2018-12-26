gsub("\\(", " ", routes_dt$line[1])
gsub("\\)", " ", routes_dt$line[1])
gsub("\\[", " ", routes_dt$line[1])
gsub("\\]", " ", routes_dt$line[1])
gsub("\\,", " ", routes_dt$line[1])

matrix(scan(text = x),ncol = 2)