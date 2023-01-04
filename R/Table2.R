

t1 <- function_regression(pandemic_start=as.Date(datlim1),  pandemic_end=as.Date(datlim2), Pandemic_Name="1918 first wave")
t2 <- function_regression(pandemic_start=as.Date(datlim3),  pandemic_end=as.Date(datlim6), Pandemic_Name="1918-1919 second wave")
t3 <- function_regression(pandemic_start=as.Date(datlim7),  pandemic_end=as.Date(datlim8),Pandemic_Name="1920")

Table2 <- rbind(t1, t2, t3)

write.xlsx(Table2,file=paste0("output/Table2.xlsx"),row.names=FALSE, overwrite = TRUE)