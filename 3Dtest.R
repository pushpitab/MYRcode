
install.packages("plotly")

library(plotly)
packageVersion('plotly')


mission <- c("Taxi","Medical","Commuter",	"Cargo",	"Recreation")
power <- c("FossilFuel",	"Electric",	"Hybrid")
psg <- c(1,	2	,3,	4, 6,	8)
mat <- expand.grid(mission,power,psg)

colnames(mat) <- c("mission","power","psg")
mat



p <- plot_ly(x = mat$mission, y = mat$power, z = mat$psg) %>% add_surface()
p
chart_link = api_create(p, filename="surface-2")
chart_link
