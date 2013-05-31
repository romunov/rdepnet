# Naloga 2 pri predmetu Analiza omrežij. Pri tej nalogi je treba narisat 
# dependency graf.
# 
# Author: Roman Luštrik, 28.5.2013
###############################################################################

# Na spletu sem našel dva prispevka, ki rišeta graf odvisnosti. Eden je za "vse"
# pakete na CRAN sistemu, drugi pa samo za pakete, ki se neposredno dotikajo
# prostorske analize.
# http://librestats.com/2012/05/17/visualizing-the-cran-graphing-package-dependencies/
# (barry rowlingson) http://www.maths.lancs.ac.uk/~rowlings/Teaching/UseR2012/introduction2Talk.html

library(igraph)
library(reshape2)
library(stringr)
library(RColorBrewer)

setwd("q:/workspace/faks/analiza_omrezij/")

# pogledamo spisek vseh možnih R paketov na CRAN-u
dep.data <- available.packages()

# uporabimo samo spodnje stolpce
dep.data <- dep.data[, c("Package", "Depends", "Imports", "Suggests", "Enhances")]

# za vsak paket pogledamo na katere pakete se sklicuje
make.data <- apply(dep.data, 1, FUN = function(x) {

      if (all(is.na(x[-1]))) return(data.frame(from = x[1], to = x[1], relation = "none"))
      
      x2 <- na.omit(x[-1])
      x2 <- sapply(x2, strsplit, ",")
      
      out <- data.frame(from = rep(x[1], length(unlist(x2))), to = unlist(x2), relation = rep(names(x2), times = sapply(x2, length)))
      rownames(out) <- 1:nrow(out)
      out
   })
done.data <- do.call("rbind", make.data)
rownames(done.data) <- 1:nrow(done.data)

# odstrani R (>= ...) ali \n
clean.data <- done.data
clean.data$to <- sub("\\((.*?)\\)", "", x = clean.data$to, perl = TRUE)
clean.data$to <- sub("\\\n", "", x = clean.data$to, perl = TRUE)
clean.data <- clean.data[!grepl("R|R ", clean.data$to), ]
clean.data$to <- as.factor(str_trim(clean.data$to, "both"))

## za Imports zamenjaj from in to, da bodo puščie kazale pravilno
#imp.rev <- which(clean.data$relation == "Imports")
#clean.data$from <- as.character(clean.data$from)
#clean.data$to <- as.character(clean.data$to)
#
#clean.data[imp.rev, c("from", "to")] <- clean.data[clean.data$relation == "Imports", c("to", "from")]
#clean.data[imp.rev, ]

graph.data <- graph.data.frame(clean.data, directed = TRUE)
# polepšaj imena, da ne bo verzij paketov
V(graph.data)$name <- sub("\\((.*?)\\)", "", x = V(graph.data)$name, perl = TRUE)
V(graph.data)$name <- str_trim(V(graph.data)$name, side = "both")
# dodaj barve
cls <- clean.data$relation
pal <- brewer.pal(5, "Set1")[c(1, 2, 5, 3, 4)]
pal[5] <- "black"
cls <- pal[cls]
rm(pal)
E(graph.data)$color <- cls

###################################################################
# Kreiranje omrežja vseh relacij (imports, suggests, enhances...) #
###################################################################

# ker imena nodeov ne izvozi, probam naredit file na roke
# shrani file v en začasen file
write.graph(graph = graph.data, file = "r_packages_temp.net", format = "pajek")
hand.graph <- readLines("r_packages_temp.net")

fn <- "r_packages.net"
# napiši prvo vrstico in število nodeov
cat(paste("*Vertices", vcount(graph.data), "\n", file = fn))
# zapiši imena nodeov z njihovo zaporedno številko
write.table(V(graph.data)$name, col.names = FALSE, row.names = TRUE, file = fn, append = TRUE, quote = FALSE)
# zapiši še povezave, ki jih write.graph zapiše OK
write(hand.graph[-1], file = fn, append = TRUE)

##################################
# Graf, ki kaže odnose "Depends" #
##################################
depends.graph.data <- clean.data[clean.data$relation == "Depends", ]
depends.graph <- graph.data.frame(depends.graph.data, directed = TRUE)
plot(depends.graph, vertex.label.color="blue",
   edge.width=.5, edge.color="#ACC49D", edge.arrow.size=.5,
   vertex.label.cex=.7, vertex.label=row.names(depends.graph.data), vertex.size=0, vertex.color="white",
#   layout=layout.reingold.tilford
#   layout=layout.kamada.kawai
#   layout = layout.svd
layout = graphopt
)

plotGraph <- function(x, relation) {
      gd <- x[x$relation == relation, ]
      g <- graph.data.frame(gd, directed = TRUE)
      plot(g, vertex.label.color = "blue",
         edge.width = 0.5, edge.color = "#ACC49D", edge.arrow.size = 0.5,
         vertex.label.cex = 0.7, vertex.size=0, vertex.color="white",
         layout=layout.kamada.kawai)
}
###################################
# Graf, ki kaže odnose "Suggests" #
###################################
#suggests.graph.data <- clean.data[clean.data$relation == "Suggests", ]
#suggests.graph <- graph.data.frame(suggests.graph.data, directed = TRUE)
plotGraph(x = clean.data, relation = "Suggests")
##################################
# Graf, ki kaže odnose "Imports" #
##################################
imports.graph.data <- clean.data[clean.data$relation == "Imports", ]
imports.graph <- graph.data.frame(imports.graph.data, directed = TRUE)

###################################
# Graf, ki kaže odnose "Enhances" #
###################################
enhances.graph.data <- clean.data[clean.data$relation == "Enhances", ]
enhances.graph <- graph.data.frame(enhances.graph.data, directed = TRUE)
