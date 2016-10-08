require(pacman)
pacman::p_load(igraph,network,plyr,utils, rgl)

# load our csv; for an online version of the data see https://docs.google.com/spreadsheets/d/1R2CqmUzsBFD7PwC4Mk3GROozNjGDa8gEo0ySUoL-9To/edit#gid=1935061396
visdata <- read.csv("vis.csv")

# vip holds an array with the persons of interest for whom we want to create a co-authorship network; the names have to be indicated exactly as in the "Deduped author names" column in the csv file; 

vip <- "[:print:]*?(Lucas, P|Roth, S.F|Kolojejchick, J|Senn, J.A|Gomberg, C.C|Burks, M.B|Stroffolino, P.J|Dunmire, C)[:print:]*?"

# example for a larger network
#vip <- "[:print:]*?(Munzner, T|Heer, J|Stasko, J)[:print:]*?"


# filter based on the vip expression
vipdata <- filter(visdata, grepl(vip, visdata$Deduped.author.names))
authordata <- data.frame(lapply(vipdata, as.character), stringsAsFactors = F)

# we need a two-column dataframe of relations, so we create all combinations between authors of one publication
coauthors <- ldply(authordata[,11], .fun = function(x){
  avector <- unlist(strsplit(x, ';', fixed = T)) 
  if(length(avector)>1) {
    return(as.data.frame(t(combn(avector, 2))))
  } else {
    return(data.frame(V1 = character(), V2 = character()))
  }
})
  
# create the graph
g <- graph.data.frame(coauthors, directed=F)


# draw the graph

rgl.open()
rgl.bg(color="white", alpha=c(.3), back="fill", sphere = F, fogtype = "none", line_antialias = TRUE)
rgl.viewpoint(0, 0, fov=100, zoom=.5)

# there are a bunch of different options; some are better to see interesting things in the data, others are better for 3d printing

# force-directed (Kamada-Kawai layout algorithm)
#coords <- layout_with_kk(g, dim=3)

# layout on sphere; better for 3D printing
coords <- layout_on_sphere(g)

# plot
rglplot(g, edge.arrow.size=0, edge.arrow.width=0, edge.width=5, layout = coords, vertex.label = NA)

# export as stl
writeSTL("network.stl")

# export for other programs such as Blender
writeOBJ("network.obj", withNormals=T, separateObjects=T)
