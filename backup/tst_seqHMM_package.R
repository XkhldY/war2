library("seqHMM")
data(biofam3c)

# Building sequence objects (starting at age 15)
marr.seq <- seqdef(biofam3c$married, start = 15)
child.seq <- seqdef(biofam3c$children, start = 15)
left.seq <- seqdef(biofam3c$left, start = 15)

# Choosing colours for states
attr(marr.seq, "cpal") <- c("#AB82FF", "#E6AB02", "#E7298A")
attr(child.seq, "cpal") <- c("#66C2A5", "#FC8D62")
attr(left.seq, "cpal") <- c("#A6CEE3", "#E31A1C")

ssplot(list(marr.seq, child.seq, left.seq), title = "State distribution plots")
