rm(list = ls(all = TRUE))
library(igraph)
library(data.table)
library(ggplot2)
library(ggthemes)

setwd("~/Desktop/EnronCorporation_Email_NetworkAnalysis/data")

load("enron-mysqldump.RData")
#data (4):
#"employeelist"(149 * 9), "message"(252759 * 7), "recipientinfo"(2064442 * 4), "referenceinfo"(54778 * 3)

#########first look at data meta-info & some cleaning accordingly###########
#"employeelist":
sapply(employeelist, class)         #column data type

#"message":
sapply(message, class)

min(message$date)           #there is un-cleaned data in "date" (on both ends)!
max(message$date)

#clean the unreasonable dates discovered:
message <- message[!message$date %like% "0001-", ]
message <- message[!message$date %like% "0002-", ]
message <- message[!(year(message$date)>2007), ]
#View(message)
min(message$date)         #"1979-12-31" ("Enron" was formally founded in 1985 as a company, but as early as 1979 it registered its website and outlook as the main subsidiary of a holding company "InterNorth")
max(message$date)         #"2007-02-11"
length(unique(message$date))       #1215 unique dates

length(unique(message$sender))           #17522 individual senders(sender emails) in messaging relation
length(unique(message$subject))          #140621 unique email subject
length(unique(message$folder))           #3887 unique "exact folder of email stored"
#length(unique(message$body))            #217847 unique body contents

#"recipientinfo":
sapply(recipientinfo, class)
table(recipientinfo$rtype)           #count of emails within the 3 classes of email sending 'types': 'BCC', 'CC', 'TO'
length(unique(recipientinfo$rvalue))            #68084 individual receivers(receiver emails) in messaging relation

#"referenceinfo":
sapply(referenceinfo, class)
#length(unique(referenceinfo$reference))       #45303 unique "whole email with shortend headers"


#########data wrangling########
employeelist <- data.table(employeelist)
message <- data.table(message)
recipientinfo <- data.table(recipientinfo)
referenceinfo <- data.table(referenceinfo)

tmp <- merge(employeelist[,c('Email_id', 'status')], 
             message[,c('mid', 'sender', 'date', 'subject', 'body', 'folder')], 
             by.x='Email_id', by.y='sender')        #(104766 rows)

tmp <- merge(tmp, recipientinfo[, -"rid"], by='mid')        #(382850 rows -- all email messages)
#View(tmp)

edges <- tmp[,c("Email_id", "rvalue", "rtype", "date", "subject", "folder")]
colnames(edges)[1:4] <- c('sender', 'receiver', 'type', 'date')
View(edges)

sapply(edges, class)
#edges$sender <- as.factor(edges$sender)
#edges$receiver <- as.factor(edges$receiver)
#edges$type <- as.factor(edges$type)
#edges$date <- as.Date(edges$date)
edges$subject <- as.factor(edges$subject)

edges$subject            #61923 unique subject of emails in existing merged data


#"count" variable created below: in each unique date for each type ("TO" etc.), the number of emails between each unique pair of sender-receiver:
edges[ , count := .N, by=.(sender,receiver,type,date)]
View(edges)                                   #Total: 382850 emails in this network
max(edges$count)       #biggest number of emails between a sender and a receiver(e.g.: *"no.address@enron.com") for a specific type(e.g.: "TO") within a day: 333 [--> e.g.: "stacey.w.white@enron.com"'s emails sent on "2001-06-26" under folder "max(edges$count)"]

#remove any leading or trailing whitespaces in "sender" & "receiver" columns of the "edges" data! -> would affect the edge list treatment later:
edges$sender <- trimws(edges$sender)
edges$receiver <- trimws(edges$receiver)
#sort the "edges" data based on "date" order!:
edges <- edges[order(edges$date),]
View(edges)
min(edges$date)       #existing edge earliest date: "1979-12-31"
max(edges$date)       #existing edge latest date: "2002-09-22"

nodes <- unique(rbind(data.table(edges$sender), data.table(edges$receiver)))
colnames(nodes) <- c('Email_id')
View(nodes)            #Total: 16169 unique employee email address(person!) appearing in messages so far

nodes <- merge(nodes, employeelist[, .(Email_id, firstName, lastName, status)], by='Email_id', all.x = T)
nodes$status[is.na(nodes$status)] <- 'N/A'       #use "N/A" in "status" variable specifically to denote 'unknown' status(role)

View(nodes)         #NOTICE: THE VAST MAJORITY OF EMPLOYEES in this "email messaging" network do not have further "employee" information, as we only have 149 employees' data in the "employeelist" dataset

############visualization and descriptive statistics of some network properties############
#sapply(edges, class)
# make email messaging network graph:
network <- graph_from_data_frame(edges, directed = T, vertices = nodes)
network         #(382850 edges)
#which_multiple(network)


#IMPORTANT ISSUE THAT MAY BE NOTICED: THERE ARE ACTUALLY QUITE A LOT "SELF-LOOPS"('emailing to one's self')! --> DELETE THEM
network <- simplify(network, remove.loops=TRUE, remove.multiple=FALSE)
network        #INTERESTING: number of edges SHARPLY reduced from "382850" to "33947"!
vertex_attr_names(network)       #see what attributes we have for nodes ["name"(i.e.: "vertex name"--email address), "firstName", "lastName", "status"]
edge_attr_names(network)           #all attributes for edges ["type", "date", "subject", "folder", "count"]

years <- unique(year(edges$date))       #1979, 1998, 1999, 2000, 2001, 2002
edges[, year_month := format(date, "%Y-%m")]
View(edges)

length(unique(edges$year_month))        #47 unique continuous months
year_months <- unique(edges$year_month)
statistics <- data.frame()
#try to store the network for each year_month:
nets_simple <- list()
for (i in year_months) {
  edge_current_ym <- edges[edges$year_month==i, ]
  nodes_current_ym <- nodes[(nodes$Email_id %in% edge_current_ym$sender | nodes$Email_id %in% edge_current_ym$receiver), ]
  graph_current_ym <- graph_from_data_frame(edge_current_ym, directed=T, vertices=nodes_current_ym)
  graph_current_ym <- simplify(graph_current_ym, remove.loops=TRUE, remove.multiple=FALSE)        #***IMPORTANT!: "SELF-LOOP" ISSUE!!!
  nets_simple[[as.character(i)]] <- graph_current_ym         #store the network graphs of all year_months(element named by each year_month)
  
  deg_in_current_ym <- degree(graph_current_ym, mode="in")
  deg_out_current_ym <- degree(graph_current_ym, mode="out")
  closeness_current_ym <- closeness(graph_current_ym, mode="total")
  betweenness_current_ym <- betweenness(graph_current_ym)
  eigen_cent_current_ym <- eigen_centrality(graph_current_ym)$vector      #[*! NOTICE HERE: "eigen_centrality()" function could not use "directed=TRUE"(otherwise error)!]
  page_rank_current_ym <- page_rank(graph_current_ym)$vector
  statistics_current_ym <- data.frame(deg_in=deg_in_current_ym, deg_out=deg_out_current_ym, closeness=closeness_current_ym, betweenness=betweenness_current_ym, eigen_cent=eigen_cent_current_ym, page_rank=page_rank_current_ym, year_month=i)
  statistics_current_ym <- cbind(Email_id=rownames(statistics_current_ym), statistics_current_ym)
  rownames(statistics_current_ym) <- NULL
  #View(statistics_current_ym)
  statistics <- rbind(statistics, statistics_current_ym)
}
View(statistics)       #52092 rows during 47 months of records
#nets_simple          #we have the "nets_simple" list with storage of all network graphs

####Visualize/Plot the Networks#####

#save the 47 plots of network graphs (at 47 points of year_months) automatically to the Desktop:
nets_simple_1 <- nets_simple
for (i in year_months) {
  V(nets_simple_1[[i]])$color <- c("orange", "red")[factor(V(nets_simple_1[[i]])$name %in% employeelist$Email_id)]      #plot vertex distinguished by colors based on "whether this employee email is in the (sensitive) 'employeelist' or not"
  V(nets_simple_1[[i]])$label <- NA              # do not plot vertex labels
  V(nets_simple_1[[i]])$size <- 6
  E(nets_simple_1[[i]])$width <- 0.5
  E(nets_simple_1[[i]])$arrow.size <- 0.4
  E(nets_simple_1[[i]])$arrow.width <- 0.4
  mypath <- file.path("/Users/baixiao/Desktop/Entire\ Network", paste(i, ".jpeg", sep=""))
  jpeg(file=mypath)
  mytitle = paste("Entire Network: ", i)
  plot(nets_simple_1[[i]], layout=layout_nicely, main=mytitle)
  dev.off()
}


statistics <- data.table(statistics)
nodes_statistics <- merge(nodes, statistics, by="Email_id")
View(nodes_statistics)

#check the statistics data we have now with corresponding "employees" in the "employeelist" data:
employee_in_network <- unique(nodes_statistics[!is.na(nodes_statistics$firstName), c("firstName", "lastName")])
employee_in_employeelist <- unique(employeelist[, c("firstName", "lastName")])
fintersect(employee_in_network, employee_in_employeelist)        #147 (out of 149 in list) with network data
fsetdiff(employee_in_employeelist, employee_in_network)         #(2 people missing)

nodes_statistics <- nodes_statistics[order(nodes_statistics$year_month),]
View(nodes_statistics)

nodes_statistics_inlist <- nodes_statistics[!is.na(firstName),]
View(nodes_statistics_inlist)
write.csv(nodes_statistics_inlist, file="/Users/baixiao/Desktop/nodes_stat_inlist.csv", row.names=FALSE)


#Visualization: aggregate statistics by year_month:
nodes_statistics_inlist[, degree_in_by_yearmonth := mean(deg_in), by=year_month]
data_for_yearmonth <- unique(nodes_statistics_inlist[, c("year_month", "degree_in_by_yearmonth")])
#data_for_yearmonth$year_month <- as.Date(data_for_yearmonth$year_month, "%Y-%m")
View(data_for_yearmonth)
ggplot(data_for_yearmonth, aes(x=year_month, y=degree_in_by_yearmonth)) + geom_bar(stat="identity", fill="darkorchid1") +
  geom_text(aes(label=round(degree_in_by_yearmonth, digits=0)), vjust=0) +
  scale_x_discrete(name="month", breaks=c("1998-10", "1999-02", "1999-06", "1999-10", "2000-02", "2000-06", "2000-10", "2001-02", "2001-06", "2001-10", "2002-02", "2002-06", "2002-09"),
                   labels=c("10/98", "02/99", "06/99", "10/99", "02/00", "06/00", "10/00", "02/01", "06/01", "10/01", "02/02", "06/02", "09/02")) +
  scale_y_continuous(name="average in-degree", limits=c(NA, 70)) +
  ggtitle("Email Network: Average In-degree of Employees In the List") +
  theme_economist() +
  theme(axis.text.x=element_text(size=9, angle=30, vjust=1), axis.title.x=element_text(face="bold"), axis.title.y=element_text(face="bold"))

nodes_statistics_inlist[, degree_out_by_yearmonth := mean(deg_out), by=year_month]
data_for_yearmonth <- unique(nodes_statistics_inlist[, c("year_month", "degree_out_by_yearmonth")])
#data_for_yearmonth$year_month <- as.Date(data_for_yearmonth$year_month, "%Y-%m")
View(data_for_yearmonth)
ggplot(data_for_yearmonth, aes(x=year_month, y=degree_out_by_yearmonth)) + geom_bar(stat="identity", fill="orchid2") +
  geom_text(aes(label=round(degree_out_by_yearmonth, digits=0)), vjust=0) +
  scale_x_discrete(name="month", breaks=c("1998-10", "1999-02", "1999-06", "1999-10", "2000-02", "2000-06", "2000-10", "2001-02", "2001-06", "2001-10", "2002-02", "2002-06", "2002-09"),
                   labels=c("10/98", "02/99", "06/99", "10/99", "02/00", "06/00", "10/00", "02/01", "06/01", "10/01", "02/02", "06/02", "09/02")) +
  scale_y_continuous(name="average out-degree", limits=c(NA, 300)) +
  ggtitle("Email Network: Average Out-degree of Employees In the List") +
  theme_economist() +
  theme(axis.text.x=element_text(size=9, angle=30, vjust=1), axis.title.x=element_text(face="bold"), axis.title.y=element_text(face="bold"))

nodes_statistics_inlist[, closeness_by_yearmonth := mean(closeness), by=year_month]
data_for_yearmonth <- unique(nodes_statistics_inlist[, c("year_month", "closeness_by_yearmonth")])
#data_for_yearmonth$year_month <- as.Date(data_for_yearmonth$year_month, "%Y-%m")
View(data_for_yearmonth)
ggplot(data_for_yearmonth, aes(x=year_month, y=closeness_by_yearmonth)) + geom_bar(stat="identity", fill="seagreen2") +
  geom_text(aes(label=round(closeness_by_yearmonth, digits=2)), vjust=0) +
  scale_x_discrete(name="month", breaks=c("1998-10", "1999-02", "1999-06", "1999-10", "2000-02", "2000-06", "2000-10", "2001-02", "2001-06", "2001-10", "2002-02", "2002-06", "2002-09"),
                   labels=c("10/98", "02/99", "06/99", "10/99", "02/00", "06/00", "10/00", "02/01", "06/01", "10/01", "02/02", "06/02", "09/02")) +
  scale_y_continuous(name="average closeness", limits=c(NA, 0.6)) +
  ggtitle("Email Network: Average Closeness of Employees In the List") +
  theme_economist() +
  theme(axis.text.x=element_text(size=9, angle=30, vjust=1), axis.title.x=element_text(face="bold"), axis.title.y=element_text(face="bold"))

nodes_statistics_inlist[, betweenness_by_yearmonth := mean(betweenness), by=year_month]
data_for_yearmonth <- unique(nodes_statistics_inlist[, c("year_month", "betweenness_by_yearmonth")])
#data_for_yearmonth$year_month <- as.Date(data_for_yearmonth$year_month, "%Y-%m")
View(data_for_yearmonth)
ggplot(data_for_yearmonth, aes(x=year_month, y=betweenness_by_yearmonth)) + geom_bar(stat="identity", fill="coral1") +
  geom_text(aes(label=round(betweenness_by_yearmonth, digits=2)), size=3, vjust=0) +
  scale_x_discrete(name="month", breaks=c("1998-10", "1999-02", "1999-06", "1999-10", "2000-02", "2000-06", "2000-10", "2001-02", "2001-06", "2001-10", "2002-02", "2002-06", "2002-09"),
                   labels=c("10/98", "02/99", "06/99", "10/99", "02/00", "06/00", "10/00", "02/01", "06/01", "10/01", "02/02", "06/02", "09/02")) +
  scale_y_continuous(name="average betweenness", limits=c(NA, 10000)) +
  ggtitle("Email Network: Average Betweenness of Employees In the List") +
  theme_economist() +
  theme(axis.text.x=element_text(size=9, angle=30, vjust=1), axis.title.x=element_text(face="bold"), axis.title.y=element_text(face="bold"))

nodes_statistics_inlist[, eigen_cent_by_yearmonth := mean(eigen_cent), by=year_month]
data_for_yearmonth <- unique(nodes_statistics_inlist[, c("year_month", "eigen_cent_by_yearmonth")])
#data_for_yearmonth$year_month <- as.Date(data_for_yearmonth$year_month, "%Y-%m")
View(data_for_yearmonth)
ggplot(data_for_yearmonth, aes(x=year_month, y=eigen_cent_by_yearmonth)) + geom_bar(stat="identity", fill="yellow2") +
  geom_text(aes(label=round(eigen_cent_by_yearmonth, digits=3)), size=3, vjust=0) +
  scale_x_discrete(name="month", breaks=c("1998-10", "1999-02", "1999-06", "1999-10", "2000-02", "2000-06", "2000-10", "2001-02", "2001-06", "2001-10", "2002-02", "2002-06", "2002-09"),
                   labels=c("10/98", "02/99", "06/99", "10/99", "02/00", "06/00", "10/00", "02/01", "06/01", "10/01", "02/02", "06/02", "09/02")) +
  scale_y_continuous(name="average eigenvector centrality", limits=c(NA, NA)) +
  ggtitle("Email Network: Average Eigenvector Centrality of Employees In the List") +
  theme_economist() +
  theme(axis.text.x=element_text(size=9, angle=30, vjust=1), axis.title.x=element_text(face="bold"), axis.title.y=element_text(face="bold"))

nodes_statistics_inlist[, page_rank_by_yearmonth := mean(page_rank), by=year_month]
data_for_yearmonth <- unique(nodes_statistics_inlist[, c("year_month", "page_rank_by_yearmonth")])
#data_for_yearmonth$year_month <- as.Date(data_for_yearmonth$year_month, "%Y-%m")
View(data_for_yearmonth)
ggplot(data_for_yearmonth, aes(x=year_month, y=page_rank_by_yearmonth)) + geom_bar(stat="identity", fill="royalblue2") +
  geom_text(aes(label=round(page_rank_by_yearmonth, digits=3)), size=3, vjust=0) +
  scale_x_discrete(name="month", breaks=c("1998-10", "1999-02", "1999-06", "1999-10", "2000-02", "2000-06", "2000-10", "2001-02", "2001-06", "2001-10", "2002-02", "2002-06", "2002-09"),
                   labels=c("10/98", "02/99", "06/99", "10/99", "02/00", "06/00", "10/00", "02/01", "06/01", "10/01", "02/02", "06/02", "09/02")) +
  scale_y_continuous(name="average page rank", limits=c(NA, 0.3)) +
  ggtitle("Email Network: Average Page Rank of Employees In the List") +
  theme_economist() +
  theme(axis.text.x=element_text(size=9, angle=30, vjust=1), axis.title.x=element_text(face="bold"), axis.title.y=element_text(face="bold"))

###statistics by "status"(role) at SPECIFIC TIME POINT for Comparison ("normal" and "unusual" times)###

##at "1998-12":
data_98_12 <- nodes_statistics_inlist[year_month=="1998-12", c("Email_id", "firstName", "lastName", "status", "deg_in", "deg_out", "closeness", "betweenness", "eigen_cent", "page_rank")]
data_98_12 <- data_98_12[order(data_98_12$status), ]
View(data_98_12)

ggplot(data_98_12, aes(x=reorder(Email_id, status, FUN=sort), y=deg_in, fill=status)) + geom_bar(stat="identity") +
  geom_text(aes(label=round(deg_in, digits=0)), vjust=0) +
  scale_x_discrete(name="Email") +
  scale_y_continuous(name="In-degree", limits=c(NA, NA)) +
  ggtitle("Email Network at Dec. 1998: Individual In-degree of People In the List by Status(Role)") +
  theme_economist() +
  theme(axis.text.x=element_text(size=7, angle=0, vjust=1), axis.title.x=element_text(face="bold"), axis.title.y=element_text(face="bold"), legend.title=element_blank())

ggplot(data_98_12, aes(x=reorder(Email_id, status, FUN=sort), y=deg_out, fill=status)) + geom_bar(stat="identity") +
  geom_text(aes(label=round(deg_out, digits=0)), vjust=0) +
  scale_x_discrete(name="Email") +
  scale_y_continuous(name="Out-degree", limits=c(NA, 120)) +
  ggtitle("Email Network at Dec. 1998: Individual Out-degree of People In the List by Status(Role)") +
  theme_economist() +
  theme(axis.text.x=element_text(size=7, angle=0, vjust=1), axis.title.x=element_text(face="bold"), axis.title.y=element_text(face="bold"), legend.title=element_blank())

ggplot(data_98_12, aes(x=reorder(Email_id, status, FUN=sort), y=closeness, fill=status)) + geom_bar(stat="identity") +
  geom_text(aes(label=round(closeness, digits=4)), vjust=0) +
  scale_x_discrete(name="Email") +
  scale_y_continuous(name="Closeness", limits=c(NA, NA)) +
  ggtitle("Email Network at Dec. 1998: Individual Closeness of People In the List by Status(Role)") +
  theme_economist() +
  theme(axis.text.x=element_text(size=7, angle=0, vjust=1), axis.title.x=element_text(face="bold"), axis.title.y=element_text(face="bold"), legend.title=element_blank())

ggplot(data_98_12, aes(x=reorder(Email_id, status, FUN=sort), y=betweenness, fill=status)) + geom_bar(stat="identity") +
  geom_text(aes(label=round(betweenness, digits=0)), vjust=0) +
  scale_x_discrete(name="Email") +
  scale_y_continuous(name="Betweenness", limits=c(NA, NA)) +
  ggtitle("Email Network at Dec. 1998: Individual Betweenness of People In the List by Status(Role)") +
  theme_economist() +
  theme(axis.text.x=element_text(size=7, angle=0, vjust=1), axis.title.x=element_text(face="bold"), axis.title.y=element_text(face="bold"), legend.title=element_blank())

ggplot(data_98_12, aes(x=reorder(Email_id, status, FUN=sort), y=eigen_cent, fill=status)) + geom_bar(stat="identity") +
  geom_text(aes(label=round(eigen_cent, digits=3)), vjust=0) +
  scale_x_discrete(name="Email") +
  scale_y_continuous(name="Eigenvector Centrality", limits=c(NA, NA)) +
  ggtitle("Email Network at Dec. 1998: Individual Eigenvector Centrality of People In the List by Status(Role)") +
  theme_economist() +
  theme(axis.text.x=element_text(size=7, angle=0, vjust=1), axis.title.x=element_text(face="bold"), axis.title.y=element_text(face="bold"), legend.title=element_blank())

ggplot(data_98_12, aes(x=reorder(Email_id, status, FUN=sort), y=page_rank, fill=status)) + geom_bar(stat="identity") +
  geom_text(aes(label=round(page_rank, digits=3)), vjust=0) +
  scale_x_discrete(name="Email") +
  scale_y_continuous(name="Page Rank", limits=c(NA, 0.04)) +
  ggtitle("Email Network at Dec. 1998: Individual Page Rank of People In the List by Status(Role)") +
  theme_economist() +
  theme(axis.text.x=element_text(size=7, angle=0, vjust=1), axis.title.x=element_text(face="bold"), axis.title.y=element_text(face="bold"), legend.title=element_blank())


##at "2000-07":
data_00_07 <- nodes_statistics_inlist[year_month=="2000-07", c("Email_id", "firstName", "lastName", "status", "deg_in", "deg_out", "closeness", "betweenness", "eigen_cent", "page_rank")]
data_00_07 <- data_00_07[order(data_00_07$status), ]
View(data_00_07)

ggplot(data_00_07, aes(x=reorder(Email_id, status, FUN=sort), y=deg_in, fill=status)) + geom_bar(stat="identity") +
  geom_text(aes(label=round(deg_in, digits=0)), vjust=0) +
  scale_x_discrete(name="Email_id") +
  scale_y_continuous(name="In-degree", limits=c(NA, 125)) +
  ggtitle("Email Network at July 2000: Individual In-degree of People In the List by Status(Role)") +
  theme_economist() +
  theme(axis.text.x=element_text(size=4, angle=30, vjust=0.5), axis.title.x=element_text(face="bold"), axis.title.y=element_text(face="bold"), legend.title=element_blank())

ggplot(data_00_07, aes(x=reorder(Email_id, status, FUN=sort), y=deg_out, fill=status)) + geom_bar(stat="identity") +
  geom_text(aes(label=round(deg_out, digits=0)), vjust=0) +
  scale_x_discrete(name="Email_id") +
  scale_y_continuous(name="Out-degree", limits=c(NA, 1000)) +
  ggtitle("Email Network at July 2000: Individual Out-degree of People In the List by Status(Role)") +
  theme_economist() +
  theme(axis.text.x=element_text(size=4, angle=30, vjust=0.5), axis.title.x=element_text(face="bold"), axis.title.y=element_text(face="bold"), legend.title=element_blank())

ggplot(data_00_07, aes(x=reorder(Email_id, status, FUN=sort), y=closeness, fill=status)) + geom_bar(stat="identity") +
  geom_text(aes(label=round(closeness, digits=5)), size=2, vjust=0) +
  scale_x_discrete(name="Email_id") +
  scale_y_continuous(name="Closeness", limits=c(NA, 1.5e-04)) +
  ggtitle("Email Network at July 2000: Individual Closeness of People In the List by Status(Role)") +
  theme_economist() +
  theme(axis.text.x=element_text(size=4, angle=30, vjust=0.5), axis.title.x=element_text(face="bold"), axis.title.y=element_text(face="bold"), legend.title=element_blank())

ggplot(data_00_07, aes(x=reorder(Email_id, status, FUN=sort), y=betweenness, fill=status)) + geom_bar(stat="identity") +
  geom_text(aes(label=round(betweenness, digits=0)), vjust=0) +
  scale_x_discrete(name="Email_id") +
  scale_y_continuous(name="Betweenness", limits=c(NA, NA)) +
  ggtitle("Email Network at July 2000: Individual Betweenness of People In the List by Status(Role)") +
  theme_economist() +
  theme(axis.text.x=element_text(size=4, angle=30, vjust=0.5), axis.title.x=element_text(face="bold"), axis.title.y=element_text(face="bold"), legend.title=element_blank())

ggplot(data_00_07, aes(x=reorder(Email_id, status, FUN=sort), y=eigen_cent, fill=status)) + geom_bar(stat="identity") +
  geom_text(aes(label=round(eigen_cent, digits=3)), size=2, vjust=0) +
  scale_x_discrete(name="Email_id") +
  scale_y_continuous(name="Eigenvector Centrality", limits=c(NA, NA)) +
  ggtitle("Email Network at July 2000: Individual Eigenvector Centrality of People In the List by Status(Role)") +
  theme_economist() +
  theme(axis.text.x=element_text(size=4, angle=30, vjust=0.5), axis.title.x=element_text(face="bold"), axis.title.y=element_text(face="bold"), legend.title=element_blank())

ggplot(data_00_07, aes(x=reorder(Email_id, status, FUN=sort), y=page_rank, fill=status)) + geom_bar(stat="identity") +
  geom_text(aes(label=round(page_rank, digits=4)), size=1.5, vjust=0) +
  scale_x_discrete(name="Email_id") +
  scale_y_continuous(name="Page Rank", limits=c(NA, NA)) +
  ggtitle("Email Network at July 2000: Individual Page Rank of People In the List by Status(Role)") +
  theme_economist() +
  theme(axis.text.x=element_text(size=4, angle=30, vjust=0.5), axis.title.x=element_text(face="bold"), axis.title.y=element_text(face="bold"), legend.title=element_blank())


##at "2001-09"(right before bad news):
data_01_09 <- nodes_statistics_inlist[year_month=="2001-09", c("Email_id", "firstName", "lastName", "status", "deg_in", "deg_out", "closeness", "betweenness", "eigen_cent", "page_rank")]
data_01_09 <- data_01_09[order(data_01_09$status), ]
View(data_01_09)

ggplot(data_01_09, aes(x=reorder(Email_id, status, FUN=sort), y=deg_in, fill=status)) + geom_bar(stat="identity") +
  geom_text(aes(label=round(deg_in, digits=0)), vjust=0) +
  scale_x_discrete(name="Email_id") +
  scale_y_continuous(name="In-degree", limits=c(NA, 200)) +
  ggtitle("Email Network at Sept. 2001: Individual In-degree of People In the List by Status(Role)") +
  theme_economist() +
  theme(axis.text.x=element_text(size=4, angle=30, vjust=0.5), axis.title.x=element_text(face="bold"), axis.title.y=element_text(face="bold"), legend.title=element_blank())

ggplot(data_01_09, aes(x=reorder(Email_id, status, FUN=sort), y=deg_out, fill=status)) + geom_bar(stat="identity") +
  geom_text(aes(label=round(deg_out, digits=0)), size=3, vjust=0) +
  scale_x_discrete(name="Email_id") +
  scale_y_continuous(name="Out-degree", limits=c(NA, NA)) +
  ggtitle("Email Network at Sept. 2001: Individual Out-degree of People In the List by Status(Role)") +
  theme_economist() +
  theme(axis.text.x=element_text(size=4, angle=30, vjust=0.5), axis.title.x=element_text(face="bold"), axis.title.y=element_text(face="bold"), legend.title=element_blank())

ggplot(data_01_09, aes(x=reorder(Email_id, status, FUN=sort), y=closeness, fill=status)) + geom_bar(stat="identity") +
  #geom_text(aes(label=round(closeness, digits=5)), size=2, vjust=0) +
  scale_x_discrete(name="Email_id") +
  scale_y_continuous(name="Closeness", limits=c(NA, NA)) +
  ggtitle("Email Network at Sept. 2001: Individual Closeness of People In the List by Status(Role)") +
  theme_economist() +
  theme(axis.text.x=element_text(size=4, angle=30, vjust=0.5), axis.title.x=element_text(face="bold"), axis.title.y=element_text(face="bold"), legend.title=element_blank())

ggplot(data_01_09, aes(x=reorder(Email_id, status, FUN=sort), y=betweenness, fill=status)) + geom_bar(stat="identity") +
  geom_text(aes(label=round(betweenness, digits=0)), size=2, vjust=0) +
  scale_x_discrete(name="Email_id") +
  scale_y_continuous(name="Betweenness", limits=c(NA, NA)) +
  ggtitle("Email Network at Sept. 2001: Individual Betweenness of People In the List by Status(Role)") +
  theme_economist() +
  theme(axis.text.x=element_text(size=4, angle=30, vjust=0.5), axis.title.x=element_text(face="bold"), axis.title.y=element_text(face="bold"), legend.title=element_blank())

ggplot(data_01_09, aes(x=reorder(Email_id, status, FUN=sort), y=eigen_cent, fill=status)) + geom_bar(stat="identity") +
  geom_text(aes(label=round(eigen_cent, digits=3)), size=2, vjust=0) +
  scale_x_discrete(name="Email_id") +
  scale_y_continuous(name="Eigenvector Centrality", limits=c(NA, NA)) +
  ggtitle("Email Network at Sept. 2001: Individual Eigenvector Centrality of People In the List by Status(Role)") +
  theme_economist() +
  theme(axis.text.x=element_text(size=4, angle=30, vjust=0.5), axis.title.x=element_text(face="bold"), axis.title.y=element_text(face="bold"), legend.title=element_blank())

ggplot(data_01_09, aes(x=reorder(Email_id, status, FUN=sort), y=page_rank, fill=status)) + geom_bar(stat="identity") +
  geom_text(aes(label=round(page_rank, digits=4)), size=1.5, vjust=0) +
  scale_x_discrete(name="Email_id") +
  scale_y_continuous(name="Page Rank", limits=c(NA, NA)) +
  ggtitle("Email Network at Sept. 2001: Individual Page Rank of People In the List by Status(Role)") +
  theme_economist() +
  theme(axis.text.x=element_text(size=4, angle=30, vjust=0.5), axis.title.x=element_text(face="bold"), axis.title.y=element_text(face="bold"), legend.title=element_blank())


##at "2002-01"(right after "filing for bankrupcy"):
data_02_01 <- nodes_statistics_inlist[year_month=="2002-01", c("Email_id", "firstName", "lastName", "status", "deg_in", "deg_out", "closeness", "betweenness", "eigen_cent", "page_rank")]
data_02_01 <- data_02_01[order(data_02_01$status), ]
View(data_02_01)

ggplot(data_02_01, aes(x=reorder(Email_id, status, FUN=sort), y=deg_in, fill=status)) + geom_bar(stat="identity") +
  geom_text(aes(label=round(deg_in, digits=0)), vjust=0) +
  scale_x_discrete(name="Email_id") +
  scale_y_continuous(name="In-degree", limits=c(NA, 150)) +
  ggtitle("Email Network at Jan. 2002: Individual In-degree of People In the List by Status(Role)") +
  theme_economist() +
  theme(axis.text.x=element_text(size=4, angle=30, vjust=0.5), axis.title.x=element_text(face="bold"), axis.title.y=element_text(face="bold"), legend.title=element_blank())

ggplot(data_02_01, aes(x=reorder(Email_id, status, FUN=sort), y=deg_out, fill=status)) + geom_bar(stat="identity") +
  geom_text(aes(label=round(deg_out, digits=0)), size=3, vjust=0) +
  scale_x_discrete(name="Email_id") +
  scale_y_continuous(name="Out-degree", limits=c(NA, NA)) +
  ggtitle("Email Network at Jan. 2002: Individual Out-degree of People In the List by Status(Role)") +
  theme_economist() +
  theme(axis.text.x=element_text(size=4, angle=30, vjust=0.5), axis.title.x=element_text(face="bold"), axis.title.y=element_text(face="bold"), legend.title=element_blank())

ggplot(data_02_01, aes(x=reorder(Email_id, status, FUN=sort), y=closeness, fill=status)) + geom_bar(stat="identity") +
  #geom_text(aes(label=round(closeness, digits=5)), size=2, vjust=0) +
  scale_x_discrete(name="Email_id") +
  scale_y_continuous(name="Closeness", limits=c(NA, NA)) +
  ggtitle("Email Network at Jan. 2002: Individual Closeness of People In the List by Status(Role)") +
  theme_economist() +
  theme(axis.text.x=element_text(size=4, angle=30, vjust=0.5), axis.title.x=element_text(face="bold"), axis.title.y=element_text(face="bold"), legend.title=element_blank())

ggplot(data_02_01, aes(x=reorder(Email_id, status, FUN=sort), y=betweenness, fill=status)) + geom_bar(stat="identity") +
  geom_text(aes(label=round(betweenness, digits=0)), size=2, vjust=0) +
  scale_x_discrete(name="Email_id") +
  scale_y_continuous(name="Betweenness", limits=c(NA, NA)) +
  ggtitle("Email Network at Jan. 2002: Individual Betweenness of People In the List by Status(Role)") +
  theme_economist() +
  theme(axis.text.x=element_text(size=4, angle=30, vjust=0.5), axis.title.x=element_text(face="bold"), axis.title.y=element_text(face="bold"), legend.title=element_blank())

ggplot(data_02_01, aes(x=reorder(Email_id, status, FUN=sort), y=eigen_cent, fill=status)) + geom_bar(stat="identity") +
  geom_text(aes(label=round(eigen_cent, digits=3)), size=2, vjust=0) +
  scale_x_discrete(name="Email_id") +
  scale_y_continuous(name="Eigenvector Centrality", limits=c(NA, NA)) +
  ggtitle("Email Network at Jan. 2002: Individual Eigenvector Centrality of People In the List by Status(Role)") +
  theme_economist() +
  theme(axis.text.x=element_text(size=4, angle=30, vjust=0.5), axis.title.x=element_text(face="bold"), axis.title.y=element_text(face="bold"), legend.title=element_blank())

ggplot(data_02_01, aes(x=reorder(Email_id, status, FUN=sort), y=page_rank, fill=status)) + geom_bar(stat="identity") +
  geom_text(aes(label=round(page_rank, digits=4)), size=1.5, vjust=0) +
  scale_x_discrete(name="Email_id") +
  scale_y_continuous(name="Page Rank", limits=c(NA, NA)) +
  ggtitle("Email Network at Jan. 2002: Individual Page Rank of People In the List by Status(Role)") +
  theme_economist() +
  theme(axis.text.x=element_text(size=4, angle=30, vjust=0.5), axis.title.x=element_text(face="bold"), axis.title.y=element_text(face="bold"), legend.title=element_blank())


########statistics for a few specific "sensitive" person (key "villain")#########
#read in the "nodes_stat_inlist.csv" file:
nodes_statistics_inlist <- fread("/Users/baixiao/Desktop/nodes_stat_inlist.csv", header=TRUE)
colnames(nodes_statistics_inlist)        #(11 columns)

###*Re-create the "whole" network consisting of only the 149 people in the "employeelist"###
nodes_inlist <- merge(nodes, employeelist[, c("Email_id")], by="Email_id")
edges_inlist <- edges[edges$sender %in% employeelist$Email_id & edges$receiver %in% employeelist$Email_id, ]
network_inlist <- graph_from_data_frame(edges_inlist, directed = T, vertices = nodes_inlist)      # *"vertices=nodes": use "employee's information" stored in "nodes" variable as Node Info(attributes etc.) in the network!
network_inlist         #(69407 edges)
#sapply(edge_attr(network), class)

#IMPORTANT ISSUE THAT MAY BE NOTICED: THERE ARE ACTUALLY QUITE A LOT "SELF-LOOPS"('emailing to one's self')! --> DELETE THESE!!
network_inlist <- simplify(network_inlist, remove.loops=TRUE, remove.multiple=FALSE)
network_inlist        #61673 edges in the network_inlist now
vertex_attr_names(network)       #see what attributes we have for nodes ["name"(i.e.: "vertex name"--email address), "firstName", "lastName", "status"]
edge_attr_names(network)           #all attributes for edges ["type", "date", "subject", "folder", "count"]

years <- unique(year(edges_inlist$date))       #1998, 1999, 2000, 2001, 2002
edges_inlist[, year_month := format(date, "%Y-%m")]
View(edges_inlist)

length(unique(edges_inlist$year_month))        #44 unique continuous months
year_months_inlist <- unique(edges_inlist$year_month)
#try to store the network for each year_month:
nets_inlist <- list()
for (i in year_months_inlist) {
  edge_current_ym <- edges_inlist[edges_inlist$year_month==i, ]
  nodes_current_ym <- nodes_inlist[(nodes_inlist$Email_id %in% edge_current_ym$sender | nodes_inlist$Email_id %in% edge_current_ym$receiver), ]
  graph_current_ym <- graph_from_data_frame(edge_current_ym, directed=T, vertices=nodes_current_ym)
  graph_current_ym <- simplify(graph_current_ym, remove.loops=TRUE, remove.multiple=FALSE)        #*IMPORTANT: "SELF-LOOP" ISSUE
  nets_inlist[[as.character(i)]] <- graph_current_ym         #store the network graphs of all year_months(element named by each year_month)
}
nets_inlist


#####"critical" person: "Kenneth Lay"(founder and former CEO)#####
Lay_nets <- list()          #Lay's networks for each months
Lay_edgelists <- list()
for (i in year_months_inlist) {
  try ( {
  Lay_nets[[as.character(i)]] <- make_ego_graph(nets_inlist[[i]], order=1, c("kenneth.lay@enron.com"))[[1]]        #(NOTICE:"make_ego_graph()" function returns a LIST OF GRAPHS)
  Lay_edgelists[[as.character(i)]] <- as_edgelist(Lay_nets[[i]])
  }
  )
}
Lay_nets
length(Lay_nets)         #21 months when we have Lay's emailing data
names(Lay_nets)          #which months -- starting: "1999-10", ending: "2002-01"
Lay_edgelists
Lay_edgelists <- lapply(Lay_edgelists, as.data.table)
for (i in names(Lay_edgelists)) {
  Lay_edgelists[[i]][, count := .N, by=c("V1", "V2")]
  colnames(Lay_edgelists[[i]])[1:2] <- c("From", "To")
  Lay_edgelists[[i]] <- unique(Lay_edgelists[[i]])
  #View(Lay_edgelists[[i]])
  Lay_edgelists[[i]] <- Lay_edgelists[[i]][order(-Lay_edgelists[[i]]$count), ]      #sort table according to tie number in DESCENDING order
  write.csv(Lay_edgelists[[i]], paste0("/Users/baixiao/Desktop/Lay_edgelists/", i, ".csv"), row.names=FALSE)
}
Lay_edgelists

#Plot Lay's networks (21):
Lay_nets_1 <- Lay_nets
for (i in names(Lay_nets)) {
  V(Lay_nets_1[[i]])$color <- c("orange", "red")[factor(V(Lay_nets_1[[i]])$name=="kenneth.lay@enron.com")]      #plot vertex distinguished by colors based on "whether this employee email is in the (sensitive) 'employeelist' or not"
  V(Lay_nets_1[[i]])$label <- paste(V(Lay_nets_1[[i]])$firstName, V(Lay_nets_1[[i]])$lastName, sep=" ")
  V(Lay_nets_1[[i]])$size <- 8
  E(Lay_nets_1[[i]])$width <- 0.5
  E(Lay_nets_1[[i]])$arrow.size <- 0.8
  E(Lay_nets_1[[i]])$arrow.width <- 0.6
  mypath <- file.path("/Users/baixiao/Desktop/Kenneth\ Lay", paste(i, ".jpeg", sep=""))
  jpeg(file=mypath)
  mytitle = paste("Kenneth Lay's Network: ", i)
  plot(Lay_nets_1[[i]], layout=layout_nicely, main=mytitle)
  dev.off()
}

#Lay's statistics:
Lay_statistics <- nodes_statistics_inlist[Email_id=="kenneth.lay@enron.com"]
View(Lay_statistics)

#ggplot2: plot Lay's statistics change over time:
data_for_yearmonth <- Lay_statistics
View(data_for_yearmonth)
ggplot(data_for_yearmonth, aes(x=year_month, y=deg_in)) + geom_bar(stat="identity", fill="darkorchid1") +
  geom_text(aes(label=round(deg_in, digits=0)), vjust=0) +
  scale_x_discrete(name="month") +
  #scale_x_discrete(name="month", breaks=c("1998-10", "1999-02", "1999-06", "1999-10", "2000-02", "2000-06", "2000-10", "2001-02", "2001-06", "2001-10", "2002-02", "2002-06", "2002-09"),
                   #labels=c("10/98", "02/99", "06/99", "10/99", "02/00", "06/00", "10/00", "02/01", "06/01", "10/01", "02/02", "06/02", "09/02")) +
  scale_y_continuous(name="Lay's in-degree", limits=c(NA, NA)) +
  ggtitle("Lay's Email Network In-degree Over Time") +
  theme_economist() +
  theme(axis.text.x=element_text(size=9, angle=25, vjust=1), axis.title.x=element_text(face="bold"), axis.title.y=element_text(face="bold"))

ggplot(data_for_yearmonth, aes(x=year_month, y=deg_out)) + geom_bar(stat="identity", fill="orchid2") +
  geom_text(aes(label=round(deg_out, digits=0)), vjust=0) +
  scale_x_discrete(name="month") +
  #scale_x_discrete(name="month", breaks=c("1998-10", "1999-02", "1999-06", "1999-10", "2000-02", "2000-06", "2000-10", "2001-02", "2001-06", "2001-10", "2002-02", "2002-06", "2002-09"),
  #labels=c("10/98", "02/99", "06/99", "10/99", "02/00", "06/00", "10/00", "02/01", "06/01", "10/01", "02/02", "06/02", "09/02")) +
  scale_y_continuous(name="Lay's out-degree", limits=c(NA, NA)) +
  ggtitle("Lay's Email Network Out-degree Over Time") +
  theme_economist() +
  theme(axis.text.x=element_text(size=9, angle=25, vjust=1), axis.title.x=element_text(face="bold"), axis.title.y=element_text(face="bold"))

ggplot(data_for_yearmonth, aes(x=year_month, y=closeness)) + geom_bar(stat="identity", fill="seagreen2") +
  #geom_text(aes(label=closeness), vjust=0) +
  scale_x_discrete(name="month") +
  #scale_x_discrete(name="month", breaks=c("1998-10", "1999-02", "1999-06", "1999-10", "2000-02", "2000-06", "2000-10", "2001-02", "2001-06", "2001-10", "2002-02", "2002-06", "2002-09"),
  #labels=c("10/98", "02/99", "06/99", "10/99", "02/00", "06/00", "10/00", "02/01", "06/01", "10/01", "02/02", "06/02", "09/02")) +
  scale_y_continuous(name="Lay's closeness", limits=c(NA, NA)) +
  ggtitle("Lay's Email Network Closeness Over Time") +
  theme_economist() +
  theme(axis.text.x=element_text(size=9, angle=25, vjust=1), axis.title.x=element_text(face="bold"), axis.title.y=element_text(face="bold"))

ggplot(data_for_yearmonth, aes(x=year_month, y=betweenness)) + geom_bar(stat="identity", fill="coral1") +
  geom_text(aes(label=round(betweenness, digits=0)), vjust=0) +
  scale_x_discrete(name="month") +
  #scale_x_discrete(name="month", breaks=c("1998-10", "1999-02", "1999-06", "1999-10", "2000-02", "2000-06", "2000-10", "2001-02", "2001-06", "2001-10", "2002-02", "2002-06", "2002-09"),
  #labels=c("10/98", "02/99", "06/99", "10/99", "02/00", "06/00", "10/00", "02/01", "06/01", "10/01", "02/02", "06/02", "09/02")) +
  scale_y_continuous(name="Lay's betweenness", limits=c(NA, NA)) +
  ggtitle("Lay's Email Network Betweenness Over Time") +
  theme_economist() +
  theme(axis.text.x=element_text(size=9, angle=25, vjust=1), axis.title.x=element_text(face="bold"), axis.title.y=element_text(face="bold"))

ggplot(data_for_yearmonth, aes(x=year_month, y=eigen_cent)) + geom_bar(stat="identity", fill="yellow2") +
  #geom_text(aes(label=round(eigen_cent, digits=0)), vjust=0) +
  scale_x_discrete(name="month") +
  #scale_x_discrete(name="month", breaks=c("1998-10", "1999-02", "1999-06", "1999-10", "2000-02", "2000-06", "2000-10", "2001-02", "2001-06", "2001-10", "2002-02", "2002-06", "2002-09"),
  #labels=c("10/98", "02/99", "06/99", "10/99", "02/00", "06/00", "10/00", "02/01", "06/01", "10/01", "02/02", "06/02", "09/02")) +
  scale_y_continuous(name="Lay's eigenvector centrality", limits=c(NA, 0.2)) +
  ggtitle("Lay's Email Network Eigenvector Centrality Over Time") +
  theme_economist() +
  theme(axis.text.x=element_text(size=9, angle=25, vjust=1), axis.title.x=element_text(face="bold"), axis.title.y=element_text(face="bold"))

ggplot(data_for_yearmonth, aes(x=year_month, y=page_rank)) + geom_bar(stat="identity", fill="royalblue2") +
  #geom_text(aes(label=round(page_rank, digits=0)), vjust=0) +
  scale_x_discrete(name="month") +
  #scale_x_discrete(name="month", breaks=c("1998-10", "1999-02", "1999-06", "1999-10", "2000-02", "2000-06", "2000-10", "2001-02", "2001-06", "2001-10", "2002-02", "2002-06", "2002-09"),
  #labels=c("10/98", "02/99", "06/99", "10/99", "02/00", "06/00", "10/00", "02/01", "06/01", "10/01", "02/02", "06/02", "09/02")) +
  scale_y_continuous(name="Lay's page rank", limits=c(NA, NA)) +
  ggtitle("Lay's Email Network Page Rank Centrality Over Time") +
  theme_economist() +
  theme(axis.text.x=element_text(size=9, angle=25, vjust=1), axis.title.x=element_text(face="bold"), axis.title.y=element_text(face="bold"))


## data management:
save(edges, nodes, network, file = "/Users/baixiao/Desktop/updated_enron.RData")

