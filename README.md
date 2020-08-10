# Enron Corporation Email Network Analysis
## --Unveiling the Dynamics, Characteristics, and Indications Underlying "the Enron Scandal" (governmental investigation in October 2001)

Enron Corporation, an American energy, commodities, and services company, has been named by Fortune magazine ‘America’s most innovative company’ for six consecutive years. It was Ameri-ca’s seventh-largest company in the year 2001, with its annual revenue exceeding $100B. The company seemed to be operating in perfect condition, with its stock price kept rising and share-holders gained ever-increasing shareholder equity. However, the turning point was just about to happen. On October 16, 2001, Enron reports a $618M loss, its first quarterly loss in the company's history, along with a $1.2B reduction in its shareholder equity. Soon afterwards, Enron filed Bankruptcy. As a result, nearly 100,000 people lose their jobs. The Enron scandal has become one of the largest corporate corruption and accounting frauds in American’s history. Numerous of its senior management members were sentenced to jail. 

So, what happened? In order to understand it, we would like to find out if there is any indication of Enron Corporation's collapse before the crisis happened. In this project, We want to find out some of the characteristics of its email network before and after the company's ‘crisis point’. We conduct both network-level analysis and node-level analysis on the email network.

About the data:

In the event of disclosure of Enron’s having hidden millions of dollars of debt from failed projects by SPV and financial loopholes, the US SEC and Federal Energy Regulatory Commission(FERC) quickly instituted investigation in October 2001, where about 500000 internal emails were made public online for transparency originally, making it the largest public domain database of real-world company internal emails in the world. This grounds the foundational data for the engaging research topic of our study. We use mainly the cleaned data version posted in Arne Hendrik Ruhe’s website (http://www.ahschulz.de/enron-email-data/) for our project, primarily a RData file containing the email message data (such as senders and receivers) and a little further information for some specific "sensitive" (critical) individuals in the event (referred to as the special “employeelist” of 149 people here). We complete our data wrangling, visualization and modeling analysis based on this data.

About the project methodology:

This project presents how we conduct the social network analysis using the Enron corpus. Three different angles of looking at the network were applied. These different approaches are (a) comparing the network-level measures over time, (b) identifying group-level features using ERGM model, and (c) detect the activity and presence of important actors with respect to the Enron Scandal.

A variety of social network analysis tools, including igraph, ggplot2, R Shiny and ERGM modeling packages in R, are used in this project.

