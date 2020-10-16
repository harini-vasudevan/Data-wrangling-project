library(tidyverse)
library(pdftools)
options(digits = 3)    # report 3 significant digits

#Puerto Rico Hurricane Mortality: Part 1
# 1
fn <- system.file("extdata", "RD-Mortality-Report_2015-18-180531.pdf", package="dslabs")
system("cmd.exe", input = paste("start", fn))

#It is a report combining graphs and tables. Extracting the data seems possible.

# 2
txt <- pdf_text(fn) # creating a tidy dataset 
txt
#A character string of length 12. Each entry represents the text in each page. 
#The mortality data is in there somewhere.

# 3
#Extract the ninth page of the PDF file from the object.

Mortality_Report <- txt[9]
Mortality_Report%>% head
#create a list with the lines of the text as elements
x <- str_split(Mortality_Report, "\n")
x   #I can see the table! But there is a bunch of other stuff we need to get rid of.
class(x)  # list with one entry

# 4
#Define s to be the first entry of the x object

s <- x[[1]]
s %>% head
class(s)  #character vector
s     # s have 40 entries

# 5
#When inspecting the string we obtained above, we see a common problem: 
#white space before and after the other characters. 
#Trimming is a common first step in string processing.
#These extra spaces will eventually make splitting the strings hard so we start by removing them.
s <- s %>% str_trim()
s
s[1]    # print string, visually inspect last character

#6
#We want to extract the numbers from the strings stored in s. 
#However, there are a lot of non-numeric characters that will get in the way.
#We can remove these, but before doing this we want to preserve the string with the column header,
#which includes the month abbreviation.

header_index <- str_which(s,"2015")[1]
header_index

#7
#We want to extract two objects from the header row:
#month will store the month and header will store the column names.
tmp <- str_split(s[header_index], "\\s+", simplify = TRUE)
tmp
month <- tmp[1]
header <- tmp[-1]
month
header[3]

#8
#Notice that towards the end of the page defined by s you see a "Total" row followed by rows with other summary statistics.
#Create an object called tail_index with the index of the "Total" entry.
s
tail_index <- str_which(s,"Total")[1]
tail_index

#9
#Because our PDF page includes graphs with numbers,
#some of our rows have just one number (from the y-axis of the plot).

#Use the str_count() function to create an object n with the count of numbers in each row

#How many rows have a single number in them?
n <- str_count(s,"\\d+")
n
sum(n == 1)


#10
# #We are now ready to remove entries from rows that we know we don't need. 
# The entry header_index and everything before it should be removed. 
# Entries for which n is 1 should also be removed
#and the entry tail_index and everything that comes after it should be removed as well.

out <- c(1:header_index, which(n==1), tail_index:length(s))
s <- s[-out]
length(s)

#11
#Now we are ready to remove all text that is not a digit or space.
#Do this using regular expressions (regex) and the str_remove_all() function.
s <- str_remove_all(s, "[^\\d\\s]")
s

#12
#Use the str_split_fixed function to convert s into a data matrix 
#with just the day and death count data:

s <- str_split_fixed(s, "\\s+", n = 6)[,1:5]
tab <- s %>% 
  as_data_frame() %>% 
  setNames(c("day", header)) %>%
  mutate_all(as.numeric)
tab
mean(tab$"2015")  #What was the mean number of deaths per day in September 2015?
mean(tab$"2016")  #What is the mean number of deaths per day in September 2016?
mean(tab$"2017"[1:19]) #Hurricane María hit Puerto Rico on September 20, 2017. What was the mean number of deaths per day from September 1-19, 2017, 
#before the hurricane hit?
mean(tab$"2017"[20:30]) #What was the mean number of deaths per day from September 20-30, 2017, 
#after the hurricane hit?

#13
#changing tab to a tidy format
tab <- tab %>% gather(year, deaths, -day) %>%
  mutate(deaths = as.numeric(deaths))
tab

#14
#plot of deaths versus day with color to denote year. 
#Exclude 2018 since we have no data. 
#Add a vertical line at day 20, the day that Hurricane María hit in 2017

tab %>% filter(year < 2018) %>% 
  ggplot(aes(day, deaths, color = year)) +
  geom_line() +
  geom_vline(xintercept = 20) +
  geom_point()
  
#September 2015 and 2016 deaths by day are roughly equal to each other.
#After the hurricane in September 2017, there were over 100 deaths per day every day for the rest of the month.
#No days before September 20, 2017 have over 100 deaths per day. 