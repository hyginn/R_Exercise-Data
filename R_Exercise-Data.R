# R_exercise-Data.R
#
# Practising importing and writing data.
#
# Boris Steipe (boris.steipe@utoronto.ca)
# Jan 2016
# (C) copyrighted material. Do not share without permission.
# ====================================================================

"Before we work with data, we obviously need to read data into our R session. That is, unless we create (random?) data from first principles. This set of exercises focusses on how to read and write data in various formats and from various sources.

What you should already know includes:
- data types in R (logical, character, numeric...);
- vectors, matrices, data frames and lists, and the difference between them;
- creating vectors with the c() function and using the related rbind() and
    cbind() functions for matrices or dataframes;

"


# ====================================================================
#        PART ONE: READING AND WRITING LOCAL TEXT DATA
# ====================================================================
'The most simple-minded way to get data into R is of course to define it in a script file and assign it. I find myself from time to time putting words or numbers into a text file and replacing all linebreaks with ", " ; wrap this into the c() function, copy it into a script, assign it and you are done. Sometimes this is the right way, in particular if the data is not well structured and needs to be edited in a text file anyway. But more frequently our data has some predictable structure or is simply to large.'


# ===== Unstructured Text ============================================


"Let us look at textual data first, i.e. data that we have to parse in order to provide structure."

file.edit("GO_sample.obo.txt")

"This is an extract from the Gene Ontology. If we wanted to write a parser for such files, the first question we would need to ask is: how is it structured? What types of information items does it contain?

At first glance, we would conclude that this is not simpy free text, but items are structured line by line. Let's open the file and read its lines:"

?readLines

text <- readLines("GO_sample.obo.txt")
length(text)
text[1:6]

"If the file is very large, we can limit the number of lines to read with the parameter n..."

readLines("GO_sample.obo.txt", n=15)

'Our variable "text" is now a vector of character items, one line per element - we would also call such lines "records". Note that the paragraph breaks are no longer present. To further process this, we need to use regular expressions to extract patterns. For example we could be interested in what record-type markers are present. These are found at the beginning of a line, contain no white space, and are concluded by a colon.'

m <- regexpr("^([^:]+):", text) # anchor at beginning of line (^),
                                # capture one or more (+)  of not-a-colon ([^:]),
                                # followed by a colon (:)
regmatches(text, m)

"Of course we need only each pattern only once:"

unique(regmatches(text, m))

'... well, we obviously have a bit of complexity even in this short excerpt. Let us try excerpting only "id" and "is_a" records, and writing them to a file.'

            grep("^id:|is_a:", text)
sel <- text[grep("^id:|is_a:", text)]

?writeLines
writeLines(sel, "test.txt")  # paragraph marks are added with "sep" argument - defaults to "\n"
file.edit("test.txt")        # did it work?


# ===== Tabular data =================================================

"Often our data is formatted as a table: rows represent entities and their columns are organized in attributes. Spreadsheet data is like that and one of our most frequent tasks is to read data from Excel tables. A word to the wise: don't do it. R has functions to read from Excel spreadsheets, but since the Excel format is not really open, the safer approach is to save data in a .csv (Comma Separated Values) format and the read it. The most important function is read.csv(), one of a number of functions derived from read.table()"

"Let's dive in, I have adapted a set of drug effect values on a cancer cell-line to illustrate."

file.edit("Synergy-sample.csv")

"These certainly are comma separted values. Let's read them:"

dat <- read.csv("Synergy-sample.csv")
head(dat)

"Not too bad, isn't it? But the devil is in the details:"

str(dat)

"The whole thing is a data frame. That's good, because we usually have character and numeric types in our data. In a data frame, the values in a column must always have the same type. In our case we see numeric columns and Factors. Why Factors? That's because analyzing models in terms of factors is common in statistics and thus the R authors assumed it would be a good idea to turn all character values automatically into factors. It turned out not to be such a good idea and the invocation stringsAsFactors = FALSE is found in R code everywhere. But that's too late to change. One can change this behaviour globally, but that's a bad idea because it may break assumptions e.g. by careless package authors... We just deal with it."

dat <- read.csv("Synergy-sample.csv", stringsAsFactors = FALSE)
str(dat)

"Note: you have to set the parameter whenever a data frame is created, obviously in the read.csv() function, but also when using data.frame()."

dat

"The next issue is with column X.1. It's a character column. Why?"
dat$X.1

"There is one missing value, but someone has coded a second missing value as 'n/a' in the original data. That's not good. When that happens, the whole column is turned into character. Can we convert this?"

as.numeric("n/a")   # Looks good: NA is actually what we want
as.numeric(dat$X.1)
dat$X.1 <- as.numeric(dat$X.1)
str(dat)

"Finally, read.csv() has made some assumptions about haders and values that turned out to be wrong in our case. The first line was not a header, but some additional information. The row and column headers (concentrations) ended up in row 1 and column 1, and row 8 and column X.5 are not actually needed. Let's first use row 1 and column 1 as row- resp. column names. To make sure they are actually valid names, we'll run them through the make.names() function as we do that:"

rownames(dat) <- make.names(dat[ , 1])
colnames(dat) <- make.names(dat[1 , ])
dat

"Now we can remove the unwanted rows and columns:"
dat <- dat[ c(-1, -8), ]
dat <- dat[ , c(-1, -8) ]
dat
str(dat)

"That's what we need. Let's interpolate out the NA values ..."

for (i in 1:nrow(dat)) {
  for(j in 1:ncol(dat)) {
    if (is.na(dat[i, j])) {
      dat[i, j] <- mean(c(dat[i+1, j], dat[i-1, j], dat[i, j+1], dat[i, j-1]))
    }
  }
}

"Now we can e.g. plot the effects. Let's use the plot3D package for a perspective plot of the dose-effect surface:"
if (! require(plot3D, quietly=TRUE)) {
  install.packages("plot3D",
                   repos="http://cran.us.r-project.org")
  library(plot3D)
}

persp3D(
        z = as.matrix(dat),
        theta = 135, phi = 30,
        zlim=c(0,100),
        col = "cornflowerblue",
        border = "black",
        ltheta = 120, shade = 0.75,
        zlab = "% survival"
)

"Accurately and robustly reading data is not trivial and there are many things that can go wrong if you are not careful. Unfortunately this will not always lead to crashing code, more often your results are just going to be subtly wrong. For example, 'NA' strings may have been inserted that are actually characters, people accidentally type commas for decimal points, factors are created and then erroneously converted to integers, and let's note ven get started on what Excel does to dates... Bottomline: validate your data, use str(), and make sure every column is what it should be. Write all of the commands that you use to read your data into a script, and make sure you can perfectly recreate your data object from source data with the script."


# ===== write.csv() ===========================================

"write.csv() is complementary to read.csv and allows you to save your data frame in a consistent format."

write.csv(dat, "test.csv")
file.edit("test.csv")

"You see that the row-names and column names have been written in the first row and column respectively. But here's a problem: they are all numeric! When we read them back in, they won't be recognized as row and column names, but R will think they are data values instead - even though they were written to file with quotation marks."

tmp <- read.csv("test.csv",
                header = TRUE,
                row.names = 1,
                stringsAsFactors = FALSE)
str(tmp)
"Note: you might think of trying ..."
identical(dat, tmp)
" ... but this will often fail, because of the limited accuracy of floating point numbers. All values in the two dataframes seem to be identical, but in fact:"

print(dat[3,3], digits=22)
print(tmp[3,3], digits=22)

"... the ones we computed for interpolating NA's are not."


# ===== dput()  and dget() ===========================================

"dput() and dget() are convenient functions to write out the contents of R objects in a text-format. This can e.g. be sent by e-mail, or posted to a help forum, and easily be read in again."

dput(dat)
x <- dget('structure(list(X0 = c(100, 93.5, 92.6, 88, 83.5, 60), X0.01 = c(84.4,
87.9, 87.6, 78.7, 72.5, 55.6), X0.03 = c(82.4, 78.1, 78.525,
          76.1, 65.85, 50.1), X0.1 = c(71.1, 72.4, 72.3, 72.4, 64.7, 41.5
          ), X0.3 = c(57.2, 63.6, 58.4, 61.1, 45.5, 27.6), X1 = c(46.1,
          48.3, 48.9, 50.2, 29.5, 18.4)), .Names = c("X0", "X0.01", "X0.03",
          "X0.1", "X0.3", "X1"), row.names = c("X0", "X0.01", "X0.03",
          "X0.1", "X0.3", "X1"), class = "data.frame")')
x     # but note that the default accuracy here is only three significant digits.


# ====================================================================
#        PART TWO: BINARY DATA: THE .RDATA FORMAT
# ====================================================================

"Can we save and reload an R object exactly as it is? Yes! This is using the functions save() and load(). This is the best way to export and import even complex R objects."

tmp <- dat
identical(tmp, dat)
save(tmp, file = "tmp.Rdata")
rm(tmp)
load("tmp.Rdata") # note: NOT  tmp <- load(...): the variable is automatically created
identical(dat, tmp)





# ====================================================================
#        APPENDIX: OUTLOOK
# ====================================================================
"There are many more function for data import that this tutorial did not cover, especially how to read data over the Web, or handling JSON data. An excellent, in-depth tutorial on these and many other variations on the theme is online at the DataCamp blog:

  https://www.datacamp.com/community/tutorials/r-data-import-tutorial
  https://www.datacamp.com/community/tutorials/importing-data-r-part-two

Browse over this, so you know what's out there...
"

"You should know about the following packages:
   http://blog.rstudio.org/2015/04/15/readxl-0-1-0/  improved handling of Excel files
   http://blog.rstudio.org/2015/04/09/readr-0-1-0/   especially suited for large, tabular datasets.
"



# [END]
