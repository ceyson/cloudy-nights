## Inits (load essential packages)
libs <- c("rvest","xml2","magrittr","stringr","dplyr")
lapply(libs,require,character.only=TRUE)

## My machine has a folder structure of:
##   ../cloudy-nights
##   ../cloudy-nights/data
##   ../cloudy-nights/scripts
setwd("C:/Users/Antares/Documents/Learning/R/Web-Scraping/cloudy-nights")

## Idea here is to loop through all the pages of classified ads and return the item, item category,
## item price and the ad date. To set the max iterations on the loop scape the total number of ad
## pages from the 'classifieds' home page. For the loop, read in the html for each classified ad page
## url. These pages follow a particular format with ending paramater that is a multiple of 25 (there
## are 25 ads per classified page). The four fields described above are read into individual vectors
## that are then column binded into a single data frame. For the first iteration of the loop an initial
## 'catalog' data frame is created and held in memory. With 'catalog' in memory, for subsequent iterations
## a 'tmp' data frame is created and row binded to the previous 'catalog' incarnation. Thus for each
## iteration of the the data frame has the next page of classifieds information appended. 

## There is meta data for how many views and ad received and also whether it was sold or not. Unfortunately
## there is some missing data in these vectors when I read them in (there length is not always equalt to 25)
## and thus I cannot cbind them correctly - the data would be come misaligned. Maybe there is a way to handle 
## this?

## The date field is troublesome since ads posted in the past two days are listed as being either from
## 'Today' or 'Yesterday'. Furthermore, for the instances where we can scrape a date the date value is 
## not a date type. There are some convoluted steps to shape this information into a single date formatted
## column.

## At the end of it all I write the data frame to a csv file. 

## Further processin of the 'items' field might be conversion to lowercase and whatever else standardization.
## Statistics and graphs can be run now (see: http://r4ds.had.co.nz/)

siteUrl <- "http://www.cloudynights.com/classifieds/"
classifiedsHome <- read_html(siteUrl)

totalPages <- classifiedsHome %>%
	html_nodes(".pagejump a") %>%
	html_text() 

totalPages <- as.numeric(word(totalPages,-2)[1])

for (i in 0:totalPages) {

	pageParam <- i*25
	rootUrl <- "http://www.cloudynights.com/index.php?app=classifieds&do=view_category&category_id=&sort_key=date_added&sort_order=desc&filter=0&st="
	url <- paste(rootUrl,pageParam, sep="")
	print(url)

	# Store web url
	classifieds <- read_html(url)

 	if (i==0) {

	 	## Ad item
		items <- classifieds %>%
			html_nodes(".ipsType_subtitle a") %>%
			html_text() %>%
			str_replace_all("[\\\\]","")

		## Item category
		category <- classifieds %>%
			html_nodes(".cat_name a") %>%
			html_text()

		## Item price
		price <- classifieds %>%
			html_nodes("br+ .right") %>%
			html_text() %>%
			str_replace_all("[\n\t $]","") %>%
			as.numeric()

		## Ad date
		date <- classifieds %>%
			html_nodes(".item_info .ipsType_small:nth-child(1)") %>%
			html_text()

		## Ad views
		# views <- classifieds %>%
		# 	html_nodes(".ipsType_small+ .ipsType_small") %>%
		# 	html_text() %>%
		# 	str_replace_all("[\n\t:Views ]","") %>%
		# 	as.numeric()

		## Meta (for sale, wanted)
		# meta <- classifieds %>%
		# 	html_nodes(".ipsBadge_green") %>%
		# 	html_text() 

		## Outcome (was it sold)
		# outcome <- classifieds %>%
		# 	html_nodes(".priceBadge") %>%
		# 	html_text()  %>%
		# 	str_replace_all("[!]","")

		## Bind vectors to data frame
	    tmp <- cbind(items, category, price, date)
 		catalog <- as.data.frame(tmp)

 	} else {

	 	## Ad item
		items <- classifieds %>%
			html_nodes(".ipsType_subtitle a") %>%
			html_text() %>%
			str_replace_all("[\\\\]","")

		## Item category
		category <- classifieds %>%
			html_nodes(".cat_name a") %>%
			html_text()

		## Item price
		price <- classifieds %>%
			html_nodes("br+ .right") %>%
			html_text() %>%
			str_replace_all("[\n\t $]","") %>%
			as.numeric()

		## Ad date
		date <- classifieds %>%
			html_nodes(".item_info .ipsType_small:nth-child(1)") %>%
			html_text()

		## Ad views
		# views <- classifieds %>%
		# 	html_nodes(".ipsType_small+ .ipsType_small") %>%
		# 	html_text() %>%
		# 	str_replace_all("[\n\t:Views ]","") %>%
		# 	as.numeric()

		## Meta (for sale, wanted)
		# meta <- classifieds %>%
		# 	html_nodes(".ipsBadge_green") %>%
		# 	html_text() 

		## Outcome (was it sold)
		# outcome <- classifieds %>%
		# 	html_nodes(".priceBadge") %>%
		# 	html_text()  %>%
		# 	str_replace_all("[!]","")

		## Bind vectors to data frame
	    tmp <- cbind(items, category, price, date)
 		catalog <- rbind(catalog, as.data.frame(tmp))
 	}

}

# Fix Dates (better way to do this)

## Todays
catalog$today <- ifelse(substr(catalog$date,1,5) == "Today", substr(Sys.time(),1,10),NA)
catalog$today <- as.Date(catalog$today)

## Yesterdays
yesterdayIndx <- grep("Yesterday", catalog$date)
catalog$yesterday <- ifelse(substr(catalog$date,1,9) == "Yesterday", as.character(as.Date(Sys.time())-1), NA)
catalog$yesterday <- as.Date(catalog$yesterday)

## Dates
x <- tolower(as.character(catalog$date))
x <- str_replace_all(x, fixed(" "), "")
catalog$dates <- ifelse(substr(catalog$date,1,3) != "Tod" & substr(catalog$date,1,3) != "Yes", x, NA)
catalog$dates <- as.Date(catalog$dates,"%d%B%Y")

## Coalesce dates
catalog$adDate <- coalesce(catalog$today,catalog$yesterday,catalog$dates)

## Drop columns
catalog$date <- NULL
catalog$today <- NULL
catalog$yesterday <- NULL
catalog$dates <- NULL


## Write data
write.csv(catalog,"./data/cloud-nights-classifieds.csv",row.names=FALSE)





