# Load libraries
if (system.file(packge == "pacman") == "") {
  install.packages("pacman")
}
pacman::p_load(rvest, httr, dplyr, ggplot2, googlesheets4, RSelenium, stringr, # Libraries used in the code 
               stringi, netstat, tibble, tidyverse, Rcrawler, splashr, magick, # Libraries used in the code
               purrr, V8, seleniumPipes, chromote, magrittr, parallel, foreach, doParallel) # Libraries not used in the code, but could be useful for optimization cases

# rvest does not work right away because Homzmart's website is Java-script rendered. 
# Use RSelenium to simulate a human browsing through the website.

# Two necessary steps before starting to use RSelenium according to this Stackoverflow
# https://stackoverflow.com/questions/46202062/i-got-error-like-error-in-if-file-accessphantompath-1-0-argument-is-o
# binman::rm_platform("phantomjs")
# wdman::selenium(retcommand = TRUE)

# Create a function that measures tha amount of time it takes the website to respond to a "GET" request
response_time_get <- function(webpage) {
  
  # While scraping, we want to slow down requests to allow the page to fully load
  # Since it is difficult to determine how long a page takes to load in an automated scrape, we use the automated solution outlined below
  t0 <- Sys.time()
  response <- httr::GET(webpage) # Send a request to the website
  t1 <- Sys.time()
  response_delay <- as.numeric(t1-t0) # Measure how long the website took to respond back to us
  
  return(response_delay)
}

# Create a function that gets the HTML code of a webpage through RSelenium
navigate_selenium_func <- function(browser_var, webpage){
  # Use RSelenium to open a blank new firefox browser according to this website 
  # http://joshuamccrain.com/tutorials/web_scraping_R_selenium.html
  rD <- rsDriver(browser = browser_var, port = free_port(), verbose = FALSE, check = TRUE)
  remDr <- rD[["client"]]
  
  # Navigate to the required page of interest
  remDr$navigate(webpage)
  
  # Calculate the response delay, which will be used in the "sleep" function below
  response_delay <- response_time_get(webpage)
  
  # Give the page some time to fully load --> 10 times longer than response_delay to be safe
  Sys.sleep(max(2.5,10 * response_delay))
  
  # Save the HTML file to an object
  html <- remDr$getPageSource()[[1]]
  
  # Store the html code into a variable
  page <- read_html(html)
  
  # Close the browser and delete the rD variable so that you can re-use the port
  remDr$close()
  rD$server$stop()
  rm(rD, remDr)
  gc()
  system("taskkill /im java.exe /f", intern=FALSE, ignore.stdout=FALSE) # Kill the Java process behind RSelenium
  
  # Return the result (i.e. the HTML code)
  return(page)
}

##-------------------------------------------------------START OF Home Page Scraping----------------------------------------------------##

# Open the home page
homepage <- navigate_selenium_func("firefox", "https://homzmart.com/en")

## Automation
# Grab the names of the **categories** from the home page
mkc_names <- homepage %>% 
  html_nodes(xpath = '//div[contains(@class, "site-menu__item")]/a') %>% # XPATH of the category names
  html_text(trim = TRUE)

# Grab the URLs of the **categories** from the home page
mkc_urls <- homepage %>% 
  html_nodes(xpath = '//div[contains(@class, "site-menu__item")]/a') %>% # XPATH of the category names
  html_attr("href") %>% # Instead of grabbing the names, grab the URLs
  paste0("https://homzmart.com", .)

df_mkc <- data.frame(mkc_names, mkc_urls)

##----------------------------Separator----------------------------##

# Grab the names of the sub-categories from the home page
df_scrap <- data.frame()
for (i in df_mkc$mkc_urls) {
  catpages <- navigate_selenium_func("firefox", i)
  
  sub_mkc_names <- catpages %>% 
    html_nodes(xpath = '//div[@role = "tabpanel"]/a/div[@class = "header"]') %>% 
    html_text(trim = TRUE)
  
  # Grab the URLs of the sub-categories from the home page
  sub_mkc_urls <- catpages %>% 
    html_nodes(xpath = '//div[@role = "tabpanel"]/a') %>% 
    html_attr("href") %>% 
    paste0("https://homzmart.com", .)
  
  # Create a data frame of the "sub_mkc_names" and "sub_mkc_urls" columns. 
  # This will be the data frame that will be fed with the scraped information from the product pages later on in the code
  df_scrap <- rbind(df_scrap, data.frame(sub_mkc_names, sub_mkc_urls, mkc_urls = i))
}

##----------------------------Separator----------------------------##

# Get the names of the categories based on the names of the sub-categories
df_scrap <- df_scrap %>% 
  left_join(df_mkc, by = "mkc_urls") %>% 
  mutate(mkc_names = mkc_names)

##-------------------------------------------------------END OF Home Page Scraping-------------------------------------------------------##

##-------------------------------------------------------START OF Product Link Scraping--------------------------------------------------##

# Create a function that grabs ALL the product links under each sub-category
prod_links_func <- function(browser_var, webpage, sub_mkc, mkc){
  t_start <- Sys.time() # Start time of scraping one sub-category
  
  # Use RSelenium to open a blank new firefox browser according to this website
  # http://joshuamccrain.com/tutorials/web_scraping_R_selenium.html)
  rD <- rsDriver(browser = browser_var, port = free_port(), verbose = FALSE, check = TRUE)
  remDr <- rD[["client"]]
  
  # Navigate to the required page of interest
  remDr$navigate(webpage)
  
  # Calculate the response delay, which will be used in the "sleep" function below
  response_delay <- response_time_get(webpage)
  print(response_delay) # Print the response delay for troubleshooting purposes
  
  # Give the page some time to fully load --> 3 seconds or 10 times longer than response_delay, whichever is greater 
  Sys.sleep(max(3, 10 * response_delay))
  
  # Save the HTML file to an object
  html_first_page <- remDr$getPageSource()[[1]]
  
  # Store the HTML code of the first page into a variable
  first_page_html_code <- read_html(html_first_page)
  
  # Extract the last page's number from the sub-category page
  last_page_num <- first_page_html_code %>% 
    html_nodes(xpath = '//ul[contains(@class, "v-pagination")]/li[last() - 1]') %>% 
    html_text()
  print(last_page_num) # Print the last_page_num for troubleshooting purposes

  # Initialize a variable that contains the HTML code that changes based on the page number. This will be used in the loop below.
  changing_html_code <- first_page_html_code
  
  # Create an empty data frame that will contain the product page URLs
  prod_page_url <- data.frame() 
  
  # Iterate over all sub-category pages
  for(i in 1:2) { # You can also scrape until "last_page_num", but we choose to scrape until page 2 only
    
    # Print the system time to keep track of the function's performance
    print(paste(i, " | ", Sys.time()))
    
    # Extract the URLs of all product pages on the sub-category page that is currently being checked
    prod_urls <- changing_html_code %>% 
      html_nodes(xpath = '//div[@class = "product-card"]/a') %>% # Go to the div tag with class equals "product-card", then go down to its "a" child
      html_attr("href") %>% # Extract the product page URLs of ALL products on the page
      paste0("https://homzmart.com", .) # Append homzmart.com to the webpage title
    
    # Populate the data frame with the product page links of the sub-category ppage that was just scraped
    df_temp <- cbind(prod_urls, page_num = i, sub_mkc_names = sub_mkc, mkc_names = mkc)
    prod_page_url <- rbind(prod_page_url, df_temp)
    
    Sys.sleep(1) # Wait 1 second before entering the for loop
    
    # If the sub-category page contains only one page OR we are at the last page, break out of the loop, otherwise, navigate to the next page
    if (last_page_num == 1 | i == last_page_num) {
      break
    } else {
      # Find the "Next Page" button element
      next_page_element <- remDr$findElements("//li/button[@aria-label = 'Next page']", using = "xpath")
      
      # Click on it
      next_page_element[[1]]$clickElement()
      
      # Calculate the response delay, which will be used in the "sleep" function below
      response_delay_loop <- response_time_get(remDr$getCurrentUrl()[[1]])
      
      Sys.sleep(max(3, response_delay_loop * 10)) # Wait for the new page to fully load. 3 seconds or 10 times longer than response_delay, whichever is greater
      
      changing_html_code <- read_html(remDr$getPageSource()[[1]]) # Get the HTML code of the new page 
    }
  }
  
  # Close the browser and delete the rD variable so that you can re-use the port
  remDr$close()
  rD$server$stop()
  rm(rD, remDr)
  gc()
  system("taskkill /im java.exe /f", intern=FALSE, ignore.stdout=FALSE) # Kill the Java session behind RSelenium
  
  t_end <- Sys.time() # Finishing time of scraping one sub-category
  
  t_duration <- t_end - t_start # Total duration of scraping one sub-category
  
  prod_page_url$tot_scrap_dur <- t_duration # Append the total scraping duration of each sub-category to the results data frame that contains the URLs of the product pages  
  
  # Return the result (i.e., the data frame that contains the URLs of the product pages)
  return(prod_page_url)
}

# Apply the prod_links_func function to the df_scrap data frame to scrape the product page links of ALL sub-categories
# Scrape the product pages from the first two sub-categories only
df_prod_links_raw <- mapply(prod_links_func, "firefox", df_scrap$sub_mkc_urls[c(1,2)], df_scrap$sub_mkc_names[c(1,2)], df_scrap$mkc_names[c(1,2)]) 

# Transpose the output of "mapply" and convert it into a tibble
df_prod_links <- t(df_prod_links_raw) %>% as_tibble()

# Unnest the output of "mapply" into rows
df_prod_links <- df_prod_links %>% 
  unnest(., cols = colnames(df_prod_links))

##----------------------------Separator----------------------------##

# LEFT JOIN the "df_prod_links" data frame to df_scrap
df_scrap <- df_scrap %>% 
  left_join(df_prod_links %>% select(-tot_scrap_dur), by = c("mkc_names", "sub_mkc_names"))

##-------------------------------------------------------END OF Product Link Scraping---------------------------------------------------------##

##--------------------------------------------------START OF Individual Product Page Scraping-------------------------------------------------##

# Create a function that takes an individual product page link from df_scrap, extracts the HTML code, and scrapes all info on it
prod_page_func <- function(url){
  pp_url <- navigate_selenium_func("firefox", url) # Navigate to the individual product page which will be scraped
  
  ## Extract the "General Page Info"
  # Product Display Name
  prod_display_name <- pp_url %>% 
    html_nodes(xpath = '//*[@class = "name"]') %>% 
    html_text(trim = TRUE)
  
  ##-------------------------------------------------------END OF General Page Info-------------------------------------------------------##
  
  ## Merchandising
  # Main image link
  main_img_link <- pp_url %>% 
    html_nodes(xpath = '//div[@class = "zoomer-cont"]/img') %>% # Div with class equals zoomer-cont 
    html_attr("src") %>% # Extract the style attribute from the HTML tag 
    str_extract(., pattern = '(https)(.+)(.jpg)') %>% # Match the part of the strings that has a URL
    ifelse(length(.) == 0, "NA", .) %>% # If character(0), replace with NA. Else, keep the scraped value
    head(1)
  
  # ALL
  all_img_links <- pp_url %>% 
    html_nodes(xpath = '//div[@class = "v-image__image v-image__image--contain"]') %>% # Div with class v-image__image... 
    html_attr("style") %>% # Extract the style attribute from the HTML tag 
    str_extract(., pattern = '(https)(.+)(.jpg)') %>% # Match the part of the strings that has a URL
    paste0(., collapse = " | ") %>% # Collapse the image links into one character vector of length 1
    ifelse(length(.) == 0, "NA", .) # If character(0), replace with NA. Else, keep the scraped value
  
  # Number of Images
  img_num <- pp_url %>% 
    html_nodes(xpath = '//div[@class = "v-image__image v-image__image--contain"]') %>% # Div with class v-image__image... 
    html_attr("style") %>% # Extract the style attribute from the HTML tag 
    length(.) %>% # Count the number of elements in the style attribute
    ifelse(length(.) == 0, "NA", .) # If character(0), replace with NA. Else, keep the scraped value
  
  # Extract the product description
  prod_description <- pp_url %>%
    html_nodes(xpath = '//*[contains(@class, "product-data")]//li | //*[contains(@class, "product-data")]//p') %>% # Select the "p" or "li" descendants of class containing "product-data"
    html_text(trim = TRUE) %>% # Trim white spaces
    stri_remove_empty(.) %>% # Removes all empty strings from a character vector
    paste0(., collapse = " | ") %>% 
    ifelse(length(.) == 0, "NA", .) # If character(0), replace with NA. Else, keep the scraped value
  
  ##-------------------------------------------------------END OF Image Links-------------------------------------------------------##
  
  ## Price Data
  # Extract the current price
  current_price <- pp_url %>% 
    html_nodes(xpath = '//div/h3[@class = "price"]') %>% # div descendant h3 with class = "price"
    html_nodes(xpath = './text()') %>% # Choose text elements only
    html_text(trim = TRUE) %>% # Trim any white spaces
    head(1) # Choose the first element of the resulting vector because the function produces several ones
  
  # Extract the discount tag
  discount_tag <- pp_url %>% 
    html_nodes(xpath = '//div/h3[@class = "price"]/span[@class = "sale"]') %>% # div descendant h3 with class = "price" and child span with class = "sale"
    html_text(trim = TRUE) %>% # Trim any white spaces
    ifelse(length(.) == 0, "NA", .) # If character(0), replace with NA. Else, keep the scraped value
  
  # Extract the original price with a strike-through
  original_price <- pp_url %>% 
    html_nodes(xpath = '//div/h3[@class = "price"]/span[@class = "original-price"]') %>% # div descendant h3 with class = "price" and child span with class = "original-price"
    html_nodes(xpath = './text()') %>% 
    html_text(trim = TRUE) %>% # Trim any white spaces
    ifelse(length(.) == 0, "NA", .) # If character(0), replace with NA. Else, keep the scraped value
  
  ##-------------------------------------------------------END OF Price Data-------------------------------------------------------##
  
  ## Product Info List
  # Extract the name of the vendor
  vendor_name <- pp_url %>% 
    html_nodes(xpath = '//div[@class = "flex"]/ul/li/h3/a') %>% # div with descendants ul, li, h3, a 
    html_text(trim = TRUE) %>% # Trim any white spaces
    ifelse(length(.) == 0, "NA", .) # If character(0), replace with NA. Else, keep the scraped value
  
  # Extract the URL of the vendor on homzmart's website
  vendor_url_homzmart <- pp_url %>% 
    html_nodes(xpath = '//div[@class = "flex"]/ul/li/h3/a') %>% # div with descendants ul, li, h3, a
    html_attr("href") %>% # Grab the URL from the HTML code
    paste0("https://homzmart.com", .) %>% # Append the URL to the constant prefix
    ifelse(length(.) == 0, "NA", .) # If character(0), replace with NA. Else, keep the scraped value
  
  # Extract the number of delivery days
  promised_delivery <- pp_url %>% 
    html_nodes(xpath = '//div/ul/li[h4[contains(text(), "Delivery")]]') %>% # div descendant ul descendant li such that descendant's text contains the word "Delivery"
    html_nodes(xpath = "./text()") %>% # Grab the text portion of the previous tag
    html_text(trim = TRUE) %>% # Trim any white spaces
    str_remove_all(., ':') %>% # Remove the colon and blank spaces
    str_trim() %>% # Remove the blank space at the start or end of the string
    ifelse(length(.) == 0, "NA", .) # If character(0), replace with NA. Else, keep the scraped value
  
  # Extract the availability type
  avail_type <- pp_url %>% 
    html_nodes(xpath = '//div/ul/li[h4[contains(text(), "Available")]]') %>% # div descendant ul descendant li such that descendant's text contains the word "Available"
    html_nodes(xpath = "./text()") %>% # Grab the text portion of the previous tag
    html_text(trim = TRUE) %>% # Trim any white spaces
    str_remove_all(., ':') %>% # Remove the colon and blank spaces
    str_trim() %>% # Remove the blank space at the start or end of the string
    ifelse(length(.) == 0, "NA", .) # If character(0), replace with NA. Else, keep the scraped value
  
  # Extract the dimensions
  dims <- pp_url %>% 
    html_nodes(xpath = '//div/ul/li[h4[contains(text(), "Dimension")]]') %>% # div descendant ul descendant li such that descendant's text contains the word "Dimension"
    html_nodes(xpath = "./text()") %>% # Grab the text portion of the previous tag
    html_text(trim = TRUE) %>% # Trim any white spaces
    str_remove_all(., ':|\\s+') %>% # Remove the colon and blank spaces
    str_trim() %>% # Remove the blank space at the start or end of the string
    ifelse(length(.) == 0, "NA", .) # If character(0), replace with NA. Else, keep the scraped value
  
  # Extract the material used to build the product
  material <- pp_url %>% 
    html_nodes(xpath = '//div/ul/li[h4[contains(text(), "Material")]]') %>% # div descendant ul descendant li such that descendant's text contains the word "Material"
    html_nodes(xpath = "./text()") %>% # Grab the text portion of the previous tag
    html_text(trim = TRUE) %>% # Trim any white spaces
    str_remove_all(., ':') %>% # Remove the colon
    str_trim() %>% # Remove the blank space at the start or end of the string
    ifelse(length(.) == 0, "NA", .) # If character(0), replace with NA. Else, keep the scraped value
  
  # Extract the country of origin
  country_origin <- pp_url %>% 
    html_nodes(xpath = '//div/ul/li[h4[contains(text(), "Made")]]') %>% # div descendant ul descendant li such that descendant's text contains the word "Made"
    html_nodes(xpath = "./text()") %>% # Grab the text portion of the previous tag
    html_text(trim = TRUE) %>% # Trim any white spaces
    str_remove_all(., ':') %>% # Remove the colon
    str_trim() %>% # Remove the blank space at the start or end of the string
    ifelse(length(.) == 0, "NA", .) # If character(0), replace with NA. Else, keep the scraped value
  
  # Extract the SKU name
  sku_name <- pp_url %>% 
    html_nodes(xpath = '//div/ul/li[h4[contains(text(), "SKU")]]') %>% # div descendant ul descendant li such that descendant's text contains the word "SKU"
    html_nodes(xpath = "./text()") %>% # Grab the text portion of the previous tag
    html_text(trim = TRUE) %>% # Trim any white spaces
    str_remove_all(., ':') %>% # Remove the colon
    str_trim() %>% # Remove the blank space at the start or end of the string
    ifelse(length(.) == 0, "NA", .) # If character(0), replace with NA. Else, keep the scraped value
  
  ##-------------------------------------------------------END OF Product Info List-------------------------------------------------------##
  
  # Combine all the scraped info in one data frame
  scraped_data <- data.frame(url, prod_display_name, main_img_link, all_img_links, img_num, prod_description, 
                             current_price, discount_tag, original_price, vendor_name, vendor_url_homzmart,
                             promised_delivery, avail_type, dims, material, country_origin, sku_name)
  
  return(scraped_data)
}

# Loop over all the product links in df_scrap and apply the "prod_page_func" function to each one of them
list_pp_data <- sapply(df_scrap$prod_urls[!is.na(df_scrap$prod_urls)][1:10], FUN = prod_page_func) # Only pull data for 10 product pages

# Transpose the list (switch rows and columns)
list_pp_data <- t(list_pp_data)

# Convert the row names into the first column so that we can join on df_scrap
list_pp_data <- rownames_to_column(as.data.frame(list_pp_data), "prod_urls")

# Join the scraped data to df_scrap
df_scrap <- df_scrap %>% 
  left_join(list_pp_data, by = "prod_urls")

##-------------------------------------------------------END OF Scraping-------------------------------------------------------##
