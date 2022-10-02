# scraping_with_r_selenium_and_rvest
This repo contains a multi-stage R-based script that scrapes a JavaScript-rendered E-commerce website using RSelenium and RVest. It also formats and cleans the data and stores it in a table for analysis purposes.

# 1. Objective of the Project
This is the R version of the Python crawling script I created to crawl the ```homzmart.com/en``` website. For a full overview of the project's aim and outcomes, please check the other [repo](https://github.com/omar-elmaria/python_scrapy_airflow_pipeline/blob/master/README.md). It contains the Python version of this code and all the details about the project.

# 2. Usability and Reproducability
- You need to have **R** and **RStudio** installed before you can run this code on your machine. You can install **R** from this [link](https://cran.r-project.org/bin/windows/base/) and **RStudio** (IDE) from this [link](https://www.rstudio.com/products/rstudio/download/)

- R doesn't need as much installation steps as Python. Simply clone the repo by typing this command in **Git Bash** and run the script right away.
  - ```git clone https://github.com/omar-elmaria/scraping_with_r_selenium_and_rvest.git```
- Please keep in mind that the homzmart website might update its backend at some point in the future, which might potentially make the CSS/XPath selectors used in the script **invalid**. This could cause the script to throw errors.

# 3. Questions?
If you have any questions or wish to build a scraper for a particular use case, feel free to contact me on [LinkedIn](https://www.linkedin.com/in/omar-elmaria/)
