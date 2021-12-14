# Download the data set
if (!file.exists("manhattan.xlsx")) {
    library(curl)
    curl::curl_download(
        "https://www1.nyc.gov/assets/finance/downloads/pdf/rolling_sales/rollingsales_manhattan.xlsx",
        "manhattan.xlsx"
    )
}

library(tidyverse)

MINIMUM_SALE_PRICE <- 50000
MAXIMUM_SALE_PRICE <- 50000000

# Read the excel file into memory
df <- readxl::read_excel("manhattan.xlsx", sheet = "Manhattan", skip = 4)
cat("original -> num rows = ", nrow(df), "\n")

# Remove any rows that are duplicates
df <- dplyr::distinct(df)
cat("distinct -> num rows = ", nrow(df), "\n")

# Rename columns to be more R-friendly
df <- dplyr::rename(df, borough = BOROUGH,
                    neighborhood = NEIGHBORHOOD,
                    build_category = "BUILDING CLASS CATEGORY",
                    tax_class = "TAX CLASS AT PRESENT",
                    block = BLOCK,
                    lot = LOT,
                    easement = EASEMENT,
                    build_class = "BUILDING CLASS AT PRESENT",
                    address = ADDRESS,
                    apt_number = "APARTMENT NUMBER",
                    zip = "ZIP CODE",
                    res_units = "RESIDENTIAL UNITS",
                    com_units = "COMMERCIAL UNITS",
                    total_units = "TOTAL UNITS",
                    land_sq_ft = "LAND SQUARE FEET",
                    gross_sq_ft = "GROSS SQUARE FEET",
                    year_built = "YEAR BUILT",
                    tax_class_sale = "TAX CLASS AT TIME OF SALE",
                    build_class_sale = "BUILDING CLASS AT TIME OF SALE",
                    price = "SALE PRICE",
                    date = "SALE DATE")

# Transform the tax class and building class to just use the first character.
# Per the documentation, everything after the first character is sorta fluff
df <- dplyr::mutate(df, build_category = factor(substr(build_category, 1, 2)),
                        build_class = factor(substr(build_class, 1, 1)),
                        build_class_sale = factor(substr(build_class_sale, 1, 1)),
                        tax_class = factor(substr(tax_class, 1, 1)),
                        tax_class_sale = factor(substr(tax_class_sale, 1, 1)),
                        neighborhood = factor(neighborhood),
                        zip = factor(zip))

# Drop columns that we don't need for analysis. Examples - borough, present
# building classes (care more about transaction), specific address is also
# really specific (we probably don't care about things at that level),
# easement is also almost completely empty
df <- dplyr::select(df, neighborhood:build_category, block:lot, zip:date)

# Replace NAs with zeroes where appropriate
df$res_units[is.na(df$res_units)] <- 0
df$com_units[is.na(df$com_units)] <- 0

# Select only the rows that are tax class 1 or 2 (gets us only the residential
# properties, or very small businesses)
df_res <- dplyr::filter(df, tax_class_sale %in% c(1, 2))
cat("residential -> num rows = ", nrow(df_res), "\n")

# Filter rows that are more than 3 units
df_res <- dplyr::filter(df_res, total_units <= 3)

# TODO: Maybe we softImpute these instead?
# Filter rows for sale prices that are unreasonably low
df_res <- dplyr::filter(df_res, price > MINIMUM_SALE_PRICE)
cat("min price -> num rows = ", nrow(df_res), "\n")

# filter out outliers in price
q1_class_1 <- quantile(filter(df_res, tax_class_sale == 1)$price, 0.25)
q3_class_1 <- quantile(filter(df_res, tax_class_sale == 1)$price, 0.75)
iqr_class_1 <- IQR(filter(df_res, tax_class_sale == 1)$price) * 1.5

q1_class_2 <- quantile(filter(df_res, tax_class_sale == 2)$price, 0.25)
q3_class_2 <- quantile(filter(df_res, tax_class_sale == 2)$price, 0.75)
iqr_class_2 <- IQR(filter(df_res, tax_class_sale == 2)$price) * 1.5

df_res_no_outliers <- dplyr::filter(df_res,
                        (tax_class_sale == 1 & ((price > (q1_class_1 - iqr_class_1)) & (price < (q3_class_1 + iqr_class_1)))) |
                        (tax_class_sale == 2 & ((price > (q1_class_2 - iqr_class_2)) & (price < (q3_class_2 + iqr_class_2)))))
cat("no outliers -> num rows = ", nrow(df_res), "\n")

# Set random seed so we get reproducible results
set.seed(42)

# Create violin plots of property prices
library(ggplot2)

df_res %>%
    ggplot(aes(x = tax_class_sale, y = price, color = tax_class_sale)) +
    geom_boxplot(aes(x = tax_class_sale, y = price))
ggsave("boxplot_outliers.png")

df_res_no_outliers %>%
    ggplot(aes(x = tax_class_sale, y = price, color = tax_class_sale)) +
    geom_violin(aes(x = tax_class_sale, y = price))
ggsave("violin_no_outliers.png")

df_res_no_outliers <- dplyr::filter(df_res_no_outliers, tax_class_sale == 1)

df_res_no_outliers <- dplyr::select(df_res_no_outliers,
    neighborhood,
    #build_category,
    #tax_class,
    #block,
    #lot,
    #easement,
    #build_class,
    #address,
    #apt_number,
    #zip,
    res_units,
    com_units,
    #total_units,
    land_sq_ft,
    gross_sq_ft,
    year_built,
    #tax_class_sale,
    #build_class_sale,
    price
    #date
    )

LM <- lm(price ~ ., data = df_res_no_outliers)
summary(LM)

df_norm <- df_res_no_outliers %>% mutate_at(c("land_sq_ft", "gross_sq_ft", "year_built", "price", "res_units", "com_units"),
    ~(scale(.) %>% as.vector))
LM_norm <- lm(price ~ ., data = df_norm)

forest <- randomForest(price ~ ., data = df_res_no_outliers, na.action = na.omit)

forest_norm <- randomForest(price ~ ., data = df_norm, na.action = na.omit)

# Try doing PCA before regression, also do visualizations with it
