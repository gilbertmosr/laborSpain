# Build a single R script for the Spanish EPA data pipeline.

Goal:
Download, unzip, process, and output ONLY the final cleaned annual data.table.

Project path:
/workData/workSpace/ptmDrive/postDoc/labSpain/aiAgents

Final output:
Create only one final file:
data/epa_annual.rds

The saved object must be a data.table with exactly these columns:
year, age, status, pop, hour, active

Definitions:
- year: annual year, from 2006 to the latest available EPA year
- age: a provided by data source
- status: Spain born or Foreign born
- pop: total population, using the EPA survey weight
- hour: average work hours among employed persons
- active: activity rate
- Annual values must be computed from quarterly estimates, then averaged across quarters.

Data source:
Use official INE EPA microdata files. Download all available quarterly EPA files from 2006 onward from the INE EPA microdata source. Each downloaded zip may contain an RData file under R/, such as:
R/EPA_2025T4.RData

Processing steps:
1. Download all available quarterly EPA zip files from 2006 onward.
2. For each zip:
   - Extract only the R/*.RData file to a temporary directory.
   - Load the RData file.
   - Use df_micro as the microdata table.
   - Use df_meta only to identify variable meanings if needed.
   - Do not save raw extracted files.
3. Identify the correct variables for:
   - survey weight
   - age
   - work hours
   - labour-force/activity status
   - country or region of birth
4. Build quarterly estimates by year, quarter, age, and status.
5. Aggregate quarterly estimates to annual values.
6. Save only the final annual data.table to:
   data/epa_annual.rds

Restrictions:
- Do not output raw_epa.rds.
- Do not output CSV files.
- Do not keep unzipped files.
- Do not create intermediate data files.
- Do not modify unrelated project files.
- Print only a short final summary: number of rows, years covered, and column names.

Robustness:
- If a quarterly file cannot be downloaded or processed, skip it with a warning.
- If 2026 files are not yet available, continue through the latest available year.
- The script must not be hard-coded to one file such as EPA4T025.zip.

# Code for Descriptive table
Use this agent instruction:

Create R code that builds a summary table with:

Rows:

1. Population
2. Activity rate
3. Work hours
4. % of 55+ 

Columns:

1. Spanish
2. Foreign
3. Share

The table should be structured so that each row can contain values for the years 2005 and 2025. Use a clean data frame or tibble format, with placeholders for values that will be filled later.

The R code should:

* create the table structure
* include columns for indicator, year, spanish, foreigner, and share
* include one row per indicator-year combination
* use NA as placeholders
* optionally display the table using knitr::kable() or gt::gt() for reporting.

# Code for effect table

Create compact R code in `paper.r` that builds `effectMat` from the existing `eMatL` object.

`eMatL` has `year`, `nation`, `ageg`, `comp`, and `value`, where `value` is annual labour-hours. Convert values to millions of FTE workers with:

```r
value / (50 * 40 * 1e6)
```

Rows must be ordered as:

```text
Spanish: stx, wpx, whx, tpop, total
Foreign: stx, wpx, whx, tpop, total
Both:    stx, wpx, whx, tpop, total
```

`Both` aggregates Spanish and Foreign. `total` is the sum of `stx + wpx + whx + tpop`.

Columns must be:

```text
05-25, 05-09, 10-14, 15-19, 20-25
```

Use `year` as the start of each one-year interval:
- `05-25`: `2005:2024`
- `05-09`: `2005:2008`
- `10-14`: `2010:2013`
- `15-19`: `2015:2018`
- `20-25`: `2020:2024`

Round to 2 decimals and preserve negative signs. No file export is needed.


# Code for labour summary

Create R code that builds `labSummary` from the existing annual `data` object.

Columns:
- Spanish
- Foreign
- Combined

Rows:
- Population in 2005
- Population in 2025
- Population change, 2005--2025
- Labour supply in 2005
- Labour supply in 2025
- Labour-supply change, 2005--2025

Requirements:
- Population is expressed in millions.
- Labour supply is expressed in millions of full-time equivalent (FTE) workers.
- Use \(1\) FTE \(= 50 \times 40 = 2{,}000\) annual hours.
- Combined equals Spanish plus Foreign.
- Round all values to 2 decimal places.
