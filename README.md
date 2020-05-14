# nba-odds-parse

Convert and clean NBA odds from [sportsbookreviewsonline.com](https://www.sportsbookreviewsonline.com)

1. Download the excel files and place them in a directory. The functions that grab the filenames defaults to `data/`
2. Run the `build.R` script. 

`build.R` calls a `drake` plan and exports the parsed odds to the `data/` directory.

