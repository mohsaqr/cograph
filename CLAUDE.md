# cograph Project Configuration

## Environment

- **Rscript path**: `C:\Program Files\R\R-4.5.0\bin\Rscript.exe`

## Running R Scripts

```powershell
powershell -Command "& 'C:\\Program Files\\R\\R-4.5.0\\bin\\Rscript.exe' script_name.R"
```

## Project Overview

cograph is an R package for modern network visualization. Key functions:
- `splot()` - Base R graphics network plotting
- `soplot()` - Grid/ggplot2-style network plotting

## Common Test Commands

```powershell
# Run a test file
powershell -Command "& 'C:\\Program Files\\R\\R-4.5.0\\bin\\Rscript.exe' test_donut_border.R"

# Load and check package
powershell -Command "& 'C:\\Program Files\\R\\R-4.5.0\\bin\\Rscript.exe' -e 'devtools::load_all(\".\")'"
```
