# Thermal Comfort in the Historic Core: A Comparative Network Analysis of Delft and Xi’an
### Group F 
  - Chaeyeon Moon ( Geomatics)
  - Evangelia Angeliki Palli (Geomatics)
  - Julia Fossa Marques (Geomatics)
  - Prianka Girish Bali (Urbanism)

## Repository Structure

```text
├── scripts/                            # Source code and input/output data
│   ├── data/                           # All required input data
│   ├── output/                         # Generated output files
│   │      └── 01_Report/               # Figures generated for the final report
│   ├── preprocess_neatnet.py           # Preprocessing script 1: Network Simplification
│   ├── preprocess_coins.py             # Preprocessing script 2: Network Consolidation
│   ├── pipeline.R                      # Main analysis (clustering)
│   ├── report_config.R                 # Plot script 1 (for report)
│   ├── report_figure_exports.R         # Plot script 2 (for report)
│   └── report_variable_distribution.R  # Plot script 3 (for report)
├── report.qmd                          # Quarto file of report
├── report_references.bib               # Bibliography list of report
└── styles.css                          # Style sheet for report 
```

## How to Run

Please follow the sequence below to ensure data dependencies are met.

### 1. Preprocessing
Run the preprocessing scripts to prepare the network data. Ensure the output paths point to the `data/` directory.

* **Run `scripts/preprocess_neatnet.py`**
  - It simplifies the raw network data
  - **Note:** Update `INPUT_PATH` (line 6) and `OUTPUT_PATH` (line 24) in the script before running.

* **Run `scripts/preprocess_coins.py`**
  - It consolidates the simplified network data
  - **Note:** Update `INPUT_PATH` (line 7) and `OUTPUT_PATH` (line 10) in the script before running.

### 2. Main Analysis
* **Run `scripts/pipeline.R`** 
  - It executes variable calculation and spatial clustering.

### 3. [Optional] Report Visualization 
The following scripts generate the figures used in the final report:
- `scripts/report_config.R`
- `scripts/report_figure_exports.R`
- `scripts/report_variable_distribution.R`
