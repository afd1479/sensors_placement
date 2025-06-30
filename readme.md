# Satellite Positioning & Coverage Analysis for ADS-B Space Receivers

This project supports simulation and visualization of satellite and ground-based ADS-B receivers for optimal coverage analysis.

---

## 1. Getting Satellite Positions (Python Step)

1. Launch **Anaconda**, open **Jupyter Notebook**, and run the `TLE.py` file.
2. This script calculates the positions of **satellites acting as ADS-B receivers**.
3. **You must provide the TLE (Two-Line Element) data** for the satellites you wish to use. The script requires these lines to determine orbital positions.
4. The script includes different use-case scenarios:
   - Single satellite
   - Multiple satellites
   - Single time slot
   - Continuous time slots  
   Select the one that matches your needs.
5. The output file is saved as `.tex` — **rename it to `.csv`**.
6. Import the resulting CSV into R.
7. **Ground receiver positions** are obtained from the **OpenSky Network**, and are shared under their research-use terms.

---

## 2. Initial Steps in R

1. **Install and load all required packages using the `Libraries.R` file**.
2. Start with:
   - `dc_compute.R`
   - `Vis_Sat2.R`
3. Run all functions and set parameters in both files.
4. Compute **Direction Cosine (DC)** values:
   - See the `final_run_examples.R` file for an example covering a specific area.
5. Ensure sensor coordinates are correctly formatted:
   - Ground sensors: `(id, lat, lon)`
   - Space sensors: `(id, lon, alt)`  
   Adjust the order as needed.
6. Use the `fitnessNsens.R` file to calculate the **fitness function**.

---

## 3. Plotting Scripts

The following R scripts are used for visualization:

- `plot_K_Heat_coverage.R`:  
  → K-coverage heatmap (with three distinct columns layout)

- `gen_plots.R`:  
  → Standard K-coverage and GDOP plots

- `plot_Vis_Horizon_Loc.R`:  
  → Sensor locations, horizon distances, and visibility

- `Sat_over_time.R`:  
  → Satellite coverage over time

> **Note:**  
> Adjust `g_info = 75` as it may misrepresent plots when fewer than 4 sensors cover a point. This should reflect how many points are truly covered by ≥4 sensors.

---

## 4. Ground Sensor Optimization (Optional)

To optimize placement of additional **ground sensors**:

1. Generate ~400 candidate ground sensor locations.
2. Append them to the satellite sensor list.
3. Recompute **DC values**.
4. Run the **Genetic Algorithm (GA)**:
   - Ensure the fitness function **includes satellite sensors** in every evaluated population.

---

## License

This project uses satellite and ground receiver data for **research purposes** only. Ground station data provided by the **OpenSky Network**.

---
