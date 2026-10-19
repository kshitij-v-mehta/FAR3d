#!/bin/bash

# Directory where your data files are located
DATA_DIR="/Users/7yg/ornldev/Codes/KSTAR_Cases/KSTAR_case2/nonlinear_Case2/nonlinear_plots_rerun"

# Loop over each data file and generate plots
for file in "$DATA_DIR"/2D_phi_zonal*.silo; do
    # Generate the base name for the output file
    base_name=$(basename "$file" .silo)
    echo "$file"
    # Run the VisIt script for each file, passing the filename as an argument
   /Applications/VisIt.app/Contents/Resources/bin/visit -nowin -cli -s visit_script.py  "$file"

    # Save the plots with a unique name
    mv plot.png "${base_name}_plot_zonal_green.png"
    sleep 2 
    pkill -f visit
done

