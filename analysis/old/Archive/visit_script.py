import sys

# Get the filename from the command-line arguments
file_to_open = sys.argv[1]

# Open the database (your data file)
OpenDatabase(file_to_open)

# Add and draw the plot (adjust according to your needs)
AddPlot("Pseudocolor", "phi")
DrawPlots()

# Save the plot to a file
SaveWindowAttributes = SaveWindowAttributes()
SaveWindowAttributes.outputToCurrentDirectory = 1
SaveWindowAttributes.fileName = "plot"
SaveWindowAttributes.family = 0  # Do not append numbers to filenames
SetSaveWindowAttributes(SaveWindowAttributes)
SaveWindow()
#Delete all active plots 
DeleteAllPlots()
# Close the database
CloseDatabase(file_to_open)
sys.exit(0)
