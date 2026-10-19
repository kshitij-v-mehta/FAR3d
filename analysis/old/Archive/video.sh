#!/bin/bash

# Directory containing the images
image_dir="/Users/7yg/ornldev/Codes/KSTAR_Cases/KSTAR_case2/nonlinear_Case2/nonlinear_plots_rerun/"

# Navigate to the image directory
cd "$image_dir" || exit

# Create video from images
ffmpeg -framerate 2.5 -start_number 15 -i 2D_phi%d_plot.png -c:v libx264 -r 10 -pix_fmt yuv420p output_phi_green.mp4
#-framerate 10 sets the input frame rate
#-start_number 15 tells ffmpeg to start processing images from file number 15(we need to make sure they are sequential) 
#-i 2D_phi%d_plot_2.png specifies the input pattern of the image being read
#-c:v libx264: tells ffmpeg to use the H.264video codec for encoding output video (video compression standard)
#-r 30 (output set to 30 frames per second)
#-pix_fmt_yuv420p is a common pixel format for videos, especially playback (uses YUV color space)
#output.mp4 is the name of video output

echo "Video created: $output_video"

