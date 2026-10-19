for num in `seq 15 35`;do 
	cp "in$num" in 
	num2=$((num+10))
	filename="fs00${num2}z"
	./xplot_init_value_run_zonal "$filename" 2
	cp 2D_phi.silo "2D_phi_zonal$num.silo"
done
