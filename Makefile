.PHONY: plots check

check: plots

plots:
	rm -rf plots/
	./lasp_transmission_plot.sh
	./lasp_path_contraction_plot.sh
