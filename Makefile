.PHONY: plots check

check: plots

plots: clean transmission contraction overcounting partition-overcounting

clean:
	rm -rf plots/

transmission:
	./lasp_transmission_plot.sh

contraction:
	./lasp_path_contraction_plot.sh

overcounting:
	./lasp_overcounting_plot.sh

partition-overcounting:
	./lasp_partition_overcounting_plot.sh
