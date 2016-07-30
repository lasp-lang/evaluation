.PHONY: plots check

check: plots

plots: clean transmission contraction divergence partition-divergence

clean:
	rm -rf plots/

transmission:
	./lasp_transmission_plot.sh

contraction:
	./lasp_path_contraction_plot.sh

divergence:
	./lasp_divergence_plot.sh

partition-divergence:
	./lasp_partition_divergence_plot.sh
