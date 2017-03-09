.PHONY: plots check

check: divergence throughput

plots: clean transmission memory contraction overcounting partition-overcounting

clean:
	rm -rf plots/

divergence:
	./lasp_divergence_plot.sh

throughput:
	./lasp_throughput_plot.sh

transmission:
	./lasp_transmission_plot.sh

memory:
	./lasp_memory_plot.sh

contraction:
	./lasp_path_contraction_plot.sh

overcounting:
	./lasp_overcounting_plot.sh

partition-overcounting:
	./lasp_partition_overcounting_plot.sh
