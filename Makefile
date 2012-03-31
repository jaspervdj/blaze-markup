################################################################################
# Configuration
################################################################################

GHC = ghc
GHCI = ghci
GHC_FLAGS = -O2 -fforce-recomp -ibenchmarks -isrc -itests

BENCHMARK_FLAGS = --resamples 10000

################################################################################
# Tests
################################################################################

# Run the tests
test:
	$(GHC) $(GHC_FLAGS) -fhpc --make tests/TestSuite.hs
	rm -f TestSuite.tix
	./tests/TestSuite

# HPC
test-hpc:
	hpc markup --destdir=hpc TestSuite

################################################################################
# Benchmarks
################################################################################

benchmark:
	$(GHC) $(GHC_FLAGS) --make -main-is RunHtmlBenchmarks benchmarks/RunHtmlBenchmarks.hs
	./benchmarks/RunHtmlBenchmarks $(BENCHMARK_FLAGS) -u results.csv

benchmark-server:
	$(GHC) $(GHC_FLAGS) --make -threaded -main-is BenchmarkServer doc/examples/BenchmarkServer.lhs

snap-benchmark-server:
	$(GHC) $(GHC_FLAGS) --make -threaded -main-is SnapBenchmarkServer doc/examples/SnapBenchmarkServer.lhs

benchmark-bigtable-non-haskell:
	ruby benchmarks/bigtable/erb.rb
	ruby benchmarks/bigtable/erubis.rb
	php -n benchmarks/bigtable/php.php

# Cleanup
clean:
	rm -rf doc/examples/BenchmarkServer doc/examples/*.hi
	rm -rf benchmarks/HtmlBenchmarks benchmarks/*.hi
	rm -rf Text/Blaze/*.hi Text/Blaze/Html4/*.hi Text/Blaze/Html5/*.hi Text/Blaze/Renderer/*.hi Text/*.hi
	rm -rf Text/Blaze/*.o Text/Blaze/Html4/*.o Text/Blaze/Html5/*.o Text/Blaze/Renderer/*.o Text/*.o
