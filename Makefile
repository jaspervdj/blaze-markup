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
	./benchmarks/RunHtmlBenchmarks $(BENCHMARK_FLAGS) -o report.html

benchmark-bigtable-non-haskell:
	ruby benchmarks/bigtable/erb.rb
	ruby benchmarks/bigtable/erubi.rb
	php -n benchmarks/bigtable/php.php
