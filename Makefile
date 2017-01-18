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
	# the -fsimpl-tick-factor=115 eliminates a GHC error (sent bug report to GHC HQ)
	$(GHC) $(GHC_FLAGS) --make -main-is RunHtmlBenchmarks -fsimpl-tick-factor=115 benchmarks/RunHtmlBenchmarks.hs
	./benchmarks/RunHtmlBenchmarks $(BENCHMARK_FLAGS) -o report.html

benchmark-bigtable-non-haskell:
	ruby benchmarks/bigtable/erb.rb
	ruby benchmarks/bigtable/erubis.rb
	php -n benchmarks/bigtable/php.php
