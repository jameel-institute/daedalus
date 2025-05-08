# see `help(run_script, package = 'touchstone')` on how to run this
# interactively

# TODO OPTIONAL Add directories you want to be available in this file or during the
# benchmarks.
# touchstone::pin_assets("some/dir")

# installs branches to benchmark
touchstone::branch_install()

# benchmark daedalus::daedalus2()
touchstone::benchmark_run(
  test_daedalus = daedalus::daedalus("GBR", "influenza_2009", "elimination"),
  n = 10
)

# create artifacts used downstream in the GitHub Action
touchstone::benchmark_analyze()
