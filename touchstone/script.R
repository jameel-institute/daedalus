# see `help(run_script, package = 'touchstone')` on how to run this
# interactively

# TODO OPTIONAL Add directories you want to be available in this file or during the
# benchmarks.
# touchstone::pin_assets("some/dir")

# installs branches to benchmark
touchstone::branch_install()

# benchmark daedalus::daedalus()
touchstone::benchmark_run(
  test_daedalus = daedalus::daedalus("GBR", "influenza_2009", "elimination"),
  n = 10
)

# benchmark daedalus::daedalus_rtm()
touchstone::benchmark_run(
  test_daedalus_rtm = daedalus::daedalus_rtm(
    "GBR", "influenza_2009", "elimination",
    10, 90
  ),
  n = 10
)

# create artifacts used downstream in the GitHub Action
touchstone::benchmark_analyze()
