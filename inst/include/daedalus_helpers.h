// Copyright 2025 Imperial College of Science, Technology and Medicine.
// See repository licence in LICENSE.md.

#pragma once

#include <numeric>
#include <vector>

namespace daedalus {

/// @brief Helpful functions. May be split up later.
namespace helpers {

// NOTE: this function handles up to 3 strata (compartments, age, vax status)
// more general solutions should probably be written in R and pass a vector

/// @brief Get compartments to zero
/// @param seq_data_compartments A sequence of compartments holding data in the
/// single-stratum case.
/// @param n_strata The number of strata; age-groups or other.
/// @param n_vax The number of vaccination groups, or any strata in the third
/// dimension.
/// @return A vector of compartments which
inline std::vector<size_t> zero_which(
    const std::vector<size_t> &seq_data_compartments, const int &n_strata,
    const int &n_vax) {
  std::vector<size_t> i_to_zero;

  std::vector<int> seq_strata(n_strata);
  std::iota(seq_strata.begin(), seq_strata.end(), 1);

  std::vector<int> seq_vax(n_vax);
  std::iota(seq_vax.begin(), seq_vax.end(), 1);

  for (const auto &i : seq_data_compartments) {
    for (const auto &j : seq_strata) {
      for (const auto &k : seq_vax) {
        // cppcheck-suppress useStlAlgorithm
        i_to_zero.push_back(static_cast<size_t>(i * n_strata * k - j));
      }
    }
  }

  return i_to_zero;
}
}  // namespace helpers

}  // namespace daedalus
