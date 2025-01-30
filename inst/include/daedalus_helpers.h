// Copyright 2025 Imperial College of Science, Technology and Medicine.
// See repository licence in LICENSE.md.

#ifndef INST_INCLUDE_DAEDALUS_HELPERS_H_
#define INST_INCLUDE_DAEDALUS_HELPERS_H_

#include <numeric>
#include <vector>

namespace daedalus {

/// @brief Helpful functions. May be split up later.
namespace helpers {

/// @brief Get compartments to zero
/// @param seq_data_compartments A sequence of compartments holding data in the
/// single-stratum case.
/// @param n_strata The number of strata; age-groups or other.
/// @return A vector of compartments which
inline std::vector<size_t> zero_which(
    const std::vector<size_t> &seq_data_compartments, const int &n_strata) {
  std::vector<size_t> i_to_zero;
  std::vector<int> seq_strata(n_strata);
  std::iota(seq_strata.begin(), seq_strata.end(), 1);

  for (const auto &i : seq_data_compartments) {
    for (const auto &j : seq_strata) {
      // cppcheck-suppress useStlAlgorithm
      i_to_zero.push_back(static_cast<size_t>(i * n_strata - j));
    }
  }

  return i_to_zero;
}
}  // namespace helpers

}  // namespace daedalus

#endif  // INST_INCLUDE_DAEDALUS_HELPERS_H_
