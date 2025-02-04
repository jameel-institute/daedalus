// Copyright 2025 Imperial College of Science, Technology and Medicine.
// See repository licence in LICENSE.md.

#ifndef INST_INCLUDE_DAEDALUS_CONSTANTS_H_
#define INST_INCLUDE_DAEDALUS_CONSTANTS_H_

#include <vector>

namespace daedalus {

/// @brief Model constants similar to R/constants.R
namespace constants {
// hardcoded as this would only change with model structure changes
constexpr int N_EPI_COMPARTMENTS = 7;
constexpr int N_DATA_COMPARTMENTS = 2;
constexpr int N_COMPARTMENTS = N_EPI_COMPARTMENTS + N_DATA_COMPARTMENTS;

constexpr int N_AGE_GROUPS = 4L;
constexpr int N_ECON_SECTORS = 45L;
constexpr int N_GROUPS = N_AGE_GROUPS + N_ECON_SECTORS;
constexpr int iS = 0, iE = 1, iIs = 2, iIa = 3, iH = 4, iR = 5, iD = 6, idE = 7,
              idH = 8;

// for C++ < 20
inline const std::vector<size_t> i_DATA_COMPARTMENTS = {7, 8};
inline const std::vector<size_t> seq_DATA_COMPARTMENTS = {8, 9};
}  // namespace constants

}  // namespace daedalus

#endif  // INST_INCLUDE_DAEDALUS_CONSTANTS_H_
