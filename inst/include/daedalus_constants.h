// Copyright 2025 Imperial College of Science, Technology and Medicine.
// See repository licence in LICENSE.md.

#pragma once

#include <vector>

namespace daedalus {

/// @brief Model constants similar to R/constants.R
namespace constants {
// hardcoded as this would only change with model structure changes
constexpr int N_EPI_COMPARTMENTS = 7;
constexpr int N_DATA_COMPARTMENTS = 2;
constexpr int N_COMPARTMENTS = N_EPI_COMPARTMENTS + N_DATA_COMPARTMENTS;
constexpr int N_VAX_STRATA = 3;

constexpr int iS = 0, iE = 1, iIs = 2, iIa = 3, iH = 4, iR = 5, iD = 6, idE = 7,
              idH = 8;

// Tensor indices for groups (age + econ sector), compartments, and vax strata
// Assumes distinct vaccination pathways
constexpr size_t i_GRPS = 0, i_COMPS = 1, i_VAX_GRPS = 2;

// for C++ < 20
inline const std::vector<size_t> i_DATA_COMPARTMENTS = {7, 8};
inline const std::vector<size_t> seq_DATA_COMPARTMENTS = {8, 9};
}  // namespace constants

}  // namespace daedalus
