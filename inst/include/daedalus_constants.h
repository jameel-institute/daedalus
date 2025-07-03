// Copyright 2025 Imperial College of Science, Technology and Medicine.
// See repository licence in LICENSE.md.

#pragma once

#include <vector>

namespace daedalus {

/// @brief Model constants similar to R/constants.R
namespace constants {
// hardcoded as this would only change with model structure changes
// TODO(pratik): consider whether N_VAX_STRATA can be flexible
constexpr int N_EPI_COMPARTMENTS = 7;
constexpr int N_DATA_COMPARTMENTS = 2;
constexpr int N_COMPARTMENTS = N_EPI_COMPARTMENTS + N_DATA_COMPARTMENTS;
constexpr int N_VAX_STRATA = 2;   // not including new vaccinations as a layer
constexpr int i_VAX_STRATUM = 1;  // C++ index, second layer in a tensor

// initial daedalus model specific constants
constexpr int DDL_N_AGE_GROUPS = 4;
constexpr int DDL_N_ECON_GROUPS = 45;

constexpr int iS = 0, iE = 1, iIs = 2, iIa = 3, iH = 4, iR = 5, iD = 6, idE = 7,
              idH = 8;

// Tensor indices for groups (age + econ sector), compartments, and vax strata
// Assumes distinct vaccination pathways
constexpr size_t i_GRPS = 0, i_COMPS = 1, i_VAX_GRPS = 2;

// for C++ < 20
inline const std::vector<size_t> i_DATA_COMPARTMENTS = {7, 8};
inline const std::vector<size_t> seq_DATA_COMPARTMENTS = {8, 9};

// flag positions relative to state size
// NOTE: IPR is continuous value
constexpr size_t i_rel_IPR = 0;
constexpr size_t i_rel_NPI_FLAG = 1;
constexpr size_t i_rel_VAX_FLAG = 2;
constexpr size_t i_rel_SD_FLAG = 3;
constexpr size_t i_rel_HOVFLOW_FLAG = 4;

constexpr size_t i_rel_NPI_START_TIME = 5;
constexpr size_t i_rel_VAX_START_TIME = 6;
constexpr size_t i_rel_SD_START_TIME = 7;
constexpr size_t i_rel_HOVFLOW_START_TIME = 8;

// magic numbers
constexpr double d_mort_multiplier = 1.6;

}  // namespace constants

}  // namespace daedalus
