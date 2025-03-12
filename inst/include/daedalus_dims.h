// Copyright 2025 Imperial College of Science, Technology and Medicine.
// See repository licence in LICENSE.md.

#pragma once

#include <RcppEigen.h>

#include "daedalus_constants.h"

namespace daedalus {
namespace dims {

inline const Eigen::array<Eigen::Index, 2> dim_bcast_vax = {
    1, daedalus::constants::N_VAX_STRATA};

inline const Eigen::array<Eigen::IndexPair<int>, 1> dim_product = {
    Eigen::IndexPair<int>(1, 0)};

}  // namespace dims
}  // namespace daedalus
