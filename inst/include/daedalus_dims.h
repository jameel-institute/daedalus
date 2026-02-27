// Copyright 2025 Imperial College of Science, Technology and Medicine.
// See repository licence in LICENSE.md.

#pragma once

#include <RcppEigen.h>

#include "daedalus_constants.h"
#include "daedalus_types.h"

namespace daedalus {
namespace dims {

inline const daedalus::types::bcast_dim_type dim_bcast_vax = {
    1, daedalus::constants::N_VAX_STRATA};

inline const daedalus::types::prod_dim_type dim_product = {
    Eigen::IndexPair<int>(1, 0)};

// for Tensor3D.contract(Tensor1D, dims)
inline const daedalus::types::prod_dim_type vec_tensor_dims = {
    Eigen::IndexPair<int>(2, 0)};

}  // namespace dims
}  // namespace daedalus
