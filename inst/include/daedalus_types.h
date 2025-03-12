// Copyright 2025 Imperial College of Science, Technology and Medicine.
// See repository licence in LICENSE.md.

#pragma once

#include <RcppEigen.h>

#include <unsupported/Eigen/CXX11/Tensor>

namespace daedalus {
namespace types {
// define types for Tensors of known dims
template <typename T>
using TensorVec = Eigen::Tensor<T, 1>;

template <typename T>
using TensorMat = Eigen::Tensor<T, 2>;

template <typename T>
using TensorAry = Eigen::Tensor<T, 3>;

// for dimensions
using bcast_dim_type = Eigen::array<Eigen::Index, 2>;
using prod_dim_type = Eigen::array<Eigen::IndexPair<int>, 1>;
}  // namespace types
}  // namespace daedalus
