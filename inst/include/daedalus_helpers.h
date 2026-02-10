// Copyright 2025 Imperial College of Science, Technology and Medicine.
// See repository licence in LICENSE.md.

#pragma once

// clang-format off
#include "daedalus_constants.h"
#include "daedalus_types.h"

#include <RcppEigen.h>
#include <unsupported/Eigen/CXX11/Tensor>

#include <cmath>
#include <numeric>
#include <vector>

// clang-format on

namespace daedalus {

/// @brief Helpful functions. May be split up later.
namespace helpers {

// NOTE: this function handles up to 3 strata (compartments, age, vax status)
// more general solutions should probably be written in R and pass a vector

/// @brief Get compartment idx-es. Primarily used to identify compartments to
/// zero and for events.
/// @param seq_compartments A sequence of compartments holding variables in the
/// single-stratum case. Indexing must begin at 1.
/// @param n_strata The number of strata; age-groups or other.
/// @param n_vax The number of vaccination groups, or any strata in the third
/// dimension.
/// @return A vector of compartments which
inline std::vector<size_t> get_state_idx(
    const std::vector<size_t> &seq_compartments, const int &n_strata,
    const int &n_vax) {
  std::vector<size_t> idx;

  const size_t stride = n_strata * daedalus::constants::N_COMPARTMENTS;

  std::vector<int> seq_strata(n_strata);
  std::iota(seq_strata.begin(), seq_strata.end(), 1);

  std::vector<int> seq_vax(n_vax);
  std::iota(seq_vax.begin(), seq_vax.end(), 1);

  for (const auto &i : seq_compartments) {
    const size_t max_index = i * n_strata;
    for (const auto &j : seq_strata) {
      for (const auto &k : seq_vax) {
        // cppcheck-suppress useStlAlgorithm
        idx.push_back(static_cast<size_t>(max_index + stride * (k - 1) - j));
      }
    }
  }

  return idx;
}

/// @brief Process severity parameters to define a death death, based on the
/// HFR and lengths of hospital stay of the pathogen being simulated, as
/// specified in `daedalus.data`.
inline daedalus::types::TensorMat<double> get_omega(
    const daedalus::types::TensorMat<double> &hfr,
    const double &gamma_H_recovery, const double &gamma_H_death) {
  const double thD = 1.0 / gamma_H_death;
  const double thR = 1.0 / gamma_H_recovery;

  const daedalus::types::TensorMat<double> t_hosp =
      hfr * thD + (1.0 - hfr) * thR;
  const daedalus::types::TensorMat<double> omega = hfr / t_hosp;

  return omega;
}

/// @brief Process severity parameters to define a death death, based on the
/// HFR and lengths of hospital stay of the pathogen being simulated, as
/// specified in `daedalus.data`.
inline daedalus::types::TensorMat<double> get_gamma_H(
    const daedalus::types::TensorMat<double> &hfr,
    const double &gamma_H_recovery, const double &gamma_H_death) {
  const double thD = 1.0 / gamma_H_death;
  const double thR = 1.0 / gamma_H_recovery;

  const daedalus::types::TensorMat<double> t_hosp =
      hfr * thD + (1.0 - hfr) * thR;
  const daedalus::types::TensorMat<double> gamma_H = (1.0 - hfr) / t_hosp;

  return gamma_H;
}

/// @brief Get a scaled vaccination rate to ensure that initial vaccination rate
/// is maintained as the number of eligible individuals decreases (doses remain
/// constant).
/// @param state A tensor map of state values.
/// @param nu The initial daily vaccination rate as a prportion of the total
/// population.
/// @param pop_size Total population size.
/// @param n_strata Total number of strata: age + econ groups.
/// @param a A scaling parameter that controls the smoothness of a sigmoid
/// function that scales vaccination rate as a function of proportion
/// vaccinated.
/// @param b A second parameter that offsets the uptake limit so that the
/// sigmoid function for vax rate is approximately zero at the uptake limit.
/// @return The scaled vaccination rate.
inline double scale_nu(
    const Eigen::TensorMap<const daedalus::types::TensorAry<double>> &state,
    const double &nu, const double &pop_size, const int &n_strata) {
  // vaccinated stratum slice dims
  Eigen::array<Eigen::Index, 3> offsets = {0, 0, constants::i_VAX_STRATUM};
  Eigen::array<Eigen::Index, 3> extent = {n_strata,
                                          constants::N_EPI_COMPARTMENTS, 1};

  // get total and proportion vaccinated
  const Eigen::Tensor<double, 0> t_total_vax =
      state.slice(offsets, extent).sum();
  const double total_vax = t_total_vax(0);
  const double prop_vax = total_vax / pop_size;

  // NOTE: scale vaccination rate using a sigmoid function around the uptake
  // limit for a smoother transition
  const double scaled_nu = nu / (1.0 - prop_vax);

  return scaled_nu;
}

/// @brief Get the largest eigenvalue of a matrix using power iteration.
/// More efficient than full eigenvalue decomposition when only the leading
/// eigenvalue is needed. Uses warm-starting from previous iteration vector.
/// @param m A matrix.
/// @param v Iteration vector, modified in place for warm-starting next call.
/// @param n_iter Number of power iterations (default 10).
/// @return The leading eigenvalue (by magnitude).
inline double get_leading_eigenvalue(const Eigen::MatrixXd &m,
                                     Eigen::VectorXd &v,
                                     const int n_iter = 10) {
  // Power iteration: repeatedly multiply by matrix and normalize
  for (int i = 0; i < n_iter; ++i) {
    v = m * v;
    v.normalize();
  }
  // Rayleigh quotient gives the eigenvalue
  return v.dot(m * v);
}

/// @brief Count the number of individuals in a compartment by age.
/// @param state An Eigen Tensor of the state.
/// @param idx_compartment An integer for the compartment.
/// @return An array of susceptibles per age group.
inline const Eigen::ArrayXd get_comp_age(
    const daedalus::types::TensorAry<double> &state,
    const size_t &idx_compartment) {
  // Sum across vaccination strata to get per-group totals
  const daedalus::types::TensorVec<double> t_x_comp =
      state.chip(idx_compartment, daedalus::constants::i_COMPS)
          .sum(Eigen::array<Eigen::Index, 1>{1});

  // Map tensor data directly to avoid allocation and element-wise copying
  const Eigen::Map<const Eigen::ArrayXd> comp(t_x_comp.data(),
                                              t_x_comp.dimension(0));

  // Sum economic sector groups into working-age group
  const double tail_sum =
      comp.tail(daedalus::constants::DDL_N_ECON_GROUPS).sum();

  // Build result: age groups 0, 1, 3 directly; age group 2 includes econ
  // sectors
  Eigen::ArrayXd comp_age(daedalus::constants::DDL_N_AGE_GROUPS);
  comp_age << comp(0), comp(1), comp(2) + tail_sum, comp(3);

  return comp_age;
}
}  // namespace helpers

}  // namespace daedalus
