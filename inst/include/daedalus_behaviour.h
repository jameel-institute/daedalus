// Copyright 2025 Imperial College of Science, Technology and Medicine.
// See repository licence in LICENSE.md.

#pragma once

// clang-format off
#include <cmath>
#include <vector>

#include "daedalus_constants.h"

#include "cpp11.hpp"
#include <dust2/common.hpp>

// clang-format on

namespace daedalus {
namespace behaviour {

/// @brief Original method of calculating public concern based on assumed rate
/// at which concern grows for each additional new death and a lower limit to
/// contacts.
/// @param new_deaths Number of new deaths at time = t, assumed to be daily.
/// @param rate The rate at which concern grows for each additional daily death.
/// @param lower_limit The lower limit of the concern coefficient.
/// @return The scaling factor to reduce transmission rate.
inline double scale_beta_old(const double &new_deaths,
                             const double &rate = 0.001,
                             const double lower_limit = 0.2) {
  return std::pow(1.0 - rate, new_deaths) * (1.0 - lower_limit) + lower_limit;
}

/// @brief New method of calculation public concern to scale transmission, based
/// on assumed optimism and responsiveness.
/// @param total_hosp The number of daily new hospitalisations.
/// @param delta Effectiveness of protective behaviour. Note that values are
/// expected to be [0, 1], with 0 = no effect, 1 = fully effective.
/// @param optimism Baseline optimism about the epidemic.
/// @param k0 A scaling factor used to get a nice sigmoidal curve for the
/// p_behav ~ optimism relationship.
/// @param k1 Scaling factor for optimism.
/// @param k2 Responsiveness to the epidemic signal (new hospitalisations).
/// @return The scaling factor to reduce transmission rate.
inline double scale_beta_new(const double &total_hosp, const double &delta,
                             const double &optimism, const double &k0,
                             const double &k1, const double &k2) {
  // proportion taking protective behaviour;
  // p_t = 1 / (1 + e^(-(k0 + k1 * B + k2 * hosp / hosp_capacity)))
  // NOTE: k1 is a negative value
  const double p_behav =
      1.0 / (1.0 + std::exp(-(k0 + (k1 * optimism) + (k2 * (total_hosp)))));

  // scaling factor B =
  // p_t*delta*(p_t*delta + (1 - p_t)) + (1 - p_t)*(p_t*delta + (1 - p_t))
  // which simplifies to (p_t*delta + (1 - p_t))^2
  return std::pow((p_behav * (1.0 - delta)) + (1.0 - p_behav), 2.0);
}

/// @brief Get the correct scaling function.
/// @param behav_enum An int for the behavioural module. 0 for none, 1 for old,
/// 2 for new.
/// @param behav_params A vector of parameters. Currently relies on position to
/// access the correct parameters.
/// @return A function that captures behavioural parameters for the specified
/// behavioural model and operates on an epidemic signal.
inline const std::function<double(double)> get_behav_fn(
    const int behav_enum, const std::vector<double> &behav_params) {
  std::function<double(double)> behav_fn;

  switch (behav_enum) {
    case 0: {
      // double argument included to avoid using optional, mostly for uniformity
      behav_fn = [](double x = 1.0) { return 1.0; };
      break;
    }

    case 1: {
      behav_fn = [behav_params](double x) {
        return scale_beta_old(x, behav_params[0], behav_params[1]);
      };
      break;
    }

    case 2: {
      behav_fn = [behav_params](double x) {
        return scale_beta_new(x, behav_params[0], behav_params[1],
                              behav_params[2], behav_params[3],
                              behav_params[4]);
      };
      break;
    }

    default: {
      cpp11::stop("Error in behaviour function generation!");
    }
  }

  return behav_fn;
}

/// @brief A wrapper function around a switch that applies the behavioural
/// scaling function to the correct epidemic signal.
/// @param behav_enum Which behavioural model is chosen.
/// @param behav_fn The behavioural scaling function (ideally corresponding to
/// the correct behav_enum).
/// @param new_deaths Epidemic signal for the old behavioural model.
/// @param total_hosp Epidemic signal for the new behavioural model.
/// @return The scaling applied to the transmission rate, in the range [0, 1].
inline const double get_behav_scaling(
    const int &behav_enum, const std::function<double(double)> &behav_fn,
    const double &new_deaths, const double &total_hosp) {
  double behav_scaling = 1.0;
  switch (behav_enum) {
    case 0: {
      break;
    }

    case 1: {
      behav_scaling = behav_fn(new_deaths);
      break;
    }

    case 2: {
      behav_scaling = behav_fn(total_hosp);
      break;
    }
  }

  return behav_scaling;
}

}  // namespace behaviour

}  // namespace daedalus
