// Copyright 2025 Imperial College of Science, Technology and Medicine.
// See repository licence in LICENSE.md.

#pragma once

// clang-format off
#include "daedalus_types.h"

#include <functional>

#include <dust2/common.hpp>
// clang-format on

namespace daedalus {

namespace events {

/// @brief Get values depending on a flag variable
/// @tparam T
/// @param value A value, typically of the opennness coefficient. For an 80%
/// closure of a sector, the openness coefficient would be 0.2.
/// @param flag Either 0.0 or 1.0.
/// @return Either `value` when `flag` = 1.0, or 1.0 when `flag` = 0.0.
template <typename T>
inline T switch_by_flag(T value, const double flag) {
  return (1.0 - (1.0 - value) * flag);
}

/// @brief Class holding NPI related information. Intended to live inside
/// `shared`.
class npi {
  // similar to dust2::ode
  using test_type = std::function<double(const double, const double *)>;
  using action_type = std::function<void(const double, const double, double *)>;

 public:
  const int t_start, t_end, i_flag;

  npi(const int &t_start, const int &t_end, const size_t &i_flag)
      : t_start(t_start), t_end(t_end), i_flag(i_flag) {}

  /// @brief A function factory to make event test lambdas.
  /// @return A lambda function suitable for creating a dust2::event test.
  inline test_type make_event_test(
      dust2::ode::root_type root_type = dust2::ode::root_type::both) const {
    auto fn_test = [t_start = this->t_start, t_end = this->t_end](
                       const double t, const double *y) {
      // return 0 if t_start or t_end are hit
      if (std::abs(t - t_start) < 1e-6) {
        return 0.0;
      } else if (std::abs(t - t_end) < 1e-6) {
        return 0.0;
      } else {
        return 1.0;
      }
    };

    return fn_test;
  }

  /// @brief A function factory to make event action lambdas.
  /// @return A lambda suitable as an action in a dust2::event.
  inline action_type make_event_action() const {
    auto fn_action = [i_flag = this->i_flag](const double t, const double sign,
                                             double *y) { y[i_flag] = 1.0; };

    return fn_action;
  }
};

}  // namespace events
}  // namespace daedalus
