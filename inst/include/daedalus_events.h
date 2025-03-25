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

  /// @brief Root-find on time.
  /// @return A lambda function suitable for creating a dust2::event test.
  inline test_type make_time_test(const double value) const {
    auto fn_test = [value](const double t, const double *y) {
      return t - value;
    };

    return fn_test;
  }

  /// @brief Root-find on a state value. Only offering state sum: we probably
  /// won't need other operations.
  /// @return A lambda function suitable for creating a dust2::event test.
  inline test_type make_state_test(const std::vector<size_t> &idx_state,
                                   const double value) const {
    auto fn_test = [idx_state, value](const double t, const double *y) {
      const int size_n = idx_state.size();
      const double sum_state = std::accumulate(y, y + size_n, 0);
      return sum_state - value;
    };

    return fn_test;
  }

  /// @brief Make event action lambda.
  /// @return A lambda suitable as an action in a dust2::event.
  inline action_type make_flag_setter(const size_t &flag,
                                      const double &value) const {
    auto fn_action = [flag, value](const double t, const double sign,
                                   double *y) { y[flag] = value; };

    return fn_action;
  }

  /// @brief Make a dust2::ode::event
  /// @param idx_state_test A vector of indices to access in the state.
  /// @param test A lambda to check for a condition.
  /// @param action A lambda to modify a state flag.
  /// @param root_type Whether the diff is greater or less than 0.
  /// @return An event for the `dust2` framework
  inline dust2::ode::event<double> make_event(
      const std::vector<size_t> &idx_state_test, const test_type test,
      const action_type action,
      const dust2::ode::root_type root_type =
          dust2::ode::root_type::both) const {
    dust2::ode::event<double> event(idx_state_test, test, action, root_type);

    return event;
  }


}  // namespace events
}  // namespace daedalus
