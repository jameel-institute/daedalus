// Copyright 2025 Imperial College of Science, Technology and Medicine.
// See repository licence in LICENSE.md.

#pragma once

// clang-format off
#include "daedalus_types.h"

#include <functional>
#include <vector>

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
class ddl_response {
  // taken from to dust2::ode
  using test_type = std::function<double(const double, const double *)>;
  using action_type = std::function<void(const double, const double, double *)>;

 public:
  const double time_on, time_off, state_on, state_off;
  const size_t i_flag;
  const std::vector<size_t> i_state_on;
  const size_t i_state_off;

  ddl_response(const double &time_on, const double &time_off,
               const double &state_on, const double &state_off,
               const size_t &i_flag, const std::vector<size_t> &i_state_on,
               const size_t &i_state_off)
      : time_on(time_on),
        time_off(time_off),
        state_on(state_on),
        state_off(state_off),
        i_flag(i_flag),
        i_state_on(i_state_on),
        i_state_off(i_state_off) {}

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

  /// @brief Make a vector of events associated with this NPI.
  /// @return A vector of events to be returned by the events function of a
  /// dust2 ode class.
  inline dust2::ode::events_type<double> make_events() const {
    // 1. launch event by some threshold time
    // 2. end event on time
    // 3. launch event on state threshold
    dust2::ode::event<double> ev_time_on =
        make_event({}, make_time_test(time_on), make_flag_setter(i_flag, 1.0));

    dust2::ode::event<double> ev_time_off =
        make_event({}, make_time_test(time_off), make_flag_setter(i_flag, 0.0));

    dust2::ode::event<double> ev_state_on = make_event(
        {i_state_on}, make_state_test(i_state_on, state_on),
        make_flag_setter(i_flag, 1.0), dust2::ode::root_type::increase);

    return dust2::ode::events_type<double>(
        {ev_time_on, ev_time_off, ev_state_on});
  }
};

/// @brief Flattern multiple vectors of dust2 events into a single events vec.
/// @param events_vecs A vector of event vectors.
/// @return A combined vector of events.
inline dust2::ode::events_type<double> get_combined_events(
    const std::vector<dust2::ode::events_type<double>> &events_vecs) {
  size_t size_n =
      std::accumulate(events_vecs.begin(), events_vecs.end(), 0,
                      [](size_t sum, const auto &v) { return sum + v.size(); });

  dust2::ode::events_type<double> combined_events;
  combined_events.reserve(size_n);

  for (const auto &vec : events_vecs) {
    combined_events.insert(combined_events.end(), vec.begin(), vec.end());
  }

  return combined_events;
}

}  // namespace events
}  // namespace daedalus
