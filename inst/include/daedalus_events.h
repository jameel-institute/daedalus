// Copyright 2025 Imperial College of Science, Technology and Medicine.
// See repository licence in LICENSE.md.

#pragma once

// clang-format off
#include "daedalus_types.h"

#include <R_ext/Arith.h>
#include <functional>
#include <string>
#include <vector>

#include <dust2/common.hpp>
// clang-format on

namespace daedalus {

namespace events {

/// @brief Get values depending on a flag variable
/// @tparam T
/// @param value A value, typically of the openness coefficient. For an 80%
/// closure of a sector, the openness coefficient would be 0.2.
/// @param flag Either 0.0 or 1.0.
/// @return Either `value` when `flag` = 1.0, or 1.0 when `flag` = 0.0.
template <typename T>
inline T switch_by_flag(T value, const double flag) {
  return (1.0 - (1.0 - value) * flag);
}

/// @brief Class holding NPI related information. Intended to live inside
/// `shared`.
class response {
  // taken from to dust2::ode
  using test_type = std::function<double(const double, const double *)>;
  using action_type = std::function<void(const double, const double, double *)>;

 public:
  const std::string name;
  const double time_on, time_off, state_on, state_off;
  const size_t i_flag;
  const std::vector<size_t> i_state_on, i_state_off;

  /// @brief Constructor for a response.
  /// @param name A string for the name, used to generate event names.
  /// @param time_on The time at which the response should start. 0.0 indicates
  /// no response.
  /// @param time_off The time at which the response should end. 0.0 indicates
  /// no response.
  /// @param state_on The state (sum) value at which the response should start.
  /// @param state_off The state (sum) value at which the response should end.
  /// @param i_flag The index of the state variable holding the flag to modify,
  /// indicating whether the response is active (1.0) or not (0.0).
  /// @param i_state_on The indices of the state variables to be summed to
  /// calculate the state value which is compared against `state_on`.
  /// @param i_state_off The indices of the state variables to be summed to
  /// calculate the state value which is compared against `state_off`.
  response(const std::string &name, const double &time_on,
           const double &time_off, const double &state_on,
           const double &state_off, const size_t &i_flag,
           const std::vector<size_t> &i_state_on,
           const std::vector<size_t> &i_state_off)
      : name(name),
        time_on(time_on),
        time_off(time_off),
        state_on(state_on),
        state_off(state_off),
        i_flag(i_flag),
        i_state_on(i_state_on),
        i_state_off(i_state_off) {}

  /// @brief Root-find on time.
  /// @param value The time value to check current time against.
  /// @return A lambda function suitable for creating a dust2::event test.
  inline test_type make_time_test(const double value) const {
    auto fn_test = [value](const double t, const double *y) {
      return t - value;  // time - start_time
    };

    return fn_test;
  }

  /// @brief Root-find on a duration after some time read from state.
  /// @param value The duration to check against, which is added to the
  /// logged/realised start-time to get the value.
  /// @return A lambda function suitable for creating a dust2::event test.
  inline test_type make_duration_test(const size_t &id_state,
                                      const double value) const {
    auto fn_test = [id_state, value](const double t, const double *y) {
      if (y[id_state] > 0.0) {
        return t - (value + y[id_state]);
      } else
        return 1.0;  // prevent (t - value) when event has not launched yet
    };

    return fn_test;
  }

  /// @brief Root-find on a state value. Only offering state sum: we probably
  /// won't need other operations.
  /// @param idx_state The indices at which to sum state.
  /// @param value The value against which to compare the summed state.
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
  inline action_type make_flag_setter(const std::vector<size_t> &flags,
                                      const std::vector<double> &values) const {
    auto fn_action = [flags, values](const double t, const double sign,
                                     double *y) {
      for (size_t i = 0; i < flags.size(); i++) {
        const size_t yi = flags[i];

        // set flag only if not already set
        if (!y[yi]) {
          // set value to time if special value passed
          const double val = values[i];
          if ((val + 999.000) < 1e-6) {
            y[yi] = t;  // it's fine if these are decimal values
          } else {
            y[yi] = values[i];
          }
        }
      }
    };

    return fn_action;
  }

  /// @brief Make a dust2::ode::event
  /// @param name The event name.
  /// @param idx_state_test A vector of indices to access in the state.
  /// @param test A lambda to check for a condition.
  /// @param action A lambda to modify a state flag.
  /// @param root_type Whether the diff is greater or less than 0.
  /// @return An event for the `dust2` framework
  inline dust2::ode::event<double> make_event(
      const std::string &name, const std::vector<size_t> &idx_state_test,
      const test_type test, const action_type action,
      const dust2::ode::root_type root_type =
          dust2::ode::root_type::both) const {
    dust2::ode::event<double> event(name, idx_state_test, test, action,
                                    root_type);

    return event;
  }

  /// @brief Make a vector of events associated with this NPI.
  /// @return A vector of events to be returned by the events function of a
  /// dust2 ode class.
  inline dust2::ode::events_type<double> make_events() const {
    // 1. launch event by some threshold time
    // 2. end event on time
    // 3. launch event on state threshold

    dust2::ode::events_type<double> events;

    if (!ISNA(time_on)) {
      std::string name_ev_time_on = name + "_time_on";
      dust2::ode::event<double> ev_time_on =
          make_event(name_ev_time_on, {}, make_time_test(time_on),
                     make_flag_setter(i_flag, 1.0));

      events.push_back(ev_time_on);
    }

    if (!ISNA(time_off)) {
      std::string name_ev_time_off = name + "_time_off";
      dust2::ode::event<double> ev_time_off =
          make_event(name_ev_time_off, {}, make_time_test(time_off),
                     make_flag_setter(i_flag, 0.0));

      events.push_back(ev_time_off);
    }

    if (!ISNA(state_on)) {
      std::string name_ev_state_on = name + "_state_on";
      dust2::ode::event<double> ev_state_on = make_event(
          name_ev_state_on, i_state_on, make_state_test(i_state_on, state_on),
          make_flag_setter(i_flag, 1.0), dust2::ode::root_type::increase);

      events.push_back(ev_state_on);
    }

    if (!ISNA(state_off)) {
      std::string name_ev_state_off = name + "_state_off";
      dust2::ode::event<double> ev_state_off = make_event(
          name_ev_state_off, {i_state_off},
          make_state_test(i_state_off, state_off),
          make_flag_setter(i_flag, 0.0), dust2::ode::root_type::decrease);

      events.push_back(ev_state_off);
    }

    return events;
  }
};

/// @brief Flatten multiple vectors of dust2 events into a single events vec.
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
