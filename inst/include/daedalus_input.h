// Copyright 2025 Imperial College of Science, Technology and Medicine.
// See repository licence in LICENSE.md.

#pragma once

#include <algorithm>
#include <string>
#include <vector>

#include "cpp11.hpp"
#include "daedalus_events.h"

namespace daedalus {

/// @brief Namespace with methods for reading in from R.
namespace inputs {

/// @brief Read daedalus_response S3 class from R. Must inherit from R list.
/// @param args The parameter list passed to dust2.
/// @param name A string for the response name.
/// @return A response class object which mirrors the R S3 class
/// daedalus_response.
inline daedalus::events::response read_response(cpp11::list args,
                                                const char *name) {
  cpp11::list this_list = args[name];
  if (this_list == R_NilValue) {
    cpp11::stop("A this_list is expected for '%s'", name);
  } else if (!Rf_inherits(this_list, "daedalus_response")) {
    cpp11::stop("'%s' must inherit from `<daedalus_response>`", name);
  }

  // collect params
  // NOTE: needs to be generalised
  std::string ev_name = "vaccination";

  // only allow single values on all `value_*`
  // no checks as these are handled on the R side
  cpp11::doubles sxp_time_on(this_list["time_on"]);
  const double time_on = sxp_time_on[0];

  cpp11::doubles sxp_duration(this_list["duration"]);
  const double duration = sxp_duration[0];

  cpp11::integers sxp_id_state_on(this_list["id_state_on"]);
  std::vector<size_t> id_state_on(sxp_id_state_on.size());
  std::copy(sxp_id_state_on.begin(), sxp_id_state_on.end(),
            id_state_on.begin());

  cpp11::doubles sxp_value_state_on(this_list["value_state_on"]);
  const double value_state_on = sxp_value_state_on[0];

  cpp11::integers sxp_id_state_off(this_list["id_state_off"]);
  std::vector<size_t> id_state_off(sxp_id_state_off.size());
  std::copy(sxp_id_state_off.begin(), sxp_id_state_off.end(),
            id_state_off.begin());

  cpp11::doubles sxp_value_state_off(this_list["value_state_off"]);
  const double value_state_off = sxp_value_state_off[0];

  cpp11::integers sxp_id_time_log(this_list["id_time_log"]);
  const size_t id_time_log = static_cast<size_t>(sxp_id_time_log[0]);

  cpp11::integers sxp_root_state_on(this_list["root_state_on"]);
  const int root_state_on = sxp_root_state_on[0];

  cpp11::integers sxp_root_state_off(this_list["root_state_off"]);
  const int root_state_off = sxp_root_state_off[0];

  cpp11::integers sxp_id_flag(this_list["id_flag"]);
  const size_t id_flag = static_cast<size_t>(sxp_id_flag[0]);

  daedalus::events::response this_response(
      ev_name, time_on, duration, value_state_on, value_state_off, id_flag,
      id_state_on, id_state_off, root_state_on, root_state_off, id_time_log);

  return this_response;
}

}  // namespace inputs
}  // namespace daedalus
