# Constructor for the \<daedalus_response\> super-class and sub-classes

Constructor for the \<daedalus_response\> super-class and sub-classes

## Usage

``` r
new_daedalus_response(
  name,
  class = response_class_names,
  parameters = list(),
  identifier = NA_character_,
  id_flag = NA_integer_,
  time_on = NA_real_,
  time_off = NA_real_,
  max_duration = NA_real_,
  id_state_on = NA_integer_,
  value_state_on = NA_real_,
  id_state_off = NA_integer_,
  value_state_off = NA_real_,
  root_state_on = 1L,
  root_state_off = -1L,
  id_time_log = NA_integer_
)

response_class_names

is_daedalus_response(x)

validate_daedalus_response(x)
```

## Format

An object of class `character` of length 4.

## Arguments

- name:

  Response name. This will eventually be passed to the name argument in
  the C++ class `daedalus::events::response`. No default for the
  super-class constructor; each sub-class has a fixed name to aid
  identifiability in `dust2` events outputs.

- class:

  The name of the sub-class being created. May be one of "daedalus_npi",
  "daedalus_vaccination", "daedalus_behaviour", or "daedalus_mortality".
  No default argument, users are forced to specify a class when building
  off this constructor.

- parameters:

  Sub-class parameters passed from sub-class constructors. Defaults to
  an empty list to aid development.

- identifier:

  An optional string giving the name of a pre-defined response strategy.
  Sub-class constructors and helper functions provide the names and
  handle cases where the strategy is a custom one.

- id_flag:

  Index for the state-flag that should be changed when this response is
  on or off. Typically auto-calculated by sub-class constructor or
  helper functions; see e.g.
  [`daedalus_vaccination()`](https://jameel-institute.github.io/daedalus/reference/class_vaccination.md).

- time_on:

  Intended to be a numeric vector of start times.

- time_off:

  Intended to be a numeric vector of end times.

- max_duration:

  Intended to be a number for the maximum duration.

- id_state_on:

  Intended to be a numeric vector of compartment indices which are
  checked for roots that when found launch an event. The check is always
  performed on the *sum* of the state at the indices.

- value_state_on:

  Intended to be a numeric vector of roots which when found change a
  switch-like state-flag to 'on'. The check is always performed on the
  *sum* of the state at the indices.

- id_state_off:

  Intended to be a numeric vector of compartment indices which are
  checked for roots that when found end an event.

- value_state_off:

  Intended to be a numeric vector of roots which when found change a
  switch-like state-flag to 'off'.

- root_state_on:

  A single integer, either 1 or -1, for whether state-dependent launch
  is on an increasing or decreasing root, respectively. Defaults to 1
  for an increasing root as this is the most common use case.

- root_state_off:

  A single integer, either 1 or -1, for whether state-dependent end is
  on an increasing or decreasing root, respectively. Defaults to -1 for
  a decreasing root as this is the most common use case.

- id_time_log:

  Intended to be a single number for the state index where the
  start-time of this response is stored. See also
  [`initial_flags()`](https://jameel-institute.github.io/daedalus/reference/initial_flags.md).
  This attribute is intended solely to enable time-limitation on
  state-triggers.

- x:

  For all functions taking `x`, must be an object of class
  `<daedalus_response>`, or an object to be validated as of the class.

## Value

`new_daedalus_response()` returns a `<daedalus_response>`. Since there
is no default value to `class`, each `<daedalus_response>` must also be
of one of the sub-classes returned by the sub-class constructors:

- `new_daedalus_npi()`: `<daedalus_npi>`;

- `new_daedalus_vax()`: `<daedalus_vax>`;

- `new_daedalus_behaviour()`: `<daedalus_behaviour>`;

- [`new_daedalus_hosp_overflow()`](https://jameel-institute.github.io/daedalus/reference/new_daedalus_hosp_overflow.md):
  `<daedalus_hosp_overflow>`. This function is not exposed to users.
