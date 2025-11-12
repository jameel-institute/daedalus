# Represent behaviour-change mechanisms in DAEDALUS

Helper functions to prepare `<daedalus_behaviour>` class responses
corresponding to either old or new behavioural change mechanisms.
Inherits from `<daedalus_response>` to allow for more control in future
over when these mechanisms are active.

## Usage

``` r
daedalus_old_behaviour(rate = 0.001, lower_limit = 0.2)

daedalus_new_behaviour(
  hospital_capacity,
  behav_effectiveness = 0.5,
  baseline_optimism = 0.5,
  responsiveness = 1.5,
  k0 = 4.59,
  k1 = -9.19
)

is_daedalus_behaviour(x)

# S3 method for class 'daedalus_behaviour'
print(x, ...)
```

## Arguments

- rate:

  The marginal rate of decrease in the scaling factor for each
  additional daily death in the 'old' behavioural model.

- lower_limit:

  The lower limit of the scaling factor in the 'old' behavioural model;
  prevents scaling factor from reaching zero.

- hospital_capacity:

  The emergency hospital capacity of a country, but **may also be** a
  `<daedalus_country>` object from which the hospital capacity is
  extracted. See \\\bar H\\ in Details.

- behav_effectiveness:

  A single double value for the effectiveness of adopting behavioural
  measures against the risk of infection. Expected to be in the range
  \\\[0, 1\]\\, where 0 represents no effectiveness, and 1 represents
  full effectiveness. See \\\delta\\ in Details. Defaults to 0.5.

- baseline_optimism:

  A single double value for the baseline optimism about pandemic
  outcomes. Expected to be in the range \\\[0, 1\]\\. See \\\bar B\\ in
  Details. Defaults to 0.5.

- responsiveness:

  A single double value for the population responsiveness to an epidemic
  signal. Must have a lower value of 0, but the upper bound is open. See
  \\k_2\\ in Details. Defaults to 1.5.

- k0:

  A single, optional double value which is a scaling parameter for the
  sigmoidal relationship between \\p_t\\ and `behav_effectiveness`. See
  \\k_0\\ in Details. Defaults to 4.59.

- k1:

  A single, optional double value which is another scaling parameter for
  the sigmoidal relationship between \\p_t\\ and `behav_effectiveness`.
  See \\k_1\\ in Details. Defaults to -9.19. Expected to be a negative
  number.

- x:

  A `<daedalus_behaviour>` object.

- ...:

  Other arguments passed to
  [`format()`](https://rdrr.io/r/base/format.html).

## Value

An object of class `<daedalus_behaviour>` which inherits from
`<daedalus_response>`. This is primarily a list holding behavioural
parameters.

## Details

Daedalus currently supports two behavioural models (and the option to
have neither model active). These models simulate population-level
changes towards adopting behaviours that help reduce the risk of
infection, which might be expected during a major disease outbreak.

In both models, the transmission rate of the infection \\\beta\\ is
reduced for all transmissions by a scaling factor; the models differ in
how the scaling factor is calculated.

**Note that** a major issue with including this in a model run (any
value other than `"off"`) is that it leads to substantially lower
response costs, and generally better health outcomes (lives lost),
**without accounting for any attendant economic or social costs**. As
such, please treat the behavioural models as experimental.

### Deaths-based public concern model

This model existed prior to `daedalus v0.2.25` and is referred to as the
`"old"` behavioural model.

The scaling factor for \\\beta\\ in this model is \$\$ (1 -
\text{rate})^{\Delta D} \times (1 - L) + L \$\$ where \\\text{rate}\\ is
the marginal rate of reduction for each additional death per day
(\\\Delta D\\), and \\L\\ is the lower limit of the scaling factor to
prevent the FOI going to zero.

### Optimism-responsiveness-effectiveness model

This model is referred to as the `"new"` behavioural model, and the
scaling factor for \\\beta\\ is given by

\$\$ \mathcal{B}(p_t, \delta) = p_t(1 - \delta)\[p_t(1 - \delta) + (1 -
p_t)\] + (1 - p_t)\[p_t(1 - \delta) + (1 - p_t)\] \$\$

where \\p_t\\ is the proportion of the population taking protective
behaviour and \\\delta\\ is the effectiveness of the behaviour, both in
the range \\\[0, 1\]\\. When either is zero, \\\beta\\ is not scaled
down.

\\p_t\\ is computed during the model run, and is given by

\$\$ p_t = \frac{1}{1 + \text{exp}\left\\-\left(k_0 + k_1 \bar B + k_2
\frac{H_t}{\bar H}\right)\right\\} \$\$

where \\k_0 = 4.59, k_1 = -9.19, k_2\\ are scaling parameters. \\k_0,
k_1\\ have constant values that cannot be changed by the user, and these
have been chosen to produce a sigmoidal relationship between \\\bar B\\
and \\p_t\\.

\\k_2\\ a constant user-supplied parameter that should be interpreted as
population responsiveness to a signal of epidemic severity \\H_t / \bar
H\\, which is the relative burden on hospital capacity (\\H_t\\:
hospital demand at time \\t\\, and \\\bar H\\: emergency hospital
capacity).

\\\bar B\\ is a constant user-supplied parameter that captures the
baseline population optimism about the outbreak.
