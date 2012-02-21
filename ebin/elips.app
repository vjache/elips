%% -*- mode: erlang; -*-
{application, elips,
 [{description, "An ELIPS library."},
  {vsn, "0.0.1"},
  {modules, [elips,
             elips_transform,
             elips_builder,
             elips_engine,
             elips_sup]},
  {registered, []},
  {build_dependencies, []},
  {env, []},
  {applications, [kernel, stdlib, sasl]}
  ]}.
