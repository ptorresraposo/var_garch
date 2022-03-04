n_refits =50

# Fondo_A -----------------------------------------------------------------

roll_A = vector(mode = 'list', length = n_spec)
for (i in 1:n_spec)
  roll_A[[i]] = ugarchroll(
    spec_A[[i]],
    ret_A,
    n.ahead = 1,
    n.start = 247,
    refit.every = n_refits,
    refit.window = 'moving',
    #   windows.size = 741,
    #   solver = 'nloptr',
       solver = 'solnp',
    #   solver.control = list(tol = 1e-06, delta = 1e-07),
    #   solver.control = list(tol = 1e-12, delta = 1e-13),
    #   solver.control = list(tol = 1e-12, delta = 1e-13, trace = 1),
    #   solver.control = list("solver" = 8),
    #   solver.control = list(trace = 1),
    #   fit.control = list(scale = 1),
    #   cluster = cluster,
    calculate.VaR = TRUE,
    VaR.alpha = c(0.01),
    keep.coef = TRUE
  )

roll_A_norm = vector(mode = 'list', length = 9)
for (i in 37:n_spec)
  roll_A_norm[[i-36]] = ugarchroll(
    spec_A[[i]],
    ret_A,
    n.ahead = 1,
    n.start = 247,
    refit.every = n_refits,
    refit.window = 'moving',
    #   windows.size = 741,
    solver = 'nloptr',
    #   solver = 'solnp',
    #   solver.control = list(tol = 1e-06, delta = 1e-07),
    #   solver.control = list(tol = 1e-12, delta = 1e-13),
    #   solver.control = list(tol = 1e-12, delta = 1e-13, trace = 1),
    solver.control = list("solver" = 8),
    #   solver.control = list(trace = 1),
    #   fit.control = list(scale = 1),
    #   cluster = cluster,
    calculate.VaR = TRUE,
    VaR.alpha = c(0.01),
    keep.coef = TRUE
  )

roll_A_total = vector(mode = 'list', length = 45)

for (i in 1:36) 
  roll_A_total[[i]] <- roll_A[[i]]

for (i in c(1,2,3,4,5,6,7,8,9))
 roll_A_total[[i+36]] <- roll_A_norm[[i]]

# Fondo_C -----------------------------------------------------------------

roll_C = vector(mode = 'list', length = n_spec)
for (i in 1:n_spec) 
  roll_C[[i]] = ugarchroll(
    spec_C[[i]],
    ret_C,
    n.ahead = 1,
    n.start = 247,
    refit.every = n_refits,
    refit.window = 'moving',
    #    windows.size = 741,
    solver = 'nloptr',
    #    solver = 'solnp',
    #   solver.control = list(tol = 1e-06, delta = 1e-07),
    #   solver.control = list(tol = 1e-12, delta = 1e-13),
    #   solver.control = list(tol = 1e-12, delta = 1e-13, trace = 1),
    solver.control = list("solver" = 8),
    #   solver.control = list(trace = 1),
    #   fit.control = list(scale = 1),
    #   cluster = cluster,
    calculate.VaR = TRUE,
    VaR.alpha = c(0.01),
    keep.coef = TRUE
  )

roll_C_norm = vector(mode = 'list', length = 9)
for (i in 37:n_spec)
  roll_C_norm[[i-36]] = ugarchroll(
    spec_C[[i]],
    ret_C,
    n.ahead = 1,
    n.start = 247,
    refit.every = n_refits,
    refit.window = 'moving',
    #   windows.size = 741,
    solver = 'nloptr',
    #   solver = 'solnp',
    #   solver.control = list(tol = 1e-06, delta = 1e-07),
    #   solver.control = list(tol = 1e-12, delta = 1e-13),
    #   solver.control = list(tol = 1e-12, delta = 1e-13, trace = 1),
    solver.control = list("solver" = 8),
    #   solver.control = list(trace = 1),
    #   fit.control = list(scale = 1),
    #   cluster = cluster,
    calculate.VaR = TRUE,
    VaR.alpha = c(0.01),
    keep.coef = TRUE
  )

roll_C_total = vector(mode = 'list', length = 45)
for (i in 1:36) 
  roll_C_total[[i]] <- roll_C[[i]]

for (i in c(1,2,3,4,5,6,7,8,9))
  roll_C_total[[i+36]] <- roll_C_norm[[i]]

# Fondo_E -----------------------------------------------------------------

roll_E = vector(mode = 'list', length = n_spec)
for (i in 1:n_spec) 
  roll_E[[i]] = ugarchroll(
    spec_E[[i]],
    ret_E,
    n.ahead = 1,
    n.start = 247,
    refit.every = n_refits,
    refit.window = 'moving',
    #    windows.size = 741,
    solver = 'nloptr',
    #    solver = 'solnp',
    #   solver.control = list(tol = 1e-06, delta = 1e-07),
    #   solver.control = list(tol = 1e-12, delta = 1e-13),
    #   solver.control = list(tol = 1e-12, delta = 1e-13, trace = 1),
    solver.control = list("solver" = 8),
    #   solver.control = list(trace = 1),
    #   fit.control = list(scale = 1),
    #   cluster = cluster,
    calculate.VaR = TRUE,
    VaR.alpha = c(0.01),
    keep.coef = TRUE
  )

roll_E_norm = vector(mode = 'list', length = 9)
for (i in 37:n_spec)
  roll_E_norm[[i-36]] = ugarchroll(
    spec_E[[i]],
    ret_E,
    n.ahead = 1,
    n.start = 247,
    refit.every = n_refits,
    refit.window = 'moving',
    #   windows.size = 741,
    solver = 'nloptr',
    #   solver = 'solnp',
    #   solver.control = list(tol = 1e-06, delta = 1e-07),
    #   solver.control = list(tol = 1e-12, delta = 1e-13),
    #   solver.control = list(tol = 1e-12, delta = 1e-13, trace = 1),
    solver.control = list("solver" = 8),
    #   solver.control = list(trace = 1),
    #   fit.control = list(scale = 1),
    #   cluster = cluster,
    calculate.VaR = TRUE,
    VaR.alpha = c(0.01),
    keep.coef = TRUE
  )

roll_E_total = vector(mode = 'list', length = 45)
for (i in 1:36) 
  roll_E_total[[i]] <- roll_E[[i]]

for (i in c(1,2,3,4,5,6,7,8,9))
  roll_E_total[[i+36]] <- roll_E_norm[[i]]
