devtools::load_all()
testthat::test_that(desc, code)

testthat::test_that("I_lim function works", I_lim)
testthat::test_that("T_lim function works", 
                    T_lim(TT = 20, spec_params = c(T_opt = 22, T_min = 4, T_max = 30)))
export(Q_lim)

export(NN)
export(N_int)
export(QQ)

export(MM_uptake)
export(lin_uptake)

export(u_c)
export(u_b)
export(C_t)

export(plot_I_range)
export(plot_N_uptake)
export(plot_T_range)
export(plot_h)
