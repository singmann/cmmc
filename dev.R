require(devtools)
options(error = recover)
options(error = NULL)
load_all()

mod1 <- "# MPT Model from Riefer and Batchelder (1988), Figure 1
# This model consists of only one tree!
p * q * r
p * q * (1-r)
p * (1-q) * r  # blub
p * (1-q) * (1-r) + (1-p)
"

mpt1 <- make_model(mod1)
str(mpt1, 2)
data(rb.fig1.data, package = "MPTinR")

(m1 <- fit(mpt1, rb.fig1.data))
(m2 <- fit(mpt1, rb.fig1.data[1,]))
(m3 <- fit(mpt1, rb.fig1.data, aggregated=FALSE))

#mpt2 <- new("CmmcMod", mpt1)

ls.str(mpt1@model_environment)
ls.str(mpt2@model_environment)


fit_nlminb(mpt2, rb.fig1.data[3,], runif(3))

fit_nlminb(mpt1, rb.fig1.data[1,], runif(3), use_hessian=TRUE)

fit_optimx(mpt1, rb.fig1.data[1,], runif(3), use_hessian=TRUE)

fit_optimx(mpt1, rb.fig1.data[1,], runif(3), use_hessian=TRUE, control = list(method = c("nlminb", "Rcgmin", "bobyqa")))

fit_optimx(mpt1, rb.fig1.data[1,], runif(3), control = list(maxit = 1000, kkt = FALSE))

fit_optimx(mpt1, rb.fig1.data[1,], runif(3), control = list(method = c("Rcgmin")))

require(Rcgmin)
fit_optimx(mpt1, rb.fig1.data[1,], runif(3), control = list(method = c("Rcgmin", "bobyqa")))
fit_optimx(mpt1, rb.fig1.data[1,], runif(3), control = list(all.methods = TRUE))



require(microbenchmark)

p1 <- runif(3)
microbenchmark(
  fit_nlminb(mpt1, rb.fig1.data[1,], p1),
  fit_optimx(mpt1, rb.fig1.data[1,], p1)
  )

get_start_values(mpt1@bounds, 1)

sum(mpt1@predict(runif(3)))

mpt1@objective(runif(3))

make_model(sdt1)


read_model(mod1)
read_model(sdt1)
read_model(sdt2)

make_model_list(mod1)

predict <- predict_model(unlist())





sdt1 <- "
1-pnorm((cr1-mu)/ss)
pnorm((cr1-mu)/ss)

1-pnorm(cr1)
pnorm(cr1)

1-pnorm((cr2-mu)/ss)
pnorm((cr2-mu)/ss)

1-pnorm(cr2)
pnorm(cr2)

1-pnorm((cr3-mu)/ss)
pnorm((cr3-mu)/ss)

1-pnorm(cr3)
pnorm(cr3)

1-pnorm((cr4-mu)/ss)
pnorm((cr4-mu)/ss)

1-pnorm(cr4)
pnorm(cr4)

1-pnorm((cr5-mu)/ss)
pnorm((cr5-mu)/ss)

1-pnorm(cr5)
pnorm(cr5)
"

sdt2 <- "
1-pnorm((cr1-mu)/ss)
pnorm((cr1-mu)/ss)

1-pnorm(cr1)
pnorm(cr1)

1-pnorm((cr2-mu)/ss)
pnorm((cr2-mu)/ss)

1-pnorm(cr2)
pnorm(cr2)

1-pnorm((cr3-mu)/ss)
pnorm((cr3-mu)/ss)

1-pnorm(cr3)
pnorm(cr3)

1-pnorm((cr4-mu)/ss)
pnorm((cr4-mu)/ss)

1-pnorm(cr4)
pnorm(cr4)

1-pnorm((cr5-mu)/ss)
pnorm((cr5-mu)/ss)

1-pnorm(cr5)
pnorm(cr5)
"