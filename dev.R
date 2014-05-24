require(devtools)
load_all()

mpt1 <- make_model(mod1)
str(mpt1, 2)

data(rb.fig1.data, package = "MPTinR")

fit(mpt1, rb.fig.data)

fit_nlminb(mpt1, rb.fig1.data[1,], runif(3))

get_start_values(mpt1@bounds, 1)

sum(mpt1@predict(runif(3)))

mpt1@objective(runif(3))

make_model(sdt1)


read_model(mod1)
read_model(sdt1)
read_model(sdt2)

make_model_list(mod1)

predict <- predict_model(unlist())



mod1 <- "# MPT Model from Riefer and Batchelder (1988), Figure 1
# This model consists of only one tree!
p * q * r
p * q * (1-r)
p * (1-q) * r  # blub
p * (1-q) * (1-r) + (1-p)
"

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