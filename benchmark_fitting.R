
# benachmark rb.fig1 model


mod1 <- "# MPT Model from Riefer and Batchelder (1988), Figure 1
# This model consists of only one tree!
p * q * r
p * q * (1-r)
p * (1-q) * r  # blub
p * (1-q) * (1-r) + (1-p)
"
data(rb.fig1.data, package = "MPTinR")

mpt1 <- make_model(mod1)
mpt2 <- make_model_cmp(mod1)
mpt3 <- make_model_env(mod1)
mpt4 <- make_model_env_cmp(mod1)
mpt5 <- make_model_env2_cmp(mod1)


p1 <- runif(3)

fit_nlminb(mpt1, rb.fig1.data[1,], p1)
fit_nlminb(mpt2, rb.fig1.data[1,], p1)
fit_nlminb(mpt3, rb.fig1.data[1,], p1)
fit_nlminb(mpt4, rb.fig1.data[1,], p1)

all.equal(fit_nlminb(mpt1, rb.fig1.data[1,], p1), fit_nlminb(mpt2, rb.fig1.data[1,], p1))
all.equal(fit_nlminb(mpt1, rb.fig1.data[1,], p1), fit_nlminb(mpt3, rb.fig1.data[1,], p1))
all.equal(fit_nlminb(mpt1, rb.fig1.data[1,], p1), fit_nlminb(mpt4, rb.fig1.data[1,], p1))
all.equal(fit_nlminb(mpt1, rb.fig1.data[1,], p1), fit_nlminb(mpt5, rb.fig1.data[1,], p1))

fit_nlminb(mpt4, c(10, 20, 30, 40), p1)

microbenchmark(
  fit_nlminb(mpt1, rb.fig1.data[1,], p1),
  fit_nlminb(mpt2, rb.fig1.data[1,], p1),
  fit_nlminb(mpt3, rb.fig1.data[1,], p1),
  fit_nlminb(mpt4, rb.fig1.data[1,], p1),
  fit_nlminb(mpt5, rb.fig1.data[1,], p1)
  )


ls.str(mpt4@model_environment)

microbenchmark(
  fit_nlminb(mpt1, rb.fig1.data[1,], runif(3)),
  fit_nlminb(mpt2, rb.fig1.data[1,], runif(3)),
  fit_nlminb(mpt3, rb.fig1.data[1,], runif(3)),
  fit_nlminb(mpt4, rb.fig1.data[1,], runif(3)),
  fit_nlminb(mpt5, rb.fig1.data[1,], runif(3))
  )


### benchmark Bröder & Schütz (2009):

mpt2 <- "Do + (1-Do)*G1
(1-Do)*(1-G1)

(1-Dn)*G1
Dn + (1-Dn)*(1-G1)

Do + (1-Do)*G2
(1-Do)*(1-G2)

(1-Dn)*G2
Dn + (1-Dn)*(1-G2)

Do + (1-Do)*G3
(1-Do)*(1-G3)

(1-Dn)*G3
Dn + (1-Dn)*(1-G3)

Do + (1-Do)*G4
(1-Do)*(1-G4)

(1-Dn)*G4
Dn + (1-Dn)*(1-G4)

Do + (1-Do)*G5
(1-Do)*(1-G5)

(1-Dn)*G5
Dn + (1-Dn)*(1-G5)
"

data(d.broeder, package = "MPTinR")

br1 <- make_model(mpt2)
br2 <- make_model_cmp(mpt2)
br3 <- make_model_env(mpt2)
br4 <- make_model_env_cmp(mpt2)
br5 <- make_model_env2_cmp(mpt2)

id <- sample(1:nrow(d.broeder), 1)
p2 <- runif(7)

fit_nlminb(br1, d.broeder[id,], p2)
fit_nlminb(br5, d.broeder[id,], p2)

all.equal(fit_nlminb(br1, d.broeder[id,], p2), fit_nlminb(br2, d.broeder[id,], p2))
all.equal(fit_nlminb(br1, d.broeder[id,], p2), fit_nlminb(br3, d.broeder[id,], p2))
all.equal(fit_nlminb(br1, d.broeder[id,], p2), fit_nlminb(br4, d.broeder[id,], p2))
all.equal(fit_nlminb(br1, d.broeder[id,], p2), fit_nlminb(br5, d.broeder[id,], p2))

microbenchmark(
  fit_nlminb(br1, d.broeder[id,], p2),
  fit_nlminb(br2, d.broeder[id,], p2),
  fit_nlminb(br3, d.broeder[id,], p2),
  fit_nlminb(br4, d.broeder[id,], p2),
  fit_nlminb(br5, d.broeder[id,], p2)
  )
