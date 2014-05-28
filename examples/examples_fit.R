data(d.broeder, package = "MPTinR")
m.2htm <- "
Do + (1-Do)*G1
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

five_point_mpt <- make_model(m.2htm)

(f1 <- fit(five_point_mpt, d.broeder))
f1$optinfo$optima
f1$check_fits()

(f2 <- fit(five_point_mpt, d.broeder, method="optimx", runs=5, control=list(method = c("nlminb", "bobyqa"), follow.on=TRUE)))
f2$optinfo$optima
f2$check_fits()

f2$optinfo$five_best[[2]]

all.equal(f1$coef, f2$coef, tolerance=0.001)

require(MPTinR)
data(d.broeder, package = "MPTinR")
m.2htm <- system.file("extdata", "5points.2htm.model", package = "MPTinR")
require(microbenchmark)

microbenchmark(
  fit.mpt(d.broeder, m.2htm, fit.aggregated=FALSE),
  fit(five_point_mpt, d.broeder, aggregated=FALSE)
)
