
test_that(
  "Experimental TreeBUGS support: Latent-trait approach"
  , {
    skip_on_cran()
    skip_on_travis()
    set.seed(12305L)
    EQNfile <- system.file("MPTmodels/2htsm.eqn", package="TreeBUGS")
    d.encoding <- subset(TreeBUGS::arnold2013, group == "encoding", select = -(1:4))
    fit <- TreeBUGS::traitMPT(EQNfile, d.encoding, n.thin=5,
                    restrictions=list("D1=D2=D3","d1=d2","a=g"), n.iter = 4e3L)

    summary_trait <- summary(fit)

    list(
      "Parameter means" = apa_print(summary_trait, "mean")
      , "Parameter correlations" = apa_print(summary_trait, "rho")
    )
  }
)
