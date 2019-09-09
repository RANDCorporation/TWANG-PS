foo <- read.csv("debug/lalonde.csv")
bar <- read_excel("debug/lalonde.xlsx")

formula <- "treat ~ age + educ + black + hispan + married + nodegree + re74 + re75"

m.foo <- ps(as.formula(formula), foo)
m.bar <- ps(as.formula(formula), bar)
m.bar <- ps(as.formula(formula), as.data.frame(bar))

all.equal(m.foo$balance, m.bar$balance)
all.equal(m.foo$ps, m.bar$ps)
