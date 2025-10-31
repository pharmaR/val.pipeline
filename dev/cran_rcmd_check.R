
## This can be rather slow  with a non-local CRAN mirror
## and might fail (slowly) without Internet access in that case.

options(repos = c(CRAN = "https://cloud.r-project.org"))
set.seed(11)  # but the packages chosen will change as soon as CRAN does.
pdb <- CRAN_package_db()
dim(pdb)

## DESCRIPTION fields included:
colnames(pdb)

## Summarize publication dates:
summary(as.Date(pdb$Published))

## Summarize numbers of packages according to maintainer:
summary(lengths(split(pdb$Package, pdb$Maintainer)))

## Packages with 'LASSO' in their Description:
pdb$Package[grepl("LASSO", pdb$Description)]

results <- CRAN_check_results()

## Available variables:
names(results)

## Tabulate overall check status according to flavor:
with(results, table(Flavor, Status))

details <- CRAN_check_details()

## Available variables:
names(details)

## Tabulate checks according to their status:
tab <- with(details, table(Check, Status))
tab

## Inspect some installation problems:
bad <- subset(details,
              ((Check == "whether package can be installed") &
                 (Status != "OK")))
## Show a random sample of up to 6
head(bad[sample(seq_len(NROW(bad)), NROW(bad)), ])

issues <- CRAN_check_issues()
head(issues)

## Show counts of issues according to kind:
table(issues[, "kind"])

## Summarize CRAN check status for 10 randomly-selected packages
## (reusing the information already read in):
pos <- sample(seq_len(NROW(pdb)), 10L)
summarize_CRAN_check_status(pdb[pos, "Package"],
                            results, details, issues)