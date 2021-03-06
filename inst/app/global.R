## sourcing from radiant.data
library(tm)
library(wordcloud)
options(radiant.path.data = system.file(package = "radiant.data"))
source(file.path(getOption("radiant.path.data"), "app/global.R"), encoding = getOption("radiant.encoding", default = "UTF-8"), local = TRUE)

ifelse (grepl("radiant.basics", getwd()) && file.exists("../../inst") , "..", system.file(package = "radiant.basics")) %>%
  options(radiant.path.basics = .)

## setting path for figures in help files
addResourcePath("figures_basics", "tools/help/figures/")

## loading urls and ui
source("init.R", encoding = getOption("radiant.encoding"), local = TRUE)
options(radiant.url.patterns = make_url_patterns())
