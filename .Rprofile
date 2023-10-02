message("starting ", here::here(), "/.Rprofile")
#myPaths = unique(c("/Users/dmorrison/Documents/R/win-library/4.0", "/Users/dmorrison/R/win-library/4.0",
#.libPaths()))
#.libPaths(myPaths)

Sys.setlocale(category = "LC_COLLATE", locale = "en_US.UTF-8")

if (interactive()) {
  suppressMessages(require(devtools)) # loads usethis
  suppressMessages(require(rsconnect)) # loads rsconnect
  suppressMessages(require(dplyr))
  require("pryr") |> suppressMessages()
  suppressMessages(require(lubridate))
  suppressMessages(require(magrittr))
  suppressMessages(require(conflicted))
  suppressMessages(require(reprex))
  
  # credentials::set_github_pat() # key to be able to install_github() private repos
  conflicted::conflict_prefer("filter", "dplyr")
  conflicted::conflict_prefer("lag", "dplyr")
  conflicted::conflict_prefer("summarise", "dplyr")
  conflicted::conflict_prefer("summarize", "dplyr")
  conflicted::conflict_prefer("select", "dplyr")
  conflicted::conflict_prefer("is_in", "magrittr")
}

#source("/Users/dmorrison/Directory/Work/Projects/multiuse/R_settings/Code/rprofile_mac_DM.R")

options(
  digits = 8,
  keep.source.pkgs = TRUE,
  # dplyr.summarise.inform = FALSE,
  License = "MIT + file LICENSE",
  usethis.protocol  = "ssh"
)

message("Ending ", here::here(), "/.Rprofile")
