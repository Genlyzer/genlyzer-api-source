# Default database paths
DB_MOUSE_PATH <- "db/genes_info_mouse.sqlite"
DB_HUMAN_PATH <- "db/genes_info_human.sqlite"
DB_MOUSE_TEST_PATH <- "db/genes_info_mouse_test.sqlite"
DB_HUMAN_TEST_PATH <- "db/genes_info_human_test.sqlite"

# Function to get database path by organism
getDatabasePath <- function(organism) {
  if (organism == "Mus musculus") {
    DB_MOUSE_PATH
  } else if (organism == "Homo sapiens") {
    DB_HUMAN_PATH
  } else {
    stop("Unsupported organism: ", organism)
  }
}

# Function to get test database path by organism
getTestDatabasePath <- function(organism) {
  if (organism == "Mus musculus") {
    DB_MOUSE_TEST_PATH
  } else if (organism == "Homo sapiens") {
    DB_HUMAN_TEST_PATH
  } else {
    stop("Unsupported organism: ", organism)
  }
}

# Function to get production database path by organism
getProductionDatabasePath <- function(organism) {
  if (organism == "Mus musculus") {
    DB_MOUSE_PATH
  } else if (organism == "Homo sapiens") {
    DB_HUMAN_PATH
  } else {
    stop("Unsupported organism: ", organism)
  }
}
