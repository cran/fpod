## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(fpod)
fn <- fp_example("gullars_period1.FP3") # <- example FP3 file

## -----------------------------------------------------------------------------
fpod_files <- rep(fn, 5) # simulate 5 FPOD files
basename(fpod_files)


## -----------------------------------------------------------------------------
#fpod_files <- list.files("/users/andre/projects/fpod_troms/data", pattern = "FP3$", full = TRUE, recursive = TRUE)

## -----------------------------------------------------------------------------
dat <- lapply(fpod_files, fp_read)
str(dat, 2)

## -----------------------------------------------------------------------------
dat <- vector(mode = "list", length = length(fpod_files))
for (i in 1:length(fpod_files)) {
    dat[[i]] <- fp_read(fpod_files[[i]])
}
str(dat, 2)

## -----------------------------------------------------------------------------
library(data.table)
nbhf <- lapply(dat, function(x) {
    x$clicks[species == "NBHF"]
}) |> rbindlist()

## -----------------------------------------------------------------------------
nbhf[, 1:7] # show only first 7 cols for brevity

## -----------------------------------------------------------------------------
nbhf <- lapply(dat, function(x) {
    clicks <- x$clicks[species == "NBHF"]
    if (nrow(clicks) == 0) {
        clicks <- clicks[0L]
    }
    clicks
}) |> rbindlist()

## -----------------------------------------------------------------------------
dpm <- lapply(dat, function(x) {
    nbhf <- x$clicks[species == "NBHF"]
    dolphins <- x$clicks[species == "OtherCet"]
    sonar <- x$clicks[species == "Sonar"]
    nbhf$buzz <- fp_find_buzzes(nbhf)
    nbhf_dpm <- fp_summarize(nbhf)
    dol_dpm <- fp_summarize(dolphins)
    sonar_dpm <- fp_summarize(sonar)

    # checks to handle cases of no detections for each category
    if (all(is.na(nbhf_dpm$pod))) nbhf_dpm[, pod := x$header$pod_id]
    if (all(is.na(dol_dpm$pod))) dol_dpm[, pod := x$header$pod_id]
    if (all(is.na(sonar_dpm$pod))) sonar_dpm[, pod := x$header$pod_id]
    
    dpm <- merge(nbhf_dpm, dol_dpm, by = c("pod", "time"), suffix = c("", "_dol"))
    dpm <- merge(dpm, sonar_dpm, by = c("pod", "time"), suffix = c("", "_sonar"))
    dpm[, -c("bpm_dol", "bpm_sonar")]
}) |> rbindlist()
dpm

