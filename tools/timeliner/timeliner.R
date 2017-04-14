# timerliner.R: Process Snabb timeline logs

library(dplyr)
library(stringr)
library(bit64)
library(readr)
library(ggplot2)

# ------------------------------------------------------------
# High-level API functions
# ------------------------------------------------------------

# Load a binary timeline and save summary data for further processing.
summarize_timeline <- function(filename, outdir) {
  save_timeline_summaries(read_timeline(filename), outdir)
}

# Load summary data and save diverse visualizations.
plot_timeline_summary <- function(summarydir, outdir) {
  dir.create(outdir, recursive=T, showWarnings=F)
  # Breaths
  br <- read_rds(file.path(summarydir, "breaths.rds.xz"))
  ggsave(file.path(outdir, "breath_duration.png"),   breath_duration(br))
  ggsave(file.path(outdir, "breath_outliers.png"),   breath_outliers(br))
  ggsave(file.path(outdir, "breath_efficiency.png"), breath_efficiency(br))
  # Callbacks
  cb <- read_rds(file.path(summarydir, "callbacks.rds.xz"))
  ggsave(file.path(outdir, "callback_efficiency.png"), callback_efficiency(cb))
  ggsave(file.path(outdir, "callback_duration.png"), callback_duration(cb))
  ggsave(file.path(outdir, "callback_outliers.png"), callback_outliers(cb))
}

# ------------------------------------------------------------
# Reading and decoding timelines
# ------------------------------------------------------------

# Read a timeline file into a data frame.
read_timeline <- function(filename) {
  tl <- read_binary_timeline(filename)
  tl$numa <- as.factor(tl$numa)
  tl$core <- as.factor(tl$core)
  tl$unixtime <- calculate_unixtime(tl)
  tl$tsc.cycles <- calculate_levels_delta(tl$level, tl$tsc)
  tl$core.cycles <- calculate_levels_delta(tl$level, tl$pmu.corecycles)
  tl$instructions <- calculate_levels_delta(tl$level, tl$pmu.instructions)
  tl$allocation <- calculate_levels_delta(tl$level, tl$heapsize)
  # Sort entries by unix time. Should roughly take care of log wrap-around.
  # See FIXME comment in unixtime() though.
#  tl <- arrange(tl, unixtime)
  tl
}

# Read a timeline file into a tibble.
read_binary_timeline <- function(filename) {
  f <- file(filename, "rb")
  # Read fields
  magic <- readBin(f, raw(), n=8, endian="little")
  version <- readBin(f, "integer", n=2, size=2, endian="little")
  log_bytes <- readBin(f, "integer", n=1, size=4, endian="little")
  strings_bytes <- readBin(f, "integer", n=1, size=4, endian="little")
  # Check compat
  if (!all(magic == c(0x01, 0x00, 0x1d, 0x44, 0x23, 0x72, 0xff, 0xa3))) {
    stop("bad magic number")
  }
  # Handle timeline version evolution
  bytes_per_record <- NA
  if (version[1] == 2) {
    bytes_per_record <- 64
  } else if (version[1] == 3) {
    bytes_per_record <- 88
  } else {
    stop("unrecognized major version")
  }
  seek(f, 64)
  entries <- readBin(f, "double", n=log_bytes/8, size=8, endian="little")
  elem0 = seq(1, log_bytes/8, bytes_per_record/8)
  # Tricky: Second element is integer on disk but double in R
  tmp <- entries[elem0+1]
  class(tmp) <- "integer64"
  entries[elem0+1] <- as.numeric(tmp)
  tl <- tibble(tsc = entries[elem0],
               msgid = bitwAnd(entries[elem0+1], 0xFFFF),
               core = bitwAnd(bitwShiftR(entries[elem0+1], 16), 0xF),
               numa = bitwShiftR(entries[elem0+1], 24),
               arg0 = entries[elem0+2],
               arg1 = entries[elem0+3],
               arg2 = entries[elem0+4],
               arg3 = entries[elem0+5],
               arg4 = entries[elem0+6],
               arg5 = entries[elem0+7])
  if (version[1] == 3) {
    tl$pmu.instructions <- entries[elem0+8]
    tl$pmu.corecycles <- entries[elem0+9]
    tl$heapsize <- entries[elem0+10]
  }
  tl <- na.omit(tl)
  # Read strings
  stringtable <- character(strings_bytes/16) # dense array
  start <- 64+log_bytes
  seek(f, start)
  repeat {
    id <- 1+(seek(f)-start)/16
    s <- readBin(f, "character")
    if (s == "") break;
    stringtable[id] <- s
    seek(f, ceiling(seek(f)/16) * 16) # seek to 16-byte alignment
  }
  # Decode string messages
  messages <- tibble(msgid = 0:(length(stringtable)-1), message = stringtable) %>%
    filter(message != "") %>%
    mutate(summary = str_extract(message, "^[^\n]+"),
           level = as.integer(str_extract(summary, "^[0-9]")),
           event = gsub("^[0-9]\\|([^:]+):.*", "\\1", summary))
  # Combine messages with events
  left_join(tl, messages, by="msgid")
}

# Calculate unix timestamps for each entry.
calculate_unixtime <- function(tl) {
  times <- filter(tl, grepl("got_monotonic_time", event))

  # FIXME: Make sure the delta is taken between two timestamps from
  # the _same CPU core_. If we take the delta between two timestamps
  # whose TSCs are not synchronized (e.g. different NUMA nodes) then
  # we will misestimate the clock speed (maybe even negative...)
  
  if (length(times) < 2) {
    stop("could not calculate unix time: need two timestamps to compare.")  
  } else {

    # Calculate GHz (cycles per nanosecond) from timestamp deltas.
    GHz <- (max(times$tsc)-min(times$tsc)) / (max(times$arg0)-min(times$arg0))
    # Pick an epoch (any will do)
    reftsc <- last(times$tsc)
    reftime <- last(times$arg0)
    # Function from cycles to unix nanoseconds
    unixtime <- function(tsc) {
      reftime + ((tsc - reftsc) / GHz)
    }
    mapply(unixtime, tl$tsc)
  }
}

# Calculate delta values. The delta is taken between the current value
# and the previous value _with the same log level or higher_.
# XXX Explain :).
calculate_levels_delta <- function(levels, values) {
  ref <- as.numeric(rep(NA, 9))
  delta <- function(level, value) {
    delta <- value - ref[level]
    ref[1:level] <<- value
    delta
  }
  mapply(delta, levels, values)
}

# Calculate cycles since log entry of >= level ("lag") for each entry.
calculate_cycles <- function(tl) {
  # reference timestamp accumulator for update inside closure.
  # index is log level and value is reference timestamp for delta.
  ref <- as.numeric(rep(NA, 9))
  tscdelta <- function(level, time) {
    if (is.na(level)) { stop("level na") }
    if (is.na(time)) { stop("time na") }
    delta <- time - ref[level]
    ref[1:level] <<- time
    delta
  }
  mapply(tscdelta, tl$level, tl$tsc)
}

# ------------------------------------------------------------
# Saving CSV summaries of timelines
# ------------------------------------------------------------

# Save R object summaries of a timeline.
save_timeline_summaries <- function(tl, outdir=".") {
  if (!dir.exists(outdir)) { dir.create(outdir, recursive=T) }
  br <- breaths(tl)
  cb <- callbacks(tl)
  save_data(br, file.path(outdir, "breaths.rds.xz"))
  save_data(cb, file.path(outdir, "callbacks.rds.xz"))
}

save_data <- function(data, filename) {
  message("Saving ", filename)
  write_rds(data, filename, compress="xz")
}

# Create a data frame with one row for each breath.
breaths <- function(tl) {
  tl %>% 
    filter(grepl("breath_end|breath_start", event)) %>%
    mutate(breath = arg0,
           total_packets = arg1, total_bytes = arg2, total_ethbits = arg3,
           packets = arg1-lag(arg1), bytes = arg2-lag(arg2), ethbits = arg3-lag(arg3)) %>%
    filter(grepl("breath_end", event)) %>%
    na.omit() %>%
    select(tsc, unixtime, numa, core, heapsize,
           tsc.cycles, core.cycles, instructions, allocation,
           breath, total_packets, total_bytes, total_ethbits, packets, bytes, ethbits)
}

# Create a data frame with one row for each app callback.
callbacks <- function(tl) {
  tl %>% filter(grepl("^app.(pull|push)", event)) %>%
    mutate(inpackets = arg0 - lag(arg0), inbytes = arg1 - lag(arg1),
           outpackets = arg2 - lag(arg2), outbytes = arg3 - lag(arg3)) %>%
    mutate(packets = pmax(inpackets, outpackets), bytes = pmax(inbytes + outbytes)) %>%
    filter(grepl("^app.(pushed|pulled)", event)) %>%
    na.omit() %>%
    select(tsc, unixtime, numa, core,
           tsc.cycles, core.cycles, instructions, allocation,
           event, packets, bytes, inpackets, inbytes, outpackets, outbytes)
}

# ------------------------------------------------------------
# Visualizing the callbacks summary
# ------------------------------------------------------------

breath_outliers <- function(br, cutoff=0.5e6) {
  d <- filter(br, tsc.cycles>cutoff)
  ggplot(d, aes(y = tsc.cycles, x = packets)) +
    scale_y_continuous(labels = scales::comma) +
    geom_point(alpha=0.5, color="blue") +
    labs(title = "Outlier breaths",
           subtitle = paste("Breaths that took more than ", scales::comma(cutoff), " cycles ",
                            "(", scales::percent(nrow(d)/nrow(br)), " of sampled breaths)", sep=""),
           x = "packets processed in engine breath (burst size)")
}

breath_duration <- function(br, cutoff=0.5e6) {
  d <- filter(br, tsc.cycles <= cutoff)
  ggplot(d, aes(y = tsc.cycles, x = packets)) +
    geom_point(color="blue", alpha=0.25, shape=1) +
    geom_smooth(se=F, weight=1, alpha=0.1) +
    scale_y_continuous(labels = scales::comma) +
    labs(title = "Breath duration",
         subtitle = "")
}

breath_efficiency <- function(br, cutoff=5000) {
  nonzero <- filter(br, packets>0)
  d <- nonzero %>% filter(tsc.cycles/packets <= 5000)
  pct <- (nrow(nonzero) - nrow(d)) / nrow(nonzero)
  ggplot(d, aes(y = tsc.cycles / packets, x = packets)) +
    geom_point(color="blue", alpha=0.25, shape=1) +
    geom_smooth(se=F, weight=1, alpha=0.1) +
    labs(title = "Engine breath efficiency",
         subtitle = paste("Processing cost in cycles per packet ",
                          "(ommitting ", scales::percent(pct), " outliers above ", scales::comma(cutoff), " cycles/packet cutoff)",
                          sep=""),
         y = "cycles/packet",
         x = "packets processed in engine breath (burst size)")
}

callback_outliers <- function(cb, cutoff=0.1e6) {
  d <- cb
  ggplot(d, aes(y = tsc.cycles, x = packets)) +
    geom_point(color="blue", alpha=100/nrow(filter(d, tsc.cycles>cutoff)), shape=1) +
    scale_y_log10(labels = scales::comma) +
    annotation_logticks(sides="l") +
    geom_hline(yintercept = cutoff, color = "red", alpha=0.5) +
    theme(aspect.ratio = 1) +
    facet_wrap(~ event)
}

callback_duration <- function(cb, cutoff=0.1e6) {
  d <- filter(cb, tsc.cycles <= cutoff) %>%
        mutate(packets = pmax(inpackets, outpackets))
  pct <- (nrow(cb) - nrow(d)) / nrow(cb)
  ggplot(d, aes(y = tsc.cycles/2000, x = 1)) +
    geom_jitter(color="blue", alpha=0.5, shape=1) +
    scale_y_continuous(labels = scales::comma) +
    theme(aspect.ratio = 1, legend.position="none", axis.text.x = element_text(hjust=.9, angle=45), axis.title.x = element_blank()) +
    facet_wrap(~ event) +
    labs(title = "Callback duration",
         y = "microseconds",
         subtitle = paste("Duration of an app callback in cycles ",
                          "(ommitting ", scales::percent(pct), " outliers above ", scales::comma(cutoff), " cycles/packet cutoff)",
                          sep=""))
}

callback_efficiency <- function(cb, cutoff=0.1e6) {
  nonzero <- cb %>%
        mutate(packets = pmax(inpackets, outpackets)) %>%
        filter(packets > 0)
  d <- filter(nonzero, tsc.cycles <= cutoff)
  pct <- (nrow(nonzero) - nrow(d)) / nrow(nonzero)
  ggplot(d, aes(y = pmin(1000, tsc.cycles/packets), x = packets)) +
    geom_point(color="blue", alpha=0.25, shape=1) +
    geom_smooth(se=F, weight=1, alpha=0.1) +
    theme(aspect.ratio = 1) +
    facet_wrap(~ event) +
    labs(title = "Callback efficiency",
         subtitle = paste("Processing cost in cycles per packet ",
                          "(ommitting ", scales::percent(pct), " outliers above ", scales::comma(cutoff), " cycles/packet cutoff)",
                          sep=""),
         x = "packets processed (trasnmitted or received, whichever is greater) in callback")
}

callback_allocation <- function(cb) {
  ggplot(cb, aes(x=pmin(100,allocation))) +
#    geom_point() +
#    stat_binhex() +
    stat_ecdf() +
#    scale_x_log10() +
    theme(aspect.ratio = 1) +
    scale_y_continuous(labels=scales::percent) +
#    scale_x_continuous(limits = c(1, 100)) +
    facet_wrap(~ event) +
#    scale_y_log10() +
  labs(y = "Percentage of calls that do not allocate more this threshold",
       x = "Thresh")
}
