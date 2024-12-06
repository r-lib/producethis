#' Execute a deployed project on a schedule
#'
#' For projects that does not run continuously, such as APIs or apps, it may
#' make sense to execute them periodically rather than only once when deployed
#' or manually when you trigger it. This function records an intended execution
#' schedule in the DESCRIPTION file that will be set during deployment. Do note
#' that not all deployment environments support all scheduling settings. If it
#' is deducible what type of deployment you are using the provided schedule will
#' be tested before recording it.
#'
#' @param minute,hour,day,weekday,week,month,year Scheduling times. See Datails
#' on how to set them
#' @param x An interval or time location
#' @param to The end of the schedule range
#' @param step The step size of the schedule range
#'
#' @return
#'
#' @export
use_schedule <- function(minute = NULL, hour = NULL, day = NULL, weekday = NULL, week = NULL, month = NULL, year = NULL) {
  type <- tolower(desc::desc(usethis::proj_path("DESCRIPTION"))$get_field("Type"))
  if (!type %in% c("batch", "report")) {
    cli::cli_abort("Scheduling is only available for batch and report projects")
  }
  #TODO: timezone support
  if (is.character(month)) {
    if (all(nchar(month) == 3)) {
      month[] <- match(tolower(month), tolower(month.abb))
    } else {
      month[] <- match(tolower(month), tolower(month.name))
    }
    if (anyNA(month)) {
      cli::cli_abort("{.arg month} must name one or more valid months")
    }
  }
  if (is.character(weekday)) {
    if (all(nchar(weekday) == 3)) {
      weekday[] <- match(tolower(weekday), tolower(weekday.abb)) - 1
    } else {
      weekday[] <- match(tolower(weekday), tolower(weekday.name)) - 1
    }
    if (anyNA(weekday)) {
      cli::cli_abort("{.arg month} must name one or more valid months")
    }
  }

  minute <- as_schedule_value(minute)
  hour <- as_schedule_value(hour)
  day <- as_schedule_value(day)
  week <- as_schedule_value(week)
  weekday <- as_schedule_value(weekday)
  if (is_schedule_interval(weekday)) {
    cli::cli_abort("{.arg weekday} cannot be an interval")
  }
  month <- as_schedule_value(month)
  year <- as_schedule_value(year)

  desc <- desc::desc(usethis::proj_path("DESCRIPTION"))
  call <- glue::glue(
  "

      list(
        minute = {format(minute)},
        hour = {format(hour)},
        day = {format(day)},
        weekday = {format(weekday)},
        week = {format(week)},
        month = {format(month)},
        year = {format(year)}
      )
  ")
  call_value <- eval(parse_expr(call))
  if (proj_uses_connect()) {
    get_connect_schedule(call_value)
  }
  if (proj_uses_gha()) {
    get_cron_schedule(call_value)
  }
  desc$set("Schedule" = call)
  desc$write()
}

#' @rdname use_schedule
#' @export
at <- function(x = NULL, to = NULL, step = NULL) {
  check_number_whole(to, allow_null = TRUE)
  check_number_whole(step, allow_null = TRUE)
  check_obj(x, \(x) is.null(x) || is_integerish(x) || is_character(x), "NULL or a vector of integers or strings")
  if (length(x) > 1 && (!is.null(to) || !is.null(step))) {
    cli::cli_abort("{.arg to} and {.arg step} can only be used if {.arg x} is a scalar")
  }
  if (is.null(x) && !is.null(to)) {
    cli::cli_abort("{.arg to} cannot be given if {.arg x} is {.val NULL}")
  }
  structure(list(x), to = to, step = step, class = "schedule_at")
}
is_schedule_at <- function(x) inherits(x, "schedule_at")
#' @export
format.schedule_at <- function(x, ...) {
  val <- x[[1]]
  if (!is.null(val) && all(grepl("\\d", val))) val <- as.numeric(val)
  glue::glue("at({deparse(val, 500)}{if (!is.null(attr(x, 'to'))) paste0(', to = ', deparse(attr(x, 'to'))) else ''}{if (!is.null(attr(x, 'step'))) paste0(', step = ', deparse(attr(x, 'step'))) else ''})")
}

#' @rdname use_schedule
#' @export
interval <- function(x) {
  check_obj(x, \(x) is_integerish(x, 1, TRUE), "an integer")
  structure(list(x), class = "schedule_interval")
}
is_schedule_interval <- function(x) inherits(x, "schedule_interval")
#' @export
format.schedule_interval <- function(x, ...) {
  glue::glue("interval({deparse(x[[1]], 500)})")
}

# Connect Helpers --------------------------------------------------------------

get_connect_schedule <- function(schedule) {
  if (length(schedule) == 0) {
    return(NULL)
  }
  if (!is.null(schedule$year)) connect_yearly(schedule)
  else if (!is.null(schedule$month)) connect_monthly(schedule)
  else if (!is.null(schedule$weekday) || !is.null(schedule$week)) connect_weekly(schedule)
  else if (!is.null(schedule$day)) connect_daily(schedule)
  else if (!is.null(schedule$hour)) connect_hourly(schedule)
  else if (!is.null(schedule$minute)) connect_minutely(schedule)
  else NULL
}

connect_yearly <- function(schedule) {
  connect_standard(
    schedule,
    "year",
    get_start_time(schedule[c("minute", "hour", "day", "month")], "year"),
    23L
  )
}

connect_monthly <- function(schedule) {
  start <- get_start_time(schedule[c("minute", "hour")], "month")
  if (is_schedule_interval(schedule$month)) {
    interval <- as.vector(schedule$month)
  } else {
    sequence <- at_as_sequence(schedule$month, 12)
    df <- diff(c(sequence, sequence[1] + max + 1))
    if (length(unique(df)) != 1) {
      cli::cli_abort(c(
        "Posit Connect does not support scheduling at multiple different monthly intervals",
        i = "Change the {.field month} interval to have evenly spaced values"
      ))
    }
    interval <- df[1]
  }
  if (!is.null(schedule$week)) {
    if (!is_schedule_at(schedule$week)) {
      cli::cli_abort("Posit Connect does not support scheduling at both weekly and monthly intervals")
    }
    week <- at_as_sequence(schedule$week, 5)
    if (length(week) != 1) {
      cli::cli_abort("Posit Connect does not support running multiple weeks on a monthly schedule")
    }
    if (is.null(schedule$weekday)) {
      weekday <- start$wday
    } else {
      weekday <- at_as_sequence(schedule$weekday, 6)
      if (length(weekday) != 1) {
        cli::cli_abort("Posit Connect does not support scheduling at multiple different weekdays of a specific week")
      }
    }
    settings <- list(
      type = "dayweekofmonth",
      schedule = list(N = interval, Day = weekday, Week = week)
    )
  } else {
    if (!is.null(weekday)) {
      cli::cli_abort("Posit Connect does not support scheduling on a specific weekday of all weeks with a monthly interval")
    }
    if (is.null(schedule$day)) {
      day <- start$mday
    } else if (is_schedule_at(schedule$day)) {
      day <- at_as_sequence(schedule$day, 31)
      if (length(day) != 1) {
        cli::cli_abort("Posit Connect does not support scheduling on multiple days on a monthly interval")
      }
    } else {
      cli::cli_abort("Cannot schedule at both daily and monthly intervals")
    }
    settings <- list(
      type = "dayofmonth",
      schedule = list(N = interval, Day = day)
    )
  }
  settings$start_time <- as.POSIXct(start)
  finish_schedule(settings)
}

connect_weekly <- function(schedule) {
  start <- get_start_time(schedule[c("minute", "hour")], "week")
  if (!is.null(schedule$day)) {
    cli::cli_abort("Can't use {.field day} schedule in conjunction with weekly scheduling")
  }
  if (is_schedule_interval(schedule$week) && as.vector(schedule$week) != 1) {
    settings <- list(
      type = "week",
      schedule = list(N = as.vector(schedule$week))
    )
    if (!is.null(schedule$weekday)) {
      weekday <- at_as_sequence(schedule$weekday, 6)
      if (length(weekday) != 1) {
        cli::cli_abort("Posit Connect does not support scheduling multiple weekdays with a weekly interval")
      }
      start <- as.POSIXlt(as.POSIXct(start) - (start$wday + 6 - weekday))
    }
  } else if (is.null(schedule$week) || (is_schedule_interval(schedule$week) && as.vector(schedule$week) == 1)) {
    if (is.null(schedule$weekday)) {
      settings <- list(
        type = "week",
        schedule = list(N = as.vector(schedule$week))
      )
    } else {
      weekday <- at_as_sequence(schedule$weekday, 6)
      settings <- list(
        type = "dayofweek",
        schedule = list(Days = as.vector(schedule$weekday))
      )
    }
  } else {
    sequence <- at_as_sequence(schedule$week, max)
    if (length(sequence) != 1) {
      cli::cli_abort("Posit Connect does not support scheduling at multiple different weeks")
    }
    if (is.null(schedule$weekday)) {
      weekday <- start$wday
    } else {
      weekday <- at_as_sequence(schedule$weekday, 6)
      if (length(weekday) != 1) {
        cli::cli_abort("Posit Connect does not support scheduling at multiple different weekdays of a specific week")
      }
    }
    settings <- list(
      type = "dayweekofmonth",
      schedule = list(N = 1, Day = weekday, Week = sequence)
    )
  }
  settings$start_time <- as.POSIXct(start)
  finish_schedule(settings)
}

connect_daily <- function(schedule) {
  start <- get_start_time(schedule[c("minute", "hour")], "day")
  if (is_schedule_interval(schedule$day)) {
    settings <- list(
      type = "day",
      schedule = list(N = as.vector(schedule$day))
    )
  } else {
    sequence <- at_as_sequence(schedule$day, 30)
    if (all.equal(sort(sequence), c(1, 15))) {
      settings <- list(
        type = "semimonth",
        schedule = list(First = TRUE)
      )
    } else if (all.equal(sort(sequence), c(-1, 14))) {
      settings <- list(
        type = "semimonth",
        schedule = list(First = FALSE)
      )
    } else if (length(sequence) == 1) {
      settings <- list(
        type = "dayofmonth",
        schedule = list(N = 1, Day = sequence)
      )
    } else {
      cli::cli_abort(
        "Posit Connect does not support scheduling at multiple different month days except 1st & 15th, or 14th & -1st (last)"
      )
    }
  }
  settings$start_time <- as.POSIXct(start)
  finish_schedule(settings)
}

connect_hourly <- function(schedule) {
  connect_standard(
    schedule,
    "hour",
    get_start_time(schedule["minute"], "hour"),
    23L
  )
}

connect_minutely <- function(schedule) {
  connect_standard(
    schedule,
    "minute",
    get_start_time(list(), "minute"),
    59L
  )
}
connect_standard <- function(schedule, at, start, max) {
  if (is_schedule_interval(schedule[[at]])) {
    settings <- list(
      type = at,
      schedule = list(N = as.vector(schedule[[at]]))
    )
  } else {
    sequence <- at_as_sequence(schedule[[at]], max)
    df <- diff(c(sequence, sequence[1] + max + 1))
    if (length(unique(df)) != 1) {
      cli::cli_abort(c(
        "Posit Connect does not support scheduling at multiple different intervals",
        i = "Change the {.field {at}} interval to have evenly spaced values"
      ))
    }
    start[[if (at == "minute") "min" else at]] <- min(df[1])
    settings <- list(
      type = at,
      schedule = list(N = df[1])
    )
  }
  settings$start_time <- as.POSIXct(start)
  finish_schedule(settings)
}
finish_schedule <- function(settings) {
  settings$start_time <- settings$start_time %||% Sys.time()
  settings$activate <- settings$activate %||% TRUE
  settings$email <- settings$email %||% FALSE
  settings$timezone <- settings$timezone %||% Sys.timezone()
  settings
}
get_start_time <- function(schedule, name) {
  start <- as.POSIXlt(Sys.time())
  if (!is.null(schedule$month)) {
    start$mon <- get_single_time(schedule$month, "month", name)
  }
  if (!is.null(schedule$day)) {
    start$mday <- get_single_time(schedule$day, "day", name)
  }
  if (!is.null(schedule$hour)) {
    start$hour <- get_single_time(schedule$hour, "hour", name)
  }
  if (!is.null(schedule$minute)) {
    start$min <- get_single_time(schedule$minute, "minute", name)
  }
  start
}
get_single_time <- function(x, time, name, call = caller_env()) {
  if (is_schedule_interval(x)) {
    cli::cli_abort("Periodic {time}s when setting {name}s are not supported on Posit Connect", call = call)
  }
  x <- at_as_sequence(x)
  if (length(x) != 1) {
    cli::cli_abort("Multiple {time}s when setting {name}s are not supported on Posit Connect", call = call)
  }
  x
}

# cron Helper ------------------------------------------------------------------

get_cron_schedule <- function(schedule) {
  if (length(schedule) == 0) {
    return(NULL)
  }
  if (!is.null(schedule$week)) {
    cli::cli_warn("Ignoring weekly schedules as it is not supported by cron")
  }
  if (!is.null(schedule$year)) {
    cli::cli_warn("Ignoring yearly schedules as it is not supported by cron")
  }
  cron_schedules <- mapply(function(x, n) {
    at_as_cron(as_schedule_at(x, n))
  }, x = schedule[c("minute", "hour", "day", "month", "weekday")], n = c(60, 24, 30, 12, 1))
  paste(cron_schedules, collapse = " ")
}

at_as_cron <- function(x) {
  from <- x[[1]]
  to <- attr(x, "to")
  step <- attr(x, "step")
  if (length(from) > 1) {
    return(paste0(from, collapse = ","))
  }
  if (is.null(from) && !is.null(to)) {
    # Should not happen but for safety
    from <- 0
  }
  res <- ""
  if (is.null(from)) {
    res <- "*"
  } else if (is.null(to)) {
    res <- as.character(from)
  } else {
    res <- paste0(from, "-", to)
  }
  if (!is.null(step)) {
    res <- paste0(res, "/", step)
  }
  res
}

# at/interval Helpers ----------------------------------------------------------
as_schedule_at <- function(x, length = NULL) {
  if (is_schedule_at(x)) {
    return(x)
  }
  if (is.null(x)) {
    return(at(x))
  }
  if (!is_schedule_interval(x)) {
    cli::cli_abort("{.arg x} must be {.val NULL} or a {.cls schedule_interval} object")
  }
  check_number_whole(length, allow_null = TRUE)
  if (!is.null(length) && !is_integerish(length/x[[1]]) ) {
    cli::cli_abort("The given length {length} isn't exactly divisible by the provided interval ({x[[1]]})")
  }
  at(NULL, step = x[[1]])
}

at_as_sequence <- function(x, max) {
  if (is.null(x)) return(seq(0, max))
  to <- attr(x, "to")
  step <- attr(x, "step")
  sequence <- if (length(x) > 1) x else seq(x %||% 0, to || max)
  if (is.null(to) && is.null(step)) {
    return(x)
  } else if (!is.null(step)) {
    sequence <- sequence[sequence %% step == 0]
  }
  sequence
}

is_bare_schedule_value <- function(x) is.null(x) || is_schedule_at(x) || is_schedule_interval(x)

as_schedule_value <- function(x) {
  if (!is_bare_schedule_value(x)) {
    at(x)
  } else {
    x
  }
}

weekday.abb <- c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")
weekday.name <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
