# Standalone player Analysis Report. Edit player-logins.txt for key/participantId.
# Packages and editable files
library(shiny)
library(dplyr)
library(DT)
library(plotly)

app_title <- readLines("player-title.txt", encoding = "UTF-8", warn = FALSE)[1]

analysisBalls <- readRDS("vpcprofileballsv6.RDS")

analysisLookup <- readRDS("vpcprofilelookupv6.RDS")

player_access <- read.csv("player-logins.txt", colClasses = "character", stringsAsFactors = FALSE, strip.white = TRUE)

# Player lookup and row indices shared across sessions
player_index <- bind_rows(transmute(analysisBalls, participant_id = as.character(strikerParticipantId), name = strikerFullName), 
                          transmute(analysisBalls, participant_id = as.character(bowlerParticipantId), name = bowlerFullName)) %>% filter(!is.na(participant_id), 
                                                                                                                                          participant_id != "") %>% group_by(participant_id) %>% summarise(name = names(sort(table(name), decreasing = TRUE))[1], 
                                                                                                                                                                                                           .groups = "drop")

batting_rows <- split(seq_len(nrow(analysisBalls)), as.character(analysisBalls$strikerParticipantId))

bowling_rows <- split(seq_len(nrow(analysisBalls)), as.character(analysisBalls$bowlerParticipantId))

# Match joins, filters and cached filter choices
match_summary_lookup_columns <- c("matchName", "startDate", "grade", "format", "round", "venue", "season")

analysis_report_lookup_columns <- c("matchName", "startDate", "grade", "format", "round", "venue", "season", "standard")

match_level_columns <- setdiff(names(analysisLookup), "matchId")

attach_match_lookup <- function(data, lookup = analysisLookup, columns = match_level_columns) {
  columns_to_add <- setdiff(columns, names(data))
  if (!length(columns_to_add)) 
    return(data)
  data %>% left_join(lookup %>% select(matchId, all_of(columns_to_add)), by = "matchId")
}

filter_analysis_data <- function(data = analysisBalls, ball_predicates = list(), match_predicates = list(), lookup_columns = match_level_columns) {
  lookup <- analysisLookup
  if (length(match_predicates)) {
    lookup <- filter(lookup, !!!match_predicates)
    data <- filter(data, matchId %in% lookup$matchId)
  }
  if (length(ball_predicates)) {
    data <- filter(data, !!!ball_predicates)
  }
  attach_match_lookup(data, lookup, lookup_columns)
}

filter_predicates <- function(values = list(), ranges = list()) {
  predicates <- lapply(names(values), function(column) {
    selected <- values[[column]]
    if (is.null(selected)) 
      return(NULL)
    rlang::expr(.data[[!!column]] %in% !!selected)
  })
  predicates <- c(predicates, lapply(names(ranges), function(column) {
    selected <- ranges[[column]]
    if (is.null(selected)) return(NULL)
    rlang::expr(.data[[!!column]] >= !!selected[1] & .data[[!!column]] <= !!selected[2])
  }))
  Filter(Negate(is.null), predicates)
}

filter_analysis_state <- function(data = analysisBalls, ball_values = list(), match_values = list(), ball_ranges = list(), 
                                  exclusions = character(), lookup_columns = match_level_columns) {
  ball_predicates <- filter_predicates(ball_values, ball_ranges)
  match_predicates <- c(filter_predicates(match_values), if ("Incomplete Games" %in% 
                                                             exclusions) rlang::expr(gameComplete != "No"), if ("Finals Games" %in% exclusions) rlang::expr(!isFinal), 
                        if ("Season Games" %in% exclusions) rlang::expr(isFinal))
  filter_analysis_data(data, ball_predicates, match_predicates, lookup_columns)
}

has_caught_behind_data <- sum(analysisBalls$caughtBehind, na.rm = TRUE) > 0

analysis_range_cache <- new.env(parent = emptyenv())

analysis_values_cache <- new.env(parent = emptyenv())

analysis_choices_cache <- new.env(parent = emptyenv())

analysis_column_source <- function(column) {
  if (column %in% match_level_columns) 
    analysisLookup
  else analysisBalls
}

cached_analysis_column <- function(column, cache, transform) {
  if (!exists(column, envir = cache, inherits = FALSE)) {
    assign(column, transform(analysis_column_source(column)[[column]]), envir = cache)
  }
  get(column, envir = cache, inherits = FALSE)
}

analysis_column_range <- function(column) cached_analysis_column(column, analysis_range_cache, function(x) range(as.numeric(x), 
                                                                                                                 na.rm = TRUE))

analysis_column_values <- function(column) cached_analysis_column(column, analysis_values_cache, function(x) unique(na.omit(as.character(x))))

analysis_column_choices <- function(column) cached_analysis_column(column, analysis_choices_cache, unique)

# Scoring and wicket calculations
wicket_definitions <- function(discipline) {
  suffixes <- c("Bowled", "Lbw", if (has_caught_behind_data) "CaughtField" else "Caught", "Stumped", if (has_caught_behind_data) "CaughtBehind", 
                if (discipline == "Batting") "RunOut")
  labels <- c(Bowled = "Bowled", Lbw = "LBW", Caught = "Caught", CaughtField = "Caught Field", Stumped = "Stumped", 
              CaughtBehind = "Caught Behind", RunOut = "Run Out")
  data.frame(suffix = suffixes, label = unname(labels[suffixes]), source = c(Bowled = "bowled", Lbw = "lbw", 
                                                                             Caught = "caught", CaughtField = "caughtField", Stumped = "stumped", CaughtBehind = "caughtBehind", RunOut = "strikerRunOut")[suffixes], 
             row.names = NULL)
}

wicket_display_names <- function(discipline) wicket_definitions(discipline)$label

wicket_source_suffixes <- function(discipline) wicket_definitions(discipline)$suffix

wicket_actual_sources <- function(discipline) wicket_definitions(discipline)$source

mode <- function(x) {
  ux <- unique(x)
  if (length(ux) == 1L) 
    return(ux)
  ux[which.max(tabulate(match(x, ux)))]
}

capped_win_probability <- function(x, bowling = FALSE) {
  capped <- pmin(pmax(x, -10), 10)
  if (bowling) 
    -capped
  else capped
}

expected_average <- function(runs, dismissals, expected_runs, expected_dismissals, batting = TRUE) {
  if (batting) {
    ifelse(dismissals != 0 & (dismissals + expected_dismissals) != 0, runs/dismissals - (runs - expected_runs)/(dismissals + 
                                                                                                                  expected_dismissals), NA_real_)
  }
  else {
    ifelse(dismissals != 0 & (dismissals - expected_dismissals) != 0, (runs + expected_runs)/(dismissals - 
                                                                                                expected_dismissals) - runs/dismissals, NA_real_)
  }
}

summarise_matchup_metrics <- function(data, discipline, entity_group, entity_name, matchup_group, matchup_name) {
  batting <- discipline == "Batting"
  summary <- data %>% group_by(.data[[entity_group]], .data[[matchup_group]]) %>%
    summarise(entity = mode(.data[[entity_name]]), matchup = mode(.data[[matchup_name]]),
              runs = sum(.data[[if (batting) "runsBat" else "runsBowl"]], na.rm = TRUE),
              balls = sum(.data[[if (batting) "ballFaced" else "ballBowled"]], na.rm = TRUE),
              dismissals = sum(.data[[if (batting) "strikerOut" else "bowlerWicket"]], na.rm = TRUE),
              expected_runs = round(sum(.data[[if (batting) "xr" else "xrc"]], na.rm = TRUE), 1),
              expected_dismissals = round(sum(.data[[if (batting) "xd" else "xw"]], na.rm = TRUE), 1),
              .groups = "drop") %>% filter(balls >= 0) %>%
    mutate(Average = round(runs / ifelse(dismissals == 0, NA, dismissals), 1),
           expected = round(expected_average(runs, dismissals, expected_runs, expected_dismissals, batting), 1))
  if (batting) {
    summary %>% arrange(desc(runs)) %>% transmute(Batter = entity, Matchup = matchup,
                                                  `Batter Runs` = runs, `Balls Faced` = balls, Dismissals = dismissals, Average,
                                                  `xBatting Average` = expected)
  } else {
    summary %>% arrange(desc(dismissals)) %>% transmute(Bowler = entity, Matchup = matchup,
                                                        `Runs Conceded` = runs, `Balls Bowled` = balls, Wickets = dismissals, Average,
                                                        `xBowling Average` = expected)
  }
}

# Match table controls and display schemas
export_button <- function(excel_download_id = NULL) {
  export_options <- list(list(extend = "copy", text = "Copy", className = "btn btn-sm"))
  if (!is.null(excel_download_id)) {
    export_options <- append(export_options, list(list(extend = "copy", text = "Excel", className = "btn btn-sm", 
                                                       action = DT::JS(sprintf("function(e, dt, node, config){document.getElementById('%s').click();}", excel_download_id)))))
  }
  list(extend = "collection", text = "Export", className = "btn-group btn-group-sm", autoClose = TRUE, buttons = export_options)
}

column_view_button <- function(label, columns_js) {
  list(extend = "copy", text = label, action = DT::JS(sprintf(paste0("function (e, dt, button, config) {", "dt.columns().visible(false);", 
                                                                     "dt.columns(%s).visible(true);", "}"), columns_js)))
}

metric_view_button <- function(label, input_id) {
  list(extend = "copy", text = label, action = DT::JS(sprintf("function(e, dt, node, config){Shiny.setInputValue('%s', '%s');}", 
                                                              input_id, label)))
}

dashboard_datatable <- function(df, storage_key = NULL, default_columns = NULL, stat_views = NULL, allow_column_toggle = TRUE, 
                                excel_download_id = NULL, column_groups = NULL, metric_view_input = NULL, innings_view_input = NULL, innings_choices = c("Match", 
                                                                                                                                                         "First Innings", "Second Innings"), page_length = 25, preserve_row_order = FALSE, percentage_suffix = TRUE) {
  buttons <- list()
  if (length(stat_views)) {
    view_buttons <- unname(Map(column_view_button, names(stat_views), unname(stat_views)))
    buttons <- append(buttons, list(list(extend = "collection", text = "Table View", className = "btn-group btn-group-sm", 
                                         autoClose = TRUE, buttons = view_buttons)))
  }
  if (!is.null(metric_view_input)) {
    metric_buttons <- lapply(c("Percentage", "Expected", "Raw"), metric_view_button, input_id = metric_view_input)
    buttons <- append(buttons, list(list(extend = "collection", text = "Stat View", className = "btn-group btn-group-sm", 
                                         autoClose = TRUE, buttons = metric_buttons)))
  }
  if (!is.null(innings_view_input)) {
    innings_buttons <- lapply(innings_choices, metric_view_button, input_id = innings_view_input)
    buttons <- append(buttons, list(list(extend = "collection", text = "Innings", className = "btn-group btn-group-sm", 
                                         autoClose = TRUE, buttons = innings_buttons)))
  }
  if (allow_column_toggle) {
    buttons <- append(buttons, list(list(extend = "colvis", text = "Toggle Columns")))
  }
  buttons <- append(buttons, list(export_button(excel_download_id)))
  options <- list(orderClasses = TRUE, orderCellsTop = FALSE, deferRender = TRUE, processing = TRUE, searchDelay = 300, 
                  dom = "<\"dashboard-table-toolbar\"Bf>t<\"dashboard-table-footer\"lip>", pageLength = page_length, buttons = buttons, 
                  scrollX = TRUE)
  if (preserve_row_order) 
    options$order <- list()
  wp_columns <- which(names(df) %in% c("wP", "wP p/100", "Bat wP", "Bowl wP", "Win Probability", "Batting Win Probability", 
                                       "Bowling Win Probability")) - 1
  expected_columns <- which(grepl("^x", names(df)) | grepl("^(Bat |Bowl )?(Avg\\. Rating|Impact)$", names(df))) - 
    1
  column_defs <- list()
  if (length(wp_columns)) {
    column_defs <- append(column_defs, list(list(targets = wp_columns, render = DT::JS(paste0("function(data, type, row, meta){", 
                                                                                              "if(type !== 'display' || data === null || data === '') return data;", "var value = Number(data); if(!isFinite(value)) return data;", 
                                                                                              "return (value > 0 ? '+' : '') + value.toFixed(1) + '%';}")))))
  }
  if (isTRUE(percentage_suffix)) {
    percentage_columns <- which(grepl("%$", names(df))) - 1
    if (length(percentage_columns)) {
      column_defs <- append(column_defs, list(list(targets = percentage_columns, render = DT::JS(paste0("function(data, type, row, meta){", 
                                                                                                        "if(type !== 'display' || data === null || data === '') return data;", "var value = Number(data); if(!isFinite(value)) return data;", 
                                                                                                        "return value.toFixed(1) + '%';}")))))
    }
  }
  if (length(expected_columns)) {
    column_defs <- append(column_defs, list(list(targets = expected_columns, render = DT::JS(paste0("function(data, type, row, meta){", 
                                                                                                    "if(type !== 'display' || data === null || data === '') return data;", "var value = Number(data); if(!isFinite(value)) return data;", 
                                                                                                    "return (value > 0 ? '+' : '') + value.toFixed(1);}")))))
  }
  if (length(column_defs)) {
    options$columnDefs <- column_defs
  }
  if (length(stat_views)) {
    first_view <- gsub("[", "", stat_views[[1]], fixed = TRUE)
    first_view <- gsub("]", "", first_view, fixed = TRUE)
    first_view <- gsub(" ", "", first_view, fixed = TRUE)
    default_columns <- as.integer(strsplit(first_view, ",")[[1]])
  }
  if (!is.null(storage_key)) {
    options$stateSave <- TRUE
    options$stateDuration <- 0
    options$stateSaveCallback <- DT::JS(sprintf("function(settings, data){try { sessionStorage.setItem('%s', JSON.stringify(data)); } catch(e) {}}", 
                                                storage_key))
    options$stateLoadCallback <- DT::JS(sprintf(paste0("function(settings){try {var raw = sessionStorage.getItem('%s');", 
                                                       "return raw ? JSON.parse(raw) : null;} catch(e) {return null;}}"), storage_key))
    options$initComplete <- DT::JS(sprintf(paste0("function(settings, json){var dt = this.api();if (!dt.state.loaded()) {", 
                                                  "dt.columns().visible(false);dt.columns([%s]).visible(true);}}"), paste(default_columns, collapse = ",")))
  }
  column_group_labels <- rep(NA_character_, ncol(df))
  grouping_views <- if (length(column_groups)) 
    column_groups
  else stat_views
  if (length(grouping_views)) {
    for (view_index in seq_along(grouping_views)) {
      view_columns <- gsub("[", "", grouping_views[[view_index]], fixed = TRUE)
      view_columns <- gsub("]", "", view_columns, fixed = TRUE)
      view_columns <- gsub(" ", "", view_columns, fixed = TRUE)
      indices <- as.integer(strsplit(view_columns, ",")[[1]]) + 1L
      indices <- indices[!is.na(indices) & indices >= 1L & indices <= ncol(df)]
      indices <- indices[is.na(column_group_labels[indices])]
      column_group_labels[indices] <- if (length(column_groups)) {
        names(grouping_views)[view_index]
      }
      else if (view_index == 1L) {
        "Overview"
      }
      else {
        names(grouping_views)[view_index]
      }
    }
  }
  else {
    column_group_labels[] <- "Overview"
  }
  column_group_labels[is.na(column_group_labels)] <- "Other"
  runs <- rle(column_group_labels)
  grouped_header <- htmltools::withTags(table(class = "display compact dashboard-data-table", thead(tr(lapply(seq_along(runs$values), 
                                                                                                              function(i) {
                                                                                                                th(colspan = runs$lengths[i], class = "column-group-heading", runs$values[i])
                                                                                                              })), tr(lapply(names(df), th)))))
  widget <- datatable(df, extensions = "Buttons", options = options, class = "compact stripe hover row-border", 
                      rownames = FALSE, container = grouped_header)
  directional_columns <- names(df)[vapply(df, is.numeric, logical(1)) & grepl("^x|wP($| p/100$)|Win Probability$|Impact", 
                                                                              names(df), ignore.case = TRUE) & !grepl("Impact Rating$", names(df)) & !names(df) %in% c("Average Rating", 
                                                                                                                                                                       "Avg. Rating")]
  if (length(directional_columns)) {
    widget <- formatStyle(widget, directional_columns, color = styleInterval(-1e-12, c("#A33A31", "#16794B")), 
                          backgroundColor = styleInterval(-1e-12, c("#FBEDEC", "#EAF6F0")))
  }
  rating_columns <- names(df)[grepl("Impact Rating$", names(df)) | names(df) == "Average Rating" | grepl("Avg\\. Rating$", 
                                                                                                         names(df))]
  rating_columns <- rating_columns[vapply(df[rating_columns], is.numeric, logical(1))]
  if (length(rating_columns)) {
    widget <- formatStyle(widget, rating_columns, color = DT::JS("isNaN(parseFloat(value)) ? '' : parseFloat(value) < 5 ? '#A33A31' : '#16794B'"), 
                          backgroundColor = DT::JS("isNaN(parseFloat(value)) ? '' : parseFloat(value) < 5 ? '#FBEDEC' : '#EAF6F0'"))
  }
  result_columns <- intersect(names(df), "Result")
  if (length(result_columns)) {
    widget <- formatStyle(widget, result_columns, color = styleEqual(c("Win", "Won", "Loss", "Lost"), c("#16794B", 
                                                                                                        "#16794B", "#A33A31", "#A33A31")), backgroundColor = styleEqual(c("Win", "Won", "Loss", "Lost"), c("#EAF6F0", 
                                                                                                                                                                                                           "#EAF6F0", "#FBEDEC", "#FBEDEC")))
  }
  widget
}

column_view_spec <- function(df, columns) {
  indices <- match(columns, names(df)) - 1L
  indices <- indices[!is.na(indices)]
  paste0("[", paste(indices, collapse = ","), "]")
}

# Player match summaries and innings views
innings_scope_orders <- function(scope = "Match") {
  switch(scope, `First Innings` = 1:2, `Second Innings` = 3:4, NULL)
}

normalise_innings_scope <- function(scope) {
  if (is.null(scope) || !length(scope) || !scope %in% c("Match", "First Innings", "Second Innings")) {
    "Match"
  }
  else {
    scope
  }
}

filter_innings_scope <- function(data, scope = "Match") {
  orders <- innings_scope_orders(scope)
  if (is.null(orders)) 
    data
  else filter(data, inningsOrder %in% orders)
}

has_second_innings <- function(data) {
  nrow(data) > 0 && any(data$inningsOrder %in% 3:4, na.rm = TRUE)
}

available_innings_choices <- function(data) {
  if (has_second_innings(data)) {
    c("Match", "First Innings", "Second Innings")
  }
  else {
    "Match"
  }
}

normalise_available_innings_scope <- function(scope, data) {
  scope <- normalise_innings_scope(scope)
  if (!has_second_innings(data)) 
    "Match"
  else scope
}

collapse_innings_values <- function(values) {
  values <- values[!is.na(values) & nzchar(values)]
  if (length(values)) 
    paste(values, collapse = " & ")
  else NA_character_
}

format_match_overs <- function(balls) {
  ifelse(balls%%6 == 0, as.character(balls%/%6), paste0(balls%/%6, ".", balls%%6))
}

format_innings_score <- function(wickets, runs, balls) {
  ifelse(balls > 0, paste0(wickets, "/", runs, " (", format_match_overs(balls), ")"), NA_character_)
}

format_player_batting_score <- function(runs, balls, dismissals) {
  ifelse(balls > 0, paste0(runs, ifelse(dismissals == 0, "*", ""), " (", balls, ")"), NA_character_)
}

summarise_match_data_players <- function(data, participant_id, innings_scope = "Match") {
  data <- filter_innings_scope(data, innings_scope)
  batting_mvp <- if (innings_scope == "Match") 
    "strikerBatMatchMvp"
  else "strikerBatInningsMvp"
  bowling_mvp <- if (innings_scope == "Match") 
    "bowlerBowlMatchMvp"
  else "bowlerBowlInningsMvp"
  batting_source <- data %>% filter(as.character(strikerParticipantId) == participant_id) %>% mutate(dismissal = case_when(dismissalTypeId == 
                                                                                                                             0 ~ "Did Not Bat", dismissalTypeId == 1 ~ "Not Out", TRUE ~ paste(case_when(dismissalTypeId == 2 ~ "Caught", 
                                                                                                                                                                                                         dismissalTypeId == 3 ~ "LBW", dismissalTypeId == 4 ~ "Bowled", dismissalTypeId == 5 ~ "Stumped", dismissalTypeId == 
                                                                                                                                                                                                           6 ~ "Run Out", dismissalTypeId == 7 ~ "Hit Wicket", dismissalTypeId == 12 ~ "Obstruct Field", dismissalTypeId == 
                                                                                                                                                                                                           13 ~ "Retired", TRUE ~ NA_character_), "v", sub(".* ", "", bowlerFullName))))
  batting_labels <- batting_source %>% group_by(matchId, playerId = strikerParticipantId, inningsOrder) %>% summarise(innings_runs = sum(runsBat, 
                                                                                                                                         na.rm = TRUE), innings_balls = sum(ballFaced, na.rm = TRUE), innings_dismissals = sum(strikerOut, na.rm = TRUE), 
                                                                                                                      Dismissal = if (!length(na.omit(dismissal))) {
                                                                                                                        NA_character_
                                                                                                                      }
                                                                                                                      else if (any(!is.na(dismissal) & !dismissal %in% c("Not Out", "Did Not Bat"))) {
                                                                                                                        na.omit(dismissal[!dismissal %in% c("Not Out", "Did Not Bat")])[1]
                                                                                                                      }
                                                                                                                      else if (any(dismissal == "Not Out", na.rm = TRUE)) 
                                                                                                                        "Not Out"
                                                                                                                      else "Did Not Bat", .groups = "drop") %>% arrange(matchId, playerId, inningsOrder) %>% mutate(`Batting Score` = format_player_batting_score(innings_runs, 
                                                                                                                                                                                                                                                                  innings_balls, innings_dismissals)) %>% group_by(matchId, playerId) %>% summarise(`Batting Score` = collapse_innings_values(`Batting Score`), 
                                                                                                                                                                                                                                                                                                                                                    Dismissal = collapse_innings_values(Dismissal), .groups = "drop")
  batting <- batting_source %>% group_by(matchId, playerId = strikerParticipantId) %>% summarise(Player = mode(strikerFullName), 
                                                                                                 Team = mode(battingTeam), Match = first(matchName), Grade = sub(".*\\s+", "", first(grade)), Format = first(format), 
                                                                                                 Result = mode(strikerResult), Season = first(season), Round = first(round), Venue = first(venue), .start_date = first(startDate), `Batting Impact Rating` = mode(.data[[batting_mvp]]), 
                                                                                                 `Bat #` = mode(strikerOrder), runs = sum(runsBat, na.rm = TRUE), balls = sum(ballFaced, na.rm = TRUE), 
                                                                                                 dismissals = sum(strikerOut, na.rm = TRUE), `Strike Rate` = round(runs/balls * 100, 1), xR = round(sum(xr, 
                                                                                                                                                                                                        na.rm = TRUE), 1), xD = round(sum(xd, na.rm = TRUE), 1), bat_dots = sum(batterDots, na.rm = TRUE), 
                                                                                                 bat_multiples = sum(multiples, na.rm = TRUE), bat_boundaries = sum(fours + sixes, na.rm = TRUE), bat_xdots = round(sum(xds, 
                                                                                                                                                                                                                        na.rm = TRUE)/balls * 100, 1), bat_xmultiples = round(sum(xms, na.rm = TRUE)/balls * 100, 1), bat_xboundaries = round(sum(xbs, 
                                                                                                                                                                                                                                                                                                                                                  na.rm = TRUE)/balls * 100, 1), battingWP = sum(capped_win_probability(winProbChange), na.rm = TRUE), 
                                                                                                 battingVotes = mode(strikerCompVotes), .groups = "drop") %>% left_join(batting_labels, by = c("matchId", 
                                                                                                                                                                                               "playerId")) %>% mutate(`Bat Dot %` = round(bat_dots/balls * 100, 1), `Bat Multiple %` = round(bat_multiples/balls * 
                                                                                                                                                                                                                                                                                                100, 1), `Bat Boundary %` = round(bat_boundaries/balls * 100, 1))
  bowling_source <- filter(data, as.character(bowlerParticipantId) == participant_id)
  bowling_labels <- bowling_source %>% group_by(matchId, playerId = bowlerParticipantId, inningsOrder) %>% summarise(innings_wickets = sum(bowlerWicket, 
                                                                                                                                           na.rm = TRUE), innings_runs = sum(runsBowl, na.rm = TRUE), innings_balls = sum(ballBowled, na.rm = TRUE), 
                                                                                                                     .groups = "drop") %>% arrange(matchId, playerId, inningsOrder) %>% mutate(`Bowling Score` = format_innings_score(innings_wickets, 
                                                                                                                                                                                                                                      innings_runs, innings_balls)) %>% group_by(matchId, playerId) %>% summarise(`Bowling Score` = collapse_innings_values(`Bowling Score`), 
                                                                                                                                                                                                                                                                                                                  .groups = "drop")
  bowling <- bowling_source %>% group_by(matchId, playerId = bowlerParticipantId) %>% summarise(Player = mode(bowlerFullName), 
                                                                                                Team = mode(bowlingTeam), Match = first(matchName), Grade = sub(".*\\s+", "", first(grade)), Format = first(format), 
                                                                                                Result = mode(bowlerResult), Season = first(season), Round = first(round), Venue = first(venue), .start_date = first(startDate), `Bowling Impact Rating` = mode(.data[[bowling_mvp]]), 
                                                                                                `Bowl #` = mode(bowlerOrder), wickets = sum(bowlerWicket, na.rm = TRUE), runs_conceded = sum(runsBowl, 
                                                                                                                                                                                             na.rm = TRUE), balls_bowled = sum(ballBowled, na.rm = TRUE), Economy = round(runs_conceded/balls_bowled * 
                                                                                                                                                                                                                                                                            6, 1), Extras = sum(wides + noBalls, na.rm = TRUE), Byes = sum(byes + legByes, na.rm = TRUE), xRC = round(sum(xrc, 
                                                                                                                                                                                                                                                                                                                                                                                          na.rm = TRUE), 1), xW = round(sum(xw, na.rm = TRUE), 1), bowl_dots = sum(bowlerDots, na.rm = TRUE), 
                                                                                                bowl_multiples = sum(multiples, na.rm = TRUE), bowl_boundaries = sum(fours + sixes, na.rm = TRUE), bowl_xdots = round(sum(xdc, 
                                                                                                                                                                                                                          na.rm = TRUE)/balls_bowled * 100, 1), bowl_xmultiples = round(sum(xmc, na.rm = TRUE)/balls_bowled * 
                                                                                                                                                                                                                                                                                          100, 1), bowl_xboundaries = round(sum(xbc, na.rm = TRUE)/balls_bowled * 100, 1), bowlingWP = sum(capped_win_probability(winProbChange, 
                                                                                                                                                                                                                                                                                                                                                                                                                  bowling = TRUE), na.rm = TRUE), bowlingVotes = mode(bowlerCompVotes), .groups = "drop") %>% left_join(bowling_labels, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        by = c("matchId", "playerId")) %>% mutate(`Bowl Dot %` = round(bowl_dots/balls_bowled * 100, 1), `Bowl Multiple %` = round(bowl_multiples/balls_bowled * 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     100, 1), `Bowl Boundary %` = round(bowl_boundaries/balls_bowled * 100, 1))
  summary <- full_join(batting, bowling, by = c("matchId", "playerId"), suffix = c(".bat", ".bowl")) %>% transmute(matchId, 
                                                                                                                   Player = coalesce(Player.bat, Player.bowl), Team = coalesce(Team.bat, Team.bowl), Match = coalesce(Match.bat, 
                                                                                                                                                                                                                      Match.bowl), Grade = coalesce(Grade.bat, Grade.bowl), Format = coalesce(Format.bat, Format.bowl), Result = coalesce(Result.bat, 
                                                                                                                                                                                                                                                                                                                                          Result.bowl), Season = coalesce(Season.bat, Season.bowl), Round = coalesce(Round.bat, Round.bowl), 
                                                                                                                   Venue = coalesce(Venue.bat, Venue.bowl), .start_date = coalesce(.start_date.bat, .start_date.bowl),  Batting = `Batting Score`, Bowling = `Bowling Score`, 
                                                                                                                   `Batting Win Probability` = round(coalesce(battingWP, 0), 1), `Bowling Win Probability` = round(coalesce(bowlingWP, 
                                                                                                                                                                                                                            0), 1), Votes = coalesce(battingVotes, bowlingVotes), `Batting Impact Rating`, `Bat #`, `Batting SR` = `Strike Rate`, 
                                                                                                                   Dismissal, xR, xD, `Bat Dot %`, `Bat Multiple %`, `Bat Boundary %`, .batXDots = bat_xdots, .batXMultiples = bat_xmultiples, 
                                                                                                                   .batXBoundaries = bat_xboundaries, .batRawDots = bat_dots, .batRawMultiples = bat_multiples, .batRawBoundaries = bat_boundaries, 
                                                                                                                   `Bowling Impact Rating`, `Bowl #`, Economy, Extras, Byes, xRC, xW, `Bowl Dot %`, `Bowl Multiple %`, `Bowl Boundary %`, 
                                                                                                                   .bowlXDots = bowl_xdots, .bowlXMultiples = bowl_xmultiples, .bowlXBoundaries = bowl_xboundaries, .bowlRawDots = bowl_dots, 
                                                                                                                   .bowlRawMultiples = bowl_multiples, .bowlRawBoundaries = bowl_boundaries)
  summary
}

normalise_metric_view <- function(view) {
  if (is.null(view) || !view %in% c("Percentage", "Expected", "Raw")) "Percentage" else view
}

apply_match_data_metric_view <- function(data, view = "Percentage") {
  if (!nrow(data)) 
    return(data)
  view <- normalise_metric_view(view)
  target_columns <- c("Bat Dot %", "Bat Multiple %", "Bat Boundary %", "Bowl Dot %", "Bowl Multiple %", "Bowl Boundary %")
  source_columns <- switch(view, Percentage = target_columns, Expected = c(".batXDots", ".batXMultiples", ".batXBoundaries", 
                                                                           ".bowlXDots", ".bowlXMultiples", ".bowlXBoundaries"), Raw = c(".batRawDots", ".batRawMultiples", ".batRawBoundaries", 
                                                                                                                                         ".bowlRawDots", ".bowlRawMultiples", ".bowlRawBoundaries"))
  labels <- switch(view, Percentage = target_columns, Expected = c("xDots Scored p/100", "xMultiples Scored p/100", 
                                                                   "xBoundaries Scored p/100", "xDots Conceded p/100", "xMultiples Conceded p/100", "xBoundaries Conceded p/100"), 
                   Raw = c("Bat Dots", "Bat Multiples", "Bat Boundaries", "Bowl Dots", "Bowl Multiples", "Bowl Boundaries"))
  available <- target_columns %in% names(data)
  for (i in which(available)) data[[target_columns[i]]] <- data[[source_columns[i]]]
  names(data)[match(target_columns[available], names(data))] <- labels[available]
  select(data, -matches("^\\.(bat|bowl)(X|Raw)"))
}

match_data_metric_columns <- function(view = "Percentage") {
  view <- normalise_metric_view(view)
  switch(view, Percentage = list(batting = c("Bat Dot %", "Bat Multiple %", "Bat Boundary %"), bowling = c("Bowl Dot %", 
                                                                                                           "Bowl Multiple %", "Bowl Boundary %")), Expected = list(batting = c("xDots Scored p/100", "xMultiples Scored p/100", 
                                                                                                                                                                               "xBoundaries Scored p/100"), bowling = c("xDots Conceded p/100", "xMultiples Conceded p/100", "xBoundaries Conceded p/100")), 
         Raw = list(batting = c("Bat Dots", "Bat Multiples", "Bat Boundaries"), bowling = c("Bowl Dots", "Bowl Multiples", 
                                                                                            "Bowl Boundaries")))
}

# Filter controls and Excel export
analysis_range_slider <- function(id, label, column) {
  limits <- analysis_column_range(column)
  sliderInput(id, label, min = limits[1], max = limits[2], value = limits)
}

filter_summary_data <- function(values) {
  rows <- lapply(names(values), function(label) {
    value <- values[[label]]
    if (is.null(value) || !length(value) || all(is.na(value) | as.character(value) == "")) 
      return(NULL)
    if (inherits(value, "Date")) {
      display_value <- paste(format(value, "%d %b %Y"), collapse = " \u2013 ")
    }
    else if (is.numeric(value) && length(value) == 2L) {
      display_value <- paste(format(value, trim = TRUE), collapse = " \u2013 ")
    }
    else {
      display_value <- paste(as.character(value), collapse = ", ")
    }
    data.frame(Filter = label, Value = display_value, check.names = FALSE)
  })
  rows <- Filter(Negate(is.null), rows)
  if (!length(rows)) 
    return(data.frame(Filter = character(), Value = character()))
  bind_rows(rows)
}

write_dashboard_workbook <- function(file, data, filter_values) {
  writexl::write_xlsx(list(Data = as.data.frame(data), Info = filter_summary_data(filter_values)), path = file)
}

# Shared chart styling
dashboard_plot_style <- list(font = list(family = "Segoe UI, Inter, Arial, sans-serif", color = "#222222"), hoverlabel = list(bgcolor = "#222222", 
                                                                                                                              bordercolor = "#222222", font = list(color = "#FFFFFF", size = 12)), plot_bgcolor = "#FFFFFF", paper_bgcolor = "#FFFFFF")

report_plot_layout <- function(plot, ...) {
  do.call(plotly::layout, c(list(p = plot), list(...), dashboard_plot_style))
}

dashboard_plot_config <- function(plot) {
  plot %>% plotly::config(displaylogo = FALSE, displayModeBar = "hover", responsive = TRUE, scrollZoom = FALSE, 
                          modeBarButtonsToRemove = c("lasso2d", "select2d", "autoScale2d", "toggleSpikelines", "hoverClosestCartesian", 
                                                     "hoverCompareCartesian"), toImageButtonOptions = list(format = "png", scale = 2))
}

analysis_report_card <- function(part, ..., class = "analysis-report-card") {
  div(class = class, tags$h2(class = "match-report-card-title", textOutput(paste0("analysisReport", part, "Title"), 
                                                                           inline = TRUE)), div(class = "analysis-report-card-caption", textOutput(paste0("analysisReport", part, 
                                                                                                                                                          "Caption"), inline = TRUE)), ...)
}

# The login is the only UI sent before authentication. There is no player/type selector.
# Login and responsive report layout
login_ui <- function() {
  div(class = "login-shell", div(class = "login-card",
                                 tags$img(src = "fgilogo.png", class = "login-logo", alt = "First Grade Insights"),
                                 h1(app_title),
                                 passwordInput("accessKey", "Your key"),
                                 actionButton("login", "Sign in", class = "login-button"),
                                 div(class = "login-message", role = "alert", textOutput("loginMessage"))
  ))
}

player_main_filters <- function() {
  tagList(h3("Main Filters"),
          selectInput("analysisReportDiscipline", "Discipline", c("Batting", "Bowling")),
          lapply(c(Season = "season", Grade = "grade", Format = "format", Team = "battingTeam"), function(column) {
            label <- c(season = "Season", grade = "Grade", format = "Format", battingTeam = "Team")[[column]]
            selectInput(paste0("analysisReport", label), label,
                        choices = sort(analysis_column_choices(column)), selected = NULL, multiple = TRUE)
          }),
          selectInput("analysisReportExclude", "Exclude",
                      c("Incomplete Games", "Finals Games", "Season Games"), selected = NULL, multiple = TRUE),
          analysis_range_slider("analysisReportOverRange", "Over Selection", "overNumber")
  )
}

report_ui <- function(player_name) {
  div(id = "playerReport", class = "filters-open",
      tags$header(class = "player-header",
                  div(class = "player-brand",
                      tags$img(src = "fgilogo.png", class = "dashboard-logo", alt = "First Grade Insights"),
                      div(h1(player_name), p("Player analysis report"))),
                  actionButton("logout", "Sign out", class = "signout-button")
      ),
      div(class = "report-toolbar",
          tags$button(type = "button", class = "btn filter-toggle", `aria-controls` = "playerFilters",
                      `aria-expanded` = "true", onclick = "togglePlayerFilters()", icon("sliders"), " Filters"),
          div(class = "generate-controls",
              actionButton("generateAnalysis", "Generate Data", icon = icon("play")),
              textOutput("generateAnalysisStatus", container = span)
          )
      ),
      div(class = "player-workspace",
          tags$aside(id = "playerFilters", class = "player-filters",
                     div(class = "filters-heading", h2("Filters"),
                         tags$button(type = "button", class = "btn close-filters", onclick = "togglePlayerFilters()",
                                     `aria-label` = "Close filters", icon("xmark"))),
                     player_main_filters()
          ),
          tags$main(class = "player-results",
                    conditionalPanel("input.generateAnalysis == 0", div(class = "report-welcome",
                                                                        h2("Player report"), p("Choose batting or bowling in Filters, then select Generate Data."))),
                    conditionalPanel("input.generateAnalysis > 0",
                                     uiOutput("analysisReportTitleCard"), uiOutput("analysisReportMessage"),
                                     conditionalPanel("output.analysisReportReady == 'ready'",
                                                      div(class = "report-grid",
                                                          analysis_report_card("Scoring", plotlyOutput("analysisReportScoringPlot", height = "350px")),
                                                          analysis_report_card("Wicket", plotlyOutput("analysisReportWicketPlot", height = "350px")),
                                                          analysis_report_card("Summary", uiOutput("analysisReportSummaryTable")),
                                                          div(class = "report-stack",
                                                              analysis_report_card("Matchup", plotlyOutput("analysisReportMatchupPlot", height = "260px")),
                                                              analysis_report_card("Composition", plotlyOutput("analysisReportCompositionPlot", height = "260px"))),
                                                          analysis_report_card("WinProbability", plotlyOutput("analysisReportWinProbabilityPlot", height = "350px")),
                                                          uiOutput("analysisReportMetrics", class = "metric-output"),
                                                          analysis_report_card("Impact", plotlyOutput("analysisReportImpactPlot", height = "380px"),
                                                                               class = "analysis-report-card full-width"),
                                                          analysis_report_card("Table",
                                                                               downloadButton("downloadAnalysisReportExcel", "Excel", class = "dashboard-excel-download"),
                                                                               DTOutput("analysisReportTable"), class = "analysis-report-card match-table-card full-width")
                                                      )
                                     )
                    )
          )
      )
  )
}

ui <- fluidPage(
  title = app_title,
  tags$head(
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1, viewport-fit=cover"),
    tags$style(HTML(r"---(:root { --blue: #2176ff; --ink: #222; --muted: #6e716b; --line: #e3e5e2; }
* { box-sizing: border-box; }
body { background: #f5f5f3; color: var(--ink); font-family: 'Segoe UI', Arial, sans-serif; }
.container-fluid { max-width: 1680px; margin: auto; padding: 22px 28px 40px; }
button, a, input, select { touch-action: manipulation; }
.btn { min-height: 44px; border-radius: 8px; font-weight: 600; }
.btn-default { background: var(--blue); border-color: var(--blue); color: white; }
.btn-default:hover, .btn-default:focus { background: #1763dc; border-color: #1763dc; color: white; }
.player-brand { display: flex; align-items: center; gap: 14px; min-width: 0; }
.player-brand > div { min-width: 0; }
.dashboard-logo { height: 50px; width: auto; flex-shrink: 0; }
.login-logo { height: 64px; max-width: 100%; object-fit: contain; }
.login-shell { min-height: 85vh; display: grid; place-items: center; }
.login-card { width: min(100%, 420px); padding: 32px; background: white; border: 1px solid var(--line); border-radius: 16px; box-shadow: 0 10px 40px #22222209; }
.login-card h1 { font-size: 28px; font-weight: 700; margin: 14px 0; }
.login-card .form-control { min-height: 46px; font-size: 16px; }
.login-button { width: 100%; margin-top: 6px; }
.login-message { color: #a33a31; min-height: 22px; margin-top: 12px; }
.player-header { display: flex; justify-content: space-between; align-items: center; gap: 16px; padding: 6px 0 20px; }
.player-header h1 { font-weight: 750; font-size: 29px; margin: 8px 0; overflow-wrap: anywhere; }
.player-header p { color: var(--muted); margin: 0; }
.signout-button { background: white; color: var(--ink); border: 1px solid var(--line); flex-shrink: 0; }
.report-toolbar { display: flex; justify-content: space-between; align-items: center; gap: 12px; padding: 13px 16px; margin-bottom: 20px; background: #222; border-left: 5px solid #fbd437; border-radius: 10px; }
.filter-toggle { background: white; color: #222; }
.generate-controls { display: flex; gap: 16px; align-items: center; color: #eee; font-size: 12px; }
.player-workspace { display: block; }
.player-filters { display: none; background: white; border: 1px solid var(--line); border-radius: 12px; padding: 16px; margin-bottom: 20px; }
.filters-open .player-workspace { display: grid; grid-template-columns: 285px minmax(0, 1fr); gap: 20px; align-items: start; }
.filters-open .player-filters { display: block; }
.filters-heading { display: flex; align-items: center; justify-content: space-between; margin-bottom: 14px; }
.filters-heading h2 { font-size: 19px; margin: 0; }
.close-filters { background: #f5f5f3; border: 1px solid var(--line); }
.player-filters .panel-heading a { display: block; padding: 12px 8px; font-size: 14px; }
.player-filters .panel-heading { padding: 0; }
.player-filters .panel-primary { border-color: var(--blue); }
.player-filters .panel-primary > .panel-heading { background: var(--blue); border-color: var(--blue); }
.player-results, .report-grid, .report-stack, .metric-output { min-width: 0; }
.report-grid { display: grid; grid-template-columns: repeat(2, minmax(0, 1fr)); gap: 20px; align-items: stretch; }
.report-stack { display: grid; gap: 20px; }
.full-width { grid-column: 1 / -1; }
.analysis-report-card, .report-welcome { min-width: 0; padding: 20px; background: white; border: 1px solid var(--line); border-radius: 12px; box-shadow: 0 2px 10px #22222206; }
.match-report-card-title { margin: 0 0 10px; font-size: 17px; font-weight: 700; line-height: 1.35; overflow-wrap: anywhere; }
.analysis-report-card-caption { color: var(--muted); font-size: 12px; line-height: 1.5; margin-bottom: 14px; }
.analysis-report-title-card { background: white; border: 1px solid var(--line); border-radius: 12px; padding: 20px; margin-bottom: 20px; }
.analysis-report-title { font-size: 22px; font-weight: 700; margin: 0 0 14px; overflow-wrap: anywhere; }
.analysis-report-title-meta { display: flex; flex-wrap: wrap; gap: 8px 20px; color: #555; font-size: 12px; }
.analysis-report-title-meta-item { overflow-wrap: anywhere; }
.analysis-report-summary-table { width: 100%; table-layout: fixed; font-size: 12px; }
.analysis-report-summary-table th, .analysis-report-summary-table td { padding: 10px 6px; border-bottom: 1px solid #eee; overflow-wrap: anywhere; }
.analysis-report-summary-table thead th { font-size: 11px; color: var(--muted); }
.analysis-report-summary-table th:first-child { width: 34%; }
.analysis-report-summary-value.positive { background: #e7f5ee; color: #16794b; }
.analysis-report-summary-value.negative { background: #fbeceb; color: #a33a31; }
.analysis-report-summary-value.neutral { background: #f5f5f5; color: #74756c; }
.metric-output { display: flex; }
.analysis-report-metric-grid { display: grid; grid-template-columns: repeat(2, minmax(0, 1fr)); grid-template-rows: repeat(2, minmax(0, 1fr)); gap: 16px; width: 100%; }
.analysis-report-metric-tile { display: flex; flex-direction: column; justify-content: center; }
.analysis-report-metric-label { font-size: 13px; font-weight: 650; }
.analysis-report-metric-value { font-size: 32px; font-weight: 750; margin: 10px 0; }
.analysis-report-metric-note { font-size: 12px; color: var(--muted); line-height: 1.5; }
.analysis-report-metric-tile.positive .analysis-report-metric-value { color: #16794b; }
.analysis-report-metric-tile.negative .analysis-report-metric-value { color: #a33a31; }
.generated-filter-summary { border-radius: 8px; background: #fff7da; margin-bottom: 16px; padding: 14px; }
.dashboard-excel-download { display: none; }
.dashboard-table-toolbar, .dashboard-table-footer { display: flex; flex-wrap: wrap; align-items: center; justify-content: space-between; gap: 12px; margin: 12px 0; }
.dataTables_wrapper { width: 100%; min-width: 0; }
.dataTables_scrollBody { -webkit-overflow-scrolling: touch; overscroll-behavior-x: contain; }
.dataTables_wrapper .dt-buttons { display: flex; flex-wrap: wrap; gap: 6px; }
.dataTables_wrapper .dt-button { min-height: 44px; border-radius: 7px; padding: 8px 10px; }
table.dataTable { white-space: nowrap; font-size: 12px; }
table.dataTable th { background: var(--blue); color: white; }
table.dataTable td { padding: 8px; }
.column-group-heading { text-transform: uppercase; font-size: 10px; letter-spacing: .06em; }
.dataTables_filter input { min-height: 40px; max-width: 180px; }
.plotly, .plot-container { max-width: 100%; }
@media (max-width: 900px) {
  .container-fluid { padding: 16px; }
  .filters-open .player-workspace { display: block; }
  .player-filters { width: 100%; }
  .report-grid { grid-template-columns: minmax(0, 1fr); gap: 16px; }
  .report-stack { display: contents; }
  .analysis-report-card { padding: 16px 12px; }
  .full-width { grid-column: auto; }
  .player-filters input, .player-filters select, .selectize-input { font-size: 16px; }
  .selectize-input { min-height: 44px; }
  .report-toolbar { position: sticky; top: 0; z-index: 20; }
  .generate-controls { flex-direction: column; gap: 4px; align-items: flex-end; }
  .analysis-report-metric-grid { grid-template-columns: minmax(0, 1fr); grid-template-rows: none; }
}
@media (max-width: 400px) {
  .container-fluid { padding: 10px; }
  .login-card { padding: 24px 20px; }
  .player-header { flex-wrap: wrap; }
  .player-header h1 { font-size: 24px; }
  .dashboard-logo { height: 40px; }
  .analysis-report-title { font-size: 19px; }
  .analysis-report-summary-table { font-size: 11px; }
  .analysis-report-summary-table th, .analysis-report-summary-table td { padding: 9px 3px; }
  .report-toolbar { padding: 10px; }
}
)---")),
tags$script(HTML(r"---(function resizePlayerCharts() {
  window.dispatchEvent(new Event('resize'));
  document.querySelectorAll('.js-plotly-plot').forEach(function(plot) {
    if (plot.offsetWidth && window.Plotly) Plotly.Plots.resize(plot);
  });
  if ($.fn.dataTable) $.fn.dataTable.tables({visible: true, api: true}).columns.adjust();
}
function togglePlayerFilters() {
  var open = document.getElementById('playerReport').classList.toggle('filters-open');
  document.querySelectorAll('.filter-toggle').forEach(function(button) {
    button.setAttribute('aria-expanded', String(open));
  });
  resizePlayerCharts();
  if (open && window.innerWidth <= 900) document.getElementById('playerFilters').scrollIntoView({block: 'start', behavior: 'smooth'});
}
Shiny.addCustomMessageHandler('setDashboardInput', function(message) {
  Shiny.setInputValue(message.id, message.value, {priority: 'event'});
});
Shiny.addCustomMessageHandler('logoutPlayer', function() {
  Object.keys(sessionStorage).filter(function(key) { return key.indexOf('DT-player-report-') === 0; })
    .forEach(function(key) { sessionStorage.removeItem(key); });
});
document.addEventListener('keydown', function(event) {
  if (event.key === 'Enter' && event.target.id === 'accessKey') {
    event.preventDefault(); document.getElementById('login').click();
  }
  var report = document.getElementById('playerReport');
  if (event.key === 'Escape' && report && report.classList.contains('filters-open')) togglePlayerFilters();
});
)---"))
  ),
uiOutput("screen")
)

# Session login, report state and player data
server <- function(input, output, session) {
  identity <- reactiveVal(NULL)
  login_message <- reactiveVal("")
  observeEvent(input$login, {
    req(is.null(identity()))
    account <- which(player_access$key == input$accessKey)[1]
    person <- match(player_access$participantId[account], player_index$participant_id)
    if (is.na(account) || is.na(person)) {
      login_message("Invalid key.")
      return()
    }
    identity(as.list(player_index[person, ]))
    login_message("")
    updateTextInput(session, "accessKey", value = "")
  })
  observeEvent(input$logout, {
    identity(NULL)
    session$sendCustomMessage("logoutPlayer", list())
    session$reload()
  })
  output$loginMessage <- renderText(login_message())
  output$screen <- renderUI({
    if (is.null(identity())) 
      login_ui()
    else report_ui(identity()$name)
  })
  player_balls <- reactive({
    person <- req(identity())
    rows <- sort(unique(c(batting_rows[[person$participant_id]], bowling_rows[[person$participant_id]])))
    analysisBalls[rows, , drop = FALSE]
  })
  generated_state <- eventReactive(input$generateAnalysis, {
    person <- req(identity())
    c(capture_report_state(), list(discipline = input$analysisReportDiscipline, 
                                   participant_id = person$participant_id, selected = person$name))
  }, ignoreInit = FALSE)
  analysis_report_state <- reactive({
    person <- req(identity())
    state <- generated_state()
    req(identical(state$participant_id, person$participant_id))
    state
  })
  observeEvent(input$generateAnalysis, {
    req(identity())
    if (!has_second_innings(analysis_report_filtered_data())) 
      reset_dashboard_input("analysisReportInningsView")
  }, ignoreInit = TRUE)
  register_generation_status <- function(button_id) {
    output[[paste0(button_id, "Status")]] <- renderText({
      clicks <- input[[button_id]]
      if (is.null(clicks) || clicks < 1) {
        "Ready to generate"
      }
      else {
        paste("Last generated", format(Sys.time(), "%d %b %Y at %I:%M %p"))
      }
    })
  }
  report_state_fields <- c(season = "Season", grade = "Grade", format = "Format",
                           team = "Team", exclude = "Exclude", over_range = "OverRange")
  capture_report_state <- function() {
    setNames(lapply(unname(report_state_fields), function(suffix) {
      input[[paste0("analysisReport", suffix)]]
    }), names(report_state_fields))
  }
  analysis_report_filter_snapshot <- reactive({
    state <- analysis_report_state()
    captured <- list(Discipline = state$discipline, Season = state$season,
                     Grade = state$grade, Format = state$format, Team = state$team, Exclude = state$exclude)
    for (label in c("Season", "Grade", "Format", "Team")) {
      column <- c(Season = "season", Grade = "grade", Format = "format", Team = "battingTeam")[[label]]
      if (setequal(as.character(captured[[label]]), analysis_column_values(column))) captured[[label]] <- NULL
    }
    if (!isTRUE(all.equal(state$over_range, analysis_column_range("overNumber")))) {
      captured[["Over Selection"]] <- state$over_range
    }
    captured
  })
  reset_dashboard_input <- function(input_id, value = "Match") {
    session$sendCustomMessage("setDashboardInput", list(id = input_id, value = value))
  }
  analysis_report_current_filter_snapshot <- reactive({
    captured <- analysis_report_filter_snapshot()
    captured[["Player"]] <- analysis_report_state()$selected
    scope <- normalise_available_innings_scope(input$analysisReportInningsView, analysis_report_filtered_data())
    if (scope != "Match") 
      captured[["Innings"]] <- scope
    state <- analysis_report_state()
    balls_column <- if (state$discipline == "Batting") 
      "ballFaced"
    else "ballBowled"
    captured[["Sample Size"]] <- sum(analysis_report_filtered_data()[[balls_column]], na.rm = TRUE)
    captured
  })
  output$analysisReportTitleCard <- renderUI({
    req(input$generateAnalysis > 0)
    state <- analysis_report_state()
    captured <- analysis_report_current_filter_snapshot()
    data <- analysis_report_filtered_data()
    observed_values <- function(column, decreasing = FALSE) {
      values <- trimws(as.character(data[[column]]))
      values <- unique(values[!is.na(values) & values != ""])
      if (!length(values)) 
        return(NULL)
      sort(values, decreasing = decreasing)
    }
    team_column <- if (state$discipline == "Batting") 
      "battingTeam"
    else "bowlingTeam"
    overs <- suppressWarnings(as.numeric(data$overNumber))
    overs <- overs[is.finite(overs)]
    observed_overs <- if (length(overs)) 
      range(overs)
    else NULL
    major_filters <- list(Season = observed_values("season", decreasing = TRUE), Grade = observed_values("grade"), 
                          Format = observed_values("format"), Team = observed_values(team_column))
    if (!is.null(captured[["Over Selection"]])) {
      major_filters[["Overs"]] <- observed_overs
    }
    if (length(state$exclude)) {
      major_filters[["Exclude"]] <- state$exclude
    }
    major_filters[["Sample Size"]] <- captured[["Sample Size"]]
    metadata <- filter_summary_data(major_filters)
    if ("Sample Size" %in% metadata$Filter) {
      sample_row <- metadata$Filter == "Sample Size"
      sample_value <- suppressWarnings(as.numeric(metadata$Value[sample_row]))
      if (is.finite(sample_value)) {
        metadata$Value[sample_row] <- format(sample_value, big.mark = ",", scientific = FALSE, trim = TRUE)
      }
    }
    title <- if (length(state$selected)) {
      paste0(paste(state$selected, collapse = ", "), ": ", state$discipline, " Report")
    }
    else {
      paste(state$discipline, "Report")
    }
    div(class = "analysis-report-title-card", tags$h2(class = "analysis-report-title", title), if (nrow(metadata)) 
      div(class = "analysis-report-title-meta", lapply(seq_len(nrow(metadata)), function(row) {
        div(class = "analysis-report-title-meta-item", tags$strong(paste0(metadata$Filter[row], ": ")), 
            span(metadata$Value[row]))
      })))
  })
  register_generation_status("generateAnalysis")
  # Report data and competition percentile benchmarks
  filter_analysis_report_data <- function(data, state, apply_over_selection = TRUE) {
    batting <- state$discipline == "Batting"
    subject_column <- if (batting) "strikerParticipantId" else "bowlerParticipantId"
    team_column <- if (batting) "battingTeam" else "bowlingTeam"
    ball_values <- setNames(list(state$participant_id), subject_column)
    ball_values[[team_column]] <- state$team
    filter_analysis_state(data, ball_values = ball_values,
                          match_values = list(season = state$season, grade = state$grade, format = state$format),
                          ball_ranges = list(overNumber = if (apply_over_selection) state$over_range else NULL),
                          exclusions = state$exclude, lookup_columns = analysis_report_lookup_columns)
  }
  analysis_report_source_data <- reactive({
    req(input$generateAnalysis)
    state <- analysis_report_state()
    player_balls()
  })
  analysis_report_base_filtered_data <- reactive({
    state <- analysis_report_state()
    filter_analysis_report_data(analysis_report_source_data(), state, apply_over_selection = FALSE)
  })
  analysis_report_filtered_data <- reactive({
    state <- analysis_report_state()
    filter_analysis_report_data(analysis_report_source_data(), state, apply_over_selection = TRUE)
  })
  analysis_report_expected_sources <- function(state) {
    batting <- state$discipline == "Batting"
    scoring <- if (batting) 
      c("r", "d", "ds", "ms", "bs")
    else c("rc", "w", "dc", "mc", "bc")
    list(scoring = paste0("x", scoring), scoring_labels = c(if (batting) c("xR", "xD") else c("xRC", "xW"), 
                                                            "Dot", "Multiples", "Boundaries"), wickets = paste0("x", if (batting) "Bat" else "Bowl", wicket_source_suffixes(state$discipline)), 
         wicket_labels = wicket_display_names(state$discipline))
  }
  analysis_report_entity_summary <- reactive({
    state <- analysis_report_state()
    sources <- analysis_report_expected_sources(state)
    required <- c(sources$scoring, sources$wickets)
    actual_wicket_columns <- wicket_actual_sources(state$discipline)
    id_column <- if (state$discipline == "Batting") 
      "strikerParticipantId"
    else "bowlerParticipantId"
    name_column <- if (state$discipline == "Batting") 
      "strikerFullName"
    else "bowlerFullName"
    balls_column <- if (state$discipline == "Batting") 
      "ballFaced"
    else "ballBowled"
    runs_column <- if (state$discipline == "Batting") 
      "runsBat"
    else "runsBowl"
    group_columns <- id_column
    required_columns <- unique(c(group_columns, name_column, "matchId", "inningsOrder", "overNumber", balls_column, 
                                 runs_column, "strikerOut", "bowlerWicket", "strikerRunOut", "winProbChange", required, actual_wicket_columns))
    lookup_columns <- intersect(required_columns, match_level_columns)
    ball_columns <- setdiff(required_columns, lookup_columns)
    data <- analysisBalls %>% select(all_of(ball_columns))
    if (length(lookup_columns)) {
      data <- data %>% left_join(analysisLookup %>% select(matchId, all_of(lookup_columns)), by = "matchId")
    }
    if (!is.null(state$over_range)) {
      data <- filter(data, overNumber >= state$over_range[1], overNumber <= state$over_range[2])
    }
    data %>% filter(!is.na(.data[[name_column]]), .data[[name_column]] != "", .data[[name_column]] != "Private") %>% 
      group_by(across(all_of(group_columns))) %>% summarise(Matches = n_distinct(matchId), Innings = n_distinct(matchId, 
                                                                                                                inningsOrder), balls = sum(.data[[balls_column]], na.rm = TRUE), runs = sum(.data[[runs_column]], na.rm = TRUE), 
                                                            dismissals = if (state$discipline == "Batting") {
                                                              sum(strikerOut, na.rm = TRUE)
                                                            }
                                                            else {
                                                              sum(bowlerWicket, na.rm = TRUE)
                                                            }, wP = sum(capped_win_probability(winProbChange, bowling = state$discipline == "Bowling"), na.rm = TRUE), 
                                                            across(all_of(unique(c(required, actual_wicket_columns))), ~sum(.x, na.rm = TRUE)), .groups = "drop") %>% 
      filter(balls >= 100) %>% mutate(.xAverage = expected_average(runs, dismissals, .data[[sources$scoring[1]]], 
                                                                   .data[[sources$scoring[2]]], state$discipline == "Batting"), .wPPerMatch = ifelse(balls != 0 & if (state$discipline == 
                                                                                                                                                                      "Batting") {
                                                                     dismissals != 0
                                                                   }
                                                                   else {
                                                                     Innings != 0
                                                                   }, ((wP/balls * 100)/100) * (balls/if (state$discipline == "Batting") 
                                                                     dismissals
                                                                     else Innings), NA_real_))
  }) %>% bindCache(analysis_report_state()$discipline, analysis_report_state()$over_range, cache = "app")
  analysis_report_match_data_summary <- reactive({
    req(input$generateAnalysis)
    state <- analysis_report_state()
    source_data <- player_balls()
    data <- filter_analysis_state(source_data,
                                  match_values = list(season = state$season, grade = state$grade, format = state$format),
                                  ball_ranges = list(overNumber = state$over_range), exclusions = state$exclude,
                                  lookup_columns = match_summary_lookup_columns)
    innings_scope <- normalise_available_innings_scope(input$analysisReportInningsView, analysis_report_filtered_data())
    summary <- summarise_match_data_players(data, state$participant_id, innings_scope)
    if (!is.null(state$team)) {
      summary <- filter(summary, Team %in% state$team)
    }
    summary %>% arrange(desc(.start_date)) %>% select(-Team, -.start_date)
  })
  analysis_report_match_display_data <- reactive({
    state <- analysis_report_state()
    view <- normalise_metric_view(input$analysisReportMetricView)
    data <- apply_match_data_metric_view(analysis_report_match_data_summary(), view)
    if (!nrow(data)) 
      return(data)
    data <- if (state$discipline == "Batting") {
      data %>% rename(`Bat wP` = `Batting Win Probability`)
    }
    else {
      data %>% rename(`Bowl wP` = `Bowling Win Probability`)
    }
    active_metrics <- match_data_metric_columns(view)[[if (state$discipline == "Batting") 
      "batting"
      else "bowling"]]
    common <- {
      c("Player", "Match", "Grade", "Format", "Result", "Season", "Round", "Venue", if (state$discipline == 
                                                                                        "Batting") "Bat wP" else "Bowl wP", "Votes")
    }
    discipline_columns <- if (state$discipline == "Batting") {
      c("Batting Impact Rating", "Bat #", "Batting", "Batting SR", "Dismissal", "xR", "xD", active_metrics)
    }
    else {
      c("Bowling Impact Rating", "Bowl #", "Bowling", "Economy", "Extras", "Byes", "xRC", "xW", active_metrics)
    }
    select(data, all_of(unique(c(common, discipline_columns))))
  })
  summarise_analysis_report_matchup <- function(group_column, matchup_label, matchup_order = NULL, numeric_order = FALSE) {
    state <- analysis_report_state()
    data <- analysis_report_filtered_data()
    if (state$discipline == "Batting" && group_column == "bowlerCategory") {
      batter_styles <- trimws(tolower(coalesce(as.character(data$strikerStyle), "")))
      if (length(batter_styles) && all(batter_styles %in% c("", "unknown"))) {
        data <- data %>% mutate(bowlerCategory = case_when(bowlerCategory %in% c("Spin In", "Spin Away") ~ 
                                                             "Spin", bowlerStyle %in% c("RLS", "ROS", "LLS", "LOS") ~ "Spin", TRUE ~ as.character(bowlerCategory)))
      }
    }
    matchup_values <- trimws(as.character(data[[group_column]]))
    matchup_values[is.na(matchup_values) | matchup_values == "" | tolower(matchup_values) == "unknown"] <- "Unknown"
    data[[group_column]] <- matchup_values
    batting <- state$discipline == "Batting"
    entity_group <- {
      if (batting) 
        "strikerParticipantId"
      else "bowlerParticipantId"
    }
    entity_name <- {
      if (batting) 
        "strikerFullName"
      else "bowlerFullName"
    }
    summary <- data %>% filter((.data[[entity_name]] != "Private" & .data[[entity_name]] != "")) %>% summarise_matchup_metrics(state$discipline, 
                                                                                                                               entity_group, entity_name, group_column, group_column)
    names(summary)[1:2] <- c({
      if (state$discipline == "Batting") "Batter" else "Bowler"
    }, matchup_label)
    if (!is.null(matchup_order)) {
      summary <- summary %>% arrange(match(.data[[matchup_label]], matchup_order))
    }
    else if (numeric_order) {
      summary <- summary %>% arrange(suppressWarnings(as.numeric(as.character(.data[[matchup_label]]))))
    }
    summary
  }
  analysis_report_style_summary <- reactive({
    state <- analysis_report_state()
    if (state$discipline == "Batting") {
      summarise_analysis_report_matchup("bowlerCategory", "Bowler Category", matchup_order = c("RF", "LF", 
                                                                                               "Spin", "Spin In", "Spin Away", "Unknown"))
    }
    else {
      summarise_analysis_report_matchup("strikerStyle", "Batter Style", matchup_order = c("RHB", "LHB", "Unknown"))
    }
  })
  analysis_report_card_summary <- reactive({
    state <- analysis_report_state()
    data <- analysis_report_filtered_data()
    req(nrow(data))
    sources <- analysis_report_expected_sources(state)
    entity_column <- {
      if (state$discipline == "Batting") 
        "strikerParticipantId"
      else "bowlerParticipantId"
    }
    appearances <- data %>% transmute(matchId, inningsOrder, entity = as.character(.data[[entity_column]])) %>% 
      filter(!is.na(matchId), !is.na(entity), entity != "")
    matches <- n_distinct(analysis_report_match_data_summary()$matchId)
    innings <- appearances %>% distinct(matchId, inningsOrder, entity) %>% nrow()
    count_text <- function(value) {
      format(round(value), big.mark = ",", scientific = FALSE, trim = TRUE)
    }
    number_text <- function(value, digits = 1) {
      if (!is.finite(value)) 
        return("\u2014")
      format(round(value, digits), trim = TRUE, scientific = FALSE)
    }
    percent_text <- function(value) {
      if (!is.finite(value)) 
        return("")
      paste0(format(round(value, 1), trim = TRUE, scientific = FALSE), "%")
    }
    expected_p100 <- function(column) {
      if (!is.finite(balls) || balls <= 0) 
        return(NA_real_)
      sum(data[[column]], na.rm = TRUE)/balls * 100
    }
    make_rows <- function(labels, raw, percentage, expected, p100, digits) {
      data.frame(Label = labels, Raw = raw, Percentage = percentage, Expected = as.numeric(expected), ExpectedP100 = as.logical(p100), 
                 ExpectedUnit = ifelse(as.logical(p100), "p/100", ""), ExpectedDigits = as.integer(digits), stringsAsFactors = FALSE)
    }
    if (state$discipline == "Batting") {
      runs <- sum(data$runsBat, na.rm = TRUE)
      balls <- sum(data$ballFaced, na.rm = TRUE)
      dismissals <- sum(data$strikerOut, na.rm = TRUE)
      innings_scores <- data %>% group_by(matchId, inningsOrder, entity = .data[[entity_column]]) %>% summarise(batter_runs = sum(runsBat, 
                                                                                                                                  na.rm = TRUE), balls = sum(ballFaced, na.rm = TRUE), dismissals = {
                                                                                                                                    sum(strikerOut, na.rm = TRUE)
                                                                                                                                  }, .groups = "drop") %>% arrange(desc(batter_runs), balls)
      best <- innings_scores[1, , drop = FALSE]
      best_label <- format_player_batting_score(best$batter_runs, best$balls, best$dismissals)
      expected_runs <- sum(data[[sources$scoring[1]]], na.rm = TRUE)
      expected_dismissals <- sum(data[[sources$scoring[2]]], na.rm = TRUE)
      x_average <- expected_average(runs, dismissals, expected_runs, expected_dismissals, batting = TRUE)
    }
    else {
      dismissals <- sum(data$bowlerWicket, na.rm = TRUE)
      runs <- sum(data$runsBowl, na.rm = TRUE)
      balls <- sum(data$ballBowled, na.rm = TRUE)
      innings_scores <- data %>% group_by(matchId, inningsOrder, entity = .data[[entity_column]]) %>% summarise(dismissals = sum(bowlerWicket, 
                                                                                                                                 na.rm = TRUE), runs = sum(runsBowl, na.rm = TRUE), balls = sum(ballBowled, na.rm = TRUE), .groups = "drop") %>% 
        arrange(desc(dismissals), runs, balls)
      best <- innings_scores[1, , drop = FALSE]
      best_label <- paste0(best$dismissals, "/", best$runs, " (", format_match_overs(best$balls), ")")
      expected_runs <- sum(data[[sources$scoring[1]]], na.rm = TRUE)
      expected_dismissals <- sum(data[[sources$scoring[2]]], na.rm = TRUE)
      x_average <- expected_average(runs, dismissals, expected_runs, expected_dismissals, batting = FALSE)
    }
    batting <- state$discipline == "Batting"
    primary <- if (batting) 
      runs
    else dismissals
    rate <- if (balls > 0) 
      runs/balls * if (batting) 
        100
    else 6
    else NA_real_
    average <- if (dismissals > 0) 
      runs/dismissals
    else NA_real_
    dots <- sum(data[[if (batting) "batterDots" else "bowlerDots"]], na.rm = TRUE)
    multiples <- sum(data$multiples, na.rm = TRUE)
    boundaries <- sum(data$fours + data$sixes, na.rm = TRUE)
    wicket_labels <- wicket_display_names(state$discipline)
    wicket_counts <- vapply(wicket_actual_sources(state$discipline), function(column) sum(data[[column]], na.rm = TRUE), 
                            numeric(1))
    rows <- bind_rows(make_rows(c("Matches", "Innings", if (batting) "Runs" else "Wickets", if (batting) "Highest Score" else "Best Bowling", 
                                  if (batting) "Batting Average" else "Bowling Average", if (batting) "Strike Rate" else "Economy", if (batting) "Balls per Dismissal" else "Strike Rate", 
                                  "Dots", "Multiples", "Boundaries"), c(count_text(matches), count_text(innings), count_text(primary), 
                                                                        best_label, number_text(average), number_text(rate), number_text(if (dismissals > 0) balls/dismissals else NA_real_), 
                                                                        count_text(dots), count_text(multiples), count_text(boundaries)), c(rep("", 7), percent_text(dots/balls * 
                                                                                                                                                                       100), percent_text(multiples/balls * 100), percent_text(boundaries/balls * 100)), c(rep(NA_real_, 4), 
                                                                                                                                                                                                                                                           x_average, vapply(sources$scoring, expected_p100, numeric(1))), c(rep(FALSE, 5), rep(TRUE, 5)), rep(1L, 
                                                                                                                                                                                                                                                                                                                                                               10)), make_rows(wicket_labels, vapply(wicket_counts, count_text, character(1)), vapply(wicket_counts/dismissals * 
                                                                                                                                                                                                                                                                                                                                                                                                                                                        100, percent_text, character(1)), vapply(sources$wickets, expected_p100, numeric(1)), rep(TRUE, length(wicket_labels)), 
                                                                                                                                                                                                                                                                                                                                                                               rep(2L, length(wicket_labels))))
    if (batting) {
      rows$ExpectedUnit[rows$Label == "Balls per Dismissal"] <- "dismissals p/100"
    }
    else {
      rows$ExpectedUnit[rows$Label == "Economy"] <- "runs p/100"
      rows$ExpectedUnit[rows$Label == "Strike Rate"] <- "wickets p/100"
    }
    list(Matches = matches, Innings = innings, Primary = primary, Average = average, Rate = rate, Best = best_label, 
         Dots = dots, Multiples = multiples, Boundaries = boundaries, Rows = rows)
  })
  analysis_report_percentiles <- reactive({
    state <- analysis_report_state()
    benchmark <- analysis_report_entity_summary()
    selected_data <- analysis_report_filtered_data()
    req(nrow(benchmark), nrow(selected_data))
    sources <- analysis_report_expected_sources(state)
    balls_column <- if (state$discipline == "Batting") 
      "ballFaced"
    else "ballBowled"
    selected_balls <- sum(selected_data[[balls_column]], na.rm = TRUE)
    standard_values <- suppressWarnings(as.numeric(as.character(selected_data$standard)))
    standard_values <- standard_values[is.finite(standard_values)]
    competition_strength <- if (length(standard_values)) {
      round(mean(standard_values), 1)
    }
    else {
      NA_real_
    }
    selected_standard_values <- vapply(c(sources$scoring, sources$wickets), function(column) sum(selected_data[[column]], 
                                                                                                 na.rm = TRUE)/selected_balls * 100, numeric(1))
    selected_runs <- if (state$discipline == "Batting") {
      sum(selected_data$runsBat, na.rm = TRUE)
    }
    else {
      sum(selected_data$runsBowl, na.rm = TRUE)
    }
    selected_dismissals <- if (state$discipline == "Batting") {
      sum(selected_data$strikerOut, na.rm = TRUE)
    }
    else {
      sum(selected_data$bowlerWicket, na.rm = TRUE)
    }
    selected_expected_runs <- sum(selected_data[[sources$scoring[1]]], na.rm = TRUE)
    selected_expected_dismissals <- sum(selected_data[[sources$scoring[2]]], na.rm = TRUE)
    selected_x_average <- expected_average(selected_runs, selected_dismissals, selected_expected_runs, selected_expected_dismissals, 
                                           state$discipline == "Batting")
    selected_wp <- sum(capped_win_probability(selected_data$winProbChange, bowling = state$discipline == "Bowling"), 
                       na.rm = TRUE)
    selected_wp_denominator <- if (state$discipline == "Batting") {
      selected_dismissals
    }
    else {
      selected_data %>% transmute(matchId, inningsOrder, entity = as.character(bowlerParticipantId)) %>% 
        distinct() %>% nrow()
    }
    selected_wp_per_match <- if (selected_balls != 0 && selected_wp_denominator != 0) {
      ((selected_wp/selected_balls * 100)/100) * (selected_balls/selected_wp_denominator)
    }
    else {
      NA_real_
    }
    scoring_count <- length(sources$scoring)
    actual_wicket_columns <- wicket_actual_sources(state$discipline)
    selected_actual_wickets <- vapply(actual_wicket_columns, function(column) sum(selected_data[[column]], 
                                                                                  na.rm = TRUE), numeric(1))
    percentile <- function(value, values) {
      values <- values[is.finite(values)]
      if (!is.finite(value) || !length(values)) 
        return(NA_real_)
      if (abs(value) < sqrt(.Machine$double.eps)) 
        return(50)
      if (value > 0) {
        positive_values <- values[values > 0]
        if (!length(positive_values)) 
          return(100)
        return(round(50 + mean(positive_values <= value) * 50, 1))
      }
      negative_values <- values[values < 0]
      if (!length(negative_values)) 
        return(0)
      round(mean(negative_values <= value) * 50, 1)
    }
    scoring_percentiles <- vapply(seq_along(sources$scoring), function(i) percentile(selected_standard_values[i], 
                                                                                     benchmark[[sources$scoring[i]]]/benchmark$balls * 100), numeric(1))
    wicket_percentiles <- vapply(seq_along(sources$wickets), function(i) {
      selected_value <- selected_standard_values[scoring_count + i]
      if (is.finite(selected_value) && abs(selected_value) < sqrt(.Machine$double.eps)) {
        return(50)
      }
      if (selected_actual_wickets[i] == 0) {
        return(if (state$discipline == "Batting") 100 else 0)
      }
      eligible <- is.finite(benchmark[[actual_wicket_columns[i]]]) & benchmark[[actual_wicket_columns[i]]] > 
        0
      percentile(selected_value, benchmark[[sources$wickets[i]]][eligible]/benchmark$balls[eligible] * 100)
    }, numeric(1))
    standard_percentiles <- c(scoring_percentiles, wicket_percentiles)
    derived_percentiles <- c(percentile(selected_x_average, benchmark$.xAverage), percentile(selected_wp_per_match, 
                                                                                             benchmark$.wPPerMatch))
    scoring_values <- c(derived_percentiles, standard_percentiles[seq_len(2)], standard_percentiles[seq.int(3, 
                                                                                                            scoring_count)])
    scoring_labels <- if (state$discipline == "Batting") {
      c("Batting Avg.", "Win Prob. Added", "Run Rate", "Dismissal Rate", "Dots Faced", "Multiples Scored", 
        "Boundaries Scored")
    }
    else {
      c("Bowling Avg.", "Win Prob. Added", "Economy", "Strike Rate", "Dots Bowled", "Multiples Conceded", 
        "Boundaries Conceded")
    }
    list(scoring = c(setNames(scoring_values, scoring_labels), `Competition Strength` = competition_strength), 
         wickets = c(setNames(standard_percentiles[-seq_len(scoring_count)], sources$wicket_labels), `Competition Strength` = competition_strength))
  })
  analysis_report_win_probability_phases <- reactive({
    state <- analysis_report_state()
    data <- analysis_report_base_filtered_data()
    balls_column <- if (state$discipline == "Batting") 
      "ballFaced"
    else "ballBowled"
    data %>% filter(is.finite(overNumber)) %>% mutate(over_block_start = floor((overNumber - 1)/5) * 5 + 1, 
                                                      phase_wp = capped_win_probability(winProbChange, bowling = state$discipline == "Bowling"), phase_runs = if (state$discipline == 
                                                                                                                                                                  "Bowling") {
                                                        runsBowl
                                                      }
                                                      else {
                                                        runsBat
                                                      }, phase_dismissals = if (state$discipline == "Bowling") {
                                                        bowlerWicket
                                                      }
                                                      else {
                                                        strikerOut
                                                      }) %>% group_by(over_block_start) %>% summarise(wP = sum(phase_wp, na.rm = TRUE), balls = sum(.data[[balls_column]], 
                                                                                                                                                    na.rm = TRUE), runs = sum(phase_runs, na.rm = TRUE), dismissals = sum(phase_dismissals, na.rm = TRUE), 
                                                                                                      .groups = "drop") %>% filter(balls >= 60) %>% mutate(over_block_end = over_block_start + 4, OverBlock = paste0(over_block_start, 
                                                                                                                                                                                                                     "-", over_block_end), wP100 = wP/balls * 100, Average = ifelse(dismissals > 0, runs/dismissals, NA_real_)) %>% 
      arrange(over_block_start)
  })
  analysis_report_performance_by_match <- reactive({
    state <- analysis_report_state()
    data <- analysis_report_filtered_data()
    batting <- state$discipline == "Batting"
    entity_column <- {
      if (batting) 
        "strikerParticipantId"
      else "bowlerParticipantId"
    }
    name_column <- {
      if (batting) 
        "strikerFullName"
      else "bowlerFullName"
    }
    scores <- data %>% group_by(matchId, inningsOrder, EntityId = .data[[entity_column]], Entity = .data[[name_column]]) %>% 
      summarise(runs = sum(if (!batting) runsBowl else runsBat, na.rm = TRUE), balls = sum(.data[[if (batting) "ballFaced" else "ballBowled"]], 
                                                                                           na.rm = TRUE), dismissals = sum(if (!batting) bowlerWicket else strikerOut, na.rm = TRUE), .groups = "drop") %>% 
      mutate(Score = if (batting) 
        format_player_batting_score(runs, balls, dismissals)
        else format_innings_score(dismissals, runs, balls))
    scores %>% filter(!is.na(Score), Score != "") %>% arrange(matchId, EntityId, inningsOrder) %>% group_by(matchId, 
                                                                                                            EntityId, Entity) %>% summarise(Score = collapse_innings_values(Score), .groups = "drop") %>% group_by(matchId) %>% 
      summarise(Performance = if (n() == 1) {
        first(Score)
      }
      else {
        paste0(Entity, ": ", Score, collapse = "; ")
      }, .groups = "drop")
  })
  summarise_report_impact <- function(data, discipline, groups = "matchId") {
    batting <- discipline == "Batting"
    entity <- if (batting) 
      "strikerParticipantId"
    else "bowlerParticipantId"
    impact <- if (batting) 
      "strikerBatMatchMvp"
    else "bowlerBowlMatchMvp"
    data %>% mutate(.entity_id = as.character(.data[[entity]]), .impact = .data[[impact]]) %>% filter(!is.na(.entity_id), 
                                                                                                      .entity_id != "", is.finite(.impact)) %>% group_by(across(all_of(c(groups, ".entity_id")))) %>% summarise(.player_impact = mode(.impact), 
                                                                                                                                                                                                                .groups = "drop") %>% group_by(across(all_of(groups))) %>% summarise(ImpactRating = mean(.player_impact, 
                                                                                                                                                                                                                                                                                                         na.rm = TRUE), .groups = "drop")
  }
  analysis_report_impact_by_match <- reactive({
    state <- analysis_report_state()
    data <- analysis_report_filtered_data()
    impact_data <- summarise_report_impact(data, state$discipline, c("matchId", "startDate", "matchName")) %>% 
      filter(is.finite(ImpactRating)) %>% arrange(startDate, matchId) %>% mutate(MatchNumber = row_number())
    impact_data %>% left_join(analysisLookup %>% select(matchId, grade, format, season), by = "matchId") %>% 
      left_join(analysis_report_performance_by_match(), by = "matchId")
  })
  analysis_report_composition <- reactive({
    state <- analysis_report_state()
    data <- analysis_report_filtered_data()
    expected_column <- analysis_report_expected_sources(state)$scoring[1]
    if (state$discipline == "Batting") {
      data %>% filter(is.finite(strikerBallsFaced)) %>% mutate(Phase = case_when(strikerBallsFaced <= 25 ~ 
                                                                                   "0-25 balls", strikerBallsFaced <= 50 ~ "26-50 balls", TRUE ~ "51+ balls"), Phase = factor(Phase, 
                                                                                                                                                                              levels = c("0-25 balls", "26-50 balls", "51+ balls"))) %>% group_by(Phase) %>% summarise(RunsScored = {
                                                                                                                                                                                sum(runsBat, na.rm = TRUE)
                                                                                                                                                                              }, BallsFaced = sum(ballFaced, na.rm = TRUE), Dismissals = {
                                                                                                                                                                                sum(strikerOut, na.rm = TRUE)
                                                                                                                                                                              }, Expected = ifelse(sum(ballFaced, na.rm = TRUE) > 0, sum(.data[[expected_column]], na.rm = TRUE)/sum(ballFaced, 
                                                                                                                                                                                                                                                                                     na.rm = TRUE) * 100, NA_real_), Balls = sum(ballFaced, na.rm = TRUE), .groups = "drop") %>% mutate(BattingAverage = ifelse(Dismissals > 
                                                                                                                                                                                                                                                                                                                                                                                                                  0, RunsScored/Dismissals, NA_real_)) %>% arrange(Phase)
    }
    else {
      data %>% filter(deliveryNumber %in% 1:6) %>% group_by(Ball = as.integer(deliveryNumber)) %>% summarise(RunsConceded = sum(runsBowl, 
                                                                                                                                na.rm = TRUE), BallsBowled = sum(ballBowled, na.rm = TRUE), Wickets = sum(bowlerWicket, na.rm = TRUE), 
                                                                                                             Expected = ifelse(sum(ballBowled, na.rm = TRUE) > 0, sum(.data[[expected_column]], na.rm = TRUE)/sum(ballBowled, 
                                                                                                                                                                                                                  na.rm = TRUE) * 100, NA_real_), Balls = sum(ballBowled, na.rm = TRUE), .groups = "drop") %>% 
        mutate(BowlingAverage = ifelse(Wickets > 0, RunsConceded/Wickets, NA_real_)) %>% arrange(Ball)
    }
  })
  analysis_report_metric_values <- reactive({
    state <- analysis_report_state()
    data <- analysis_report_base_filtered_data()
    impact_data <- summarise_report_impact(data, state$discipline)
    average_impact <- if (nrow(impact_data)) {
      round(mean(impact_data$ImpactRating, na.rm = TRUE), 1)
    }
    else {
      NA_real_
    }
    total_impact <- if (nrow(impact_data)) {
      round(sum(impact_data$ImpactRating - 5, na.rm = TRUE), 1)
    }
    else {
      NA_real_
    }
    wp_values <- capped_win_probability(data$winProbChange, bowling = state$discipline == "Bowling")
    wp_values <- wp_values[is.finite(wp_values)]
    total_wp <- if (length(wp_values)) 
      round(sum(wp_values), 1)
    else NA_real_
    innings_entity_column <- {
      if (state$discipline == "Batting") 
        "strikerParticipantId"
      else "bowlerParticipantId"
    }
    innings_count <- data %>% transmute(matchId, inningsOrder, entity = as.character(.data[[innings_entity_column]])) %>% 
      filter(!is.na(matchId), !is.na(inningsOrder), !is.na(entity), entity != "") %>% distinct() %>% nrow()
    average_wp <- if (is.finite(total_wp) && innings_count > 0) {
      round(total_wp/innings_count, 1)
    }
    else {
      NA_real_
    }
    list(average_impact = average_impact, total_impact = total_impact, average_wp = average_wp, total_wp = total_wp, 
         innings = innings_count)
  })
  # Report charts and summary tiles
  analysis_report_percentile_plot <- function(values) {
    labels <- names(values)
    numeric_values <- as.numeric(values)
    valid_values <- is.finite(numeric_values)
    display_values <- replace(numeric_values, !valid_values, 0)
    raw_values <- labels == "Competition Strength"
    colour_scale <- (grDevices::colorRampPalette(c("#A33A31", "#FBD437", "#16794B")))(101)
    point_colours <- colour_scale[pmin(pmax(round(display_values), 0), 100) + 1L]
    point_colours[!valid_values] <- "#B8B8B8"
    track_shapes <- lapply(seq_along(labels), function(i) {
      list(type = "line", xref = "x", yref = "y", x0 = 0, x1 = display_values[i], y0 = labels[i], y1 = labels[i], 
           line = list(color = point_colours[i], width = 5))
    })
    track_shapes <- append(track_shapes, list(list(type = "line", xref = "x", yref = "paper", x0 = 50, x1 = 50, 
                                                   y0 = 0, y1 = 1, line = list(color = "#74756C", width = 1.5, dash = "dot"))))
    value_text <- ifelse(!valid_values, "\u2014", ifelse(raw_values, sprintf("%.1f", 
                                                                             display_values), sprintf("%.0f%%", display_values)))
    hover_text <- ifelse(raw_values, paste0(labels, ": ", value_text, " (raw 0-100)"), paste0(labels, ": ", 
                                                                                              value_text))
    plot <- plot_ly(x = display_values, y = labels, type = "scatter", mode = "markers+text", text = value_text, 
                    textposition = "middle right", cliponaxis = FALSE, hovertext = hover_text, hoverinfo = "text", marker = list(color = point_colours, 
                                                                                                                                 size = 13, line = list(color = "#FFFFFF", width = 2))) %>% report_plot_layout(shapes = track_shapes, 
                                                                                                                                                                                                               annotations = list(list(x = 50, y = 1.06, xref = "x", yref = "paper", text = "Average (50%)", showarrow = FALSE, 
                                                                                                                                                                                                                                       font = list(color = "#74756C", size = 10))), xaxis = list(title = "Percentile", range = c(0, 105), 
                                                                                                                                                                                                                                                                                                 tickformat = ".0f", ticksuffix = "%", showgrid = FALSE, zeroline = FALSE, tickfont = list(color = "#74756C", 
                                                                                                                                                                                                                                                                                                                                                                                           size = 11), automargin = TRUE), yaxis = list(title = "", autorange = "reversed", showgrid = FALSE, 
                                                                                                                                                                                                                                                                                                                                                                                                                                        categoryorder = "array", categoryarray = labels, tickfont = list(color = "#222222", size = 12), 
                                                                                                                                                                                                                                                                                                                                                                                                                                        automargin = TRUE), margin = list(t = 35, r = 65, b = 55, l = 135), showlegend = FALSE) %>% dashboard_plot_config() %>% 
      layout(title = NULL)
  }
  analysis_report_signed_bar_plot <- function(labels, values, y_title, balls = NULL, x_title = "", hover_text = NULL) {
    values <- as.numeric(values)
    colours <- ifelse(values > 0, "#16794B", ifelse(values < 0, "#A33A31", "#FBD437"))
    extent <- max(abs(values), na.rm = TRUE)
    if (!is.finite(extent) || extent == 0) 
      extent <- 1
    axis_limit <- extent * 1.28
    label_offset <- extent * 0.05
    label_y <- ifelse(values > 0, values + label_offset, label_offset)
    label_annotations <- lapply(seq_along(labels), function(i) {
      list(x = i - 1L, y = label_y[i], xref = "x", yref = "y", text = sprintf("%+.1f", values[i]), showarrow = FALSE, 
           xanchor = "center", yanchor = "bottom", font = list(color = colours[i], size = 11))
    })
    if (is.null(hover_text)) {
      hover_text <- paste0(labels, "<br>", y_title, ": ", sprintf("%+.1f", values), if (!is.null(balls)) 
        paste0("<br>Balls: ", balls)
        else "")
    }
    plot <- plot_ly(x = labels, y = values, type = "bar", cliponaxis = FALSE, hovertext = hover_text, hoverinfo = "text", 
                    marker = list(color = colours, line = list(color = "#FFFFFF", width = 1.5))) %>% report_plot_layout(xaxis = list(title = x_title, 
                                                                                                                                     showgrid = FALSE, zeroline = FALSE, categoryorder = "array", categoryarray = labels, tickfont = list(color = "#222222", 
                                                                                                                                                                                                                                          size = 11), automargin = TRUE), yaxis = list(title = y_title, range = c(-axis_limit, axis_limit), 
                                                                                                                                                                                                                                                                                       tickformat = "+.1f", showgrid = FALSE, zeroline = TRUE, zerolinecolor = "#74756C", zerolinewidth = 1.5, 
                                                                                                                                                                                                                                                                                       tickfont = list(color = "#74756C", size = 11), automargin = TRUE), margin = list(t = 25, r = 35, b = if (nzchar(x_title)) 58 else 48, 
                                                                                                                                                                                                                                                                                                                                                                        l = 70), showlegend = FALSE)
    plot$x$layout$annotations <- label_annotations
    dashboard_plot_config(plot)
  }
  output$analysisReportMessage <- renderUI({
    req(identity(), input$generateAnalysis > 0)
    if (nrow(analysis_report_filtered_data()) == 0) {
      div(class = "generated-filter-summary", "No data for this player with the selected filters.")
    }
  })
  output$analysisReportReady <- renderText({
    req(identity(), input$generateAnalysis > 0)
    req(nrow(analysis_report_filtered_data()) > 0)
    "ready"
  })
  outputOptions(output, "analysisReportReady", suspendWhenHidden = FALSE)
  output$analysisReportSummaryTable <- renderUI({
    rows <- analysis_report_card_summary()$Rows
    expected_text <- function(value, unit, digits) {
      if (!is.finite(value)) 
        return("")
      text <- sprintf(paste0("%+.", digits, "f"), value)
      if (nzchar(unit)) 
        paste(text, unit)
      else text
    }
    expected_tone <- function(value) {
      if (!is.finite(value) || value == 0) {
        "neutral"
      }
      else if (value > 0) 
        "positive"
      else "negative"
    }
    tags$table(class = "analysis-report-summary-table", tags$thead(tags$tr(tags$th(""), tags$th("Raw"), tags$th("Percentage"), 
                                                                           tags$th("Expected"))), tags$tbody(lapply(seq_len(nrow(rows)), function(i) {
                                                                             expected <- rows$Expected[i]
                                                                             value_class <- paste("analysis-report-summary-value", if (is.finite(expected)) 
                                                                               expected_tone(expected)
                                                                               else "")
                                                                             tags$tr(tags$th(rows$Label[i]), tags$td(class = value_class, rows$Raw[i]), tags$td(class = value_class, 
                                                                                                                                                                rows$Percentage[i]), tags$td(class = value_class, expected_text(expected, rows$ExpectedUnit[i], 
                                                                                                                                                                                                                                rows$ExpectedDigits[i])))
                                                                           })))
  })
  output$analysisReportMetrics <- renderUI({
    metrics <- analysis_report_metric_values()
    state <- analysis_report_state()
    tone <- function(value, baseline = 0) {
      if (!is.finite(value) || value == baseline) {
        "neutral"
      }
      else if (value > baseline) 
        "positive"
      else "negative"
    }
    metric_tile <- function(label, value, display, note, baseline = 0) {
      div(class = paste("analysis-report-card analysis-report-metric-tile", tone(value, baseline)), div(class = "analysis-report-metric-label", 
                                                                                                        label), div(class = "analysis-report-metric-value", display), div(class = "analysis-report-metric-note", 
                                                                                                                                                                          note))
    }
    innings <- metrics$innings
    innings_label <- paste("Across", innings, tolower(state$discipline), if (innings == 1) 
      "inning"
      else "innings")
    ordinal <- function(value) {
      value <- as.integer(round(value))
      suffix <- if (value%%100 %in% 11:13) {
        "th"
      }
      else {
        switch(as.character(value%%10), `1` = "st", `2` = "nd", `3` = "rd", "th")
      }
      paste0(value, suffix)
    }
    average_impact_note <- if (is.finite(metrics$average_impact)) {
      percentile <- metrics$average_impact * 10
      paste0("Average performance is in the ", ordinal(percentile), " percentile of all performances ", "(5 represents a league-average player)")
    }
    else {
      paste("Average performance percentile is unavailable", "(5 represents a league-average player)")
    }
    div(class = "analysis-report-metric-grid", metric_tile("Average Impact Rating", metrics$average_impact, 
                                                           if (is.finite(metrics$average_impact)) 
                                                             sprintf("%.1f", metrics$average_impact)
                                                           else "\u2014", average_impact_note, baseline = 5), metric_tile("Total Impact", 
                                                                                                                          metrics$total_impact, if (is.finite(metrics$total_impact)) 
                                                                                                                            sprintf("%+.1f", metrics$total_impact)
                                                                                                                          else "\u2014", innings_label), metric_tile("Average Win Probability per Innings", 
                                                                                                                                                                     metrics$average_wp, if (is.finite(metrics$average_wp)) 
                                                                                                                                                                       sprintf("%+.1f%%", metrics$average_wp)
                                                                                                                                                                     else "\u2014", if (!is.finite(metrics$average_wp)) {
                                                                                                                                                                       "Change to their team's chances of winning is unavailable"
                                                                                                                                                                     }
                                                                                                                                                                     else if (metrics$average_wp > 0) {
                                                                                                                                                                       "Increases their team's chances of winning"
                                                                                                                                                                     }
                                                                                                                                                                     else if (metrics$average_wp < 0) {
                                                                                                                                                                       "Decreases their team's chances of winning"
                                                                                                                                                                     }
                                                                                                                                                                     else {
                                                                                                                                                                       "Does not change their team's chances of winning"
                                                                                                                                                                     }), metric_tile("Total Wins Added", metrics$total_wp, if (is.finite(metrics$total_wp)) 
                                                                                                                                                                       sprintf("%+.2f", metrics$total_wp/100)
                                                                                                                                                                       else "\u2014", innings_label))
  })
  report_copy <- list(ScoringTitle = rep("{subjects}: {discipline} Scoring Profile", 2), ScoringCaption = rep("Percentile ranks of key {discipline_lower} stats compared to other players, adjusted for match context (positive is always better)", 
                                                                                                              2), WicketTitle = rep("{subjects}: {discipline} Wicket Profile", 2), WicketCaption = rep("Percentile ranks of {discipline_lower} dismissals compared to other players, adjusted for match context (positive is always better)", 
                                                                                                                                                                                                       2), SummaryTitle = rep("{subjects}: Report Summary", 2), SummaryCaption = rep("Key {discipline_lower} metrics with raw, percentage, and expected (positive is always better) versions", 
                                                                                                                                                                                                                                                                                     2), MatchupTitle = c("{subjects}: Expected Batting Avg. by Matchup", "{subjects}: Expected Bowling Avg. by Matchup"), 
                      MatchupCaption = c("Batting average compared to the average player, by bowler type (positive is always better).", 
                                         "Bowling average compared to the average player, by batter type (positive is always better)."), CompositionTitle = c("{subjects}: Expected Strike Rate Progression", 
                                                                                                                                                              "{subjects}: Expected Economy Progression"), CompositionCaption = c("Relative strike rate as the batter progresses through their innings", 
                                                                                                                                                                                                                                  "Relative runs conceded p/100 for each ball in an over"), WinProbabilityTitle = rep("{subjects}: {discipline} Win Probability by Phase", 
                                                                                                                                                                                                                                                                                                                      2), WinProbabilityCaption = rep("Identify strong and weak phases in performance, best used with a format filter", 
                                                                                                                                                                                                                                                                                                                                                      2), ImpactTitle = rep("{subjects}: {discipline} Impact by Match", 2), ImpactCaption = c("Every career innings in the database, ranked by impact rating (5 represents a league-average performance).", 
                                                                                                                                                                                                                                                                                                                                                                                                                                              "Every career bowling innings in the database, ranked by impact rating (5 represents a league-average performance)."), 
                      TableTitle = rep("{subjects}: {discipline} Match Data", 2), TableCaption = rep("Detailed {discipline_lower} data for every match in the database - use toggle columns to view additional metrics", 
                                                                                                     2))
  lapply(c("Scoring", "Wicket", "Summary", "Matchup", "Composition", "WinProbability", "Impact", "Table"), function(part) {
    lapply(c("Title", "Caption"), function(kind) {
      output[[paste0("analysisReport", part, kind)]] <- renderText({
        state <- analysis_report_state()
        text <- report_copy[[paste0(part, kind)]][if (state$discipline == "Batting") 
          1
          else 2]
        text <- gsub("{subjects}", paste(state$selected, collapse = ", "), text, fixed = TRUE)
        text <- gsub("{discipline}", state$discipline, text, fixed = TRUE)
        gsub("{discipline_lower}", tolower(state$discipline), text, fixed = TRUE)
      })
    })
  })
  output$analysisReportScoringPlot <- renderPlotly({
    analysis_report_percentile_plot(analysis_report_percentiles()$scoring)
  })
  output$analysisReportWicketPlot <- renderPlotly({
    analysis_report_percentile_plot(analysis_report_percentiles()$wickets)
  })
  output$analysisReportMatchupPlot <- renderPlotly({
    state <- analysis_report_state()
    category_column <- if (state$discipline == "Batting") 
      "Bowler Category"
    else "Batter Style"
    metric_column <- if (state$discipline == "Batting") 
      "xBatting Average"
    else "xBowling Average"
    source_data <- analysis_report_style_summary()
    plot_data <- if (state$discipline == "Batting") {
      source_data %>% transmute(Category = as.character(.data[[category_column]]), Value = as.numeric(.data[[metric_column]]), 
                                RunsScored = as.numeric(`Batter Runs`), BallsFaced = as.numeric(`Balls Faced`), Dismissals = as.numeric(Dismissals), 
                                Average = as.numeric(Average))
    }
    else {
      source_data %>% transmute(Category = as.character(.data[[category_column]]), Value = as.numeric(.data[[metric_column]]), 
                                Wickets = as.numeric(Wickets), RunsConceded = as.numeric(`Runs Conceded`), BallsBowled = as.numeric(`Balls Bowled`), 
                                Average = as.numeric(Average))
    }
    plot_data <- plot_data %>% filter(!is.na(Category), Category != "", is.finite(Value))
    shiny::validate(shiny::need(nrow(plot_data) > 0, "No matchup expected-average data available"))
    hover_text <- if (state$discipline == "Batting") {
      paste0(plot_data$Category, "<br>Balls faced: ", format(plot_data$BallsFaced, big.mark = ",", trim = TRUE), 
             "<br>Batting average: ", ifelse(is.finite(plot_data$Average), sprintf("%.1f", plot_data$Average), 
                                             "\u2014"), "<br>xBatting average: ", sprintf("%+.1f", plot_data$Value))
    }
    else {
      paste0(plot_data$Category, "<br>Balls bowled: ", format(plot_data$BallsBowled, big.mark = ",", trim = TRUE), 
             "<br>Bowling average: ", ifelse(is.finite(plot_data$Average), sprintf("%.1f", plot_data$Average), 
                                             "\u2014"), "<br>xBowling average: ", sprintf("%+.1f", plot_data$Value))
    }
    analysis_report_signed_bar_plot(plot_data$Category, plot_data$Value, metric_column, x_title = "Matchup Type", 
                                    hover_text = hover_text)
  })
  output$analysisReportCompositionPlot <- renderPlotly({
    state <- analysis_report_state()
    plot_data <- analysis_report_composition()
    shiny::validate(shiny::need(nrow(plot_data) > 0, "No composition data available"))
    if (state$discipline == "Batting") {
      plot_data <- plot_data %>% mutate(Group = as.character(Phase))
      metric_title <- "xR p/100"
    }
    else {
      plot_data <- plot_data %>% mutate(Group = as.character(Ball))
      metric_title <- "xRC p/100"
    }
    plot_data <- plot_data %>% filter(is.finite(Expected))
    shiny::validate(shiny::need(nrow(plot_data) > 0, "No composition data available"))
    hover_text <- if (state$discipline == "Batting") {
      paste0(plot_data$Group, "<br>Balls faced: ", format(plot_data$BallsFaced, big.mark = ",", trim = TRUE), 
             "<br>Batting average: ", ifelse(is.finite(plot_data$BattingAverage), sprintf("%.1f", plot_data$BattingAverage), 
                                             "\u2014"), "<br>xR p/100: ", sprintf("%+.1f", plot_data$Expected))
    }
    else {
      paste0(plot_data$Group, "<br>Balls bowled: ", format(plot_data$BallsBowled, big.mark = ",", trim = TRUE), 
             "<br>Bowling average: ", ifelse(is.finite(plot_data$BowlingAverage), sprintf("%.1f", plot_data$BowlingAverage), 
                                             "\u2014"), "<br>xRC p/100: ", sprintf("%+.1f", plot_data$Expected))
    }
    analysis_report_signed_bar_plot(plot_data$Group, plot_data$Expected, metric_title, plot_data$Balls, x_title = "Ball Number", 
                                    hover_text = hover_text)
  })
  output$analysisReportWinProbabilityPlot <- renderPlotly({
    state <- analysis_report_state()
    plot_data <- analysis_report_win_probability_phases()
    req(nrow(plot_data))
    point_colours <- ifelse(plot_data$wP100 >= 0, "#16794B", "#A33A31")
    plot_ly(plot_data, x = ~OverBlock, y = ~wP100, type = "scatter", mode = "lines+markers", line = list(color = "#2176FF", 
                                                                                                         width = 3), marker = list(color = point_colours, size = 11, line = list(color = "#FFFFFF", width = 2)), 
            text = ~paste0("Overs ", OverBlock, "<br>wP p/100: ", sprintf("%+.1f%%", wP100), "<br>Balls: ", balls, 
                           "<br>", state$discipline, " average: ", ifelse(is.finite(Average), sprintf("%.1f", Average), "-")), 
            hoverinfo = "text") %>% report_plot_layout(xaxis = list(title = "Overs", showgrid = FALSE, zeroline = FALSE, 
                                                                    categoryorder = "array", categoryarray = plot_data$OverBlock, tickfont = list(color = "#74756C", size = 11), 
                                                                    automargin = TRUE), yaxis = list(title = "wP p/100", tickformat = "+.1f", ticksuffix = "%", gridcolor = "#E8E8E5", 
                                                                                                     zeroline = TRUE, zerolinecolor = "#74756C", automargin = TRUE), margin = list(t = 20, r = 25, b = 55, 
                                                                                                                                                                                   l = 65), showlegend = FALSE) %>% dashboard_plot_config()
  })
  output$analysisReportImpactPlot <- renderPlotly({
    state <- analysis_report_state()
    plot_data <- analysis_report_impact_by_match()
    req(nrow(plot_data))
    bar_colours <- ifelse(plot_data$ImpactRating >= 5, "#16794B", "#A33A31")
    impact_plot <- plot_ly(plot_data, x = ~MatchNumber, y = ~ImpactRating, type = "bar", marker = list(color = bar_colours, 
                                                                                                       line = list(color = "#FFFFFF", width = 1)), hovertext = ~paste0(matchName, "<br>Grade: ", grade, "<br>Format: ", 
                                                                                                                                                                       format, "<br>Season: ", season, "<br>Impact Rating: ", sprintf("%.1f", ImpactRating), ifelse(is.na(Performance) | 
                                                                                                                                                                                                                                                                      Performance == "", "", paste0("<br>", state$discipline, ": ", Performance))), hoverinfo = "text", 
                           textposition = "none")
    if (nrow(plot_data) >= 2 && n_distinct(plot_data$MatchNumber) >= 2) {
      trend_data <- plot_data %>% mutate(Trend = vapply(seq_len(n()), function(i) mean(ImpactRating[seq.int(max(1, 
                                                                                                                i - 4), i)], na.rm = TRUE), numeric(1))) %>% select(MatchNumber, Trend)
      impact_plot <- impact_plot %>% add_trace(data = trend_data, x = ~MatchNumber, y = ~Trend, type = "scatter", 
                                               mode = "lines", line = list(color = "#2176FF", width = 3), hoverinfo = "skip", inherit = FALSE)
    }
    impact_plot %>% report_plot_layout(shapes = list(list(type = "line", xref = "paper", yref = "y", x0 = 0, 
                                                          x1 = 1, y0 = 5, y1 = 5, line = list(color = "#74756C", width = 1.5, dash = "dot"))), xaxis = list(title = paste(state$discipline, 
                                                                                                                                                                          "Match Number"), nticks = 8, showgrid = FALSE, zeroline = FALSE, tickfont = list(color = "#74756C", 
                                                                                                                                                                                                                                                           size = 11), automargin = TRUE), yaxis = list(title = "Impact Rating", gridcolor = "#E8E8E5", zeroline = FALSE, 
                                                                                                                                                                                                                                                                                                        automargin = TRUE), margin = list(t = 20, r = 25, b = 55, l = 65), showlegend = FALSE, bargap = 0.25) %>% 
      dashboard_plot_config()
  })
  output$analysisReportTable <- renderDT({
    view <- function(columns) column_view_spec(df, columns)
    state <- analysis_report_state()
    selection_message <- "No data for this player with the selected filters."
    impact_column <- paste(state$discipline, "Impact Rating")
    df <- analysis_report_match_display_data()
    df <- mutate(df, across(all_of(impact_column), ~round(.x, 1)))
    shiny::validate(shiny::need(nrow(df) > 0, selection_message))
    metric_view <- normalise_metric_view(input$analysisReportMetricView)
    metric_columns <- match_data_metric_columns(metric_view)
    batting_report <- state$discipline == "Batting"
    active_metrics <- if (batting_report) 
      metric_columns$batting
    else metric_columns$bowling
    info_columns <- c("Player", "Match", "Grade", "Format", "Result")
    extra_info_columns <- c("Season", "Round", "Venue")
    overall_columns <- c(paste(state$discipline, "Impact Rating"), state$discipline, if (batting_report) "Bat wP" else "Bowl wP")
    discipline_columns <- {
      if (batting_report) {
        c("Batting Impact Rating", "Bat #", "Batting", "Batting SR", "Dismissal", "xR", "xD", active_metrics)
      }
      else {
        c("Bowling Impact Rating", "Bowl #", "Bowling", "Economy", "Extras", "Byes", "xRC", "xW", active_metrics)
      }
    }
    df <- select(df, all_of(unique(c(info_columns, extra_info_columns, overall_columns, discipline_columns))))
    stat_views <- c(Overview = view(c(info_columns, overall_columns)), setNames(view(c(setdiff(info_columns, 
                                                                                               "Result"), discipline_columns)), state$discipline))
    column_groups <- c(Overview = view(c(info_columns, extra_info_columns)), `Overall Stats` = view(overall_columns), 
                       setNames(view(discipline_columns), state$discipline))
    dashboard_datatable(df, storage_key = paste0("DT-player-report-", state$participant_id, "-", state$discipline), 
                        stat_views = stat_views, column_groups = column_groups, excel_download_id = "downloadAnalysisReportExcel", 
                        metric_view_input = "analysisReportMetricView", innings_view_input = "analysisReportInningsView", innings_choices = available_innings_choices(analysis_report_filtered_data()), 
                        page_length = 10)
  })
  # Export only the authenticated player's current report
  output$downloadAnalysisReportExcel <- downloadHandler(filename = function() {
    req(identity())
    paste0("player-report-", Sys.Date(), ".xlsx")
  }, content = function(file) {
    req(identity())
    write_dashboard_workbook(file, analysis_report_match_display_data(), analysis_report_current_filter_snapshot())
  }, contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet")
  outputOptions(output, "downloadAnalysisReportExcel", suspendWhenHidden = FALSE)
}

shinyApp(ui, server)

