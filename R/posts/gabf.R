library(knitr)
library(markUtils)
library(tidyverse)
library(janitor)
library(htmltools)
library(reactable)
library(gganimate)
library(glue)
library(sf)
library(plotly)
library(crosstalk)
library(RColorBrewer)
library(rlang)
library(sunburstR)
library(d3r)
library(tidylo)
library(tidytext)
gabf <- tidytuesdayR::tt_load('2020-10-20') %>% 
  pluck('beer_awards')  

gabf <- gabf %>% 
  mutate(medal = fct_relevel(medal, c("Bronze", "Silver"))) %>% 
  mutate(state = str_to_upper(state)) %>%
  mutate(medal_numeric = if_else(medal == "Gold", 3, if_else(medal == "Silver", 2, 1)))


awards_by_state <- . %>%
  group_by(state) %>%
  summarise(state_total_awards = n(), 
            state_total_awards_weighted = sum(medal_numeric),
            state_years_with_awards = n_distinct(year)) %>% 
  ungroup() %>% 
  right_join(albersusa::usa_sf("laea"), by = c("state"="iso_3166_2")) %>%
  rename("state_name"="name") %>% 
  mutate(state_avg_award = state_total_awards_weighted / state_total_awards,
         state_total_awards_per_cap = (state_total_awards / pop_2014) *100000,
         state_percent_total_awards = state_total_awards / sum(state_total_awards, na.rm = T)) %>%
  replace_na(list(state_total_awards = 0, state_total_awards_weighted = 0, avg_award = 0, state_total_awards_per_cap = 0, state_percent_total_awards = 0, state_years_with_awards = 0))


awards_by_state_year <- . %>% 
  add_count(year, name = "year_total_awards") %>% 
  group_by(state, year, year_total_awards) %>% 
  summarise(state_year_total_awards = n()) %>%
  ungroup() %>% 
  full_join(tibble(state = state.abb), by = c("state" = "state")) %>% 
  replace_na(list(year = 1987)) %>% 
  complete(state, nesting(year)) %>% 
  replace_na(list(state_year_total_awards = 0)) %>% 
  group_by(year) %>% 
  mutate(year_total_awards = max(year_total_awards, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(pct_of_year_total_awards = state_year_total_awards / year_total_awards) %>% 
  group_by(state) %>% 
  nest() %>%
  right_join(albersusa::usa_sf("laea"), by = c("state"="iso_3166_2"))


chorpleth <- gabf %>% 
  awards_by_state_year() %>% 
  unnest(data) %>% 
  ggplot(ggplot2::aes(geometry = geometry, fill = pct_of_year_total_awards, group = year)) +
  geom_sf() +
  scale_fill_viridis_c(option = "magma", alpha = .9, begin = .1, labels = scales::percent) + 
  labs(title = 'Percent of Total Awards by State, Year: {round(frame_time,0)}', fill = "Awards") +
  theme_blog(axis.text = ggplot2::element_blank(),
             axis.ticks = ggplot2::element_blank()) +
  transition_time(year) +
  ease_aes('linear')
animate(chorpleth, fps = 5)

gabf %>% 
  awards_by_state_year() %>% 
  left_join(y = gabf %>% awards_by_state()) %>%
  filter(state_years_with_awards > 1 & state_total_awards > 5) %>% 
  mutate(model = map(data, ~ glm(cbind(state_year_total_awards, year_total_awards - state_year_total_awards) ~ year, data = .x , family = "binomial"))) %>% View()


line_chart_data <- . %>% 
  awards_by_state_year() %>% 
  left_join(y = gabf %>% awards_by_state()) %>% 
  select(-geometry) %>% 
  filter(state_years_with_awards > 1 & state_total_awards > 5) %>% 
  unnest(data)

tie_fighter_data <- . %>% 
  awards_by_state_year() %>% 
  left_join(y = gabf %>% awards_by_state()) %>%
  select(-geometry) %>% 
  filter(state_years_with_awards > 1 & state_total_awards > 5) %>% 
  mutate(model = map(data, ~ glm(cbind(state_year_total_awards, year_total_awards - state_year_total_awards) ~ year, data = .x , family = "binomial"))) %>% 
  mutate(results = map(model, broom::tidy, conf.int = TRUE)) %>%
  unnest(results) %>%
  ungroup() %>% 
  filter(term == "year") %>% 
  mutate(state = fct_reorder(state, estimate)) %>% 
  mutate(p_value = case_when(
    p.value >= .075 ~ "not confident",
    p.value >= .025 ~ "somewhat confident",
    p.value < .025 ~ "confident"),
    trend = case_when(
      estimate >= .25 ~ "trending up",
      estimate >= -.25 ~ "flat",
      estimate < -.25 ~ "trending down")) %>%
  mutate(p_value = fct_relevel(p_value, c("not confident", "somewhat confident"))) %>% 
  SharedData$new(key = ~state_name, group = "Choose states (hold shift to select multiple):")

greys <- brewer.pal(n = 9, "Greys")
greys <- c(grey[3], grey[6], grey[9])

tie_fighter_plot <- gabf %>% 
  tie_fighter_data() %>% 
  ggplot(aes(x = estimate, y = state, key = state_name, group = state_total_awards, color = p_value,
             text = paste0("<b>", state_name, "</b>\n", if_else(p_value == 'confident', glue('P-Value: {format(round(p.value, digits = 4), nsmall = 4)}\nThe number of {state_name} awards is definitely {trend}.'), 
                                                                if_else(p_value == 'somewhat confident', glue::glue("'P-Value: {format(round(p.value, digits = 4), nsmall = 4)}\nThe number of {state_name} awards is {trend}, but there is some uncertainty."), glue::glue("'P-Value: {format(round(p.value, digits = 4), nsmall = 4)}\nThe number of {state_name} awards appears to be {trend}, but there is signficant uncertainty."))))), 
         guides = guides(color = NULL)) +
  geom_point(size = 1.75) +
  geom_vline(xintercept = 0, lty = 2, color = "#f0f0f0") +
  geom_errorbarh(aes(
    xmin = conf.low,
    xmax = conf.high),
    height = .5,
    size = 1,
    show.legend = FALSE) +
  scale_colour_manual(values = greys) +
  scale_x_continuous(limits = c(-0.15, 0.15), 
                     breaks = c(-.1, 0, .1),
                     labels = c("Descreasing", "Stagnant", "Increasing")) +
  labs(
    x = "Trend",
    title = NULL,
    y = NULL,
    color = NULL) +
  theme_blog()
tie_fighter_plot <- plotly::ggplotly(tie_fighter_plot, tooltip = "text")

line_chart <- gabf %>% 
  line_chart_data() %>% 
  ggplot(aes(x = year, y = state_year_total_awards, key = state_name, group = state_total_awards,
             text = glue::glue("<b>{state_name} Awards</b>\n{state_year_total_awards} ({year})\n{state_total_awards} (1987-2020)\n"))) +
  geom_line() +
  labs(
    x = "Year",
    title = NULL,
    y = "") +
  theme_blog(panel.grid.major.y = element_blank())
line_chart <- ggplotly(line_chart, tooltip = "text")  

linked_plots <- subplot(tie_fighter_plot,
                        line_chart,
                        widths = c(.35, .65)) %>%
  layout(showlegend = FALSE) %>% 
  highlight(on = "plotly_click", selectize = TRUE, dynamic = T)

div(class = "reactable-tbl-view",
    div(class = "reactable-subtitle",
        div(class = "reactable-title", "State Trends"),
        "Alaska & Wisconsin are losing their share of awards to North Carolina, Indiana, & Virginia with South Carolina Jumping in the mix. California, Oregon, & Colorado appear to be maintaining their hold on the lion's share of the awards."
    ),
    linked_plots
)



gabf %>% 
  #add_count(brewery, name = "brewery_total_awards") %>%
  #add_count(category, name = "category_total_awards") %>%
  count(brewery, beer_name, category, medal) %>% 
  mutate(beer_name = fct_reorder(beer_name, n, sum)) %>% 
  pivot_wider(names_from = medal, values_from = n, values_fill = 0) %>%
  clean_names("title") %>% 
  reactable(groupBy = "Beer Name",
            defaultPageSize = 5,
            filterable = TRUE, 
            searchable = TRUE,
            defaultSorted = c("Gold"),
            defaultColDef = colDef(aggregate = "sum"),
            columns =  list(
              Brewery = colDef(
                name = 'Breweries',
                aggregate = JS("
                  function(values, rows) {
                    var keep = 5
                    var l = values.length;
                    var getTrunc = function(length) {
                      if (length > keep) {
                        return '...';
                      } else {
                        return ' ';
                      }
                    };
                    var trunc = getTrunc(l);
                    var l = Math.min(l, keep);
                    var beers = values.slice(0, l);
                    var beers_trunc = beers.join(',').concat(trunc);
                    return beers_trunc
                  }
                ")  
              ),
              Gold = colDef( 
                name = "Total Golds"
              ),
              Silver = colDef( 
                name = "Total Silvers"
              ),
              Bronze = colDef( 
                name = "Total Bronzes"
              )))

gabf %>% 
  #add_count(brewery, name = "brewery_total_awards") %>%
  #add_count(category, name = "category_total_awards") %>%
  count(brewery, beer_name, category, medal) %>% 
  mutate(beer_name = fct_reorder(beer_name, n, sum)) %>% 
  pivot_wider(names_from = medal, values_from = n, values_fill = 0) %>%
  mutate(total_medals = Gold+Silver+Bronze) %>% 
  clean_names("title") %>% 
  reactable(groupBy = c("Beer Name", "Brewery","Category"),
            defaultPageSize = 5,
            filterable = TRUE, 
            searchable = TRUE,
            defaultSorted = "Total Medals",
            defaultSortOrder = "desc",
            defaultColDef = colDef(aggregate = "sum"),
            columns =  list(
              Brewery = colDef(
                name = 'Breweries',
                aggregate = JS("
                  function(values, rows) {
                    var keep = 3
                    var l = values.length;
                    var getTrunc = function(length) {
                      if (length > keep) {
                        return '...';
                      } else {
                        return ' ';
                      }
                    };
                    var trunc = getTrunc(l);
                    var l = Math.min(l, keep);
                    var beers = values.slice(0, l);
                    var beers_trunc = beers.join(',').concat(trunc);
                    return beers_trunc
                  }
                ")  
              ),
              Gold = colDef( 
                name = "Total Golds"
              ),
              Silver = colDef( 
                name = "Total Silvers"
              ),
              Bronze = colDef( 
                name = "Total Bronzes"
              )))


gabf %>% 
  add_count(brewery, name = "brewery_total_awards") %>%
  add_count(category, name = "category_total_awards") %>%
  count(brewery, beer_name, category, medal) %>% 
  mutate(beer_name = fct_reorder(beer_name, n, sum)) %>% 
  pivot_wider(names_from = medal, values_from = n, values_fill = 0) %>% 
  mutate(total_medals = Gold+Silver+Bronze) %>% 
  clean_names("title") %>% 
  reactable(groupBy = "Beer Name",
            defaultPageSize = 5,
            filterable = TRUE, 
            searchable = TRUE,
            defaultSorted = c("Total Medals"),
            defaultSortOrder = "desc",
            showPageSizeOptions = TRUE,
            defaultColDef = colDef(aggregate = "sum"),
            columns =  list(
              Brewery = colDef(
                name = "Breweries",
                aggregate = JS("
                function(values, rows) 
                {
                  let breweries = values.filter((e, i) => values.indexOf(e) === i);
                  if(breweries.length > 1){
                    var left = '(';
                    var right = ')';
                    return left.concat(breweries.length).concat(right);
                  } else {
                    return breweries;
                  }
                }")
              ),
              Category = colDef(
                name = "Categories",
                aggregate = JS("
                function(values, rows) 
                {
                  let categories = values.filter((e, i) => values.indexOf(e) === i);
                  if(categories.length > 1){
                    var left = '(';
                    var right = ')';
                    return left.concat(categories.length).concat(right);
                  } else {
                    return categories;
                  }
                }")
              ),
              Gold = colDef( 
                name = "Total Golds"
              ),
              Silver = colDef( 
                name = "Total Silvers"
              ),
              Bronze = colDef( 
                name = "Total Bronzes"
              )))


gabf %>% 
  add_count(brewery, name = "brewery_total") %>%
  add_count(category, name = "category_total") %>%
  count(beer_name, brewery, brewery_total, category, category_total, medal) %>% 
  mutate(beer_name = fct_reorder(beer_name, n, sum)) %>% 
  pivot_wider(names_from = medal, values_from = n, values_fill = 0) %>% 
  mutate(total_medals = Gold+Silver+Bronze) %>% 
  clean_names("title") %>% 
  reactable(groupBy = "Beer Name",
            defaultPageSize = 5,
            filterable = TRUE, 
            searchable = TRUE,
            defaultSorted = c("Total Medals"),
            defaultSortOrder = "desc",
            showPageSizeOptions = TRUE,
            defaultColDef = colDef(aggregate = "sum"),
            columns =  list(
              Brewery = colDef(
                name = "Breweries",
                aggregate = JS("
                function(values, rows) 
                {
                  let breweries = values.filter((e, i) => values.indexOf(e) === i);
                  if(breweries.length > 1){
                    var left = '(';
                    var right = ')';
                    return left.concat(breweries.length).concat(right);
                  } else {
                    return breweries;
                  }
                }")
              ),
              `Brewery Total` = colDef(
                aggregate = "max"
              ),
              Category = colDef(
                name = "Categories",
                aggregate = JS("
                function(values, rows) 
                {
                  let categories = values.filter((e, i) => values.indexOf(e) === i);
                  if(categories.length > 1){
                    var left = '(';
                    var right = ')';
                    return left.concat(categories.length).concat(right);
                  } else {
                    return categories;
                  }
                }")
              ),
              `Category Total` = colDef(
                aggregate = "max"
              ),
              Gold = colDef( 
                name = "Total Golds"
              ),
              Silver = colDef( 
                name = "Total Silvers"
              ),
              Bronze = colDef( 
                name = "Total Bronzes"
              )))


