library(shiny)

Northeast <- list('CT'
              , 'MA'
              , 'ME'
              , 'NH'
              , 'NJ'
              , 'NY'
              , 'PA'
              , 'RI')
South <- list('AL'
          , 'AR'
          , 'DC'
          , 'DE'
          , 'FL'
          , 'GA'
          , 'KY'
          , 'LA'
          , 'MD'
          , 'MS'
          , 'NC'
          , 'OK'
          , 'SC'
          , 'TN'
          , 'TX'
          , 'VA'
          , 'WV')
Midwest <- list('IA'
            , 'IL'
            , 'IN'
            , 'KS'
            , 'MI'
            , 'MN'
            , 'MO'
            , 'NE'
            , 'ND'
            , 'OH'
            , 'SD')
West <- list('AZ'
         ,'CA'
         ,'CO'
         ,'ID'
         ,'MT'
         ,'NM'
         ,'NV'
         ,'OR'
         ,'WA'
         ,'WY')
Pacific <- list('HI')
states_regions <- list('Northeast'= Northeast
                       , 'South' = South
                       , 'West' = West
                       , 'Midwest' = Midwest
                       , 'Pacific' = Pacific)
# # get state abbreviations
# states_regions <- state.abb
# # assign regions to the states
# names(states_regions) <- state.region
# # split names and regions to list
# states_regions <- split(states_regions, names(states_regions))
# # unname all entries of list
# states_regions <- lapply(states_regions, unname)
# states_regions <- lapply(states_regions, list)
ui <- fluidPage(
  selectInput('state'
              , 'State'
              , choices = states_regions)
)

