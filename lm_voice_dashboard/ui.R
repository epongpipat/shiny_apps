# packages ================================================================================
library(shiny)
library(shinythemes)
library(shinycssloaders)
library(shinydashboard)
library(tidyverse)
library(car)
library(broom)

# loading spinner options =================================================================
options(spinner.color = "#158CBA", spinner.type = 6)

# shiny dashboard =========================================================================
dashboardPage(
  skin = "blue",
  
  # heading ===============================================================================
  dashboardHeader(
    title = "Simple Linear Regression via Voice Command",
    titleWidth = 425
  ),
  
  # sidebar ===============================================================================
  dashboardSidebar(
    width = 425,
    sidebarMenu(
      menuItem("App", icon = icon("rocket"), tabName = "shiny_app"),
      menuItem("Description", icon = icon("align-left"), tabName = "description"),
      menuItem("Not Working?", icon = icon("question-circle"), tabName = "not_working"),
      menuItem("Code", icon = icon("code"), tabName = "code"),
      menuItem("References", icon = icon("thumbs-up"), tabName = "references"),
      menuItem("Ekarin Eric Pongpipat, M.A.",
        icon = icon("id-badge"), startExpanded = T, selected = F,
        menuSubItem("Email", icon = icon("envelope"), href = "mailto:epongpipat@gmail.com"),
        menuSubItem("Website", icon = icon("paperclip"), href = "https://ekarinpongpipat.com"),
        menuSubItem("GitHub", icon = icon("github"), href = "https://github.com/epongpipat")
      ),
      menuItem("2018, MIT License", icon = icon("copyright"), href = "https://github.com/epongpipat/shiny_apps/blob/master/LICENSE")
    )
  ),
  
  # body ==================================================================================
  dashboardBody(
    
    # shiny_app ===========================================================================
    tabItems(
      tabItem(
        tabName = "shiny_app",
        singleton(tags$head(
          tags$script(src = "//cdnjs.cloudflare.com/ajax/libs/annyang/1.4.0/annyang.min.js"),
          includeScript("init.js")
        )),
        fluidRow(
          box(
            title = "List of Variables", solidHeader = T, width = 12, status = "primary",
            p(code("rank"), code("discipline"), code("years since phd"), code("years of service"), code("sex"), code("salary"))
          )
        ),
        fluidRow(
          valueBoxOutput("DV", width = 4) %>% withSpinner(),
          valueBoxOutput("IV", width = 4) %>% withSpinner(),
          valueBoxOutput("IV_coding_scheme", width = 4) %>% withSpinner()
        ),
        fluidRow(
          column(
            width = 7,
            box(
              title = "ANOVA Source Table", width = NULL, status = "primary", solidHeader = T,
              tableOutput("anova_source_table") %>% withSpinner()
            ),
            box(
              title = "Coefficients Table", width = NULL, status = "primary", solidHeader = T,
              tableOutput("summary_table") %>% withSpinner()
            )
          ),
          column(
            width = 5,
            box(
              title = "Figure", width = NULL, status = "primary", solidHeader = T,
              plotOutput("visualization") %>% withSpinner()
            )
          )
        )
      ),
      
      # description =======================================================================
      tabItem(
        tabName = "description",
        fluidRow(
          box(
            title = "Description", width = 12, status = "primary", solidHeader = T,
            p("This shiny app allows users to run a simple linear regression using simple voice commands. Specifically, the shiny app prints out an ANOVA source table, a coefficients (summary) table, and its respective figure."),
            p("To change the dependent variable (DV), just say DV [insert variable here] or dependent variable [insert variable here]. For example, 'DV salary' or 'dependent variable salary'. This app currently only supports DVs that are continuous variables rather than categorical variables."),
            p("To change the independent variable (IV), just say IV [insert variable here] or independent variable [insert variable here]. For example, 'IV sex' or 'independent variable sex.' If the IV is a categorical variable, you can change the contrast coding scheme to either dummy, deviant (effects), or helmert. To change the contrast coding scheme, just say contrast [insert contrast here]. For example, 'contrast dummy' or 'contrast deviant'."),
            p("The list of variables are listed below and within the ", code("App"), " page under the heading ", code("List of Variables"), ".")
          )
        ),
        fluidRow(
          box(
            title = "Dataset", width = 12, status = "primary", solidHeader = T,
            p("The shiny app uses the open-source dataset ", code("Salaries"), " within the ", code("carData"), " package."),
            p("The dataset consists of nine-month salaries collected from 397 collegiate professors in the U.S. during 2008 to 2009. In addition to salaries, the professor’s rank, sex, discipline, years since Ph.D., and years of service was also collected. Thus, there is a total of 6 variables, which are described below.")
          )
        ),
        fluidRow(
          box(
            title = "Dataset Variables", width = 12, status = "primary", solidHeader = T,
            tableOutput("dataset_variables_table")
          )
        )
      ),
      
      # not_working =======================================================================
      tabItem(
        tabName = "not_working",
        fluidRow(
          box(
            title = "Not Working?", width = 12, status = "primary", solidHeader = T,
            p("The app currently only works the the desktop version of Google Chrome. If this is not a solution, please email me.")
          )
        )
      ),
      
      # code ==============================================================================
      tabItem(
        tabName = "code",
        fluidRow(
          tabBox(
            title = "Code", id = "code", width = 12,
            tabPanel("ui.R",verbatimTextOutput("ui_file")),
            tabPanel("server.R",verbatimTextOutput("server_file")),
            tabPanel("init.js",verbatimTextOutput("javascript_file"))
          )
        )
      ),
      
      # references ========================================================================
      tabItem(
        tabName = "references",
        fluidRow(
          box(
            title = "References", width = 12, status = "primary", solidHeader = T,
            p("1. ", strong("Shiny Voice App Template"), " - https://github.com/yihui/shiny-apps/tree/master/voice"),
            p("2. ", strong("Salaries Dataset"), " - Fox J. and Weisberg, S. (2011) An R Companion to Applied Regression, Second Edition Sage.")
          )
        )
      )
    )
  )
)
