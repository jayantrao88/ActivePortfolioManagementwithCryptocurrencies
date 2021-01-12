library(shiny)
library(shinydashboard)
library(ggplot2)
library(ggthemes)
library(plotly)
library(DT)
library(qwraps2)
library(easyGgplot2)
library(dplyr)
library(shinyWidgets)
library(reshape2)
library(tidyr)

####################################                     Function               ####################################

Inv <- function(dataframe){
  if (length(dataframe > 2) & "Date" %in% colnames(dataframe)   | "Dates" %in% colnames(dataframe)){
    
    df <-  dataframe %>% select(-c(contains("Date"))) 
    Dat <- dataframe %>% select(contains("Date")) %>% as.data.frame()
    a <- dim(df)
    df1 <- as.data.frame(matrix(nrow = a[1], ncol = a[2], dimnames = list(NULL,colnames(df))))  
    for (i in 1:a[2]){
      df1[,i] <- df[,i]/df[1,i]  
    }
    output <- cbind(Dat,df1)
  }
}

####################################                     Function               ####################################

CopulaType <- function(id){checkboxGroupInput(id,'Select Copula Type ', choices =
                     unique(PerformanceM[["Alltogether"]]$VineType), inline = TRUE)}

OptimType <- function(id){checkboxGroupInput(id,'Select Optimization Type ', choices =
                     unique(PerformanceM[["Alltogether"]]$Opt)[-c(1,6)], inline = TRUE)}

####################################                     Function               ####################################

ui <- dashboardPage(
  dashboardHeader(title = "Portfolio Management - 5 Portfolios"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Introduction and Index",tabName = "Intro"),
      menuItem("Market Indices", tabName = "Market",
               menuSubItem("Performance Metrics", tabName = "Market-MeanSDCAGR"),
               menuSubItem("Evolution", tabName = "Market-Evolution"),
               menuSubItem("Crypto Weights", tabName = "Market-Weights")),
      menuItem("Fixed Income", tabName = "Fixed",
               menuSubItem("Performance Metrics", tabName = "Fixed-MeanSDCAGR"),
               menuSubItem("Evolution", tabName = "Fixed-Evolution"),
               menuSubItem("Crypto Weights", tabName = "Fixed-Weights")),
      menuItem("Commodities", tabName = "Commo",
               menuSubItem("Performance Metrics", tabName = "Commo-MeanSDCAGR"),
               menuSubItem("Evolution", tabName = "Commo-Evolution"),
               menuSubItem("Crypto Weights", tabName = "Commo-Weights")),
      menuItem("Sector Indices", tabName = "Sector",
               menuSubItem("Performance Metrics", tabName = "Sector-MeanSDCAGR"),
               menuSubItem("Evolution", tabName = "Sector-Evolution"),
               menuSubItem("Crypto Weights", tabName = "Sector-Weights")),
      menuItem("Market, Fixed and Commodities", tabName = "MarFixCom",
               menuSubItem("Performance Metrics", tabName = "MarFixCom-MeanSDCAGR"),
               menuSubItem("Evolution", tabName = "MarFixCom-Evolution"),
               menuSubItem("Crypto Weights", tabName = "MarFixCom-Weights")),
      menuItem("Extras", tabName = "Extras",
               menuSubItem("Performance Metrics", tabName = "PerfM"),
               menuSubItem("Evolution", tabName = "StaticEvol"),
               menuSubItem("BoxPlots", tabName = "BoxPlots")
    )
  )),
  dashboardBody(
    tags$head(                                   #For vertical ScrollBar
      tags$style(HTML(".content {
                      height: 120vh; overflow-y: auto;
                    }"
      ) # close HTML
      )            # close tags$style
    ),             # close tags#Head
    tags$head(                                    #For horizontal ScrollBar
      tags$style(HTML(".content {
                      width: 120vh; overflow-x: auto;
                    }"
      ) # close HTML
      )            # close tags$style
    ),
    tags$head(tags$style(                         #For Complete coloring of the page when you scroll down
      HTML('.content-wrapper {height: auto !important; position:relative; overflow-x:hidden; overflow-y:hidden}')
    ),tags$script('$(document).on("shiny:connected", function(e) {
                            Shiny.onInputChange("innerWidth", window.innerWidth);
                            });
                            $(window).resize(function(e) {
                            Shiny.onInputChange("innerWidth", window.innerWidth);
                            });
                            ')),
    tabItems(
      
      tabItem("Intro",
              fluidPage(tags$div(HTML("<p>&nbsp;</p>
<h2><strong>Should one invest in cryptocurrencies? A Portfolio Optimization Perspective</strong></h2>
<p>&nbsp;</p>
<p>&nbsp;&nbsp;&nbsp;&nbsp; Since May 2017 the cryptocurrency markets started a rally that peaked to $700 Billion in January 2018. This magnified investor interest in cryptocurrencies even more. My roommate who was working towards becoming a scriptwriter also invested $100 in cryptocurrencies that he had never heard before. Everything said and done it has captivated minds, for better or for worse.&nbsp; Blockchain technology, the technology behind cryptocurrencies, has proved to be useful in varied fields. Apart from technologists, banks, and Policymakers have started showing keen interest in the world of cryptocurrencies. For instance, Facebook announced their own cryptocurrency project Libra, there are policy papers on the central banking cryptocurrencies, Investment Banks have setup departments focusing on cryptocurrency product development. This, I believe, has created a bullish attitude towards cryptocurrency markets in general. As a curious investor, a finance enthusiast, and working towards my dissertation I was interested in analyzing its performance in portfolio investing.</p>
<p>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; To see the efficacy of adding cryptocurrencies to a given portfolio with <em>n</em> different assets, I created five different types of portfolios as a benchmark called <em>Base</em> portfolio. Then to such portfolios I added four major cryptocurrencies Bitcoin, Litecoin, XRP, and Ether, called <em>Alt </em>portfolio. I chose those cryptocurrencies because they capture x% of the cryptocurrency market. The dataset from 28<sup>th</sup> April 2013 and ends on 30<sup>th</sup> March 2020. In the real world, portfolio managers actively churn their portfolios at a given interval, called rebalancing period. &nbsp;We have 41 rebalancing periods starting from 2014 to Feb 2020. For every rebalancing period, lagging 500 observations of the asset returns were considered.</p>
<p>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; I chose to do the analysis using five different types of dependence modelling: Conventional (Markowitz) and four copula modelling: Parametric distribution and R &ndash; Vine (P-R), Parametric distribution and C &ndash; Vine (P-C), Empirical distribution and R &ndash; Vine (E-R), and Empirical distribution and C &ndash; Vine (E-C). The parametric and empirical distributions are different distribution estimation techniques used as an input in modelling copula. We get them by modelling the marginal distribution of each asset individually (I used ARMA-GARCH models) and extracting the standardized residuals.</p>
<p>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Once the joint dependence is modelled and the residuals are simulated from these models, I put them in five different optimization techniques to determine the optimal weight for each asset, each period. These five optimization techniques are Naive &nbsp;portfolio, Minimum Standard Deviation Portfolio, Minimum Expected Shortfall Portfolio, Sharpe Portfolio, and Maximum Mean Portfolio.</p>
<p>&nbsp;&nbsp;&nbsp;</p>
<p>&nbsp;</p>
<p>The idea in this project is to see the following: -</p>
<p>&nbsp;</p>
<p>1) Does adding cryptocurrencies to different sets of portfolios gives a better risk return performance?</p>
<p>2) Which optimization technique has performed well over the years?</p>
<p>3) Which dependence technique has performed well over the years?</p>
<p>4) Does investing in these portfolios at different points in time and for different time horizons would have changed the outcome?</p>
<p>5) How much more return do we get adding cryptocurrency to the portfolio? Does that come with an increase in risk? By how much?</p>
<p>6) Given that cryptocurrencies are very volatile assets what is its exposure in a given portfolio?</p>
<p>&nbsp;</p>
<p>&nbsp;</p>
<p><strong>What do you see in the analysis?</strong></p>
<p>&nbsp;</p>
<p>For each Portfolio, there is a tab and when you click on that tab you will see</p>
<ul>
<li><strong>Performance Metrics</strong>: This tab shows the comparative analysis of portfolio performance measured in terms Annualized Mean, Standard Deviation, Compounded Annual growth rate, and Mean per standard deviation. You have three tabs <em>All, Optimizationwise, </em>and<em> Copulawise</em>. For <em>All</em> you can choose to display the performance results of Dependence and Optimization <br /> combination of portfolios. For <em>Optimizationwise</em>, you can choose to display the performance results of all portfolios under an optimization technique. For <em>Copulawise, </em>you can choose to display the performance results of all portfolios under a dependence.</li>
<li><strong>Evolution:</strong> This tab shows a comparative evolution of Base and Alt portfolio for a portfolio, dependence, optimization, combination. You have two tabs: <em>Optimizationwise, </em>and
<ol>
<li>And each tab has subtabs for each of the optimization and dependence technique respectively.<br /> You can adjust the rebalancing period to zoom in and out to look at the minute details. You can change the investment start period to look at different starting points of investment and its outlook.</li>
</ol>
</li>
<li><strong>Crypto Weights:</strong> This tab shows the weights of four cryptocurrencies over the investment period and is sliced into tabs: <em>Optimizationwise, </em>and And each tab has subtabs for each of the optimization and dependence technique respectively.</li>
</ul>
<p>&nbsp;</p>
<p>&nbsp;</p>
<p><strong>Five Portfolios:</strong></p>
<p><em>Market Indices :</em> SP500, DJIA, and NASDAQ</p>
<p><em>Fixed Income Assets </em>: Barclays 10 year term Index total return, Junk total returns, PIMCO Inv grade corporate bond ETF</p>
<p><em>Commodities </em>: Gold, Silver, Oil, Platinum</p>
<p><em>Sector Indices</em> : Information Technology, Energy sector, Health Care, Utilities, Financialals, Consumer Discretionary, Consumer Staples, Communication services, Industrials, Real Estate, Materials</p>
<p><em>Market, Fixed, and Commodities </em>: Market Indices, Fixed Income Assets, and Commodities.</p>
<p>&nbsp;</p>
<p><strong>Abbreviations:</strong></p>
<p>SP500 : Standard's and Poor's 500</p>
<p>BTC: Bitcoin</p>
<p>LTC: Litecoin</p>
<p>XRP: XRP (Cryptocurrency of Ripple)</p>
<p>ETH: Ether (Cryptocurrency of Etherium)</p>
<p>&nbsp;</p>
<p><strong>Dependence Techniques:</strong></p>
<p>Conv&nbsp; &nbsp;&nbsp;: Conventional Dependence</p>
<p>Copula : Copula Dependence</p>
<p>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; P-R&nbsp;&nbsp;&nbsp; : R Vine Copula Dependence using Parametric Standardized Residuals</p>
<p>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; P-C&nbsp;&nbsp;&nbsp; : C Vine Copula Dependence using Parametric Standardized Residuals</p>
<p>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; E-R&nbsp;&nbsp;&nbsp; : R Vine Copula Dependence using Empirical Standardized Residuals</p>
<p>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; E-C&nbsp;&nbsp;&nbsp; : C Vine Copula Dependence using Empirical Standardized Residuals</p>
<p>&nbsp;</p>
<p><strong>Optimization Techniques:</strong></p>
<p>Naive&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;: 1/n assets optimization technique</p>
<p>MinSD&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;: Minimum Standard Deviation optimization technique</p>
<p>MinES&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;: Minimum Expected Shortfall optimization technique</p>
<p>Sharpe&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; : Sharpe optimization technique</p>
<p>MaxMean: Maximum Mean optimization technique</p>
<p>&nbsp;</p>
<p><strong>Different Portfolios:</strong></p>
<p>Market&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;: Market Indices portfolio</p>
<p>Fixed&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;: Fixed Income Assets portfolio consisting of</p>
<p>Commodities&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;: Commodities portfolio consisting of Gold, Silver, Oil, Platinum</p>
<p>Sector&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;: Sector Indices portfolio</p>
<p>Market, Fixed, and Commodities &nbsp;: Market Indices, Fixed Income assets, and commodities portfolio</p>
<p>&nbsp;</p>
<p>Base : Base Portfolio</p>
<p>Alt &nbsp;&nbsp;&nbsp;&nbsp;: Alternate portfolio = Base + cryptocurrencies.</p>
<p>&nbsp;</p>
<p><strong>Caveat:</strong><br /> Although the liquidity in the cryptocurrency markets have gradually increased over time but I do not factor in liquidity and transaction costs in my analysis. So my analysis is sans liquidity and transaction costs.</p>
<p>&nbsp;</p>"))
                          )
                          ),
      tabItem("Market-MeanSDCAGR",
              fluidPage(h4("Shows the Annulaized Mean, 
                                      Standard Deviation(SD), Mean per SD, and Compounded Annual
                                      Growth Rate (CAGR)")),
              fluidRow(
                box(CopulaType("MC1")),
                box(OptimType("MO1"))),
              tabsetPanel(
                tabPanel("All", dataTableOutput("marketSelection")),
                tabPanel("OptimizationWise", dataTableOutput("marketSelection2")),
                tabPanel("CopulaWise",dataTableOutput("marketSelection3"))
              )
        
      ),
      tabItem("Market-Evolution",

              box(sliderTextInput("MDates","Rebalacing Period:" ,
                                  choices = EvolutionAndWeights[["Market"]]$Date,
                                  selected =EvolutionAndWeights[["Market"]]$Date, #all values by default
                                  animate = TRUE, grid = FALSE,
                                  hide_min_max = FALSE, from_fixed = FALSE,
                                  to_fixed = FALSE, from_min = NULL, from_max = NULL, to_min = NULL,
                                  to_max = NULL, force_edges = FALSE, width = NULL, pre = NULL,
                                  post = NULL, dragRange = FALSE)),
              box(sliderTextInput("MIDates","Investment Start Period:" ,
                                  choices = EvolutionAndWeights[["Market"]]$Date,
                                  selected =EvolutionAndWeights[["Market"]]$Date, #all values by default
                                  animate = TRUE, grid = FALSE,
                                  hide_min_max = FALSE, from_fixed = FALSE,
                                  to_fixed = FALSE, from_min = NULL, from_max = NULL, to_min = NULL,
                                  to_max = NULL, force_edges = FALSE, width = NULL, pre = NULL,
                                  post = NULL, dragRange = FALSE)),
              tabsetPanel(
                tabPanel("OptimizationWise",
                         tabsetPanel(
                           tabPanel("Naive",plotlyOutput("marketnaiveplot")),
                           tabPanel("MinVar",plotlyOutput("marketminsdplot")),
                           tabPanel("MinES",plotlyOutput("marketminesplot")),
                           tabPanel("Sharpe",plotlyOutput("marketsharpeplot")),
                           tabPanel("MaxMean",plotlyOutput("marketmaxmeanplot"))
                         )
                ),
                tabPanel("CopulaWise",
                         tabsetPanel(
                           tabPanel("Conventional",plotlyOutput("marketCONV")),
                           tabPanel("Parametric & R-Vine",plotlyOutput("marketPR")),
                           tabPanel("Parametric & C-Vine",plotlyOutput("marketPC")),
                           tabPanel("Empirical & R-Vine",plotlyOutput("marketER")),
                           tabPanel("Empirical & C-Vine",plotlyOutput("marketEC"))
                         )
                )
              )
      ),
      tabItem("Market-Weights",
              tabsetPanel(
                tabPanel("OptimizationWise",
                         tabsetPanel(
                           tabPanel("Naive",
                                    plotlyOutput("marketnaiveweight")),
                           tabPanel("MinVar",plotlyOutput("marketminsdweight")),
                           tabPanel("MinES",plotlyOutput("marketminesweight")),
                           tabPanel("Sharpe",plotlyOutput("marketsharpeweight")),
                           tabPanel("MaxMean",plotlyOutput("marketmaxmeanweight"))
                         )
                ),
                tabPanel("CopulaWise",
                         tabsetPanel(
                           tabPanel("Conventional",plotlyOutput("marketCONVweight")),
                           tabPanel("Parametric & R-Vine",plotlyOutput("marketPRweight")),
                           tabPanel("Parametric & C-Vine",plotlyOutput("marketPCweight")),
                           tabPanel("Empirical & R-Vine",plotlyOutput("marketERweight")),
                           tabPanel("Empirical & C-Vine",plotlyOutput("marketECweight"))
                         )

                )
              )


      ),
      tabItem("Fixed-MeanSDCAGR",
              fluidPage(h4("Shows the Annulaized Mean, 
                                      Standard Deviation(SD), Mean per SD, and Compounded Annual
                                      Growth Rate (CAGR)")),
              fluidRow(
                box(CopulaType("FC1")),
                box(OptimType("FO1"))),
              tabsetPanel(
                tabPanel("All", dataTableOutput("FixedSelection")),
                tabPanel("OptimizationWise", dataTableOutput("FixedSelection2")),
                tabPanel("CopulaWise",dataTableOutput("FixedSelection3"))
              )
      )  
       ,
      tabItem("Fixed-Weights",
              tabsetPanel(
                tabPanel("OptimizationWise",
                         tabsetPanel(
                           tabPanel("Naive",
                                    plotlyOutput("Fixednaiveweight")),
                           tabPanel("MinVar",plotlyOutput("Fixedminsdweight")),
                           tabPanel("MinES",plotlyOutput("Fixedminesweight")),
                           tabPanel("Sharpe",plotlyOutput("Fixedsharpeweight")),
                           tabPanel("MaxMean",plotlyOutput("Fixedmaxmeanweight"))
                         )
                ),
                tabPanel("CopulaWise",
                         tabsetPanel(
                           tabPanel("Conventional",plotlyOutput("FixedCONVweight")),
                           tabPanel("Parametric & R-Vine",plotlyOutput("FixedPRweight")),
                           tabPanel("Parametric & C-Vine",plotlyOutput("FixedPCweight")),
                           tabPanel("Empirical & R-Vine",plotlyOutput("FixedERweight")),
                           tabPanel("Empirical & C-Vine",plotlyOutput("FixedECweight"))
                         )
                         
                )
              )
              
              
      ),
      tabItem("Fixed-Evolution",
              box(sliderTextInput("FDates","Rebalacing Period:" ,
                                  choices = EvolutionAndWeights[["Fixed"]]$Date,
                                  selected =EvolutionAndWeights[["Fixed"]]$Date, #all values by default
                                  animate = TRUE, grid = FALSE,
                                  hide_min_max = FALSE, from_fixed = FALSE,
                                  to_fixed = FALSE, from_min = NULL, from_max = NULL, to_min = NULL,
                                  to_max = NULL, force_edges = FALSE, width = NULL, pre = NULL,
                                  post = NULL, dragRange = FALSE)),
              box(sliderTextInput("FIDates","Investment Start Period:" ,
                                  choices = EvolutionAndWeights[["Fixed"]]$Date,
                                  selected =EvolutionAndWeights[["Fixed"]]$Date, #all values by default
                                  animate = TRUE, grid = FALSE,
                                  hide_min_max = FALSE, from_fixed = FALSE,
                                  to_fixed = FALSE, from_min = NULL, from_max = NULL, to_min = NULL,
                                  to_max = NULL, force_edges = FALSE, width = NULL, pre = NULL,
                                  post = NULL, dragRange = FALSE)),
              tabsetPanel(
                tabPanel("OptimizationWise",
                         tabsetPanel(
                           tabPanel("Naive",plotlyOutput("Fixednaiveplot")),
                           tabPanel("MinVar",plotlyOutput("Fixedminsdplot")),
                           tabPanel("MinES",plotlyOutput("Fixedminesplot")),
                           tabPanel("Sharpe",plotlyOutput("Fixedsharpeplot")),
                           tabPanel("MaxMean",plotlyOutput("Fixedmaxmeanplot"))
                         )
                ),
                tabPanel("CopulaWise",
                         tabsetPanel(
                           tabPanel("Conventional",plotlyOutput("FixedCONV")),
                           tabPanel("Parametric & R-Vine",plotlyOutput("FixedPR")),
                           tabPanel("Parametric & C-Vine",plotlyOutput("FixedPC")),
                           tabPanel("Empirical & R-Vine",plotlyOutput("FixedER")),
                           tabPanel("Empirical & C-Vine",plotlyOutput("FixedEC"))
                         )
                )
              )
      ),
      tabItem("Commo-MeanSDCAGR",
              fluidPage(h4("Shows the Annulaized Mean, 
                                      Standard Deviation(SD), Mean per SD, and Compounded Annual
                                      Growth Rate (CAGR)")),
              fluidRow(
                box(CopulaType("CC1")),
                box(OptimType("CO1"))),
              tabsetPanel(
                tabPanel("All", dataTableOutput("CommoSelection")),
                tabPanel("OptimizationWise", dataTableOutput("CommoSelection2")),
                tabPanel("CopulaWise",dataTableOutput("CommoSelection3"))
              )
              
      ),
      tabItem("Commo-Evolution",
              box(sliderTextInput("CDates","Rebalacing Period:" ,
                                  choices = EvolutionAndWeights[["Commo"]]$Date,
                                  selected =EvolutionAndWeights[["Commo"]]$Date, #all values by default
                                  animate = TRUE, grid = FALSE,
                                  hide_min_max = FALSE, from_fixed = FALSE,
                                  to_fixed = FALSE, from_min = NULL, from_max = NULL, to_min = NULL,
                                  to_max = NULL, force_edges = FALSE, width = NULL, pre = NULL,
                                  post = NULL, dragRange = FALSE)),
              box(sliderTextInput("CIDates","Investment Start Period:" ,
                                  choices = EvolutionAndWeights[["Commo"]]$Date,
                                  selected =EvolutionAndWeights[["Commo"]]$Date, #all values by default
                                  animate = TRUE, grid = FALSE,
                                  hide_min_max = FALSE, from_fixed = FALSE,
                                  to_fixed = FALSE, from_min = NULL, from_max = NULL, to_min = NULL,
                                  to_max = NULL, force_edges = FALSE, width = NULL, pre = NULL,
                                  post = NULL, dragRange = FALSE)),
              tabsetPanel(
                tabPanel("OptimizationWise",
                         tabsetPanel(
                           tabPanel("Naive",plotlyOutput("Commonaiveplot")),
                           tabPanel("MinVar",plotlyOutput("Commominsdplot")),
                           tabPanel("MinES",plotlyOutput("Commominesplot")),
                           tabPanel("Sharpe",plotlyOutput("Commosharpeplot")),
                           tabPanel("MaxMean",plotlyOutput("Commomaxmeanplot"))
                         )
                ),
                tabPanel("CopulaWise",
                         tabsetPanel(
                           tabPanel("Conventional",plotlyOutput("CommoCONV")),
                           tabPanel("Parametric & R-Vine",plotlyOutput("CommoPR")),
                           tabPanel("Parametric & C-Vine",plotlyOutput("CommoPC")),
                           tabPanel("Empirical & R-Vine",plotlyOutput("CommoER")),
                           tabPanel("Empirical & C-Vine",plotlyOutput("CommoEC"))
                         )
                )
              )
      ),
      tabItem("Commo-Weights",
              tabsetPanel(
                tabPanel("OptimizationWise",
                         tabsetPanel(
                           tabPanel("Naive",
                                    plotlyOutput("Commonaiveweight")),
                           tabPanel("MinVar",plotlyOutput("Commominsdweight")),
                           tabPanel("MinES",plotlyOutput("Commominesweight")),
                           tabPanel("Sharpe",plotlyOutput("Commosharpeweight")),
                           tabPanel("MaxMean",plotlyOutput("Commomaxmeanweight"))
                         )
                ),
                tabPanel("CopulaWise",
                         tabsetPanel(
                           tabPanel("Conventional",plotlyOutput("CommoCONVweight")),
                           tabPanel("Parametric & R-Vine",plotlyOutput("CommoPRweight")),
                           tabPanel("Parametric & C-Vine",plotlyOutput("CommoPCweight")),
                           tabPanel("Empirical & R-Vine",plotlyOutput("CommoERweight")),
                           tabPanel("Empirical & C-Vine",plotlyOutput("CommoECweight"))
                         )
                         
                )
              )
              
              
      ),
      tabItem("PerfM",fluidPage(h4("Work in Progress"))

        
      ),
      tabItem("StaticEvol",  fluidPage(h4("Work in Progress"))
                
      ),
      tabItem("BoxPlots",fluidPage(h4("Work in Progress"))
                
      ),
      tabItem("Sector-MeanSDCAGR",
              fluidPage(h4("Shows the Annulaized Mean, 
                                      Standard Deviation(SD), Mean per SD, and Compounded Annual
                                      Growth Rate (CAGR)")),
              fluidRow(
                box(CopulaType("SC1")),
                box(OptimType("SO1"))),
              tabsetPanel(
                tabPanel("All", dataTableOutput("SectorSelection")),
                tabPanel("OptimizationWise", dataTableOutput("SectorSelection2")),
                tabPanel("CopulaWise",dataTableOutput("SectorSelection3"))
              )
              
      ),
      tabItem("Sector-Evolution",
              box(sliderTextInput("SDates","Rebalacing Period:" ,
                                  choices = EvolutionAndWeights[["Sector"]]$Date,
                                  selected =EvolutionAndWeights[["Sector"]]$Date, #all values by default
                                  animate = TRUE, grid = FALSE,
                                  hide_min_max = FALSE, from_fixed = FALSE,
                                  to_fixed = FALSE, from_min = NULL, from_max = NULL, to_min = NULL,
                                  to_max = NULL, force_edges = FALSE, width = NULL, pre = NULL,
                                  post = NULL, dragRange = FALSE)),
              box(sliderTextInput("SIDates","Investment Start Period:" ,
                                  choices = EvolutionAndWeights[["Sector"]]$Date,
                                  selected =EvolutionAndWeights[["Sector"]]$Date, #all values by default
                                  animate = TRUE, grid = FALSE,
                                  hide_min_max = FALSE, from_fixed = FALSE,
                                  to_fixed = FALSE, from_min = NULL, from_max = NULL, to_min = NULL,
                                  to_max = NULL, force_edges = FALSE, width = NULL, pre = NULL,
                                  post = NULL, dragRange = FALSE)),
              tabsetPanel(
                tabPanel("OptimizationWise",
                         tabsetPanel(
                           tabPanel("Naive",plotlyOutput("Sectornaiveplot")),
                           tabPanel("MinVar",plotlyOutput("Sectorminsdplot")),
                           tabPanel("MinES",plotlyOutput("Sectorminesplot")),
                           tabPanel("Sharpe",plotlyOutput("Sectorsharpeplot")),
                           tabPanel("MaxMean",plotlyOutput("Sectormaxmeanplot"))
                         )
                ),
                tabPanel("CopulaWise",
                         tabsetPanel(
                           tabPanel("Conventional",plotlyOutput("SectorCONV")),
                           tabPanel("Parametric & R-Vine",plotlyOutput("SectorPR")),
                           tabPanel("Parametric & C-Vine",plotlyOutput("SectorPC")),
                           tabPanel("Empirical & R-Vine",plotlyOutput("SectorER")),
                           tabPanel("Empirical & C-Vine",plotlyOutput("SectorEC"))
                         )
                )
              )
      ),
      tabItem("Sector-Weights",
              tabsetPanel(
                tabPanel("OptimizationWise",
                         tabsetPanel(
                           tabPanel("Naive",
                                    plotlyOutput("Sectornaiveweight")),
                           tabPanel("MinVar",plotlyOutput("Sectorminsdweight")),
                           tabPanel("MinES",plotlyOutput("Sectorminesweight")),
                           tabPanel("Sharpe",plotlyOutput("Sectorsharpeweight")),
                           tabPanel("MaxMean",plotlyOutput("Sectormaxmeanweight"))
                         )
                ),
                tabPanel("CopulaWise",
                         tabsetPanel(
                           tabPanel("Conventional",plotlyOutput("SectorCONVweight")),
                           tabPanel("Parametric & R-Vine",plotlyOutput("SectorPRweight")),
                           tabPanel("Parametric & C-Vine",plotlyOutput("SectorPCweight")),
                           tabPanel("Empirical & R-Vine",plotlyOutput("SectorERweight")),
                           tabPanel("Empirical & C-Vine",plotlyOutput("SectorECweight"))
                         )
                         
                )
              )
              
              
      ),
      tabItem("MarFixCom-MeanSDCAGR",
              fluidPage(h4("Shows the Annulaized Mean, 
                                      Standard Deviation(SD), Mean per SD, and Compounded Annual
                                      Growth Rate (CAGR)")),
              fluidRow(
                box(CopulaType("MFCC1")),
                box(OptimType("MFCO1"))),
              tabsetPanel(
                tabPanel("All", dataTableOutput("MarFixComSelection")),
                tabPanel("OptimizationWise", dataTableOutput("MarFixComSelection2")),
                tabPanel("CopulaWise",dataTableOutput("MarFixComSelection3"))
              )
              
      ),
      tabItem("MarFixCom-Evolution",
              box(sliderTextInput("MFCDates","Rebalacing Period:" ,
                                  choices = EvolutionAndWeights[["MarFixCom"]]$Date,
                                  selected =EvolutionAndWeights[["MarFixCom"]]$Date, #all values by default
                                  animate = TRUE, grid = FALSE,
                                  hide_min_max = FALSE, from_fixed = FALSE,
                                  to_fixed = FALSE, from_min = NULL, from_max = NULL, to_min = NULL,
                                  to_max = NULL, force_edges = FALSE, width = NULL, pre = NULL,
                                  post = NULL, dragRange = FALSE)),
              box(sliderTextInput("MFCIDates","Investment Start Period:" ,
                                  choices = EvolutionAndWeights[["MarFixCom"]]$Date,
                                  selected =EvolutionAndWeights[["MarFixCom"]]$Date, #all values by default
                                  animate = TRUE, grid = FALSE,
                                  hide_min_max = FALSE, from_fixed = FALSE,
                                  to_fixed = FALSE, from_min = NULL, from_max = NULL, to_min = NULL,
                                  to_max = NULL, force_edges = FALSE, width = NULL, pre = NULL,
                                  post = NULL, dragRange = FALSE)),
              tabsetPanel(
                tabPanel("OptimizationWise",
                         tabsetPanel(
                           tabPanel("Naive",plotlyOutput("MarFixComnaiveplot")),
                           tabPanel("MinVar",plotlyOutput("MarFixComminsdplot")),
                           tabPanel("MinES",plotlyOutput("MarFixComminesplot")),
                           tabPanel("Sharpe",plotlyOutput("MarFixComsharpeplot")),
                           tabPanel("MaxMean",plotlyOutput("MarFixCommaxmeanplot"))
                         )
                ),
                tabPanel("CopulaWise",
                         tabsetPanel(
                           tabPanel("Conventional",plotlyOutput("MarFixComCONV")),
                           tabPanel("Parametric & R-Vine",plotlyOutput("MarFixComPR")),
                           tabPanel("Parametric & C-Vine",plotlyOutput("MarFixComPC")),
                           tabPanel("Empirical & R-Vine",plotlyOutput("MarFixComER")),
                           tabPanel("Empirical & C-Vine",plotlyOutput("MarFixComEC"))
                         )
                )
              )
      ),
      tabItem("MarFixCom-Weights",
              tabsetPanel(
                tabPanel("OptimizationWise",
                         tabsetPanel(
                           tabPanel("Naive",
                                    plotlyOutput("MarFixComnaiveweight")),
                           tabPanel("MinVar",plotlyOutput("MarFixComminsdweight")),
                           tabPanel("MinES",plotlyOutput("MarFixComminesweight")),
                           tabPanel("Sharpe",plotlyOutput("MarFixComsharpeweight")),
                           tabPanel("MaxMean",plotlyOutput("MarFixCommaxmeanweight"))
                         )
                ),
                tabPanel("CopulaWise",
                         tabsetPanel(
                           tabPanel("Conventional",plotlyOutput("MarFixComCONVweight")),
                           tabPanel("Parametric & R-Vine",plotlyOutput("MarFixComPRweight")),
                           tabPanel("Parametric & C-Vine",plotlyOutput("MarFixComPCweight")),
                           tabPanel("Empirical & R-Vine",plotlyOutput("MarFixComERweight")),
                           tabPanel("Empirical & C-Vine",plotlyOutput("MarFixComECweight"))
                         )
                         
                )
              )
              
              
      )
      
    )
  )
)


#########################################################################

# 
# a<-EvolutionAndWeights[["Market"]]
# a <- melt(a, id.var = "Date")
# c <- b %>% separate(variable, c("Portfolio","Dependence","BorA","VineType","Opt","EorW","Asset"), sep = "_")
# ggplot(c %>% filter(Opt == "Sharpe", EorW == "Evolution"), aes(Date,value, color = BorA,  group = BorA)) + geom_line() + facet_wrap(~VineType) 

#########################################################################
server <- function(input, output) { 
  
  
#########################################              MARKET Indices EVOLUTION           ################################################  
  output$marketnaiveplot <- renderPlotly({
   
  a <- EvolutionAndWeights[["Market"]] %>% select(Date,intersect(contains("Naive"),contains("Evolution")))%>% filter(between(Date,as.Date(input$MIDates[1]),as.Date(input$MIDates[2]))) %>%
    filter(between(Date,as.Date(input$MDates[1]),as.Date(input$MDates[2])))
  
  a <- Inv(a) 
  #a <- Evolution %>% select(Date,contains("Naive")) %>% filter(between(Date,as.Date(input$MDates[1]),as.Date(input$MDates[2])))
  a <- melt(a, id.var = "Date")
  b <- a %>% separate(variable, c("Portfolio","Dependence","BorA","VineType","Opt","EorW","Asset"), sep = "_")
  b <- b %>% mutate(Opt = replace(Opt, Opt == "MinSD", "MinVar"))
  c <- ggplot(b, aes(Date,value, color = BorA,  group = BorA)) + geom_line() + facet_wrap(~VineType) + theme_wsj()+  scale_x_date(date_labels = "%y")  + theme(legend.position = "bottom",axis.text.x = element_text(angle=45, hjust = 1))+ labs(color ="")
  d <- ggplotly(c, height = 600, width = 1000)
    })
  
  output$marketminsdplot <- renderPlotly({
    
    a <- EvolutionAndWeights[["Market"]] %>% select(Date,intersect(contains("MinSD"),contains("Evolution")))%>% filter(between(Date,as.Date(input$MIDates[1]),as.Date(input$MIDates[2]))) %>%
      filter(between(Date,as.Date(input$MDates[1]),as.Date(input$MDates[2])))
    
    a <- Inv(a) 
    #a <- Evolution %>% select(Date,contains("Naive")) %>% filter(between(Date,as.Date(input$MDates[1]),as.Date(input$MDates[2])))
    a <- melt(a, id.var = "Date")
    b <- a %>% separate(variable, c("Portfolio","Dependence","BorA","VineType","Opt","EorW","Asset"), sep = "_")
    b <- b %>% mutate(Opt = replace(Opt, Opt == "MinSD", "MinVar"))
    c <- ggplot(b, aes(x = Date, y = value, color = BorA, group = BorA)) + geom_line() + facet_wrap(~VineType) + theme_wsj()+  scale_x_date(date_labels = "%y") + theme(legend.position = "bottom",axis.text.x = element_text(angle=45, hjust = 1))+ labs(color ="")
    d <- ggplotly(c, height = 600, width = 1000)
  })
  
  output$marketminesplot <- renderPlotly({
    
    a <- EvolutionAndWeights[["Market"]] %>% select(Date,intersect(contains("MinES"),contains("Evolution")))%>% filter(between(Date,as.Date(input$MIDates[1]),as.Date(input$MIDates[2]))) %>%
      filter(between(Date,as.Date(input$MDates[1]),as.Date(input$MDates[2])))
    
    a <- Inv(a) 
    #a <- Evolution %>% select(Date,contains("Naive")) %>% filter(between(Date,as.Date(input$MDates[1]),as.Date(input$MDates[2])))
    a <- melt(a, id.var = "Date")
    b <- a %>% separate(variable, c("Portfolio","Dependence","BorA","VineType","Opt","EorW","Asset"), sep = "_")
    b <- b %>% mutate(Opt = replace(Opt, Opt == "MinSD", "MinVar"))
    c <- ggplot(b, aes(Date,value, color = BorA,  group = BorA)) + geom_line() + facet_wrap(~VineType)  + theme_wsj()+  scale_x_date(date_labels = "%y") + theme(legend.position = "bottom",axis.text.x = element_text(angle=45, hjust = 1))+ labs(color ="")
    d <- ggplotly(c, height = 600, width = 1000)
  })
  
  output$marketsharpeplot <- renderPlotly({
    
    a <- EvolutionAndWeights[["Market"]] %>% select(Date,intersect(contains("Sharpe"),contains("Evolution")))%>% filter(between(Date,as.Date(input$MIDates[1]),as.Date(input$MIDates[2]))) %>%
      filter(between(Date,as.Date(input$MDates[1]),as.Date(input$MDates[2])))
    
    a <- Inv(a) 
    #a <- Evolution %>% select(Date,contains("Naive")) %>% filter(between(Date,as.Date(input$MDates[1]),as.Date(input$MDates[2])))
    a <- melt(a, id.var = "Date")
    b <- a %>% separate(variable, c("Portfolio","Dependence","BorA","VineType","Opt","EorW","Asset"), sep = "_")
    b <- b %>% mutate(Opt = replace(Opt, Opt == "MinSD", "MinVar"))
    c <- ggplot(b, aes(Date,value, color = BorA,  group = BorA)) + geom_line() + facet_wrap(~VineType) + theme_wsj()+  scale_x_date(date_labels = "%y") + theme(legend.position = "bottom",axis.text.x = element_text(angle=45, hjust = 1))+ labs(color ="")
    d <- ggplotly(c, height = 600, width = 1000)
  })
  
  output$marketmaxmeanplot <- renderPlotly({
    
    a <- EvolutionAndWeights[["Market"]] %>% select(Date,intersect(contains("MaxMean"),contains("Evolution")))%>% filter(between(Date,as.Date(input$MIDates[1]),as.Date(input$MIDates[2]))) %>%
      filter(between(Date,as.Date(input$MDates[1]),as.Date(input$MDates[2])))
    
    a <- Inv(a) 
    #a <- Evolution %>% select(Date,contains("Naive")) %>% filter(between(Date,as.Date(input$MDates[1]),as.Date(input$MDates[2])))
    a <- melt(a, id.var = "Date")
    b <- a %>% separate(variable, c("Portfolio","Dependence","BorA","VineType","Opt","EorW","Asset"), sep = "_")
    b <- b %>% mutate(Opt = replace(Opt, Opt == "MinSD", "MinVar"))
    c <- ggplot(b, aes(Date,value, color = BorA,  group = BorA)) + geom_line() + facet_wrap(~VineType) + theme_wsj()+  scale_x_date(date_labels = "%y") + theme(legend.position = "bottom",axis.text.x = element_text(angle=45, hjust = 1))+ labs(color ="")
    d <- ggplotly(c, height = 600, width = 1000)
  })
  
  ###### Copulawise
  
  
  output$marketCONV <- renderPlotly({
    
    a <- EvolutionAndWeights[["Market"]] %>% select(Date,intersect(contains("Conv"),contains("Evolution")))%>% filter(between(Date,as.Date(input$MIDates[1]),as.Date(input$MIDates[2]))) %>%
      filter(between(Date,as.Date(input$MDates[1]),as.Date(input$MDates[2]))) %>% filter()
    
    a <- Inv(a) 
    #a <- Evolution %>% select(Date,contains("Naive")) %>% filter(between(Date,as.Date(input$MDates[1]),as.Date(input$MDates[2])))
    a <- melt(a, id.var = "Date")
    b <- a %>% separate(variable, c("Portfolio","Dependence","BorA","VineType","Opt","EorW","Asset"), sep = "_")
    b <- b %>% filter(Opt != "Starr")
    b <- b %>% mutate(Opt = replace(Opt, Opt == "MinSD", "MinVar"))
    c <- ggplot(b, aes(Date,value, color = BorA,  group = BorA)) + geom_line() + facet_wrap(~Opt) + theme_wsj()+  scale_x_date(date_labels = "%y") + theme(legend.position = "bottom",axis.text.x = element_text(angle=45, hjust = 1))+ labs(color ="")
    d <- ggplotly(c, height = 600, width = 1000)
  })
  
  
  
  output$marketPR <- renderPlotly({
    
    a <- EvolutionAndWeights[["Market"]] %>% select(Date,intersect(contains("P-R"),contains("Evolution")))%>% filter(between(Date,as.Date(input$MIDates[1]),as.Date(input$MIDates[2]))) %>%
      filter(between(Date,as.Date(input$MDates[1]),as.Date(input$MDates[2])))
    
    a <- Inv(a) 
    #a <- Evolution %>% select(Date,contains("Naive")) %>% filter(between(Date,as.Date(input$MDates[1]),as.Date(input$MDates[2])))
    a <- melt(a, id.var = "Date")
    b <- a %>% separate(variable, c("Portfolio","Dependence","BorA","VineType","Opt","EorW","Asset"), sep = "_")
    b <- b %>% mutate(Opt = replace(Opt, Opt == "MinSD", "MinVar"))
    c <- ggplot(b, aes(Date,value, color = BorA,  group = BorA)) + geom_line() + facet_wrap(~Opt) + theme_wsj()+  scale_x_date(date_labels = "%y") + theme(legend.position = "bottom",axis.text.x = element_text(angle=45, hjust = 1))+ labs(color ="")
    d <- ggplotly(c, height = 600, width = 1000)
  })
  
  
  
  output$marketPC <- renderPlotly({
    
    a <- EvolutionAndWeights[["Market"]] %>% select(Date,intersect(contains("P-C"),contains("Evolution")))%>% filter(between(Date,as.Date(input$MIDates[1]),as.Date(input$MIDates[2]))) %>%
      filter(between(Date,as.Date(input$MDates[1]),as.Date(input$MDates[2])))
    
    a <- Inv(a) 
    #a <- Evolution %>% select(Date,contains("Naive")) %>% filter(between(Date,as.Date(input$MDates[1]),as.Date(input$MDates[2])))
    a <- melt(a, id.var = "Date")
    b <- a %>% separate(variable, c("Portfolio","Dependence","BorA","VineType","Opt","EorW","Asset"), sep = "_")
    b <- b %>% mutate(Opt = replace(Opt, Opt == "MinSD", "MinVar"))
    c <- ggplot(b, aes(Date,value, color = BorA,  group = BorA)) + geom_line() + facet_wrap(~Opt) + theme_wsj()+  scale_x_date(date_labels = "%y") + theme(legend.position = "bottom",axis.text.x = element_text(angle=45, hjust = 1))+ labs(color ="")
    d <- ggplotly(c, height = 600, width = 1000)
  })
  
  
  
  output$marketER <- renderPlotly({
    
    a <- EvolutionAndWeights[["Market"]] %>% select(Date,intersect(contains("E-R"),contains("Evolution")))%>% filter(between(Date,as.Date(input$MIDates[1]),as.Date(input$MIDates[2]))) %>%
      filter(between(Date,as.Date(input$MDates[1]),as.Date(input$MDates[2])))
    
    a <- Inv(a) 
    #a <- Evolution %>% select(Date,contains("Naive")) %>% filter(between(Date,as.Date(input$MDates[1]),as.Date(input$MDates[2])))
    a <- melt(a, id.var = "Date")
    b <- a %>% separate(variable, c("Portfolio","Dependence","BorA","VineType","Opt","EorW","Asset"), sep = "_")
    b <- b %>% mutate(Opt = replace(Opt, Opt == "MinSD", "MinVar"))
    c <- ggplot(b, aes(Date,value, color = BorA,  group = BorA)) + geom_line() + facet_wrap(~Opt) + theme_wsj()+  scale_x_date(date_labels = "%y") + theme(legend.position = "bottom",axis.text.x = element_text(angle=45, hjust = 1))+ labs(color ="")
    d <- ggplotly(c, height = 600, width = 1000)
  })
  
  
  
  output$marketEC <- renderPlotly({
    
    a <- EvolutionAndWeights[["Market"]] %>% select(Date,intersect(contains("E-C"),contains("Evolution")))%>% filter(between(Date,as.Date(input$MIDates[1]),as.Date(input$MIDates[2]))) %>%
      filter(between(Date,as.Date(input$MDates[1]),as.Date(input$MDates[2])))
    
    a <- Inv(a) 
    #a <- Evolution %>% select(Date,contains("Naive")) %>% filter(between(Date,as.Date(input$MDates[1]),as.Date(input$MDates[2])))
    a <- melt(a, id.var = "Date")
    b <- a %>% separate(variable, c("Portfolio","Dependence","BorA","VineType","Opt","EorW","Asset"), sep = "_")
    b <- b %>% mutate(Opt = replace(Opt, Opt == "MinSD", "MinVar"))
    c <- ggplot(b, aes(Date,value, color = BorA,  group = BorA)) + geom_line() + facet_wrap(~Opt) + theme_wsj()+  scale_x_date(date_labels = "%y") + theme(legend.position = "bottom",axis.text.x = element_text(angle=45, hjust = 1))+ labs(color ="")
    d <- ggplotly(c, height = 600, width = 1000)
  })
  
  
  
  ##############################################   Mean,SD,and CAGR   ############################################
  output$marketSelection <- renderDataTable({
    d1 <- PerformanceM[["Alltogether"]] %>% filter(Portfolio == "SP500")
    d2 <- PerformanceM[["Alltogether"]] %>% filter(Portfolio == "Market")   %>% filter(VineType %in% input$MC1) %>% 
          filter(Opt %in% input$MO1) 
    
    d3 <- rbind(d1[1,],d2)
    d3[,c(6:9)] <- d3[,c(6:9)] %>% round(digits = 4)
    datatable(d3)
  })
  
  output$marketSelection2 <- renderDataTable({
    d1 <- PerformanceM[["Alltogether"]] %>% filter(Portfolio == "SP500")
    d2 <- PerformanceM[["Alltogether"]] %>% filter(Portfolio == "Market")  %>% filter(Opt %in% input$MO1) 
    
    d3 <- rbind(d1[1,],d2) 
    d3[,c(6:9)] <- d3[,c(6:9)] %>% round(digits = 4)
    datatable(d3)
  })
  
  output$marketSelection3 <- renderDataTable({
    d1 <- PerformanceM[["Alltogether"]] %>% filter(Portfolio == "SP500")
    d2 <- PerformanceM[["Alltogether"]] %>% filter(Portfolio == "Market") %>%   filter(VineType %in% input$MC1)
    
    d3 <- rbind(d1[1,],d2) 
    d3[,c(6:9)] <- d3[,c(6:9)] %>% round(digits = 4)
    datatable(d3)
  })
  
  ##############################################   Mean,SD,and CAGR   ############################################  
  
  
  
  ##############################################       Weights      ############################################    
  
  
  ##### OptimizationWise #####
  
  output$marketnaiveweight <- renderPlotly({
    
    a <- EvolutionAndWeights[["Market"]] %>% select(Date, intersect(contains("Naive"),contains("Weights")))
    a <- a %>% select(Date,contains("BTC"),contains("LTC"),contains("XRP"),contains("XMR"),contains("ETH"),contains("BCH"),contains("EOS"))
    a <- melt(a, id.var = "Date")
    a <- a %>% separate(variable, c("Portfolio","Dependence","BorA","VineType","Opt","EorW","Asset"), sep = "_")
    a <- a %>% filter(Opt != "Starr")
    a <- a %>% mutate(Opt = replace(Opt, Opt == "MinSD", "MinVar"))
    b <- ggplot(a, aes(x = Date, y = value)) + geom_line() +facet_wrap(~Asset+VineType, scales = "free", nrow = 4) + theme_wsj()+  scale_x_date(date_labels = "%y")+ theme(panel.spacing = unit(2, "lines"))  + theme(panel.spacing = unit(2, "lines")) 
    ggplotly(b, height = 1500, width = 2000)
  })
  
  
  output$marketminsdweight <- renderPlotly({
    
    a <- EvolutionAndWeights[["Market"]] %>% select(Date, intersect(contains("MinSD"),contains("Weights")))
    a <- a %>% select(Date,contains("BTC"),contains("LTC"),contains("XRP"),contains("XMR"),contains("ETH"),contains("BCH"),contains("EOS"))
    a <- melt(a, id.var = "Date")
    a <- a %>% separate(variable, c("Portfolio","Dependence","BorA","VineType","Opt","EorW","Asset"), sep = "_")
    a <- a %>% filter(Opt != "Starr")
    a <- a %>% mutate(Opt = replace(Opt, Opt == "MinSD", "MinVar"))
    b <- ggplot(a, aes(x = Date, y = value)) + geom_line() +facet_wrap(~Asset+VineType, scales = "free", nrow = 4) + theme_wsj()+  scale_x_date(date_labels = "%y")+ theme(panel.spacing = unit(2, "lines"))  + theme(panel.spacing = unit(2, "lines")) 
    ggplotly(b, height = 1500, width = 2000)
  })
  
  output$marketminesweight <- renderPlotly({
    
    a <- EvolutionAndWeights[["Market"]] %>% select(Date, intersect(contains("MinES"),contains("Weights")))
    a <- a %>% select(Date,contains("BTC"),contains("LTC"),contains("XRP"),contains("XMR"),contains("ETH"),contains("BCH"),contains("EOS"))
    a <- melt(a, id.var = "Date")
    a <- a %>% separate(variable, c("Portfolio","Dependence","BorA","VineType","Opt","EorW","Asset"), sep = "_")
    a <- a %>% filter(Opt != "Starr")
    a <- a %>% mutate(Opt = replace(Opt, Opt == "MinSD", "MinVar"))
     b <- ggplot(a, aes(x = Date, y = value)) + geom_line() +facet_wrap(~Asset+VineType, scales = "free", nrow = 4) + theme_wsj()+  scale_x_date(date_labels = "%y")+ theme(panel.spacing = unit(2, "lines")) + theme(panel.spacing = unit(2, "lines")) 
    ggplotly(b, height = 1500, width = 2000)
  })
  
  output$marketsharpeweight <- renderPlotly({
    a <- EvolutionAndWeights[["Market"]] %>% select(Date, intersect(contains("Sharpe"),contains("Weights")))
    a <- a %>% select(Date,contains("BTC"),contains("LTC"),contains("XRP"),contains("XMR"),contains("ETH"),contains("BCH"),contains("EOS"))
    a <- melt(a, id.var = "Date")
    a <- a %>% separate(variable, c("Portfolio","Dependence","BorA","VineType","Opt","EorW","Asset"), sep = "_")
    a <- a %>% filter(Opt != "Starr")
    a <- a %>% mutate(Opt = replace(Opt, Opt == "MinSD", "MinVar"))
    b <- ggplot(a, aes(x = Date, y = value)) + geom_line() +facet_wrap(~Asset+VineType, scales = "free", nrow = 4) + theme_wsj()+  scale_x_date(date_labels = "%y")+ theme(panel.spacing = unit(2, "lines")) + theme(panel.spacing = unit(2, "lines")) 
    ggplotly(b, height = 1500, width = 2000)
  
  })
  
  output$marketmaxmeanweight <- renderPlotly({
    
    a <- EvolutionAndWeights[["Market"]] %>% select(Date, intersect(contains("MaxMean"),contains("Weights")))
    a <- a %>% select(Date,contains("BTC"),contains("LTC"),contains("XRP"),contains("XMR"),contains("ETH"),contains("BCH"),contains("EOS"))
    a <- melt(a, id.var = "Date")
    a <- a %>% separate(variable, c("Portfolio","Dependence","BorA","VineType","Opt","EorW","Asset"), sep = "_")
    a <- a %>% filter(Opt != "Starr")
    a <- a %>% mutate(Opt = replace(Opt, Opt == "MinSD", "MinVar"))
    b <- ggplot(a, aes(x = Date, y = value)) + geom_line() +facet_wrap(~Asset+VineType, scales = "free", nrow = 4) + theme_wsj()+  scale_x_date(date_labels = "%y")+ theme(panel.spacing = unit(2, "lines")) + theme(panel.spacing = unit(2, "lines")) 
    ggplotly(b, height = 1500, width = 2000)
  })
  
  
  
  ##### CopulaWise ##### 
  
  output$marketCONVweight <- renderPlotly({
    
    
    a <- EvolutionAndWeights[["Market"]] %>% select(Date, intersect(contains("Conv"),contains("Weights")))
    a <- a %>% select(Date,contains("BTC"),contains("LTC"),contains("XRP"),contains("XMR"),contains("ETH"),contains("BCH"),contains("EOS"))
    a <- melt(a, id.var = "Date")
    a <- a %>% separate(variable, c("Portfolio","Dependence","BorA","VineType","Opt","EorW","Asset"), sep = "_")
    a <- a %>% filter(Opt != "Starr")
    a <- a %>% mutate(Opt = replace(Opt, Opt == "MinSD", "MinVar"))
    b <- ggplot(a, aes(x = Date, y = value)) + geom_line() +facet_wrap(~Asset+Opt, scales = "free", nrow = 4) + theme_wsj()+  scale_x_date(date_labels = "%y")+ theme(panel.spacing = unit(2, "lines")) + theme(panel.spacing = unit(2, "lines")) 
    ggplotly(b, height = 1500, width = 2000)
  
  })
  
  
  output$marketPRweight <- renderPlotly({
    
    a <- EvolutionAndWeights[["Market"]] %>% select(Date, intersect(contains("P-R"),contains("Weights")))
    a <- a %>% select(Date,contains("BTC"),contains("LTC"),contains("XRP"),contains("XMR"),contains("ETH"),contains("BCH"),contains("EOS"))
    a <- melt(a, id.var = "Date")
    a <- a %>% separate(variable, c("Portfolio","Dependence","BorA","VineType","Opt","EorW","Asset"), sep = "_")
    a <- a %>% filter(Opt != "Starr")
    a <- a %>% mutate(Opt = replace(Opt, Opt == "MinSD", "MinVar"))
    b <- ggplot(a, aes(x = Date, y = value)) + geom_line() +facet_wrap(~Asset+Opt, scales = "free", nrow = 4) + theme_wsj()+  scale_x_date(date_labels = "%y")+ theme(panel.spacing = unit(2, "lines")) + theme(panel.spacing = unit(2, "lines")) 
    ggplotly(b, height = 1500, width = 2000)
  })
  
  output$marketPCweight <- renderPlotly({

    a <- EvolutionAndWeights[["Market"]] %>% select(Date, intersect(contains("P-C"),contains("Weights")))
    a <- a %>% select(Date,contains("BTC"),contains("LTC"),contains("XRP"),contains("XMR"),contains("ETH"),contains("BCH"),contains("EOS"))
    a <- melt(a, id.var = "Date")
    a <- a %>% separate(variable, c("Portfolio","Dependence","BorA","VineType","Opt","EorW","Asset"), sep = "_")
    a <- a %>% filter(Opt != "Starr")
    a <- a %>% mutate(Opt = replace(Opt, Opt == "MinSD", "MinVar"))
    b <- ggplot(a, aes(x = Date, y = value)) + geom_line() +facet_wrap(~Asset+Opt, scales = "free", nrow = 4) + theme_wsj()+  scale_x_date(date_labels = "%y")+ theme(panel.spacing = unit(2, "lines")) + theme(panel.spacing = unit(2, "lines")) 
    ggplotly(b, height = 1500, width = 2000)
  })
  
    output$marketERweight <- renderPlotly({
    
      
    a <- EvolutionAndWeights[["Market"]] %>% select(Date, intersect(contains("E-R"),contains("Weights")))
    a <- a %>% select(Date,contains("BTC"),contains("LTC"),contains("XRP"),contains("XMR"),contains("ETH"),contains("BCH"),contains("EOS"))
    a <- melt(a, id.var = "Date")
    a <- a %>% separate(variable, c("Portfolio","Dependence","BorA","VineType","Opt","EorW","Asset"), sep = "_")
    a <- a %>% filter(Opt != "Starr")
    a <- a %>% mutate(Opt = replace(Opt, Opt == "MinSD", "MinVar"))
    b <- ggplot(a, aes(x = Date, y = value)) + geom_line() +facet_wrap(~Asset+Opt, scales = "free", nrow = 4) + theme_wsj()+  scale_x_date(date_labels = "%y")+ theme(panel.spacing = unit(2, "lines")) + theme(panel.spacing = unit(2, "lines")) 
    ggplotly(b, height = 1500, width = 2000)
    })
    
  output$marketECweight <- renderPlotly({
    
    a <- EvolutionAndWeights[["Market"]] %>% select(Date, intersect(contains("E-C"),contains("Weights")))
    a <- a %>% select(Date,contains("BTC"),contains("LTC"),contains("XRP"),contains("XMR"),contains("ETH"),contains("BCH"),contains("EOS"))
    a <- melt(a, id.var = "Date")
    a <- a %>% separate(variable, c("Portfolio","Dependence","BorA","VineType","Opt","EorW","Asset"), sep = "_")
    a <- a %>% filter(Opt != "Starr")
    a <- a %>% mutate(Opt = replace(Opt, Opt == "MinSD", "MinVar"))
    b <- ggplot(a, aes(x = Date, y = value)) + geom_line() +facet_wrap(~Asset+Opt, scales = "free", nrow = 4) + theme_wsj()+  scale_x_date(date_labels = "%y")+ theme(panel.spacing = unit(2, "lines")) + theme(panel.spacing = unit(2, "lines")) 
    ggplotly(b, height = 1500, width = 2000)
  })
  
    
  ##############################################       Weights      ############################################   
  
  #########################################              Fixed Indices EVOLUTION           ################################################  
  output$Fixednaiveplot <- renderPlotly({
    
    a <- EvolutionAndWeights[["Fixed"]] %>% select(Date,intersect(contains("Naive"),contains("Evolution")))%>% filter(between(Date,as.Date(input$FIDates[1]),as.Date(input$FIDates[2]))) %>%
      filter(between(Date,as.Date(input$FDates[1]),as.Date(input$FDates[2])))
    
    a <- Inv(a) 
    #a <- Evolution %>% select(Date,contains("Naive")) %>% filter(between(Date,as.Date(input$FDates[1]),as.Date(input$FDates[2])))
    a <- melt(a, id.var = "Date")
    b <- a %>% separate(variable, c("Portfolio","Dependence","BorA","VineType","Opt","EorW","Asset"), sep = "_")
    b <- b %>% mutate(Opt = replace(Opt, Opt == "MinSD", "MinVar"))
    c <- ggplot(b, aes(Date,value, color = BorA,  group = BorA)) + geom_line() + facet_wrap(~VineType) + theme_wsj()+  scale_x_date(date_labels = "%y")  + theme(legend.position = "bottom",axis.text.x = element_text(angle=45, hjust = 1))+ labs(color ="")
    d <- ggplotly(c, height = 600, width = 1000)
  })
  
  output$Fixedminsdplot <- renderPlotly({
    
    a <- EvolutionAndWeights[["Fixed"]] %>% select(Date,intersect(contains("MinSD"),contains("Evolution")))%>% filter(between(Date,as.Date(input$FIDates[1]),as.Date(input$FIDates[2]))) %>%
      filter(between(Date,as.Date(input$FDates[1]),as.Date(input$FDates[2])))
    
    a <- Inv(a) 
    #a <- Evolution %>% select(Date,contains("Naive")) %>% filter(between(Date,as.Date(input$FDates[1]),as.Date(input$FDates[2])))
    a <- melt(a, id.var = "Date")
    b <- a %>% separate(variable, c("Portfolio","Dependence","BorA","VineType","Opt","EorW","Asset"), sep = "_")
    b <- b %>% mutate(Opt = replace(Opt, Opt == "MinSD", "MinVar"))
    c <- ggplot(b, aes(Date,value, color = BorA,  group = BorA)) + geom_line() + facet_wrap(~VineType) + theme_wsj()+  scale_x_date(date_labels = "%y") + theme(legend.position = "bottom",axis.text.x = element_text(angle=45, hjust = 1))+ labs(color ="")
    d <- ggplotly(c, height = 600, width = 1000)
  })
  
  output$Fixedminesplot <- renderPlotly({
    
    a <- EvolutionAndWeights[["Fixed"]] %>% select(Date,intersect(contains("MinES"),contains("Evolution")))%>% filter(between(Date,as.Date(input$FIDates[1]),as.Date(input$FIDates[2]))) %>%
      filter(between(Date,as.Date(input$FDates[1]),as.Date(input$FDates[2])))
    
    a <- Inv(a) 
    #a <- Evolution %>% select(Date,contains("Naive")) %>% filter(between(Date,as.Date(input$FDates[1]),as.Date(input$FDates[2])))
    a <- melt(a, id.var = "Date")
    b <- a %>% separate(variable, c("Portfolio","Dependence","BorA","VineType","Opt","EorW","Asset"), sep = "_")
    b <- b %>% mutate(Opt = replace(Opt, Opt == "MinSD", "MinVar"))
    c <- ggplot(b, aes(Date,value, color = BorA,  group = BorA)) + geom_line() + facet_wrap(~VineType) + theme_wsj()+  scale_x_date(date_labels = "%y") + theme(legend.position = "bottom",axis.text.x = element_text(angle=45, hjust = 1))+ labs(color ="")
    d <- ggplotly(c, height = 600, width = 1000)
  })
  
  output$Fixedsharpeplot <- renderPlotly({
    
    a <- EvolutionAndWeights[["Fixed"]] %>% select(Date,intersect(contains("Sharpe"),contains("Evolution")))%>% filter(between(Date,as.Date(input$FIDates[1]),as.Date(input$FIDates[2]))) %>%
      filter(between(Date,as.Date(input$FDates[1]),as.Date(input$FDates[2])))
    
    a <- Inv(a) 
    #a <- Evolution %>% select(Date,contains("Naive")) %>% filter(between(Date,as.Date(input$FDates[1]),as.Date(input$FDates[2])))
    a <- melt(a, id.var = "Date")
    b <- a %>% separate(variable, c("Portfolio","Dependence","BorA","VineType","Opt","EorW","Asset"), sep = "_")
    b <- b %>% mutate(Opt = replace(Opt, Opt == "MinSD", "MinVar"))
    c <- ggplot(b, aes(Date,value, color = BorA,  group = BorA)) + geom_line() + facet_wrap(~VineType) + theme_wsj()+  scale_x_date(date_labels = "%y") + theme(legend.position = "bottom",axis.text.x = element_text(angle=45, hjust = 1))+ labs(color ="")
    d <- ggplotly(c, height = 600, width = 1000)
  })
  
  output$Fixedmaxmeanplot <- renderPlotly({
    
    a <- EvolutionAndWeights[["Fixed"]] %>% select(Date,intersect(contains("MaxMean"),contains("Evolution")))%>% filter(between(Date,as.Date(input$FIDates[1]),as.Date(input$FIDates[2]))) %>%
      filter(between(Date,as.Date(input$FDates[1]),as.Date(input$FDates[2])))
    
    a <- Inv(a) 
    #a <- Evolution %>% select(Date,contains("Naive")) %>% filter(between(Date,as.Date(input$FDates[1]),as.Date(input$FDates[2])))
    a <- melt(a, id.var = "Date")
    b <- a %>% separate(variable, c("Portfolio","Dependence","BorA","VineType","Opt","EorW","Asset"), sep = "_")
    b <- b %>% mutate(Opt = replace(Opt, Opt == "MinSD", "MinVar"))
    c <- ggplot(b, aes(Date,value, color = BorA,  group = BorA)) + geom_line() + facet_wrap(~VineType) + theme_wsj()+  scale_x_date(date_labels = "%y") + theme(legend.position = "bottom",axis.text.x = element_text(angle=45, hjust = 1))+ labs(color ="")
    d <- ggplotly(c, height = 600, width = 1000)
  })
  
  ###### Copulawise
  
  
  output$FixedCONV <- renderPlotly({
    
    a <- EvolutionAndWeights[["Fixed"]] %>% select(Date,intersect(contains("Conv"),contains("Evolution")))%>% filter(between(Date,as.Date(input$FIDates[1]),as.Date(input$FIDates[2]))) %>%
      filter(between(Date,as.Date(input$FDates[1]),as.Date(input$FDates[2])))
    
    a <- Inv(a) 
    #a <- Evolution %>% select(Date,contains("Naive")) %>% filter(between(Date,as.Date(input$FDates[1]),as.Date(input$FDates[2])))
    a <- melt(a, id.var = "Date")
    b <- a %>% separate(variable, c("Portfolio","Dependence","BorA","VineType","Opt","EorW","Asset"), sep = "_")
    b <- b %>% mutate(Opt = replace(Opt, Opt == "MinSD", "MinVar"))
    c <- ggplot(b, aes(Date,value, color = BorA,  group = BorA)) + geom_line() + facet_wrap(~Opt) + theme_wsj()+  scale_x_date(date_labels = "%y") + theme(legend.position = "bottom",axis.text.x = element_text(angle=45, hjust = 1))+ labs(color ="")
    d <- ggplotly(c, height = 600, width = 1000)
  })
  
  
  
  output$FixedPR <- renderPlotly({
    
    a <- EvolutionAndWeights[["Fixed"]] %>% select(Date,intersect(contains("P-R"),contains("Evolution")))%>% filter(between(Date,as.Date(input$FIDates[1]),as.Date(input$FIDates[2]))) %>%
      filter(between(Date,as.Date(input$FDates[1]),as.Date(input$FDates[2])))
    
    a <- Inv(a) 
    #a <- Evolution %>% select(Date,contains("Naive")) %>% filter(between(Date,as.Date(input$FDates[1]),as.Date(input$FDates[2])))
    a <- melt(a, id.var = "Date")
    b <- a %>% separate(variable, c("Portfolio","Dependence","BorA","VineType","Opt","EorW","Asset"), sep = "_")
    b <- b %>% mutate(Opt = replace(Opt, Opt == "MinSD", "MinVar"))
    c <- ggplot(b, aes(Date,value, color = BorA,  group = BorA)) + geom_line() + facet_wrap(~Opt) + theme_wsj()+  scale_x_date(date_labels = "%y") + theme(legend.position = "bottom",axis.text.x = element_text(angle=45, hjust = 1))+ labs(color ="")
    d <- ggplotly(c, height = 600, width = 1000)
  })
  
  
  
  output$FixedPC <- renderPlotly({
    
    a <- EvolutionAndWeights[["Fixed"]] %>% select(Date,intersect(contains("P-C"),contains("Evolution")))%>% filter(between(Date,as.Date(input$FIDates[1]),as.Date(input$FIDates[2]))) %>%
      filter(between(Date,as.Date(input$FDates[1]),as.Date(input$FDates[2])))
    
    a <- Inv(a) 
    #a <- Evolution %>% select(Date,contains("Naive")) %>% filter(between(Date,as.Date(input$FDates[1]),as.Date(input$FDates[2])))
    a <- melt(a, id.var = "Date")
    b <- a %>% separate(variable, c("Portfolio","Dependence","BorA","VineType","Opt","EorW","Asset"), sep = "_")
    b <- b %>% mutate(Opt = replace(Opt, Opt == "MinSD", "MinVar"))
    c <- ggplot(b, aes(Date,value, color = BorA,  group = BorA)) + geom_line() + facet_wrap(~Opt) + theme_wsj()+  scale_x_date(date_labels = "%y") + theme(legend.position = "bottom",axis.text.x = element_text(angle=45, hjust = 1))+ labs(color ="") 
    d <- ggplotly(c, height = 600, width = 1000)
  })
  
  
  
  output$FixedER <- renderPlotly({
    
    a <- EvolutionAndWeights[["Fixed"]] %>% select(Date,intersect(contains("E-R"),contains("Evolution")))%>% filter(between(Date,as.Date(input$FIDates[1]),as.Date(input$FIDates[2]))) %>%
      filter(between(Date,as.Date(input$FDates[1]),as.Date(input$FDates[2])))
    
    a <- Inv(a) 
    #a <- Evolution %>% select(Date,contains("Naive")) %>% filter(between(Date,as.Date(input$FDates[1]),as.Date(input$FDates[2])))
    a <- melt(a, id.var = "Date")
    b <- a %>% separate(variable, c("Portfolio","Dependence","BorA","VineType","Opt","EorW","Asset"), sep = "_")
    b <- b %>% mutate(Opt = replace(Opt, Opt == "MinSD", "MinVar"))
    c <- ggplot(b, aes(Date,value, color = BorA,  group = BorA)) + geom_line() +facet_wrap(~Opt) + theme_wsj()+  scale_x_date(date_labels = "%y") + theme(legend.position = "bottom",axis.text.x = element_text(angle=45, hjust = 1))+ labs(color ="")  
    d <- ggplotly(c, height = 600, width = 1000)
  })
  
  
  
  output$FixedEC <- renderPlotly({
    
    a <- EvolutionAndWeights[["Fixed"]] %>% select(Date,intersect(contains("E-C"),contains("Evolution")))%>% filter(between(Date,as.Date(input$FIDates[1]),as.Date(input$FIDates[2]))) %>%
      filter(between(Date,as.Date(input$FDates[1]),as.Date(input$FDates[2])))
    
    a <- Inv(a) 
    #a <- Evolution %>% select(Date,contains("Naive")) %>% filter(between(Date,as.Date(input$FDates[1]),as.Date(input$FDates[2])))
    a <- melt(a, id.var = "Date")
    b <- a %>% separate(variable, c("Portfolio","Dependence","BorA","VineType","Opt","EorW","Asset"), sep = "_")
    b <- b %>% mutate(Opt = replace(Opt, Opt == "MinSD", "MinVar"))
    c <- ggplot(b, aes(Date,value, color = BorA,  group = BorA)) + geom_line() +facet_wrap(~Opt) + theme_wsj()+  scale_x_date(date_labels = "%y") + theme(legend.position = "bottom",axis.text.x = element_text(angle=45, hjust = 1))+ labs(color ="")  
    d <- ggplotly(c, height = 600, width = 1000)
  })
  
  
  
  ##############################################   Mean,SD,and CAGR   ############################################
  output$FixedSelection <- renderDataTable({
    d1 <- PerformanceM[["Alltogether"]] %>% filter(Portfolio == "SP500")
    d2 <- PerformanceM[["Alltogether"]] %>% filter(Portfolio == "Fixed")   %>% filter(VineType %in% input$FC1) %>% 
      filter(Opt %in% input$FO1) 
    
    d3 <- rbind(d1[1,],d2) 
    d3[,c(6:9)] <- d3[,c(6:9)] %>% round(digits = 4)
    datatable(d3)
  })
  
  output$FixedSelection2 <- renderDataTable({
    d1 <- PerformanceM[["Alltogether"]] %>% filter(Portfolio == "SP500")
    d2 <- PerformanceM[["Alltogether"]] %>% filter(Portfolio == "Fixed")  %>% filter(Opt %in% input$FO1) 
    
    d3 <- rbind(d1[1,],d2) 
    d3[,c(6:9)] <- d3[,c(6:9)] %>% round(digits = 4)
    datatable(d3)
  })
  
  output$FixedSelection3 <- renderDataTable({
    d1 <- PerformanceM[["Alltogether"]] %>% filter(Portfolio == "SP500")
    d2 <- PerformanceM[["Alltogether"]] %>% filter(Portfolio == "Fixed") %>%   filter(VineType %in% input$FC1)
    
    d3 <- rbind(d1[1,],d2) 
    d3[,c(6:9)] <- d3[,c(6:9)] %>% round(digits = 4)
    datatable(d3)
  })
  
  ##############################################   Mean,SD,and CAGR   ############################################  
  
  
  
  ##############################################       Weights      ############################################    
  
  ##### OptimizationWise #####
  
  output$Fixednaiveweight <- renderPlotly({
    
    a <- EvolutionAndWeights[["Fixed"]] %>% select(Date, intersect(contains("Naive"),contains("Weights")))
    a <- a %>% select(Date,contains("BTC"),contains("LTC"),contains("XRP"),contains("XMR"),contains("ETH"),contains("BCH"),contains("EOS"))
    a <- melt(a, id.var = "Date")
    a <- a %>% separate(variable, c("Portfolio","Dependence","BorA","VineType","Opt","EorW","Asset"), sep = "_")
    a <- a %>% filter(Opt != "Starr")
    a <- a %>% mutate(Opt = replace(Opt, Opt == "MinSD", "MinVar"))
    b <- ggplot(a, aes(x = Date, y = value)) + geom_line() +facet_wrap(~Asset+VineType, scales = "free", nrow = 4) + theme_wsj()+  scale_x_date(date_labels = "%y")+ theme(panel.spacing = unit(2, "lines")) + theme(panel.spacing = unit(2, "lines"))  + theme(legend.position = "bottom",axis.text.x = element_text(angle=45, hjust = 1))+ labs(color ="")
    ggplotly(b, height = 1500, width = 2000)
  })
  
  
  output$Fixedminsdweight <- renderPlotly({
    
    a <- EvolutionAndWeights[["Fixed"]] %>% select(Date, intersect(contains("MinSD"),contains("Weights")))
    a <- a %>% select(Date,contains("BTC"),contains("LTC"),contains("XRP"),contains("XMR"),contains("ETH"),contains("BCH"),contains("EOS"))
    a <- melt(a, id.var = "Date")
    a <- a %>% separate(variable, c("Portfolio","Dependence","BorA","VineType","Opt","EorW","Asset"), sep = "_")
    a <- a %>% filter(Opt != "Starr")
    a <- a %>% mutate(Opt = replace(Opt, Opt == "MinSD", "MinVar"))
    b <- ggplot(a, aes(x = Date, y = value)) + geom_line() +facet_wrap(~Asset+VineType, scales = "free", nrow = 4) + theme_wsj()+  scale_x_date(date_labels = "%y")+ theme(panel.spacing = unit(2, "lines")) + theme(panel.spacing = unit(2, "lines"))  + theme(legend.position = "bottom",axis.text.x = element_text(angle=45, hjust = 1))+ labs(color ="")
    ggplotly(b, height = 1500, width = 2000)
  })
  
  output$Fixedminesweight <- renderPlotly({
    
    a <- EvolutionAndWeights[["Fixed"]] %>% select(Date, intersect(contains("MinES"),contains("Weights")))
    a <- a %>% select(Date,contains("BTC"),contains("LTC"),contains("XRP"),contains("XMR"),contains("ETH"),contains("BCH"),contains("EOS"))
    a <- melt(a, id.var = "Date")
    a <- a %>% separate(variable, c("Portfolio","Dependence","BorA","VineType","Opt","EorW","Asset"), sep = "_")
    a <- a %>% filter(Opt != "Starr")
    a <- a %>% mutate(Opt = replace(Opt, Opt == "MinSD", "MinVar"))
    b <- ggplot(a, aes(x = Date, y = value)) + geom_line() +facet_wrap(~Asset+VineType, scales = "free", nrow = 4) + theme_wsj()+  scale_x_date(date_labels = "%y")+ theme(panel.spacing = unit(2, "lines")) + theme(panel.spacing = unit(2, "lines"))  + theme(legend.position = "bottom",axis.text.x = element_text(angle=45, hjust = 1))+ labs(color ="")
    ggplotly(b, height = 1500, width = 2000)
  })
  
  output$Fixedsharpeweight <- renderPlotly({
    a <- EvolutionAndWeights[["Fixed"]] %>% select(Date, intersect(contains("Sharpe"),contains("Weights")))
    a <- a %>% select(Date,contains("BTC"),contains("LTC"),contains("XRP"),contains("XMR"),contains("ETH"),contains("BCH"),contains("EOS"))
    a <- melt(a, id.var = "Date")
    a <- a %>% separate(variable, c("Portfolio","Dependence","BorA","VineType","Opt","EorW","Asset"), sep = "_")
    a <- a %>% filter(Opt != "Starr")
    a <- a %>% mutate(Opt = replace(Opt, Opt == "MinSD", "MinVar"))
    b <- ggplot(a, aes(x = Date, y = value)) + geom_line() +facet_wrap(~Asset+VineType, scales = "free", nrow = 4) + theme_wsj()+  scale_x_date(date_labels = "%y")+ theme(panel.spacing = unit(2, "lines")) + theme(panel.spacing = unit(2, "lines"))  + theme(legend.position = "bottom",axis.text.x = element_text(angle=45, hjust = 1))+ labs(color ="")
    ggplotly(b, height = 1500, width = 2000)
  })
  
  output$Fixedmaxmeanweight <- renderPlotly({
    
    a <- EvolutionAndWeights[["Fixed"]] %>% select(Date, intersect(contains("MaxMean"),contains("Weights")))
    a <- a %>% select(Date,contains("BTC"),contains("LTC"),contains("XRP"),contains("XMR"),contains("ETH"),contains("BCH"),contains("EOS"))
    a <- melt(a, id.var = "Date")
    a <- a %>% separate(variable, c("Portfolio","Dependence","BorA","VineType","Opt","EorW","Asset"), sep = "_")
    a <- a %>% filter(Opt != "Starr")
    a <- a %>% mutate(Opt = replace(Opt, Opt == "MinSD", "MinVar"))
    b <- ggplot(a, aes(x = Date, y = value)) + geom_line() +facet_wrap(~Asset+VineType, scales = "free", nrow = 4) + theme_wsj()+  scale_x_date(date_labels = "%y")+ theme(panel.spacing = unit(2, "lines")) + theme(panel.spacing = unit(2, "lines"))  + theme(legend.position = "bottom",axis.text.x = element_text(angle=45, hjust = 1))+ labs(color ="")
    ggplotly(b, height = 1500, width = 2000)
  })
  
  
  
  ##### CopulaWise ##### 
  
  output$FixedCONVweight <- renderPlotly({
    
    
    a <- EvolutionAndWeights[["Fixed"]] %>% select(Date, intersect(contains("Conv"),contains("Weights")))
    a <- a %>% select(Date,contains("BTC"),contains("LTC"),contains("XRP"),contains("XMR"),contains("ETH"),contains("BCH"),contains("EOS"))
    a <- melt(a, id.var = "Date")
    a <- a %>% separate(variable, c("Portfolio","Dependence","BorA","VineType","Opt","EorW","Asset"), sep = "_")
    a <- a %>% filter(Opt != "Starr")
    a <- a %>% mutate(Opt = replace(Opt, Opt == "MinSD", "MinVar"))
    b <- ggplot(a, aes(x = Date, y = value)) + geom_line() +facet_wrap(~Asset+Opt, scales = "free", nrow = 4) + theme_wsj()+  scale_x_date(date_labels = "%y")+ theme(panel.spacing = unit(2, "lines")) + theme(panel.spacing = unit(2, "lines"))  + theme(legend.position = "bottom",axis.text.x = element_text(angle=45, hjust = 1))+ labs(color ="")
    ggplotly(b, height = 1500, width = 2000)
    
  })
  
  
  output$FixedPRweight <- renderPlotly({
    
    a <- EvolutionAndWeights[["Fixed"]] %>% select(Date, intersect(contains("P-R"),contains("Weights")))
    a <- a %>% select(Date,contains("BTC"),contains("LTC"),contains("XRP"),contains("XMR"),contains("ETH"),contains("BCH"),contains("EOS"))
    a <- melt(a, id.var = "Date")
    a <- a %>% separate(variable, c("Portfolio","Dependence","BorA","VineType","Opt","EorW","Asset"), sep = "_")
    a <- a %>% filter(Opt != "Starr")
    a <- a %>% mutate(Opt = replace(Opt, Opt == "MinSD", "MinVar"))
    b <- ggplot(a, aes(x = Date, y = value)) + geom_line() +facet_wrap(~Asset+Opt, scales = "free", nrow = 4) + theme_wsj()+  scale_x_date(date_labels = "%y")+ theme(panel.spacing = unit(2, "lines")) + theme(panel.spacing = unit(2, "lines"))  + theme(legend.position = "bottom",axis.text.x = element_text(angle=45, hjust = 1))+ labs(color ="")
    ggplotly(b, height = 1500, width = 2000)
    
  })
  
  output$FixedPCweight <- renderPlotly({
    
    a <- EvolutionAndWeights[["Fixed"]] %>% select(Date, intersect(contains("P-C"),contains("Weights")))
    a <- a %>% select(Date,contains("BTC"),contains("LTC"),contains("XRP"),contains("XMR"),contains("ETH"),contains("BCH"),contains("EOS"))
    a <- melt(a, id.var = "Date")
    a <- a %>% separate(variable, c("Portfolio","Dependence","BorA","VineType","Opt","EorW","Asset"), sep = "_")
    a <- a %>% filter(Opt != "Starr")
    a <- a %>% mutate(Opt = replace(Opt, Opt == "MinSD", "MinVar"))
    b <- ggplot(a, aes(x = Date, y = value)) + geom_line() +facet_wrap(~Asset+Opt, scales = "free", nrow = 4) + theme_wsj()+  scale_x_date(date_labels = "%y")+ theme(panel.spacing = unit(2, "lines")) + theme(panel.spacing = unit(2, "lines"))  + theme(legend.position = "bottom",axis.text.x = element_text(angle=45, hjust = 1))+ labs(color ="")
    ggplotly(b, height = 1500, width = 2000)
    
  })
  
  output$FixedERweight <- renderPlotly({
    
    
    a <- EvolutionAndWeights[["Fixed"]] %>% select(Date, intersect(contains("E-R"),contains("Weights")))
    a <- a %>% select(Date,contains("BTC"),contains("LTC"),contains("XRP"),contains("XMR"),contains("ETH"),contains("BCH"),contains("EOS"))
    a <- melt(a, id.var = "Date")
    a <- a %>% separate(variable, c("Portfolio","Dependence","BorA","VineType","Opt","EorW","Asset"), sep = "_")
    a <- a %>% filter(Opt != "Starr")
    a <- a %>% mutate(Opt = replace(Opt, Opt == "MinSD", "MinVar"))
    b <- ggplot(a, aes(x = Date, y = value)) + geom_line() +facet_wrap(~Asset+Opt, scales = "free", nrow = 4) + theme_wsj()+  scale_x_date(date_labels = "%y")+ theme(panel.spacing = unit(2, "lines")) + theme(panel.spacing = unit(2, "lines"))  + theme(legend.position = "bottom",axis.text.x = element_text(angle=45, hjust = 1))+ labs(color ="")
    ggplotly(b, height = 1500, width = 2000)
    
  })
  
  output$FixedECweight <- renderPlotly({
    
    a <- EvolutionAndWeights[["Fixed"]] %>% select(Date, intersect(contains("E-C"),contains("Weights")))
    a <- a %>% select(Date,contains("BTC"),contains("LTC"),contains("XRP"),contains("XMR"),contains("ETH"),contains("BCH"),contains("EOS"))
    a <- melt(a, id.var = "Date")
    a <- a %>% separate(variable, c("Portfolio","Dependence","BorA","VineType","Opt","EorW","Asset"), sep = "_")
    a <- a %>% filter(Opt != "Starr")
    a <- a %>% mutate(Opt = replace(Opt, Opt == "MinSD", "MinVar"))
    b <- ggplot(a, aes(x = Date, y = value)) + geom_line() +facet_wrap(~Asset+Opt, scales = "free", nrow = 4) + theme_wsj()+  scale_x_date(date_labels = "%y")+ theme(panel.spacing = unit(2, "lines")) + theme(panel.spacing = unit(2, "lines"))  + theme(legend.position = "bottom",axis.text.x = element_text(angle=45, hjust = 1))+ labs(color ="")
    ggplotly(b, height = 1500, width = 2000)
    
  })
  
  
  ##############################################       Weights      ############################################    
  
  
  #########################################              Commo Indices EVOLUTION           ################################################  
  output$Commonaiveplot <- renderPlotly({
    
    a <- EvolutionAndWeights[["Commo"]] %>% select(Date,intersect(contains("Naive"),contains("Evolution")))%>% filter(between(Date,as.Date(input$CIDates[1]),as.Date(input$CIDates[2]))) %>%
      filter(between(Date,as.Date(input$CDates[1]),as.Date(input$CDates[2])))
    
    a <- Inv(a) 
    #a <- Evolution %>% select(Date,contains("Naive")) %>% filter(between(Date,as.Date(input$CDates[1]),as.Date(input$CDates[2])))
    a <- melt(a, id.var = "Date")
    b <- a %>% separate(variable, c("Portfolio","Dependence","BorA","VineType","Opt","EorW","Asset"), sep = "_")
    b <- b %>% mutate(Opt = replace(Opt, Opt == "MinSD", "MinVar"))
    c <- ggplot(b, aes(Date,value, color = BorA,  group = BorA)) + geom_line() + facet_wrap(~VineType)+ theme_wsj()+  scale_x_date(date_labels = "%y") + theme(legend.position = "bottom",axis.text.x = element_text(angle=45, hjust = 1))+ labs(color ="")  
    d <- ggplotly(c, height = 600, width = 1000)
  })
  
  output$Commominsdplot <- renderPlotly({
    
    a <- EvolutionAndWeights[["Commo"]] %>% select(Date,intersect(contains("MinSD"),contains("Evolution")))%>% filter(between(Date,as.Date(input$CIDates[1]),as.Date(input$CIDates[2]))) %>%
      filter(between(Date,as.Date(input$CDates[1]),as.Date(input$CDates[2])))
    
    a <- Inv(a) 
    #a <- Evolution %>% select(Date,contains("Naive")) %>% filter(between(Date,as.Date(input$CDates[1]),as.Date(input$CDates[2])))
    a <- melt(a, id.var = "Date")
    b <- a %>% separate(variable, c("Portfolio","Dependence","BorA","VineType","Opt","EorW","Asset"), sep = "_")
    b <- b %>% mutate(Opt = replace(Opt, Opt == "MinSD", "MinVar"))
    c <- ggplot(b, aes(Date,value, color = BorA,  group = BorA)) + geom_line() + facet_wrap(~VineType)+ theme_wsj()+  scale_x_date(date_labels = "%y") + theme(legend.position = "bottom",axis.text.x = element_text(angle=45, hjust = 1))+ labs(color ="")  
    d <- ggplotly(c, height = 600, width = 1000)
  })
  
  output$Commominesplot <- renderPlotly({
    
    a <- EvolutionAndWeights[["Commo"]] %>% select(Date,intersect(contains("MinES"),contains("Evolution")))%>% filter(between(Date,as.Date(input$CIDates[1]),as.Date(input$CIDates[2]))) %>%
      filter(between(Date,as.Date(input$CDates[1]),as.Date(input$CDates[2])))
    
    a <- Inv(a) 
    #a <- Evolution %>% select(Date,contains("Naive")) %>% filter(between(Date,as.Date(input$CDates[1]),as.Date(input$CDates[2])))
    a <- melt(a, id.var = "Date")
    b <- a %>% separate(variable, c("Portfolio","Dependence","BorA","VineType","Opt","EorW","Asset"), sep = "_")
    b <- b %>% mutate(Opt = replace(Opt, Opt == "MinSD", "MinVar"))
    c <- ggplot(b, aes(Date,value, color = BorA,  group = BorA)) + geom_line() + facet_wrap(~VineType)+ theme_wsj()+  scale_x_date(date_labels = "%y") + theme(legend.position = "bottom",axis.text.x = element_text(angle=45, hjust = 1))+ labs(color ="")  
    d <- ggplotly(c, height = 600, width = 1000)
  })
  
  output$Commosharpeplot <- renderPlotly({
    
    a <- EvolutionAndWeights[["Commo"]] %>% select(Date,intersect(contains("Sharpe"),contains("Evolution")))%>% filter(between(Date,as.Date(input$CIDates[1]),as.Date(input$CIDates[2]))) %>%
      filter(between(Date,as.Date(input$CDates[1]),as.Date(input$CDates[2])))
    
    a <- Inv(a) 
    #a <- Evolution %>% select(Date,contains("Naive")) %>% filter(between(Date,as.Date(input$CDates[1]),as.Date(input$CDates[2])))
    a <- melt(a, id.var = "Date")
    b <- a %>% separate(variable, c("Portfolio","Dependence","BorA","VineType","Opt","EorW","Asset"), sep = "_")
    b <- b %>% mutate(Opt = replace(Opt, Opt == "MinSD", "MinVar"))
    c <- ggplot(b, aes(Date,value, color = BorA,  group = BorA)) + geom_line() + facet_wrap(~VineType)+ theme_wsj()+  scale_x_date(date_labels = "%y") + theme(legend.position = "bottom",axis.text.x = element_text(angle=45, hjust = 1))+ labs(color ="")  
    d <- ggplotly(c, height = 600, width = 1000)
  })
  
  output$Commomaxmeanplot <- renderPlotly({
    
    a <- EvolutionAndWeights[["Commo"]] %>% select(Date,intersect(contains("MaxMean"),contains("Evolution")))%>% filter(between(Date,as.Date(input$CIDates[1]),as.Date(input$CIDates[2]))) %>%
      filter(between(Date,as.Date(input$CDates[1]),as.Date(input$CDates[2])))
    
    a <- Inv(a) 
    #a <- Evolution %>% select(Date,contains("Naive")) %>% filter(between(Date,as.Date(input$CDates[1]),as.Date(input$CDates[2])))
    a <- melt(a, id.var = "Date")
    b <- a %>% separate(variable, c("Portfolio","Dependence","BorA","VineType","Opt","EorW","Asset"), sep = "_")
    b <- b %>% mutate(Opt = replace(Opt, Opt == "MinSD", "MinVar"))
    c <- ggplot(b, aes(Date,value, color = BorA,  group = BorA)) + geom_line() + facet_wrap(~VineType)+ theme_wsj()+  scale_x_date(date_labels = "%y") + theme(legend.position = "bottom",axis.text.x = element_text(angle=45, hjust = 1))+ labs(color ="")  
    d <- ggplotly(c, height = 600, width = 1000)
  })
  
  ###### Copulawise
  
  
  output$CommoCONV <- renderPlotly({
    
    a <- EvolutionAndWeights[["Commo"]] %>% select(Date,intersect(contains("Conv"),contains("Evolution")))%>% filter(between(Date,as.Date(input$CIDates[1]),as.Date(input$CIDates[2]))) %>%
      filter(between(Date,as.Date(input$CDates[1]),as.Date(input$CDates[2])))
    
    a <- Inv(a) 
    #a <- Evolution %>% select(Date,contains("Naive")) %>% filter(between(Date,as.Date(input$CDates[1]),as.Date(input$CDates[2])))
    a <- melt(a, id.var = "Date")
    b <- a %>% separate(variable, c("Portfolio","Dependence","BorA","VineType","Opt","EorW","Asset"), sep = "_")
    b <- b %>% mutate(Opt = replace(Opt, Opt == "MinSD", "MinVar"))
    c <- ggplot(b, aes(Date,value, color = BorA,  group = BorA)) + geom_line() +facet_wrap(~Opt) + theme_wsj()+  scale_x_date(date_labels = "%y") + theme(legend.position = "bottom",axis.text.x = element_text(angle=45, hjust = 1))+ labs(color ="")  
    d <- ggplotly(c, height = 600, width = 1000)
  })
  
  
  
  output$CommoPR <- renderPlotly({
    
    a <- EvolutionAndWeights[["Commo"]] %>% select(Date,intersect(contains("P-R"),contains("Evolution")))%>% filter(between(Date,as.Date(input$CIDates[1]),as.Date(input$CIDates[2]))) %>%
      filter(between(Date,as.Date(input$CDates[1]),as.Date(input$CDates[2])))
    
    a <- Inv(a) 
    #a <- Evolution %>% select(Date,contains("Naive")) %>% filter(between(Date,as.Date(input$CDates[1]),as.Date(input$CDates[2])))
    a <- melt(a, id.var = "Date")
    b <- a %>% separate(variable, c("Portfolio","Dependence","BorA","VineType","Opt","EorW","Asset"), sep = "_")
    b <- b %>% mutate(Opt = replace(Opt, Opt == "MinSD", "MinVar"))
    c <- ggplot(b, aes(Date,value, color = BorA,  group = BorA)) + geom_line() +facet_wrap(~Opt) + theme_wsj()+  scale_x_date(date_labels = "%y") + theme(legend.position = "bottom",axis.text.x = element_text(angle=45, hjust = 1))+ labs(color ="")  
    d <- ggplotly(c, height = 600, width = 1000)
  })
  
  
  
  output$CommoPC <- renderPlotly({
    
    a <- EvolutionAndWeights[["Commo"]] %>% select(Date,intersect(contains("P-C"),contains("Evolution")))%>% filter(between(Date,as.Date(input$CIDates[1]),as.Date(input$CIDates[2]))) %>%
      filter(between(Date,as.Date(input$CDates[1]),as.Date(input$CDates[2])))
    
    a <- Inv(a) 
    #a <- Evolution %>% select(Date,contains("Naive")) %>% filter(between(Date,as.Date(input$CDates[1]),as.Date(input$CDates[2])))
    a <- melt(a, id.var = "Date")
    b <- a %>% separate(variable, c("Portfolio","Dependence","BorA","VineType","Opt","EorW","Asset"), sep = "_")
    b <- b %>% mutate(Opt = replace(Opt, Opt == "MinSD", "MinVar"))
    c <- ggplot(b, aes(Date,value, color = BorA,  group = BorA)) + geom_line() +facet_wrap(~Opt) + theme_wsj()+  scale_x_date(date_labels = "%y") + theme(legend.position = "bottom",axis.text.x = element_text(angle=45, hjust = 1))+ labs(color ="")  
    d <- ggplotly(c, height = 600, width = 1000)
  })
  
  
  
  output$CommoER <- renderPlotly({
    
    a <- EvolutionAndWeights[["Commo"]] %>% select(Date,intersect(contains("E-R"),contains("Evolution")))%>% filter(between(Date,as.Date(input$CIDates[1]),as.Date(input$CIDates[2]))) %>%
      filter(between(Date,as.Date(input$CDates[1]),as.Date(input$CDates[2])))
    
    a <- Inv(a) 
    #a <- Evolution %>% select(Date,contains("Naive")) %>% filter(between(Date,as.Date(input$CDates[1]),as.Date(input$CDates[2])))
    a <- melt(a, id.var = "Date")
    b <- a %>% separate(variable, c("Portfolio","Dependence","BorA","VineType","Opt","EorW","Asset"), sep = "_")
    b <- b %>% mutate(Opt = replace(Opt, Opt == "MinSD", "MinVar"))
    c <- ggplot(b, aes(Date,value, color = BorA,  group = BorA)) + geom_line() +facet_wrap(~Opt) + theme_wsj()+  scale_x_date(date_labels = "%y") + theme(legend.position = "bottom",axis.text.x = element_text(angle=45, hjust = 1))+ labs(color ="")  
    d <- ggplotly(c, height = 600, width = 1000)
  })
  
  
  
  output$CommoEC <- renderPlotly({
    
    a <- EvolutionAndWeights[["Commo"]] %>% select(Date,intersect(contains("E-C"),contains("Evolution")))%>% filter(between(Date,as.Date(input$CIDates[1]),as.Date(input$CIDates[2]))) %>%
      filter(between(Date,as.Date(input$CDates[1]),as.Date(input$CDates[2])))
    
    a <- Inv(a) 
    #a <- Evolution %>% select(Date,contains("Naive")) %>% filter(between(Date,as.Date(input$CDates[1]),as.Date(input$CDates[2])))
    a <- melt(a, id.var = "Date")
    b <- a %>% separate(variable, c("Portfolio","Dependence","BorA","VineType","Opt","EorW","Asset"), sep = "_")
    b <- b %>% mutate(Opt = replace(Opt, Opt == "MinSD", "MinVar"))
    c <- ggplot(b, aes(Date,value, color = BorA,  group = BorA)) + geom_line() +facet_wrap(~Opt) + theme_wsj()+  scale_x_date(date_labels = "%y") + theme(legend.position = "bottom",axis.text.x = element_text(angle=45, hjust = 1))+ labs(color ="")  
    d <- ggplotly(c, height = 600, width = 1000)
  })
  
  
  
  ##############################################   Mean,SD,and CAGR   ############################################
  output$CommoSelection <- renderDataTable({
    d1 <- PerformanceM[["Alltogether"]] %>% filter(Portfolio == "SP500")
    d2 <- PerformanceM[["Alltogether"]] %>% filter(Portfolio == "Commo")   %>% filter(VineType %in% input$CC1) %>% 
      filter(Opt %in% input$CO1) 
    
    d3 <- rbind(d1[1,],d2) 
    d3[,c(6:9)] <- d3[,c(6:9)] %>% round(digits = 4)
    datatable(d3)
  })
  
  output$CommoSelection2 <- renderDataTable({
    d1 <- PerformanceM[["Alltogether"]] %>% filter(Portfolio == "SP500")
    d2 <- PerformanceM[["Alltogether"]] %>% filter(Portfolio == "Commo")  %>% filter(Opt %in% input$CO1) 
    
    d3 <- rbind(d1[1,],d2) 
    d3[,c(6:9)] <- d3[,c(6:9)] %>% round(digits = 4)
    datatable(d3)
  })
  
  output$CommoSelection3 <- renderDataTable({
    d1 <- PerformanceM[["Alltogether"]] %>% filter(Portfolio == "SP500")
    d2 <- PerformanceM[["Alltogether"]] %>% filter(Portfolio == "Commo") %>%   filter(VineType %in% input$CC1)
    
    d3 <- rbind(d1[1,],d2) 
    d3[,c(6:9)] <- d3[,c(6:9)] %>% round(digits = 4)
    datatable(d3)
  })
  
  ##############################################   Mean,SD,and CAGR   ############################################  
  
  
  
  ##############################################       Weights      ############################################    
  
  ##### OptimizationWise #####
  
  output$Commonaiveweight <- renderPlotly({
    
    a <- EvolutionAndWeights[["Commo"]] %>% select(Date, intersect(contains("Naive"),contains("Weights")))
    a <- a %>% select(Date,contains("BTC"),contains("LTC"),contains("XRP"),contains("XMR"),contains("ETH"),contains("BCH"),contains("EOS"))
    a <- melt(a, id.var = "Date")
    a <- a %>% separate(variable, c("Portfolio","Dependence","BorA","VineType","Opt","EorW","Asset"), sep = "_")
    a <- a %>% filter(Opt != "Starr")
    a <- a %>% mutate(Opt = replace(Opt, Opt == "MinSD", "MinVar"))
    b <- ggplot(a, aes(x = Date, y = value)) + geom_line() +facet_wrap(~Asset+VineType, scales = "free", nrow = 4) + theme_wsj()+  scale_x_date(date_labels = "%y")+ theme(panel.spacing = unit(2, "lines")) 
    ggplotly(b, height = 1500, width = 2000)
  })
  
  
  output$Commominsdweight <- renderPlotly({
    
    a <- EvolutionAndWeights[["Commo"]] %>% select(Date, intersect(contains("MinSD"),contains("Weights")))
    a <- a %>% select(Date,contains("BTC"),contains("LTC"),contains("XRP"),contains("XMR"),contains("ETH"),contains("BCH"),contains("EOS"))
    a <- melt(a, id.var = "Date")
    a <- a %>% separate(variable, c("Portfolio","Dependence","BorA","VineType","Opt","EorW","Asset"), sep = "_")
    a <- a %>% filter(Opt != "Starr")
    a <- a %>% mutate(Opt = replace(Opt, Opt == "MinSD", "MinVar"))
    b <- ggplot(a, aes(x = Date, y = value)) + geom_line() +facet_wrap(~Asset+VineType, scales = "free", nrow = 4) + theme_wsj()+  scale_x_date(date_labels = "%y")+ theme(panel.spacing = unit(2, "lines")) 
    ggplotly(b, height = 1500, width = 2000)
  })
  
  output$Commominesweight <- renderPlotly({
    
    a <- EvolutionAndWeights[["Commo"]] %>% select(Date, intersect(contains("MinES"),contains("Weights")))
    a <- a %>% select(Date,contains("BTC"),contains("LTC"),contains("XRP"),contains("XMR"),contains("ETH"),contains("BCH"),contains("EOS"))
    a <- melt(a, id.var = "Date")
    a <- a %>% separate(variable, c("Portfolio","Dependence","BorA","VineType","Opt","EorW","Asset"), sep = "_")
    a <- a %>% filter(Opt != "Starr")
    a <- a %>% mutate(Opt = replace(Opt, Opt == "MinSD", "MinVar"))
    b <- ggplot(a, aes(x = Date, y = value)) + geom_line() +facet_wrap(~Asset+VineType, scales = "free", nrow = 4) + theme_wsj()+  scale_x_date(date_labels = "%y")+ theme(panel.spacing = unit(2, "lines")) 
    ggplotly(b, height = 1500, width = 2000)
  })
  
  output$Commosharpeweight <- renderPlotly({
    a <- EvolutionAndWeights[["Commo"]] %>% select(Date, intersect(contains("Sharpe"),contains("Weights")))
    a <- a %>% select(Date,contains("BTC"),contains("LTC"),contains("XRP"),contains("XMR"),contains("ETH"),contains("BCH"),contains("EOS"))
    a <- melt(a, id.var = "Date")
    a <- a %>% separate(variable, c("Portfolio","Dependence","BorA","VineType","Opt","EorW","Asset"), sep = "_")
    a <- a %>% filter(Opt != "Starr")
    a <- a %>% mutate(Opt = replace(Opt, Opt == "MinSD", "MinVar"))
    b <- ggplot(a, aes(x = Date, y = value)) + geom_line() +facet_wrap(~Asset+VineType, scales = "free", nrow = 4) + theme_wsj()+  scale_x_date(date_labels = "%y")+ theme(panel.spacing = unit(2, "lines")) 
    ggplotly(b, height = 1500, width = 2000)
  })
  
  output$Commomaxmeanweight <- renderPlotly({
    
    a <- EvolutionAndWeights[["Commo"]] %>% select(Date, intersect(contains("MaxMean"),contains("Weights")))
    a <- a %>% select(Date,contains("BTC"),contains("LTC"),contains("XRP"),contains("XMR"),contains("ETH"),contains("BCH"),contains("EOS"))
    a <- melt(a, id.var = "Date")
    a <- a %>% separate(variable, c("Portfolio","Dependence","BorA","VineType","Opt","EorW","Asset"), sep = "_")
    a <- a %>% filter(Opt != "Starr")
    a <- a %>% mutate(Opt = replace(Opt, Opt == "MinSD", "MinVar"))
    b <- ggplot(a, aes(x = Date, y = value)) + geom_line() +facet_wrap(~Asset+VineType, scales = "free", nrow = 4) + theme_wsj()+  scale_x_date(date_labels = "%y")+ theme(panel.spacing = unit(2, "lines")) 
    ggplotly(b, height = 1500, width = 2000)
  })
  
  
  
  ##### CopulaWise ##### 
  
  output$CommoCONVweight <- renderPlotly({
    
    
    a <- EvolutionAndWeights[["Commo"]] %>% select(Date, intersect(contains("Conv"),contains("Weights")))
    a <- a %>% select(Date,contains("BTC"),contains("LTC"),contains("XRP"),contains("XMR"),contains("ETH"),contains("BCH"),contains("EOS"))
    a <- melt(a, id.var = "Date")
    a <- a %>% separate(variable, c("Portfolio","Dependence","BorA","VineType","Opt","EorW","Asset"), sep = "_")
    a <- a %>% filter(Opt != "Starr")
    a <- a %>% mutate(Opt = replace(Opt, Opt == "MinSD", "MinVar"))
    b <- ggplot(a, aes(x = Date, y = value)) + geom_line() +facet_wrap(~Asset+Opt, scales = "free", nrow = 4) + theme_wsj()+  scale_x_date(date_labels = "%y")+ theme(panel.spacing = unit(2, "lines"))  + theme(legend.position = "bottom",axis.text.x = element_text(angle=45, hjust = 1))+ labs(color ="")
    ggplotly(b, height = 1500, width = 2000)
    
  })
  
  
  output$CommoPRweight <- renderPlotly({
    
    a <- EvolutionAndWeights[["Commo"]] %>% select(Date, intersect(contains("P-R"),contains("Weights")))
    a <- a %>% select(Date,contains("BTC"),contains("LTC"),contains("XRP"),contains("XMR"),contains("ETH"),contains("BCH"),contains("EOS"))
    a <- melt(a, id.var = "Date")
    a <- a %>% separate(variable, c("Portfolio","Dependence","BorA","VineType","Opt","EorW","Asset"), sep = "_")
    a <- a %>% filter(Opt != "Starr")
    a <- a %>% mutate(Opt = replace(Opt, Opt == "MinSD", "MinVar"))
    b <- ggplot(a, aes(x = Date, y = value)) + geom_line() +facet_wrap(~Asset+Opt, scales = "free", nrow = 4) + theme_wsj()+  scale_x_date(date_labels = "%y")+ theme(panel.spacing = unit(2, "lines"))  + theme(legend.position = "bottom",axis.text.x = element_text(angle=45, hjust = 1))+ labs(color ="")
    ggplotly(b, height = 1500, width = 2000)
    
  })
  
  output$CommoPCweight <- renderPlotly({
    
    a <- EvolutionAndWeights[["Commo"]] %>% select(Date, intersect(contains("P-C"),contains("Weights")))
    a <- a %>% select(Date,contains("BTC"),contains("LTC"),contains("XRP"),contains("XMR"),contains("ETH"),contains("BCH"),contains("EOS"))
    a <- melt(a, id.var = "Date")
    a <- a %>% separate(variable, c("Portfolio","Dependence","BorA","VineType","Opt","EorW","Asset"), sep = "_")
    a <- a %>% filter(Opt != "Starr")
    a <- a %>% mutate(Opt = replace(Opt, Opt == "MinSD", "MinVar"))
    b <- ggplot(a, aes(x = Date, y = value)) + geom_line() +facet_wrap(~Asset+Opt, scales = "free", nrow = 4) + theme_wsj()+  scale_x_date(date_labels = "%y")+ theme(panel.spacing = unit(2, "lines"))  + theme(legend.position = "bottom",axis.text.x = element_text(angle=45, hjust = 1))+ labs(color ="")
    ggplotly(b, height = 1500, width = 2000)
    
  })
  
  output$CommoERweight <- renderPlotly({
    
    
    a <- EvolutionAndWeights[["Commo"]] %>% select(Date, intersect(contains("E-R"),contains("Weights")))
    a <- a %>% select(Date,contains("BTC"),contains("LTC"),contains("XRP"),contains("XMR"),contains("ETH"),contains("BCH"),contains("EOS"))
    a <- melt(a, id.var = "Date")
    a <- a %>% separate(variable, c("Portfolio","Dependence","BorA","VineType","Opt","EorW","Asset"), sep = "_")
    a <- a %>% filter(Opt != "Starr")
    a <- a %>% mutate(Opt = replace(Opt, Opt == "MinSD", "MinVar"))
    b <- ggplot(a, aes(x = Date, y = value)) + geom_line() +facet_wrap(~Asset+Opt, scales = "free", nrow = 4) + theme_wsj()+  scale_x_date(date_labels = "%y")+ theme(panel.spacing = unit(2, "lines"))  + theme(legend.position = "bottom",axis.text.x = element_text(angle=45, hjust = 1))+ labs(color ="")
    ggplotly(b, height = 1500, width = 2000)
    
  })
  
  output$CommoECweight <- renderPlotly({
    
    a <- EvolutionAndWeights[["Commo"]] %>% select(Date, intersect(contains("E-C"),contains("Weights")))
    a <- a %>% select(Date,contains("BTC"),contains("LTC"),contains("XRP"),contains("XMR"),contains("ETH"),contains("BCH"),contains("EOS"))
    a <- melt(a, id.var = "Date")
    a <- a %>% separate(variable, c("Portfolio","Dependence","BorA","VineType","Opt","EorW","Asset"), sep = "_")
    a <- a %>% filter(Opt != "Starr")
    a <- a %>% mutate(Opt = replace(Opt, Opt == "MinSD", "MinVar"))
    b <- ggplot(a, aes(x = Date, y = value)) + geom_line() +facet_wrap(~Asset+Opt, scales = "free", nrow = 4) + theme_wsj()+  scale_x_date(date_labels = "%y")+ theme(panel.spacing = unit(2, "lines"))  + theme(legend.position = "bottom",axis.text.x = element_text(angle=45, hjust = 1))+ labs(color ="")
    ggplotly(b, height = 1500, width = 2000)
    
  })
  
  
  ##############################################       Weights      ############################################    
  
  #########################################              Sector Indices EVOLUTION           ################################################  
  output$Sectornaiveplot <- renderPlotly({
    
    a <- EvolutionAndWeights[["Sector"]] %>% select(Date,intersect(contains("Naive"),contains("Evolution")))%>% filter(between(Date,as.Date(input$SIDates[1]),as.Date(input$SIDates[2]))) %>%
      filter(between(Date,as.Date(input$SIDates[1]),as.Date(input$SIDates[2])))
    
    a <- Inv(a) 
    #a <- Evolution %>% select(Date,contains("Naive")) %>% filter(between(Date,as.Date(input$SIDates[1]),as.Date(input$SIDates[2])))
    a <- melt(a, id.var = "Date")
    b <- a %>% separate(variable, c("Portfolio","Dependence","BorA","VineType","Opt","EorW","Asset"), sep = "_")
    b <- b %>% mutate(Opt = replace(Opt, Opt == "MinSD", "MinVar"))
    c <- ggplot(b, aes(Date,value, color = BorA,  group = BorA)) + geom_line() + facet_wrap(~VineType)+ theme_wsj()+  scale_x_date(date_labels = "%y") + theme(legend.position = "bottom",axis.text.x = element_text(angle=45, hjust = 1))+ labs(color ="")  
    d <- ggplotly(c, height = 600, width = 1000)
  })
  
  output$Sectorminsdplot <- renderPlotly({
    
    a <- EvolutionAndWeights[["Sector"]] %>% select(Date,intersect(contains("MinSD"),contains("Evolution")))%>% filter(between(Date,as.Date(input$SIDates[1]),as.Date(input$SIDates[2]))) %>%
      filter(between(Date,as.Date(input$SIDates[1]),as.Date(input$SIDates[2])))
    
    a <- Inv(a) 
    #a <- Evolution %>% select(Date,contains("Naive")) %>% filter(between(Date,as.Date(input$SIDates[1]),as.Date(input$SIDates[2])))
    a <- melt(a, id.var = "Date")
    b <- a %>% separate(variable, c("Portfolio","Dependence","BorA","VineType","Opt","EorW","Asset"), sep = "_")
    b <- b %>% mutate(Opt = replace(Opt, Opt == "MinSD", "MinVar"))
    c <- ggplot(b, aes(Date,value, color = BorA,  group = BorA)) + geom_line() + facet_wrap(~VineType)+ theme_wsj()+  scale_x_date(date_labels = "%y") + theme(legend.position = "bottom",axis.text.x = element_text(angle=45, hjust = 1))+ labs(color ="")  
    d <- ggplotly(c, height = 600, width = 1000)
  })
  
  output$Sectorminesplot <- renderPlotly({
    
    a <- EvolutionAndWeights[["Sector"]] %>% select(Date,intersect(contains("MinES"),contains("Evolution")))%>% filter(between(Date,as.Date(input$SIDates[1]),as.Date(input$SIDates[2]))) %>%
      filter(between(Date,as.Date(input$SIDates[1]),as.Date(input$SIDates[2])))
    
    a <- Inv(a) 
    #a <- Evolution %>% select(Date,contains("Naive")) %>% filter(between(Date,as.Date(input$SIDates[1]),as.Date(input$SIDates[2])))
    a <- melt(a, id.var = "Date")
    b <- a %>% separate(variable, c("Portfolio","Dependence","BorA","VineType","Opt","EorW","Asset"), sep = "_")
    b <- b %>% mutate(Opt = replace(Opt, Opt == "MinSD", "MinVar"))
    c <- ggplot(b, aes(Date,value, color = BorA,  group = BorA)) + geom_line() + facet_wrap(~VineType)+ theme_wsj()+  scale_x_date(date_labels = "%y") + theme(legend.position = "bottom",axis.text.x = element_text(angle=45, hjust = 1))+ labs(color ="")  
    d <- ggplotly(c, height = 600, width = 1000)
  })
  
  output$Sectorsharpeplot <- renderPlotly({
    
    a <- EvolutionAndWeights[["Sector"]] %>% select(Date,intersect(contains("Sharpe"),contains("Evolution")))%>% filter(between(Date,as.Date(input$SIDates[1]),as.Date(input$SIDates[2]))) %>%
      filter(between(Date,as.Date(input$SIDates[1]),as.Date(input$SIDates[2])))
    
    a <- Inv(a) 
    #a <- Evolution %>% select(Date,contains("Naive")) %>% filter(between(Date,as.Date(input$SIDates[1]),as.Date(input$SIDates[2])))
    a <- melt(a, id.var = "Date")
    b <- a %>% separate(variable, c("Portfolio","Dependence","BorA","VineType","Opt","EorW","Asset"), sep = "_")
    b <- b %>% mutate(Opt = replace(Opt, Opt == "MinSD", "MinVar"))
    c <- ggplot(b, aes(Date,value, color = BorA,  group = BorA)) + geom_line() + facet_wrap(~VineType)+ theme_wsj()+  scale_x_date(date_labels = "%y") + theme(legend.position = "bottom",axis.text.x = element_text(angle=45, hjust = 1))+ labs(color ="")  
    d <- ggplotly(c, height = 600, width = 1000)
  })
  
  output$Sectormaxmeanplot <- renderPlotly({
    
    a <- EvolutionAndWeights[["Sector"]] %>% select(Date,intersect(contains("MaxMean"),contains("Evolution")))%>% filter(between(Date,as.Date(input$SIDates[1]),as.Date(input$SIDates[2]))) %>%
      filter(between(Date,as.Date(input$SIDates[1]),as.Date(input$SIDates[2])))
    
    a <- Inv(a) 
    #a <- Evolution %>% select(Date,contains("Naive")) %>% filter(between(Date,as.Date(input$SIDates[1]),as.Date(input$SIDates[2])))
    a <- melt(a, id.var = "Date")
    b <- a %>% separate(variable, c("Portfolio","Dependence","BorA","VineType","Opt","EorW","Asset"), sep = "_")
    b <- b %>% mutate(Opt = replace(Opt, Opt == "MinSD", "MinVar"))
    c <- ggplot(b, aes(Date,value, color = BorA,  group = BorA)) + geom_line() + facet_wrap(~VineType)+ theme_wsj()+  scale_x_date(date_labels = "%y") + theme(legend.position = "bottom",axis.text.x = element_text(angle=45, hjust = 1))+ labs(color ="")  
    d <- ggplotly(c, height = 600, width = 1000)
  })
  
  ###### Copulawise
  
  
  output$SectorCONV <- renderPlotly({
    
    a <- EvolutionAndWeights[["Sector"]] %>% select(Date,intersect(contains("Conv"),contains("Evolution")))%>% filter(between(Date,as.Date(input$SIDates[1]),as.Date(input$SIDates[2]))) %>%
      filter(between(Date,as.Date(input$SIDates[1]),as.Date(input$SIDates[2])))
    
    a <- Inv(a) 
    #a <- Evolution %>% select(Date,contains("Naive")) %>% filter(between(Date,as.Date(input$SIDates[1]),as.Date(input$SIDates[2])))
    a <- melt(a, id.var = "Date")
    b <- a %>% separate(variable, c("Portfolio","Dependence","BorA","VineType","Opt","EorW","Asset"), sep = "_")
    b <- b %>% mutate(Opt = replace(Opt, Opt == "MinSD", "MinVar"))
    c <- ggplot(b, aes(Date,value, color = BorA,  group = BorA)) + geom_line() +facet_wrap(~Opt) + theme_wsj()+  scale_x_date(date_labels = "%y") + theme(legend.position = "bottom",axis.text.x = element_text(angle=45, hjust = 1))+ labs(color ="")  
    d <- ggplotly(c, height = 600, width = 1000)
  })
  
  
  
  output$SectorPR <- renderPlotly({
    
    a <- EvolutionAndWeights[["Sector"]] %>% select(Date,intersect(contains("P-R"),contains("Evolution")))%>% filter(between(Date,as.Date(input$SIDates[1]),as.Date(input$SIDates[2]))) %>%
      filter(between(Date,as.Date(input$SIDates[1]),as.Date(input$SIDates[2])))
    
    a <- Inv(a) 
    #a <- Evolution %>% select(Date,contains("Naive")) %>% filter(between(Date,as.Date(input$SIDates[1]),as.Date(input$SIDates[2])))
    a <- melt(a, id.var = "Date")
    b <- a %>% separate(variable, c("Portfolio","Dependence","BorA","VineType","Opt","EorW","Asset"), sep = "_")
    b <- b %>% mutate(Opt = replace(Opt, Opt == "MinSD", "MinVar"))
    c <- ggplot(b, aes(Date,value, color = BorA,  group = BorA)) + geom_line() +facet_wrap(~Opt) + theme_wsj()+  scale_x_date(date_labels = "%y") + theme(legend.position = "bottom",axis.text.x = element_text(angle=45, hjust = 1))+ labs(color ="")  
    d <- ggplotly(c, height = 600, width = 1000)
  })
  
  
  
  output$SectorPC <- renderPlotly({
    
    a <- EvolutionAndWeights[["Sector"]] %>% select(Date,intersect(contains("P-C"),contains("Evolution")))%>% filter(between(Date,as.Date(input$SIDates[1]),as.Date(input$SIDates[2]))) %>%
      filter(between(Date,as.Date(input$SIDates[1]),as.Date(input$SIDates[2])))
    
    a <- Inv(a) 
    #a <- Evolution %>% select(Date,contains("Naive")) %>% filter(between(Date,as.Date(input$SIDates[1]),as.Date(input$SIDates[2])))
    a <- melt(a, id.var = "Date")
    b <- a %>% separate(variable, c("Portfolio","Dependence","BorA","VineType","Opt","EorW","Asset"), sep = "_")
    b <- b %>% mutate(Opt = replace(Opt, Opt == "MinSD", "MinVar"))
    c <- ggplot(b, aes(Date,value, color = BorA,  group = BorA)) + geom_line() +facet_wrap(~Opt) + theme_wsj()+  scale_x_date(date_labels = "%y") + theme(legend.position = "bottom",axis.text.x = element_text(angle=45, hjust = 1))+ labs(color ="")  
    d <- ggplotly(c, height = 600, width = 1000)
  })
  
  
  
  output$SectorER <- renderPlotly({
    
    a <- EvolutionAndWeights[["Sector"]] %>% select(Date,intersect(contains("E-R"),contains("Evolution")))%>% filter(between(Date,as.Date(input$SIDates[1]),as.Date(input$SIDates[2]))) %>%
      filter(between(Date,as.Date(input$SIDates[1]),as.Date(input$SIDates[2])))
    
    a <- Inv(a) 
    #a <- Evolution %>% select(Date,contains("Naive")) %>% filter(between(Date,as.Date(input$SIDates[1]),as.Date(input$SIDates[2])))
    a <- melt(a, id.var = "Date")
    b <- a %>% separate(variable, c("Portfolio","Dependence","BorA","VineType","Opt","EorW","Asset"), sep = "_")
    b <- b %>% mutate(Opt = replace(Opt, Opt == "MinSD", "MinVar"))
    c <- ggplot(b, aes(Date,value, color = BorA,  group = BorA)) + geom_line() +facet_wrap(~Opt) + theme_wsj()+  scale_x_date(date_labels = "%y") + theme(legend.position = "bottom",axis.text.x = element_text(angle=45, hjust = 1))+ labs(color ="")  
    d <- ggplotly(c, height = 600, width = 1000)
  })
  
  
  
  output$SectorEC <- renderPlotly({
    
    a <- EvolutionAndWeights[["Sector"]] %>% select(Date,intersect(contains("E-C"),contains("Evolution")))%>% filter(between(Date,as.Date(input$SIDates[1]),as.Date(input$SIDates[2]))) %>%
      filter(between(Date,as.Date(input$SIDates[1]),as.Date(input$SIDates[2])))
    
    a <- Inv(a) 
    #a <- Evolution %>% select(Date,contains("Naive")) %>% filter(between(Date,as.Date(input$SIDates[1]),as.Date(input$SIDates[2])))
    a <- melt(a, id.var = "Date")
    b <- a %>% separate(variable, c("Portfolio","Dependence","BorA","VineType","Opt","EorW","Asset"), sep = "_")
    b <- b %>% mutate(Opt = replace(Opt, Opt == "MinSD", "MinVar"))
    c <- ggplot(b, aes(Date,value, color = BorA,  group = BorA)) + geom_line() +facet_wrap(~Opt) + theme_wsj()+  scale_x_date(date_labels = "%y") + theme(legend.position = "bottom",axis.text.x = element_text(angle=45, hjust = 1))+ labs(color ="")  
    d <- ggplotly(c, height = 600, width = 1000)
  })
  
  
  
  ##############################################   Mean,SD,and CAGR   ############################################
  output$SectorSelection <- renderDataTable({
    d1 <- PerformanceM[["Alltogether"]] %>% filter(Portfolio == "SP500")
    d2 <- PerformanceM[["Alltogether"]] %>% filter(Portfolio == "Sector")   %>% filter(VineType %in% input$SC1) %>% 
      filter(Opt %in% input$SO1) 
    
    d3 <- rbind(d1[1,],d2) 
    d3[,c(6:9)] <- d3[,c(6:9)] %>% round(digits = 4)
    datatable(d3)
  })
  
  output$SectorSelection2 <- renderDataTable({
    d1 <- PerformanceM[["Alltogether"]] %>% filter(Portfolio == "SP500")
    d2 <- PerformanceM[["Alltogether"]] %>% filter(Portfolio == "Sector")  %>% filter(Opt %in% input$SO1) 
    
    d3 <- rbind(d1[1,],d2) 
    d3[,c(6:9)] <- d3[,c(6:9)] %>% round(digits = 4)
    datatable(d3)
  })
  
  output$SectorSelection3 <- renderDataTable({
    d1 <- PerformanceM[["Alltogether"]] %>% filter(Portfolio == "SP500")
    d2 <- PerformanceM[["Alltogether"]] %>% filter(Portfolio == "Sector") %>%   filter(VineType %in% input$SC1)
    
    d3 <- rbind(d1[1,],d2) 
    d3[,c(6:9)] <- d3[,c(6:9)] %>% round(digits = 4)
    datatable(d3)
  })
  
  ##############################################   Mean,SD,and CAGR   ############################################  
  
  
  
  ##############################################       Weights      ############################################    
  
  ##### OptimizationWise #####
  
  output$Sectornaiveweight <- renderPlotly({
    
    a <- EvolutionAndWeights[["Sector"]] %>% select(Date, intersect(contains("Naive"),contains("Weights")))
    a <- a %>% select(Date,contains("BTC"),contains("LTC"),contains("XRP"),contains("XMR"),contains("ETH"),contains("BCH"),contains("EOS"))
    a <- melt(a, id.var = "Date")
    a <- a %>% separate(variable, c("Portfolio","Dependence","BorA","VineType","Opt","EorW","Asset"), sep = "_")
    a <- a %>% filter(Opt != "Starr")
    a <- a %>% mutate(Opt = replace(Opt, Opt == "MinSD", "MinVar"))
    b <- ggplot(a, aes(x = Date, y = value)) + geom_line() +facet_wrap(~Asset+VineType, scales = "free", nrow = 4) + theme_wsj()+  scale_x_date(date_labels = "%y")+ theme(panel.spacing = unit(2, "lines")) 
    ggplotly(b, height = 1500, width = 2000)
  })
  
  
  output$Sectorminsdweight <- renderPlotly({
    
    a <- EvolutionAndWeights[["Sector"]] %>% select(Date, intersect(contains("MinSD"),contains("Weights")))
    a <- a %>% select(Date,contains("BTC"),contains("LTC"),contains("XRP"),contains("XMR"),contains("ETH"),contains("BCH"),contains("EOS"))
    a <- melt(a, id.var = "Date")
    a <- a %>% separate(variable, c("Portfolio","Dependence","BorA","VineType","Opt","EorW","Asset"), sep = "_")
    a <- a %>% filter(Opt != "Starr")
    a <- a %>% mutate(Opt = replace(Opt, Opt == "MinSD", "MinVar"))
    b <- ggplot(a, aes(x = Date, y = value)) + geom_line() +facet_wrap(~Asset+VineType, scales = "free", nrow = 4) + theme_wsj()+  scale_x_date(date_labels = "%y")+ theme(panel.spacing = unit(2, "lines")) 
    ggplotly(b, height = 1500, width = 2000)
  })
  
  output$Sectorminesweight <- renderPlotly({
    
    a <- EvolutionAndWeights[["Sector"]] %>% select(Date, intersect(contains("MinES"),contains("Weights")))
    a <- a %>% select(Date,contains("BTC"),contains("LTC"),contains("XRP"),contains("XMR"),contains("ETH"),contains("BCH"),contains("EOS"))
    a <- melt(a, id.var = "Date")
    a <- a %>% separate(variable, c("Portfolio","Dependence","BorA","VineType","Opt","EorW","Asset"), sep = "_")
    a <- a %>% filter(Opt != "Starr")
    a <- a %>% mutate(Opt = replace(Opt, Opt == "MinSD", "MinVar"))
    b <- ggplot(a, aes(x = Date, y = value)) + geom_line() +facet_wrap(~Asset+VineType, scales = "free", nrow = 4) + theme_wsj()+  scale_x_date(date_labels = "%y")+ theme(panel.spacing = unit(2, "lines")) 
    ggplotly(b, height = 1500, width = 2000)
  })
  
  output$Sectorsharpeweight <- renderPlotly({
    a <- EvolutionAndWeights[["Sector"]] %>% select(Date, intersect(contains("Sharpe"),contains("Weights")))
    a <- a %>% select(Date,contains("BTC"),contains("LTC"),contains("XRP"),contains("XMR"),contains("ETH"),contains("BCH"),contains("EOS"))
    a <- melt(a, id.var = "Date")
    a <- a %>% separate(variable, c("Portfolio","Dependence","BorA","VineType","Opt","EorW","Asset"), sep = "_")
    a <- a %>% filter(Opt != "Starr")
    a <- a %>% mutate(Opt = replace(Opt, Opt == "MinSD", "MinVar"))
    b <- ggplot(a, aes(x = Date, y = value)) + geom_line() +facet_wrap(~Asset+VineType, scales = "free", nrow = 4) + theme_wsj()+  scale_x_date(date_labels = "%y")+ theme(panel.spacing = unit(2, "lines")) 
    ggplotly(b, height = 1500, width = 2000)
  })
  
  output$Sectormaxmeanweight <- renderPlotly({
    
    a <- EvolutionAndWeights[["Sector"]] %>% select(Date, intersect(contains("MaxMean"),contains("Weights")))
    a <- a %>% select(Date,contains("BTC"),contains("LTC"),contains("XRP"),contains("XMR"),contains("ETH"),contains("BCH"),contains("EOS"))
    a <- melt(a, id.var = "Date")
    a <- a %>% separate(variable, c("Portfolio","Dependence","BorA","VineType","Opt","EorW","Asset"), sep = "_")
    a <- a %>% filter(Opt != "Starr")
    a <- a %>% mutate(Opt = replace(Opt, Opt == "MinSD", "MinVar"))
    b <- ggplot(a, aes(x = Date, y = value)) + geom_line() +facet_wrap(~Asset+VineType, scales = "free", nrow = 4) + theme_wsj()+  scale_x_date(date_labels = "%y")+ theme(panel.spacing = unit(2, "lines")) 
    ggplotly(b, height = 1500, width = 2000)
  })
  
  
  
  ##### CopulaWise ##### 
  
  output$SectorCONVweight <- renderPlotly({
    
    
    a <- EvolutionAndWeights[["Sector"]] %>% select(Date, intersect(contains("Conv"),contains("Weights")))
    a <- a %>% select(Date,contains("BTC"),contains("LTC"),contains("XRP"),contains("XMR"),contains("ETH"),contains("BCH"),contains("EOS"))
    a <- melt(a, id.var = "Date")
    a <- a %>% separate(variable, c("Portfolio","Dependence","BorA","VineType","Opt","EorW","Asset"), sep = "_")
    a <- a %>% filter(Opt != "Starr")
    a <- a %>% mutate(Opt = replace(Opt, Opt == "MinSD", "MinVar"))
    b <- ggplot(a, aes(x = Date, y = value)) + geom_line() +facet_wrap(~Asset+Opt, scales = "free", nrow = 4) + theme_wsj()+  scale_x_date(date_labels = "%y")+ theme(panel.spacing = unit(2, "lines"))  + theme(legend.position = "bottom",axis.text.x = element_text(angle=45, hjust = 1))+ labs(color ="")
    ggplotly(b, height = 1500, width = 2000)
    
  })
  
  
  output$SectorPRweight <- renderPlotly({
    
    a <- EvolutionAndWeights[["Sector"]] %>% select(Date, intersect(contains("P-R"),contains("Weights")))
    a <- a %>% select(Date,contains("BTC"),contains("LTC"),contains("XRP"),contains("XMR"),contains("ETH"),contains("BCH"),contains("EOS"))
    a <- melt(a, id.var = "Date")
    a <- a %>% separate(variable, c("Portfolio","Dependence","BorA","VineType","Opt","EorW","Asset"), sep = "_")
    a <- a %>% filter(Opt != "Starr")
    a <- a %>% mutate(Opt = replace(Opt, Opt == "MinSD", "MinVar"))
    b <- ggplot(a, aes(x = Date, y = value)) + geom_line() +facet_wrap(~Asset+Opt, scales = "free", nrow = 4) + theme_wsj()+  scale_x_date(date_labels = "%y")+ theme(panel.spacing = unit(2, "lines"))  + theme(legend.position = "bottom",axis.text.x = element_text(angle=45, hjust = 1))+ labs(color ="")
    ggplotly(b, height = 1500, width = 2000)
    
  })
  
  output$SectorPCweight <- renderPlotly({
    
    a <- EvolutionAndWeights[["Sector"]] %>% select(Date, intersect(contains("P-C"),contains("Weights")))
    a <- a %>% select(Date,contains("BTC"),contains("LTC"),contains("XRP"),contains("XMR"),contains("ETH"),contains("BCH"),contains("EOS"))
    a <- melt(a, id.var = "Date")
    a <- a %>% separate(variable, c("Portfolio","Dependence","BorA","VineType","Opt","EorW","Asset"), sep = "_")
    a <- a %>% filter(Opt != "Starr")
    a <- a %>% mutate(Opt = replace(Opt, Opt == "MinSD", "MinVar"))
    b <- ggplot(a, aes(x = Date, y = value)) + geom_line() +facet_wrap(~Asset+Opt, scales = "free", nrow = 4) + theme_wsj()+  scale_x_date(date_labels = "%y")+ theme(panel.spacing = unit(2, "lines"))  + theme(legend.position = "bottom",axis.text.x = element_text(angle=45, hjust = 1))+ labs(color ="")
    ggplotly(b, height = 1500, width = 2000)
    
  })
  
  output$SectorERweight <- renderPlotly({
    
    
    a <- EvolutionAndWeights[["Sector"]] %>% select(Date, intersect(contains("E-R"),contains("Weights")))
    a <- a %>% select(Date,contains("BTC"),contains("LTC"),contains("XRP"),contains("XMR"),contains("ETH"),contains("BCH"),contains("EOS"))
    a <- melt(a, id.var = "Date")
    a <- a %>% separate(variable, c("Portfolio","Dependence","BorA","VineType","Opt","EorW","Asset"), sep = "_")
    a <- a %>% filter(Opt != "Starr")
    a <- a %>% mutate(Opt = replace(Opt, Opt == "MinSD", "MinVar"))
    b <- ggplot(a, aes(x = Date, y = value)) + geom_line() +facet_wrap(~Asset+Opt, scales = "free", nrow = 4) + theme_wsj()+  scale_x_date(date_labels = "%y")+ theme(panel.spacing = unit(2, "lines"))  + theme(legend.position = "bottom",axis.text.x = element_text(angle=45, hjust = 1))+ labs(color ="")
    ggplotly(b, height = 1500, width = 2000)
    
  })
  
  output$SectorECweight <- renderPlotly({
    
    a <- EvolutionAndWeights[["Sector"]] %>% select(Date, intersect(contains("E-C"),contains("Weights")))
    a <- a %>% select(Date,contains("BTC"),contains("LTC"),contains("XRP"),contains("XMR"),contains("ETH"),contains("BCH"),contains("EOS"))
    a <- melt(a, id.var = "Date")
    a <- a %>% separate(variable, c("Portfolio","Dependence","BorA","VineType","Opt","EorW","Asset"), sep = "_")
    a <- a %>% filter(Opt != "Starr")
    a <- a %>% mutate(Opt = replace(Opt, Opt == "MinSD", "MinVar"))
    b <- ggplot(a, aes(x = Date, y = value)) + geom_line() +facet_wrap(~Asset+Opt, scales = "free", nrow = 4) + theme_wsj()+  scale_x_date(date_labels = "%y")+ theme(panel.spacing = unit(2, "lines"))  + theme(legend.position = "bottom",axis.text.x = element_text(angle=45, hjust = 1))+ labs(color ="")
    ggplotly(b, height = 1500, width = 2000)
    
  })
  
  
  ##############################################       Weights      ############################################    
  
  #########################################              MarFixCom Indices EVOLUTION           ################################################  
  output$MarFixComnaiveplot <- renderPlotly({
    
    a <- EvolutionAndWeights[["MarFixCom"]] %>% select(Date,intersect(contains("Naive"),contains("Evolution")))%>% filter(between(Date,as.Date(input$MFCIDates[1]),as.Date(input$MFCIDates[2]))) %>%
      filter(between(Date,as.Date(input$MFCDates[1]),as.Date(input$MFCDates[2])))
    
    a <- Inv(a) 
    #a <- Evolution %>% select(Date,contains("Naive")) %>% filter(between(Date,as.Date(input$MFCDates[1]),as.Date(input$MFCDates[2])))
    a <- melt(a, id.var = "Date")
    b <- a %>% separate(variable, c("Portfolio","Dependence","BorA","VineType","Opt","EorW","Asset"), sep = "_")
    b <- b %>% mutate(Opt = replace(Opt, Opt == "MinSD", "MinVar"))
    c <- ggplot(b, aes(Date,value, color = BorA,  group = BorA)) + geom_line() + facet_wrap(~VineType)+ theme_wsj()+  scale_x_date(date_labels = "%y") + theme(legend.position = "bottom",axis.text.x = element_text(angle=45, hjust = 1))+ labs(color ="")  
    d <- ggplotly(c, height = 600, width = 1000)
  })
  
  output$MarFixComminsdplot <- renderPlotly({
    
    a <- EvolutionAndWeights[["MarFixCom"]] %>% select(Date,intersect(contains("MinSD"),contains("Evolution")))%>% filter(between(Date,as.Date(input$MFCIDates[1]),as.Date(input$MFCIDates[2]))) %>%
      filter(between(Date,as.Date(input$MFCDates[1]),as.Date(input$MFCDates[2])))
    
    a <- Inv(a) 
    #a <- Evolution %>% select(Date,contains("Naive")) %>% filter(between(Date,as.Date(input$MFCDates[1]),as.Date(input$MFCDates[2])))
    a <- melt(a, id.var = "Date")
    b <- a %>% separate(variable, c("Portfolio","Dependence","BorA","VineType","Opt","EorW","Asset"), sep = "_")
    b <- b %>% mutate(Opt = replace(Opt, Opt == "MinSD", "MinVar"))
    c <- ggplot(b, aes(Date,value, color = BorA,  group = BorA)) + geom_line() + facet_wrap(~VineType)+ theme_wsj()+  scale_x_date(date_labels = "%y") + theme(legend.position = "bottom",axis.text.x = element_text(angle=45, hjust = 1))+ labs(color ="")  
    d <- ggplotly(c, height = 600, width = 1000)
  })
  
  output$MarFixComminesplot <- renderPlotly({
    
    a <- EvolutionAndWeights[["MarFixCom"]] %>% select(Date,intersect(contains("MinES"),contains("Evolution")))%>% filter(between(Date,as.Date(input$MFCIDates[1]),as.Date(input$MFCIDates[2]))) %>%
      filter(between(Date,as.Date(input$MFCDates[1]),as.Date(input$MFCDates[2])))
    
    a <- Inv(a) 
    #a <- Evolution %>% select(Date,contains("Naive")) %>% filter(between(Date,as.Date(input$MFCDates[1]),as.Date(input$MFCDates[2])))
    a <- melt(a, id.var = "Date")
    b <- a %>% separate(variable, c("Portfolio","Dependence","BorA","VineType","Opt","EorW","Asset"), sep = "_")
    b <- b %>% mutate(Opt = replace(Opt, Opt == "MinSD", "MinVar"))
    c <- ggplot(b, aes(Date,value, color = BorA,  group = BorA)) + geom_line() + facet_wrap(~VineType)+ theme_wsj()+  scale_x_date(date_labels = "%y") + theme(legend.position = "bottom",axis.text.x = element_text(angle=45, hjust = 1))+ labs(color ="")  
    d <- ggplotly(c, height = 600, width = 1000)
  })
  
  output$MarFixComsharpeplot <- renderPlotly({
    
    a <- EvolutionAndWeights[["MarFixCom"]] %>% select(Date,intersect(contains("Sharpe"),contains("Evolution")))%>% filter(between(Date,as.Date(input$MFCIDates[1]),as.Date(input$MFCIDates[2]))) %>%
      filter(between(Date,as.Date(input$MFCDates[1]),as.Date(input$MFCDates[2])))
    
    a <- Inv(a) 
    #a <- Evolution %>% select(Date,contains("Naive")) %>% filter(between(Date,as.Date(input$MFCDates[1]),as.Date(input$MFCDates[2])))
    a <- melt(a, id.var = "Date")
    b <- a %>% separate(variable, c("Portfolio","Dependence","BorA","VineType","Opt","EorW","Asset"), sep = "_")
    b <- b %>% mutate(Opt = replace(Opt, Opt == "MinSD", "MinVar"))
    c <- ggplot(b, aes(Date,value, color = BorA,  group = BorA)) + geom_line() + facet_wrap(~VineType)+ theme_wsj()+  scale_x_date(date_labels = "%y") + theme(legend.position = "bottom",axis.text.x = element_text(angle=45, hjust = 1))+ labs(color ="")  
    d <- ggplotly(c, height = 600, width = 1000)
  })
  
  output$MarFixCommaxmeanplot <- renderPlotly({
    
    a <- EvolutionAndWeights[["MarFixCom"]] %>% select(Date,intersect(contains("MaxMean"),contains("Evolution")))%>% filter(between(Date,as.Date(input$MFCIDates[1]),as.Date(input$MFCIDates[2]))) %>%
      filter(between(Date,as.Date(input$MFCDates[1]),as.Date(input$MFCDates[2])))
    
    a <- Inv(a) 
    #a <- Evolution %>% select(Date,contains("Naive")) %>% filter(between(Date,as.Date(input$MFCDates[1]),as.Date(input$MFCDates[2])))
    a <- melt(a, id.var = "Date")
    b <- a %>% separate(variable, c("Portfolio","Dependence","BorA","VineType","Opt","EorW","Asset"), sep = "_")
    b <- b %>% mutate(Opt = replace(Opt, Opt == "MinSD", "MinVar"))
    c <- ggplot(b, aes(Date,value, color = BorA,  group = BorA)) + geom_line() + facet_wrap(~VineType)+ theme_wsj()+  scale_x_date(date_labels = "%y") + theme(legend.position = "bottom",axis.text.x = element_text(angle=45, hjust = 1))+ labs(color ="")  
    d <- ggplotly(c, height = 600, width = 1000)
  })
  
  ###### Copulawise
  
  
  output$MarFixComCONV <- renderPlotly({
    
    a <- EvolutionAndWeights[["MarFixCom"]] %>% select(Date,intersect(contains("Conv"),contains("Evolution")))%>% filter(between(Date,as.Date(input$MFCIDates[1]),as.Date(input$MFCIDates[2]))) %>%
      filter(between(Date,as.Date(input$MFCDates[1]),as.Date(input$MFCDates[2])))
    
    a <- Inv(a) 
    #a <- Evolution %>% select(Date,contains("Naive")) %>% filter(between(Date,as.Date(input$MFCDates[1]),as.Date(input$MFCDates[2])))
    a <- melt(a, id.var = "Date")
    b <- a %>% separate(variable, c("Portfolio","Dependence","BorA","VineType","Opt","EorW","Asset"), sep = "_")
    b <- b %>% mutate(Opt = replace(Opt, Opt == "MinSD", "MinVar"))
    c <- ggplot(b, aes(Date,value, color = BorA,  group = BorA)) + geom_line() +facet_wrap(~Opt) + theme_wsj()+  scale_x_date(date_labels = "%y") + theme(legend.position = "bottom",axis.text.x = element_text(angle=45, hjust = 1))+ labs(color ="")  
    d <- ggplotly(c, height = 600, width = 1000)
  })
  
  
  
  output$MarFixComPR <- renderPlotly({
    
    a <- EvolutionAndWeights[["MarFixCom"]] %>% select(Date,intersect(contains("P-R"),contains("Evolution")))%>% filter(between(Date,as.Date(input$MFCIDates[1]),as.Date(input$MFCIDates[2]))) %>%
      filter(between(Date,as.Date(input$MFCDates[1]),as.Date(input$MFCDates[2])))
    
    a <- Inv(a) 
    #a <- Evolution %>% select(Date,contains("Naive")) %>% filter(between(Date,as.Date(input$MFCDates[1]),as.Date(input$MFCDates[2])))
    a <- melt(a, id.var = "Date")
    b <- a %>% separate(variable, c("Portfolio","Dependence","BorA","VineType","Opt","EorW","Asset"), sep = "_")
    b <- b %>% mutate(Opt = replace(Opt, Opt == "MinSD", "MinVar"))
    c <- ggplot(b, aes(Date,value, color = BorA,  group = BorA)) + geom_line() +facet_wrap(~Opt) + theme_wsj()+  scale_x_date(date_labels = "%y") + theme(legend.position = "bottom",axis.text.x = element_text(angle=45, hjust = 1))+ labs(color ="")  
    d <- ggplotly(c, height = 600, width = 1000)
  })
  
  
  
  output$MarFixComPC <- renderPlotly({
    
    a <- EvolutionAndWeights[["MarFixCom"]] %>% select(Date,intersect(contains("P-C"),contains("Evolution")))%>% filter(between(Date,as.Date(input$MFCIDates[1]),as.Date(input$MFCIDates[2]))) %>%
      filter(between(Date,as.Date(input$MFCDates[1]),as.Date(input$MFCDates[2])))
    
    a <- Inv(a) 
    #a <- Evolution %>% select(Date,contains("Naive")) %>% filter(between(Date,as.Date(input$MFCDates[1]),as.Date(input$MFCDates[2])))
    a <- melt(a, id.var = "Date")
    b <- a %>% separate(variable, c("Portfolio","Dependence","BorA","VineType","Opt","EorW","Asset"), sep = "_")
    b <- b %>% mutate(Opt = replace(Opt, Opt == "MinSD", "MinVar"))
    c <- ggplot(b, aes(Date,value, color = BorA,  group = BorA)) + geom_line() +facet_wrap(~Opt) + theme_wsj()+  scale_x_date(date_labels = "%y") + theme(legend.position = "bottom",axis.text.x = element_text(angle=45, hjust = 1))+ labs(color ="")  
    d <- ggplotly(c, height = 600, width = 1000)
  })
  
  
  
  output$MarFixComER <- renderPlotly({
    
    a <- EvolutionAndWeights[["MarFixCom"]] %>% select(Date,intersect(contains("E-R"),contains("Evolution")))%>% filter(between(Date,as.Date(input$MFCIDates[1]),as.Date(input$MFCIDates[2]))) %>%
      filter(between(Date,as.Date(input$MFCDates[1]),as.Date(input$MFCDates[2])))
    
    a <- Inv(a) 
    #a <- Evolution %>% select(Date,contains("Naive")) %>% filter(between(Date,as.Date(input$MFCDates[1]),as.Date(input$MFCDates[2])))
    a <- melt(a, id.var = "Date")
    b <- a %>% separate(variable, c("Portfolio","Dependence","BorA","VineType","Opt","EorW","Asset"), sep = "_")
    b <- b %>% mutate(Opt = replace(Opt, Opt == "MinSD", "MinVar"))
    c <- ggplot(b, aes(Date,value, color = BorA,  group = BorA)) + geom_line() +facet_wrap(~Opt) + theme_wsj()+  scale_x_date(date_labels = "%y") + theme(legend.position = "bottom",axis.text.x = element_text(angle=45, hjust = 1))+ labs(color ="")  
    d <- ggplotly(c, height = 600, width = 1000)
  })
  
  
  
  output$MarFixComEC <- renderPlotly({
    
    a <- EvolutionAndWeights[["MarFixCom"]] %>% select(Date,intersect(contains("E-C"),contains("Evolution")))%>% filter(between(Date,as.Date(input$MFCIDates[1]),as.Date(input$MFCIDates[2]))) %>%
      filter(between(Date,as.Date(input$MFCDates[1]),as.Date(input$MFCDates[2])))
    
    a <- Inv(a) 
    #a <- Evolution %>% select(Date,contains("Naive")) %>% filter(between(Date,as.Date(input$MFCDates[1]),as.Date(input$MFCDates[2])))
    a <- melt(a, id.var = "Date")
    b <- a %>% separate(variable, c("Portfolio","Dependence","BorA","VineType","Opt","EorW","Asset"), sep = "_")
    b <- b %>% mutate(Opt = replace(Opt, Opt == "MinSD", "MinVar"))
    c <- ggplot(b, aes(Date,value, color = BorA,  group = BorA)) + geom_line() +facet_wrap(~Opt) + theme_wsj()+  scale_x_date(date_labels = "%y") + theme(legend.position = "bottom",axis.text.x = element_text(angle=45, hjust = 1))+ labs(color ="")  
    d <- ggplotly(c, height = 600, width = 1000)
  })
  
  
  
  ##############################################   Mean,SD,and CAGR   ############################################
  output$MarFixComSelection <- renderDataTable({
    d1 <- PerformanceM[["Alltogether"]] %>% filter(Portfolio == "SP500")
    d2 <- PerformanceM[["Alltogether"]] %>% filter(Portfolio == "MarFixCom")   %>% filter(VineType %in% input$MFCC1) %>% 
      filter(Opt %in% input$MFCO1) 
    
    d3 <- rbind(d1[1,],d2) 
    d3[,c(6:9)] <- d3[,c(6:9)] %>% round(digits = 4)
    datatable(d3)
  })
  
  output$MarFixComSelection2 <- renderDataTable({
    d1 <- PerformanceM[["Alltogether"]] %>% filter(Portfolio == "SP500")
    d2 <- PerformanceM[["Alltogether"]] %>% filter(Portfolio == "MarFixCom")  %>% filter(Opt %in% input$MFCO1) 
    
    d3 <- rbind(d1[1,],d2) 
    d3[,c(6:9)] <- d3[,c(6:9)] %>% round(digits = 4)
    datatable(d3)
  })
  
  output$MarFixComSelection3 <- renderDataTable({
    d1 <- PerformanceM[["Alltogether"]] %>% filter(Portfolio == "SP500")
    d2 <- PerformanceM[["Alltogether"]] %>% filter(Portfolio == "MarFixCom") %>%   filter(VineType %in% input$MFCC1)
    
    d3 <- rbind(d1[1,],d2) 
    d3[,c(6:9)] <- d3[,c(6:9)] %>% round(digits = 4)
    datatable(d3)
  })
  
  ##############################################   Mean,SD,and CAGR   ############################################  
  
  
  
  ##############################################       Weights      ############################################    
  
  ##### OptimizationWise #####
  
  output$MarFixComnaiveweight <- renderPlotly({
    
    a <- EvolutionAndWeights[["MarFixCom"]] %>% select(Date, intersect(contains("Naive"),contains("Weights")))
    a <- a %>% select(Date,contains("BTC"),contains("LTC"),contains("XRP"),contains("XMR"),contains("ETH"),contains("BCH"),contains("EOS"))
    a <- melt(a, id.var = "Date")
    a <- a %>% separate(variable, c("Portfolio","Dependence","BorA","VineType","Opt","EorW","Asset"), sep = "_")
    a <- a %>% filter(Opt != "Starr")
    a <- a %>% mutate(Opt = replace(Opt, Opt == "MinSD", "MinVar"))
    b <- ggplot(a, aes(x = Date, y = value)) + geom_line() +facet_wrap(~Asset+VineType, scales = "free", nrow = 4) + theme_wsj()+  scale_x_date(date_labels = "%y")+ theme(panel.spacing = unit(2, "lines")) 
    ggplotly(b, height = 1500, width = 2000)
  })
  
  
  output$MarFixComminsdweight <- renderPlotly({
    
    a <- EvolutionAndWeights[["MarFixCom"]] %>% select(Date, intersect(contains("MinSD"),contains("Weights")))
    a <- a %>% select(Date,contains("BTC"),contains("LTC"),contains("XRP"),contains("XMR"),contains("ETH"),contains("BCH"),contains("EOS"))
    a <- melt(a, id.var = "Date")
    a <- a %>% separate(variable, c("Portfolio","Dependence","BorA","VineType","Opt","EorW","Asset"), sep = "_")
    a <- a %>% filter(Opt != "Starr")
    a <- a %>% mutate(Opt = replace(Opt, Opt == "MinSD", "MinVar"))
    b <- ggplot(a, aes(x = Date, y = value)) + geom_line() +facet_wrap(~Asset+VineType, scales = "free", nrow = 4) + theme_wsj()+  scale_x_date(date_labels = "%y")+ theme(panel.spacing = unit(2, "lines")) 
    ggplotly(b, height = 1500, width = 2000)
  })
  
  output$MarFixComminesweight <- renderPlotly({
    
    a <- EvolutionAndWeights[["MarFixCom"]] %>% select(Date, intersect(contains("MinES"),contains("Weights")))
    a <- a %>% select(Date,contains("BTC"),contains("LTC"),contains("XRP"),contains("XMR"),contains("ETH"),contains("BCH"),contains("EOS"))
    a <- melt(a, id.var = "Date")
    a <- a %>% separate(variable, c("Portfolio","Dependence","BorA","VineType","Opt","EorW","Asset"), sep = "_")
    a <- a %>% filter(Opt != "Starr")
    a <- a %>% mutate(Opt = replace(Opt, Opt == "MinSD", "MinVar"))
    b <- ggplot(a, aes(x = Date, y = value)) + geom_line() +facet_wrap(~Asset+VineType, scales = "free", nrow = 4) + theme_wsj()+  scale_x_date(date_labels = "%y")+ theme(panel.spacing = unit(2, "lines")) 
    ggplotly(b, height = 1500, width = 2000)
  })
  
  output$MarFixComsharpeweight <- renderPlotly({
    a <- EvolutionAndWeights[["MarFixCom"]] %>% select(Date, intersect(contains("Sharpe"),contains("Weights")))
    a <- a %>% select(Date,contains("BTC"),contains("LTC"),contains("XRP"),contains("XMR"),contains("ETH"),contains("BCH"),contains("EOS"))
    a <- melt(a, id.var = "Date")
    a <- a %>% separate(variable, c("Portfolio","Dependence","BorA","VineType","Opt","EorW","Asset"), sep = "_")
    a <- a %>% filter(Opt != "Starr")
    a <- a %>% mutate(Opt = replace(Opt, Opt == "MinSD", "MinVar"))
    b <- ggplot(a, aes(x = Date, y = value)) + geom_line() +facet_wrap(~Asset+VineType, scales = "free", nrow = 4) + theme_wsj()+  scale_x_date(date_labels = "%y")+ theme(panel.spacing = unit(2, "lines")) 
    ggplotly(b, height = 1500, width = 2000)
  })
  
  output$MarFixCommaxmeanweight <- renderPlotly({
    
    a <- EvolutionAndWeights[["MarFixCom"]] %>% select(Date, intersect(contains("MaxMean"),contains("Weights")))
    a <- a %>% select(Date,contains("BTC"),contains("LTC"),contains("XRP"),contains("XMR"),contains("ETH"),contains("BCH"),contains("EOS"))
    a <- melt(a, id.var = "Date")
    a <- a %>% separate(variable, c("Portfolio","Dependence","BorA","VineType","Opt","EorW","Asset"), sep = "_")
    a <- a %>% filter(Opt != "Starr")
    a <- a %>% mutate(Opt = replace(Opt, Opt == "MinSD", "MinVar"))
    b <- ggplot(a, aes(x = Date, y = value)) + geom_line() +facet_wrap(~Asset+VineType, scales = "free_y", nrow = 4) + theme_wsj()+  scale_x_date(date_labels = "%y")+ theme(panel.spacing = unit(2, "lines")) 
    ggplotly(b, height = 1500, width = 2000)
  })
  
  
  
  ##### CopulaWise ##### 
  
  output$MarFixComCONVweight <- renderPlotly({
    
    
    a <- EvolutionAndWeights[["MarFixCom"]] %>% select(Date, intersect(contains("Conv"),contains("Weights")))
    a <- a %>% select(Date,contains("BTC"),contains("LTC"),contains("XRP"),contains("XMR"),contains("ETH"),contains("BCH"),contains("EOS"))
    a <- melt(a, id.var = "Date")
    a <- a %>% separate(variable, c("Portfolio","Dependence","BorA","VineType","Opt","EorW","Asset"), sep = "_")
    a <- a %>% filter(Opt != "Starr")
    a <- a %>% mutate(Opt = replace(Opt, Opt == "MinSD", "MinVar"))
    b <- ggplot(a, aes(x = Date, y = value)) + geom_line() +facet_wrap(~Asset+Opt, scales = "free", nrow = 4) + theme_wsj()+  scale_x_date(date_labels = "%y")+ theme(panel.spacing = unit(2, "lines"))  + theme(legend.position = "bottom",axis.text.x = element_text(angle=45, hjust = 1))+ labs(color ="")
    ggplotly(b, height = 1500, width = 2000)
    
  })
  
  
  output$MarFixComPRweight <- renderPlotly({
    
    a <- EvolutionAndWeights[["MarFixCom"]] %>% select(Date, intersect(contains("P-R"),contains("Weights")))
    a <- a %>% select(Date,contains("BTC"),contains("LTC"),contains("XRP"),contains("XMR"),contains("ETH"),contains("BCH"),contains("EOS"))
    a <- melt(a, id.var = "Date")
    a <- a %>% separate(variable, c("Portfolio","Dependence","BorA","VineType","Opt","EorW","Asset"), sep = "_")
    a <- a %>% filter(Opt != "Starr")
    a <- a %>% mutate(Opt = replace(Opt, Opt == "MinSD", "MinVar"))
    b <- ggplot(a, aes(x = Date, y = value)) + geom_line() +facet_wrap(~Asset+Opt, scales = "free", nrow = 4) + theme_wsj()+  scale_x_date(date_labels = "%y")+ theme(panel.spacing = unit(2, "lines"))  + theme(legend.position = "bottom",axis.text.x = element_text(angle=45, hjust = 1))+ labs(color ="")
    ggplotly(b, height = 1500, width = 2000)
    
  })
  
  output$MarFixComPCweight <- renderPlotly({
    
    a <- EvolutionAndWeights[["MarFixCom"]] %>% select(Date, intersect(contains("P-C"),contains("Weights")))
    a <- a %>% select(Date,contains("BTC"),contains("LTC"),contains("XRP"),contains("XMR"),contains("ETH"),contains("BCH"),contains("EOS"))
    a <- melt(a, id.var = "Date")
    a <- a %>% separate(variable, c("Portfolio","Dependence","BorA","VineType","Opt","EorW","Asset"), sep = "_")
    a <- a %>% filter(Opt != "Starr")
    a <- a %>% mutate(Opt = replace(Opt, Opt == "MinSD", "MinVar"))
    b <- ggplot(a, aes(x = Date, y = value)) + geom_line() +facet_wrap(~Asset+Opt, scales = "free", nrow = 4) + theme_wsj()+  scale_x_date(date_labels = "%y")+ theme(panel.spacing = unit(2, "lines"))  + theme(legend.position = "bottom",axis.text.x = element_text(angle=45, hjust = 1))+ labs(color ="")
    ggplotly(b, height = 1500, width = 2000)
    
  })
  
  output$MarFixComERweight <- renderPlotly({
    
    
    a <- EvolutionAndWeights[["MarFixCom"]] %>% select(Date, intersect(contains("E-R"),contains("Weights")))
    a <- a %>% select(Date,contains("BTC"),contains("LTC"),contains("XRP"),contains("XMR"),contains("ETH"),contains("BCH"),contains("EOS"))
    a <- melt(a, id.var = "Date")
    a <- a %>% separate(variable, c("Portfolio","Dependence","BorA","VineType","Opt","EorW","Asset"), sep = "_")
    a <- a %>% filter(Opt != "Starr")
    a <- a %>% mutate(Opt = replace(Opt, Opt == "MinSD", "MinVar"))
    b <- ggplot(a, aes(x = Date, y = value)) + geom_line() +facet_wrap(~Asset+Opt, scales = "free", nrow = 4) + theme_wsj()+  scale_x_date(date_labels = "%y")+ theme(panel.spacing = unit(2, "lines"))  + theme(legend.position = "bottom",axis.text.x = element_text(angle=45, hjust = 1))+ labs(color ="")
    ggplotly(b, height = 1500, width = 2000)
    
  })
  
  output$MarFixComECweight <- renderPlotly({
    
    a <- EvolutionAndWeights[["MarFixCom"]] %>% select(Date, intersect(contains("E-C"),contains("Weights")))
    a <- a %>% select(Date,contains("BTC"),contains("LTC"),contains("XRP"),contains("XMR"),contains("ETH"),contains("BCH"),contains("EOS"))
    a <- melt(a, id.var = "Date")
    a <- a %>% separate(variable, c("Portfolio","Dependence","BorA","VineType","Opt","EorW","Asset"), sep = "_")
    a <- a %>% filter(Opt != "Starr")
    a <- a %>% mutate(Opt = replace(Opt, Opt == "MinSD", "MinVar"))
    b <- ggplot(a, aes(x = Date, y = value)) + geom_line() +facet_wrap(~Asset+Opt, scales = "free", nrow = 4) + theme_wsj()+  scale_x_date(date_labels = "%y")+ theme(panel.spacing = unit(2, "lines"))  + theme(legend.position = "bottom",axis.text.x = element_text(angle=45, hjust = 1))+ labs(color ="")
    ggplotly(b, height = 1500, width = 2000)
    
  })
  
  
  ##############################################       Weights      ############################################    
  
  
  ######### Extras ######
  

  
  
  
  
  }


shinyApp(ui, server)
  
