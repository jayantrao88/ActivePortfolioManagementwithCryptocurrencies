# Active Portfolio Management with Cryptocurrencies

 The .rmd files perform the excercise of active portfolio management with cryptocurrencies that follow the below general guideline.

     To see the efficacy of adding cryptocurrencies to a given portfolio with n different assets, I created five different types of portfolios as a benchmark (call Base portfolio). Then to such portfolios I added four major cryptocurrencies Bitcoin, Litecoin, XRP, and Ether (call Alt portfolio). I chose those cryptocurrencies because they capture 80% of the cryptocurrency market. The dataset from 28th April 2013 and ends on 30th March 2020. In the real world, portfolio managers actively churn their portfolios at a given interval, called rebalancing period.  We have 41 rebalancing periods starting from 2014 to Feb 2020. For every rebalancing period, lagging 500 observations of the asset returns were considered.

    I chose to do the analysis using five different types of dependence modelling: Conventional (Markowitz) and four copula modelling: Parametric distribution and R – Vine (P-R), Parametric distribution and C – Vine (P-C), Empirical distribution and R – Vine (E-R), and Empirical distribution and C – Vine (E-C). The parametric and empirical distributions are different distribution estimation techniques used as an input in modelling copula. We get them by modelling the marginal distribution of each asset individually (I used ARMA-GARCH models) and extracting the standardized residuals.

    Once the joint dependence is modelled and the residuals are simulated from these models, I put them in five different optimization techniques to determine the optimal weight for each asset, each period. These five optimization techniques are Naive  portfolio, Minimum Standard Deviation Portfolio, Minimum Expected Shortfall Portfolio, Sharpe Portfolio, and Maximum Mean Portfolio.
            
    
   It may also interest you to check out the interactive dashboard (https://jayantrao.shinyapps.io/FivePort/) that shows the results of adding cryptocurrencies to five different portfolios. It covers five dependence methods and five optimization methods to provide a robust analysis of the results. You will find more information about the methodology and processes in the link. This dashboard is created for a research paper titled "Active Portfolio Management with Cryptocurrencies" which is currently under draft mode and the latest draft can be found here (https://tinyurl.com/yxt4g9dy) .
