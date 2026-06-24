############################################################
# Distribution Shape Explorer
############################################################

# https://hbctraining.github.io/Training-modules/RShiny/lessons/shinylive.html
# Run the shinylive::export line to populate the docs folder 
# so that shinylive works from github
#shinylive::export(appdir = "../DistributionShapeExplorer/", destdir = "docs")
#httpuv::runStaticServer("docs/", port = 8008)

ui <- tagList(
  
  tags$head(
    tags$style(HTML("
      #resample {
        background-color: #569BBD;
        color: white;
        border: none;
        padding: 10px 16px;
      }
      #resample:hover {
        background-color: #3E7C99;
        color: white;
      }
      #resample:active {
        transform: scale(0.97);
      }
      .dist-info-table {
        width: 100%;
        border-collapse: collapse;
        margin-top: 18px;
        font-size: 15px;
      }
      .dist-info-table th {
        background-color: #569BBD;
        color: white;
        padding: 8px 14px;
        text-align: left;
        font-weight: 600;
        font-size: 15px;
      }
      .dist-info-table td {
        padding: 10px 14px;
        vertical-align: top;
        border: 1px solid #ddd;
      }
      .dist-info-table tr:nth-child(even) td {
        background-color: #f4f8fb;
      }
    "))
  ),
  
  pageWithSidebar(
    
    headerPanel("Distribution Shape Explorer"),
    
    sidebarPanel(
      selectInput(
        inputId = "dist",
        label = "Distribution:",
        choices = c(
          "Normal" = "norm",
          "Lognormal" = "lnorm",
          "Exponential" = "exp",
          "Uniform" = "unif",
          "Gamma" = "gamma",
          "Beta" = "beta",
          "Binomial" = "binom",
          "Negative Binomial" = "nbinom",
          "Geometric" = "geom",
          "Hypergeometric" = "hyper",
          "Poisson" = "pois"
        ),
        selected = "norm"
      ),
      
      sliderInput("n", "Sample size",
                  min = 10, max = 10000, value = 1000, step = 10),
      
      actionButton("resample", "Resample"),
      
      br(), br(),
      
      checkboxInput("show_sample", "Show simulated data", TRUE),
      checkboxInput("show_theory", "Show theoretical curve/PMF", TRUE),
      
      uiOutput("params"),
      
      br(),
      
      helpText("Moments:"),
      div(textOutput("moments"), style = "font-size:120%;"),
      
      br(),
      
      helpText("Typical use:"),
      div(textOutput("description"), style = "font-size:110%;"),
      
      helpText("_______________________________"),
      helpText("Glenn Tattersall, PhD"),
      helpText("For use in BIOL 3P96 - Biostatistics")
      
    ),
    
    mainPanel(
      plotOutput("plot"),
      htmlOutput("dist_info"),
      br()
    )
  )
)


server <- function(input, output) {
  
  ############################################################
  # Parameter UI
  ############################################################
  
  output$params <- renderUI({
    
    switch(input$dist,
           
           "norm" = tagList(
             sliderInput("mu", "Mean", -20, 20, 0),
             sliderInput("sd", "SD", 0.1, 10, 1, step = 0.1)
           ),
           
           "lnorm" = tagList(
             sliderInput("meanlog", "Meanlog", -2, 3, 0, step = 0.1),
             sliderInput("sdlog", "SDlog", 0.1, 2, 0.5, step = 0.1)
           ),
           
           "exp" = sliderInput("rate", "Rate (λ)", 0.1, 5, 1, step = 0.1),
           
           "unif" = tagList(
             sliderInput("min", "Min", -10, 10, 0),
             sliderInput("max", "Max", -10, 20, 5)
           ),
           
           "gamma" = tagList(
             sliderInput("shape", "Shape", 0.1, 10, 2, step = 0.1),
             sliderInput("rate_g", "Rate", 0.1, 5, 1, step = 0.1)
           ),
           
           "beta" = tagList(
             sliderInput("alpha", "Alpha", 0.1, 10, 2, step = 0.1),
             sliderInput("beta_par", "Beta", 0.1, 10, 2, step = 0.1)
           ),
           
           "binom" = tagList(
             sliderInput("size", "Trials (n)", 1, 100, 20),
             sliderInput("prob", "p", 0, 1, 0.5, step = 0.01)
           ),
           
           "nbinom" = tagList(
             sliderInput("nb_size", "Size", 1, 50, 10),
             sliderInput("nb_prob", "p", 0.01, 0.99, 0.5, step = 0.01)
           ),
           
           "geom" = sliderInput("prob_g", "p", 0.01, 0.99, 0.5, step = 0.01),
           
           "hyper" = tagList(
             sliderInput("N", "Population size", 10, 500, 100),
             sliderInput("K", "Successes in population", 1, 100, 50),
             sliderInput("n_draw", "Sample size", 1, 100, 20)
           ),
           
           "pois" = sliderInput("lambda", "Lambda (λ)", 0.1, 20, 5, step = 0.1)
    )
  })
  
  ############################################################
  # Moments
  ############################################################
  
  output$moments <- renderText({
    
    m <- NA; v <- NA
    
    switch(input$dist,
           "norm" = { m <- input$mu; v <- input$sd^2 },
           "lnorm" = {
             m <- exp(input$meanlog + input$sdlog^2/2)
             v <- (exp(input$sdlog^2)-1)*exp(2*input$meanlog + input$sdlog^2)
           },
           "exp" = { m <- 1/input$rate; v <- 1/input$rate^2 },
           "unif" = { m <- (input$min + input$max)/2; v <- (input$max - input$min)^2/12 },
           "gamma" = { m <- input$shape/input$rate_g; v <- input$shape/input$rate_g^2 },
           "beta" = {
             m <- input$alpha/(input$alpha + input$beta_par)
             v <- (input$alpha*input$beta_par)/((input$alpha+input$beta_par)^2*(input$alpha+input$beta_par+1))
           },
           "binom" = { m <- input$size*input$prob; v <- input$size*input$prob*(1-input$prob) },
           "nbinom" = {
             m <- input$nb_size*(1-input$nb_prob)/input$nb_prob
             v <- input$nb_size*(1-input$nb_prob)/input$nb_prob^2
           },
           "geom" = { m <- (1-input$prob_g)/input$prob_g; v <- (1-input$prob_g)/input$prob_g^2 },
           "hyper" = {
             m <- input$n_draw * (input$K/input$N)
             v <- input$n_draw*(input$K/input$N)*(1-input$K/input$N)*((input$N-input$n_draw)/(input$N-1))
           },
           "pois" = { m <- input$lambda; v <- input$lambda }
    )
    
    paste0("Mean = ", round(m,3), "   |   Variance = ", round(v,3))
  })
  
  ############################################################
  # Descriptions (sidebar — brief)
  ############################################################
  
  output$description <- renderText({
    
    switch(input$dist,
           "norm" = "Symmetric continuous data (e.g., measurement error, heights).",
           "lnorm" = "Right-skewed data (e.g., body size, income, biological traits).",
           "exp" = "Waiting times between random events.",
           "unif" = "All outcomes equally likely over an interval.",
           "gamma" = "Waiting time for multiple events; flexible skew.",
           "beta" = "Proportions or probabilities bounded between 0 and 1.",
           "binom" = "Number of successes in fixed trials (with replacement).",
           "nbinom" = "Overdispersed count data; repeated failures until success.",
           "geom" = "Trials until first success.",
           "hyper" = "Sampling without replacement.",
           "pois" = "Counts of rare events over time/space."
    )
    
  })
  
  ############################################################
  # Data reactive — fires on startup AND on button click
  ############################################################
  
  data_reactive <- reactive({
    
    input$resample   # re-run on button click; also runs once at startup (value = 0)
    
    n    <- input$n
    dist <- input$dist
    
    if (dist == "norm")   req(input$mu, input$sd)
    if (dist == "lnorm")  req(input$meanlog, input$sdlog)
    if (dist == "exp")    req(input$rate)
    if (dist == "unif")   req(input$min, input$max)
    if (dist == "gamma")  req(input$shape, input$rate_g)
    if (dist == "beta")   req(input$alpha, input$beta_par)
    if (dist == "binom")  req(input$size, input$prob)
    if (dist == "nbinom") req(input$nb_size, input$nb_prob)
    if (dist == "geom")   req(input$prob_g)
    if (dist == "hyper")  req(input$K, input$N, input$n_draw)
    if (dist == "pois")   req(input$lambda)
    
    switch(dist,
           "norm"   = rnorm(n,   isolate(input$mu),     isolate(input$sd)),
           "lnorm"  = rlnorm(n,  isolate(input$meanlog), isolate(input$sdlog)),
           "exp"    = rexp(n,    isolate(input$rate)),
           "unif"   = runif(n,   isolate(input$min),    isolate(input$max)),
           "gamma"  = rgamma(n,  isolate(input$shape),  isolate(input$rate_g)),
           "beta"   = rbeta(n,   isolate(input$alpha),  isolate(input$beta_par)),
           "binom"  = rbinom(n,  isolate(input$size),   isolate(input$prob)),
           "nbinom" = rnbinom(n, isolate(input$nb_size), isolate(input$nb_prob)),
           "geom"   = rgeom(n,   isolate(input$prob_g)),
           "hyper"  = rhyper(n,  isolate(input$K), isolate(input$N - input$K), isolate(input$n_draw)),
           "pois"   = rpois(n,   isolate(input$lambda))
    )
    
  })
  
  ############################################################
  # Info table below plot
  ############################################################
  
  output$dist_info <- renderUI({
    
    info <- switch(input$dist,
                   
                   "norm" = list(
                     graph = "The histogram displays the frequency density of the simulated sample, with bar height reflecting how often values fall in each bin relative to the total. The overlaid black curve shows the theoretical Normal probability density function (PDF) defined by your chosen mean and SD — in a large sample, the histogram bars should closely follow this bell-shaped curve.",
                     uses  = "The Normal distribution is the cornerstone of classical statistics and appears whenever many small, independent sources of variation add together (the Central Limit Theorem). In biology and medicine it describes traits like adult height, resting heart rate, and measurement error. It is also the assumed error distribution underlying linear regression and ANOVA, making it one of the most important distributions to recognise in practice.",
                     moments = "The mean (μ) sets the centre of the bell curve — the peak shifts left or right as you change it. The variance (σ²) controls the spread: small variance produces a tall, narrow bell; large variance a wide, flat one. For the Normal distribution the mean and variance are independent parameters you set directly, and together they completely define the distribution's shape and location."
                   ),
                   
                   "lnorm" = list(
                     graph = "The histogram shows the density of simulated values on the original (untransformed) scale, revealing the characteristic right skew of the Lognormal distribution. The black curve is the theoretical Lognormal PDF; notice how the bulk of the distribution sits near zero with a long right tail — a pattern that disappears when the same data are log-transformed, yielding a symmetric Normal distribution.",
                     uses  = "The Lognormal distribution is ubiquitous in biology because many quantities arise from multiplicative processes — each step scales the previous value by a random factor rather than adding to it. Body mass across species, parasite loads, antibiotic minimum inhibitory concentrations, and gene-expression levels all tend to follow Lognormal distributions. If your data span several orders of magnitude and are strictly positive, a Lognormal model is often a sensible first choice.",
                     moments = "The parameters meanlog (μ) and sdlog (σ) are the mean and SD of the distribution on the log scale, not the original scale. The mean on the original scale is exp(μ + σ²/2), which is always larger than the median exp(μ) — this gap grows with σ and reflects the right skew. The variance on the original scale is (exp(σ²) − 1) × exp(2μ + σ²); because it depends on both parameters, increasing either one inflates the spread and pulls the mean further above the median."
                   ),
                   
                   "exp" = list(
                     graph = "The histogram displays the density of simulated waiting times, which pile up near zero and decrease monotonically — a hallmark of the Exponential distribution. The overlaid black curve is the theoretical PDF; the rate parameter λ controls how steeply it decays, with larger λ producing shorter average waits and a sharper drop-off from the y-axis.",
                     uses  = "The Exponential distribution models the time between successive, memoryless random events — meaning the probability of waiting an additional interval is unaffected by how long you have already waited. It is used to describe inter-arrival times of radioactive decay events, the lifespan of electronic components under constant failure risk, time between nerve impulses, and intervals between earthquakes or disease outbreaks. It is the continuous counterpart of the Geometric distribution.",
                     moments = "Both the mean and the standard deviation equal 1/λ, so they are always identical for the Exponential distribution. This means the coefficient of variation (SD/mean) is always exactly 1 — a useful diagnostic: if your waiting-time data have a CV substantially different from 1, a pure Exponential may not fit well. Increasing λ simultaneously shrinks the mean and compresses the spread, while decreasing λ stretches both in tandem."
                   ),
                   
                   "unif" = list(
                     graph = "The histogram should form a roughly flat rectangular profile across the chosen interval, reflecting that every value is equally probable. The black reference line shows the theoretical constant density 1/(max − min); deviations from flatness in any single sample are due to random sampling variation and diminish as sample size increases.",
                     uses  = "The Uniform distribution is most commonly encountered as a theoretical tool rather than a model for naturally occurring data. Random-number generators produce Uniform(0,1) values that are then transformed into other distributions. In biology it can represent a null model of no preference — for example, if an animal chooses a resting location with no habitat preference, position within a transect might be approximately Uniform. It is also used in Bayesian analysis as a non-informative prior.",
                     moments = "The mean is simply the midpoint of the interval, (min + max)/2, reflecting the perfect symmetry of the distribution. The variance is (max − min)²/12; it depends only on the width of the interval, not on where the interval sits on the number line. Widening the interval increases the variance while leaving the shape unchanged — the distribution remains flat regardless of how wide or narrow the interval becomes."
                   ),
                   
                   "gamma" = list(
                     graph = "The histogram and overlaid PDF show a right-skewed, strictly positive distribution whose shape is governed by the shape parameter (α) and rate (λ). When shape = 1 the Gamma reduces to the Exponential; as shape increases the distribution becomes more symmetric and bell-like. Adjusting the sliders lets you see how these parameters stretch or compress the distribution.",
                     uses  = "The Gamma distribution generalises the Exponential to model the waiting time until the α-th event in a Poisson process — for instance, the time until an organism experiences its third parasite encounter. It is widely used to model continuous, positive, right-skewed biological measurements such as rainfall totals, insect development times, and metabolic rates. In Bayesian statistics it serves as the conjugate prior for Poisson rate parameters.",
                     moments = "The mean is shape/rate (α/λ) and the variance is shape/rate² (α/λ²). Increasing the shape parameter while holding rate fixed raises both the mean and the variance, and also makes the distribution more symmetric — you can watch the right skew diminish as shape grows. Increasing the rate while holding shape fixed compresses the distribution toward zero, reducing both mean and variance. Because variance = mean/rate, a higher rate produces less spread relative to the mean."
                   ),
                   
                   "beta" = list(
                     graph = "The histogram and theoretical PDF are both bounded strictly between 0 and 1, reflecting that the Beta distribution models proportions or probabilities. The shape parameters α and β determine whether the distribution is symmetric, skewed left or right, U-shaped, or uniform — experimenting with the sliders illustrates the full flexibility of this family.",
                     uses  = "The Beta distribution is the natural choice whenever your response variable is a proportion constrained to (0, 1). Examples in biology include the proportion of time an animal spends in a particular behaviour, fractional vegetation cover, infection prevalence in a population, and allele frequencies. It is also the foundation of Beta regression for proportion data, and in Bayesian statistics it is the conjugate prior for the binomial success probability p.",
                     moments = "The mean is α/(α + β), so equal values of α and β always give a mean of 0.5 regardless of their magnitude. The variance is α×β / [(α+β)²(α+β+1)]; it shrinks as the total α+β grows, meaning larger values of both parameters concentrate the distribution tightly around its mean even if the mean itself stays fixed. Skewness is positive (right tail) when α < β and negative (left tail) when α > β — try setting them unequal to see the distribution tilt."
                   ),
                   
                   "binom" = list(
                     graph = "Because the Binomial is discrete, the graph uses vertical line segments (a spike plot) rather than bars. Black spikes show the theoretical probability mass function (PMF) — the exact probability of observing each count — while the blue spikes show the relative frequency in your simulated sample. The distribution is centred near n × p and becomes more symmetric as n increases or as p approaches 0.5.",
                     uses  = "The Binomial distribution applies whenever you count the number of successes in a fixed number of independent trials, each with the same success probability p. Classic examples include the number of heads in coin flips, the number of germinating seeds in a tray, the number of infected individuals in a sample, and the number of recessive-phenotype offspring in a genetic cross. It underpins proportion tests, chi-square goodness-of-fit tests, and logistic regression.",
                     moments = "The mean is n × p — the expected number of successes — and the variance is n × p × (1 − p). Variance is maximised when p = 0.5 and shrinks toward zero as p approaches 0 or 1, because extreme probabilities make outcomes highly predictable. For fixed p, both mean and variance scale linearly with n. When np and n(1−p) are both large (roughly ≥ 5), the Binomial is well approximated by a Normal distribution with the same mean and variance."
                   ),
                   
                   "nbinom" = list(
                     graph = "The spike plot displays the PMF (black) and simulated relative frequencies (blue) for the number of failures before the r-th success. Unlike the Binomial, the Negative Binomial has a longer right tail and greater spread, particularly at low p values — this extra variability relative to the Poisson is called overdispersion and is clearly visible when you compare the two distributions side by side.",
                     uses  = "The Negative Binomial distribution is heavily used in ecology and genomics to model count data that are more variable than the Poisson — a situation called overdispersion. Species abundance counts in community surveys, RNA-seq read counts, and the number of parasites per host individual commonly follow Negative Binomial distributions. Many modern statistical packages (e.g., MASS::glm.nb in R) fit Negative Binomial regression as a robust alternative to Poisson regression when overdispersion is detected.",
                     moments = "The mean is r(1 − p)/p and the variance is r(1 − p)/p². Because variance = mean/p and p ≤ 1, the variance always exceeds the mean — this excess over the mean is the overdispersion that distinguishes the Negative Binomial from the Poisson. Decreasing p (lower success probability) inflates both the mean and variance while spreading the distribution rightward; increasing r (more required successes) raises the mean and reduces relative spread, making the distribution more symmetric."
                   ),
                   
                   "geom" = list(
                     graph = "The spike plot shows the probability of needing exactly k failures before the first success, starting at k = 0. The distribution always decreases monotonically from its mode at zero — higher values of p collapse it sharply toward zero, while lower values of p produce a long, flat tail extending far to the right.",
                     uses  = "The Geometric distribution models the number of failed attempts before achieving a first success in a sequence of independent Bernoulli trials. In biology it arises when asking how many unsuccessful foraging attempts a predator makes before capturing prey, how many cell divisions occur before a mutation arises, or how many PCR cycles are needed before the first successful amplification event. It is the discrete analogue of the Exponential distribution and shares the same memoryless property.",
                     moments = "The mean is (1 − p)/p and the variance is (1 − p)/p². Like the Exponential, the standard deviation always exceeds the mean (the CV is 1/√(1−p) ≥ 1), reflecting the long right tail. Both mean and variance blow up as p approaches zero — very rare successes imply a very long and variable wait. As p approaches 1 the distribution collapses onto zero: success is almost certain on the first trial, leaving little spread."
                   ),
                   
                   "hyper" = list(
                     graph = "The spike plot shows the PMF of the number of successes drawn when sampling without replacement from a finite population of size N containing K successes. Unlike the Binomial, the support is bounded on both sides (you cannot draw more successes than exist in the population, nor fewer than the shortfall forces), and the distribution narrows as the sample size approaches the population size.",
                     uses  = "The Hypergeometric distribution applies to sampling without replacement from a finite, binary-classified population — making it more appropriate than the Binomial when the sample is a non-negligible fraction of the population. Applications include mark-recapture studies estimating wildlife population size, quality-control inspection of a finite batch of items, and testing for over-representation of a gene category in a set of differentially expressed genes (as in gene-ontology enrichment analysis using Fisher's exact test).",
                     moments = "The mean is n × (K/N) — identical to the Binomial mean with p = K/N — reflecting the expected proportion of successes in the draw. The variance is n × (K/N) × (1 − K/N) × (N − n)/(N − 1), which is the Binomial variance multiplied by the finite population correction factor (N − n)/(N − 1). This correction is always less than 1 and shrinks toward zero as the sample size n approaches the population size N, capturing the intuition that sampling nearly the whole population leaves almost no uncertainty about the count."
                   ),
                   
                   "pois" = list(
                     graph = "The spike plot displays the Poisson PMF (black) alongside the simulated relative frequencies (blue). The distribution is right-skewed at small λ and becomes increasingly symmetric and Normal-like as λ grows. A key feature is that the mean and variance are both equal to λ — if your observed variance greatly exceeds the mean, overdispersion may be present and a Negative Binomial model may be more appropriate.",
                     uses  = "The Poisson distribution models the count of rare, independent events occurring at a constant average rate over a fixed interval of time or space. Biological applications are numerous: counts of mutations per genome per generation, the number of species detected on a survey plot, red blood cell counts in a haemocytometer grid square, and the number of disease cases reported per week in an epidemiological study. It is the foundation of Poisson regression (GLM with log link), one of the most widely used models for count data in biology.",
                     moments = "The Poisson has the unique property that its mean and variance are both equal to λ — there is only one parameter governing both the location and the spread. This means that if you double the event rate, you simultaneously double the expected count and double the variability. In practice, this equality is a diagnostic tool: if the observed variance in your count data is much larger than the mean (overdispersion) or much smaller (underdispersion), a Poisson model may be inappropriate and alternatives such as the Negative Binomial or Conway-Maxwell-Poisson should be considered."
                   )
    )
    
    HTML(paste0(
      '<table class="dist-info-table">',
      '<tr><th style="width:20%;">What does this graph show?</th>',
      '<td>', info$graph, '</td></tr>',
      '<tr><th>What kinds of data follow this distribution?</th>',
      '<td>', info$uses, '</td></tr>',
      '<tr><th>What do the moments (mean &amp; variance) tell you?</th>',
      '<td>', info$moments, '</td></tr>',
      '</table>'
    ))
    
  })
  
  ############################################################
  # Plot
  ############################################################
  
  output$plot <- renderPlot({
    
    x <- data_reactive()
    
    ##########################################################
    # Continuous
    ##########################################################
    
    if (input$dist %in% c("norm","lnorm","exp","unif","gamma","beta")) {
      
      if (input$show_sample) {
        hist(x, breaks=40, freq=FALSE,
             col="#569BBD", border="white",
             axes=FALSE, main="", xlab="")
      } else {
        plot(0,0,type="n",axes=FALSE,xlab="",ylab="")
      }
      
      axis(1,cex.axis=1.5)
      axis(2,cex.axis=1.5)
      
      if (input$show_theory) {
        curve(
          switch(input$dist,
                 "norm"  = dnorm(x, input$mu, input$sd),
                 "lnorm" = dlnorm(x, input$meanlog, input$sdlog),
                 "exp"   = dexp(x, input$rate),
                 "unif"  = dunif(x, input$min, input$max),
                 "gamma" = dgamma(x, input$shape, input$rate_g),
                 "beta"  = dbeta(x, input$alpha, input$beta_par)
          ),
          add=TRUE, col=1, lwd=2)
      }
      
      abline(h=0)
    }
    
    ##########################################################
    # Discrete
    ##########################################################
    
    else {
      
      if (input$dist=="binom") {
        k <- 0:input$size
        d <- dbinom(k,input$size,input$prob)
      }
      if (input$dist=="nbinom") {
        k <- 0:max(x)
        d <- dnbinom(k,input$nb_size,input$nb_prob)
      }
      if (input$dist=="geom") {
        k <- 0:max(x)
        d <- dgeom(k,input$prob_g)
      }
      if (input$dist=="hyper") {
        k <- max(0, input$n_draw - (input$N - input$K)) : min(input$n_draw, input$K)
        d <- dhyper(k, input$K, input$N - input$K, input$n_draw)
      }
      if (input$dist=="pois") {
        k <- 0:max(x)
        d <- dpois(k,input$lambda)
      }
      
      emp <- as.numeric(table(factor(x, levels=k))) / length(x)
      
      ymax <- max(c(d, emp))
      
      plot(0,0,type="n",
           xlim=c(min(k),max(k)),
           ylim=c(0,ymax),
           axes=FALSE,xlab="",ylab="")
      
      axis(1,cex.axis=1.5)
      axis(2,cex.axis=1.5)
      
      if (input$show_theory) {
        segments(k, 0, k, d, lwd=2)
        points(k, d, pch=16)
      }
      
      if (input$show_sample) {
        segments(k+0.1, 0, k+0.1, emp, col="#569BBD", lwd=2)
        points(k+0.1, emp, col="#569BBD", pch=16)
      }
      
      abline(h=0)
    }
    
  })
  
}

shinyApp(ui, server)