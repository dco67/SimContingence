if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  shiny,
  shinyWidgets,
  shinycssloaders,
  shinyjs,
  rhandsontable,
  dplyr,
  ggplot2,
  stringr,
  BayesFactor,
  reshape2,
  rstatix,
  plotly,
  StanHeaders,
  DescTools,
  RColorBrewer,
  psych
)



# library(shiny)
# library(shinyjs)
# library(rhandsontable)
# library(dplyr)
# library(ggplot2)
## library(DiagrammeR)
# library(stringr)
# library(BayesFactor)
# library(MADPop)       # chi2.stat
# library(InPosition)   # contingency.data.break(): Bootstrap or permutation resampling for contingency tables
# library(reshape2)
# library(rstatix)      # Test de Fisher (fisher_test)
# library(plotly)
# library(shinymeta)

source("global.R")

ui <- fluidPage(
  useShinyjs(),
  titlePanel(title = "Tableau de contingence"),
  tags$head(
    tags$style(HTML("
                    div.MathJax_Display{
                    text-align: left !important;
                    }
    "))
  ),
  withMathJax(),
  tags$div(HTML("<script type='text/x-mathjax-config' >
                MathJax.Hub.Config({
                tex2jax: {inlineMath: [['$','$'], ['\\(','\\)']]}
                });
                </script >
                ")),
  sidebarLayout(
    sidebarPanel(
      fluidRow(
        column(5, selectInput("nRows", "Rangées", choices = 2:6, selected = 2)),
        column(5, selectInput("nCols", "Colonnes", choices = 2:6, selected = 2))
      ),
      fluidRow(
        column(5, radioButtons("typeR",
          "Variable A:",
          choices = c("Nominale", "Ordinale"),
          selected = "Nominale"
        )),
        column(5, radioButtons("typeC",
          "Variable B:",
          choices = c("Nominale", "Ordinale"),
          selected = "Nominale"
        )),
      ),
      helpText("Composez le tableau de contingence ci-dessous. Notez que les données manquantes sont considérées comme nulles."),
      tags$div(style = "line-height:50%;", br()),
      fluidRow(
        column(
          6,
          numericInput("digits",
            "# décimales:",
            value = 3,
            min = 1,
            max = 5,
            step = 1,
            width = "150px"
          )
        ),
        #        column(6,
        #               numericInput(("ns"),
        #                            "# échantillons:",
        #                            value = 10000,
        #                            min = 100,
        #                            max = 1000000,
        #                            step = 1000,
        #                            width = "150px"))
      ),
      rHandsontableOutput("contingencyTable"),
      tags$div(style = "line-height:150%;", br()),
      conditionalPanel(
        "input.nRows == 2 & input.nCols == 2",
        radioButtons("testType",
          h4("Test Statistique"),
          choices = c("Chi-Carré" = "chi-squared", "Fisher" = "fisher"),
          selected = "chi-squared",
          inline = TRUE
        ),
      ),
      actionButton("clearButton", "Ré-initialiser"),
      tags$div(style = "line-height:150%;", br()),
      actionButton("execute", "Exécuter"),
      p(), p(),
      wellPanel(style = "background: lightblue",
                fluidRow(
                  column(4,
                         a(h4("Par Daniel Coulombe, Ph.D.")),
                         p("2022")
                  ),
                  column(4,
                         tags$a(
                           href="https://isteah.org", 
                           tags$img(src="ISTEAH_LOGO.png", 
                                    title="ISTEAH", 
                                    width="160",
                                    height="140")
                         )
                  )
                )
      )
    ),
    mainPanel(
      tabsetPanel(
        tabPanel(
          "Introduction",
          helpText(
            h2("Analyse de Tableaux de Contingence"),
            h4(
              "Cette application permet d'examiner les tableaux de contingence bi-dimensionnels. Elle accompagne ",
              "le chapitre 11 du manuel ",
              tags$a(href = "http://biblio4haiti.ddns.net:8080/proportions-multiples-et-tableaux-de-contingence.html#tableaux-de-contingence-2x2", "Probabilité & Statistiques"), ".", p(),
              "Le tableau de bord, à gauche, contient l'ensemble des entrées nécessaires pour une analyse complète d'un tableau de contingence ",
              "dont la taille est spécifiée par les deux cases supérieures, libellées ", strong("Rangées"), " et ", strong("Colonnes"), ". ",
              "La taille maximale pour chacune des dimensions est 6. Par défaut, un tableau 2x2 est proposé. ", p(),
              "Chacune des variables peut être de nature soit ", strong("Nominale"), ", soit ", strong("Ordinale"), ". Par défaut, elles sont nominales.", p(),
              "La case libellée ", strong("# décimales"), "permet de fixer la précision de l'affichage des résultats d'analyse. Par défaut, ce nombre ",
              "est fixé à 3.", p(),
              " Pour la simulation, on propose 10,000 ré-échantillonnage des tableaux de contingence, ce nombre pouvant être modifié en ",
              "inscrivant le nombre désiré dans la case libellée ", strong("# échantillons"), ". ", p(),
              "Les fréquences observées peuvent ensuite être inscrites dans le tableau.  Pour un tableau 2x2, on peut choisir entre un test ",
              "du $\\chi^2$, ou le calcul de la probabilité exacte en utilisant le test de Fisher, incluant une analyse des rapports ",
              "de cotes. Le bouton libellé ", strong("Ré-initialiser"), "permet de recommencer du début la définition du problème. L'analyse ",
              "est lancée en activant le bouton libellé ", strong("Exécuter"), ".", p(),
              "Dans la partie supérieure de la fenêtre, les différents onglets permettent: ", p(),
              "$\\qquad \\bullet$", strong("Définitions"), ": Production et présentation des fréquences observées, attendues, pourcentages et valeurs résiduelles, ",
              "de même que les définitions de ces différentes quantités.", p(),
              "$\\qquad \\bullet$", strong("Graphiques"), ": Production de diagrammes en barres et de mosaique.", p(),
              "$\\qquad \\bullet$", strong("Théorie"), ": Résumé des éléments théoriques pertinents à ce type de problème.", p(),
              "$\\qquad \\bullet$", strong("Analyse"), ": Analyse détaillée du tableau de contingence fourni.", p(),
              "$\\qquad \\bullet$", strong("Simulation"), ": Présentation de la distribution d'échantillonnage des $\\chi^2$ et des ",
              "inférences qu'elle permet.", p(),
              "$\\qquad \\bullet$", strong("Mise en Oeuvre"), ": Exercices proposés sur le thème.", p(),
            )
          )
        ),
        tabPanel(
          "Définitions",
          #      tags$div(style="line-height:100%;", br()),
          fluidRow(
            column(
              6,
              h4("Fréquences Observées"),
              tags$div(style = "line-height:25%;", br()),
              rHandsontableOutput("observedTable")
            ),
            column(
              6,
              h4("Fréquences Attendues"),
              tags$div(style = "line-height:25%;", br()),
              rHandsontableOutput("cnjTable")
            )
          ),
          tags$div(style = "line-height:150%;", br()),
          fluidRow(
            column(
              6,
              h4("% Rangées"),
              tags$div(style = "line-height:25%;", br()),
              rHandsontableOutput("cndABTable")
            ),
            column(
              6,
              h4("% Colonnes"),
              tags$div(style = "line-height:25%;", br()),
              rHandsontableOutput("cndBATable")
            )
          ),
          tags$div(style = "line-height:150%;", br()),
          fluidRow(
            column(
              6,
              h4("% Total"),
              tags$div(style = "line-height:25%;", br()),
              rHandsontableOutput("cndTOTTable")
            ),
            column(
              6,
              h4("Résidus"),
              tags$div(style = "line-height:25%;", br()),
              rHandsontableOutput("RESTable")
            )
          ),
          tags$div(style = "line-height:150%;", br()),
          fluidRow(
            column(
              6,
              h4("Résidus Std."),
              tags$div(style = "line-height:25%;", br()),
              rHandsontableOutput("RESstdTable")
            ),
            column(
              6,
              h4("Résidus Ajustés Std."),
              tags$div(style = "line-height:25%;", br()),
              rHandsontableOutput("RESadjTable")
            )
          ),
          helpText(
            tags$div(style = "line-height:50%;", br()),
            h4(
              strong("Définitions:"), p(),
              wellPanel(
                "$\\qquad \\bullet \\quad f_{ij}$ : fréquence observée dans la case occupant la rangée $i$ et la colonne $j$;", p(),
                "$\\qquad \\bullet \\quad TotA \\; \\& \\; TotB$ : totaux marginaux pour les variables A et B respectivement:", p(),
                "$\\qquad \\qquad \\circ \\quad TotA=\\displaystyle\\sum_{i=1} ^{n_B} f_{ij}, \\qquad pour \\quad j \\in [1...n_A]$;", p(),
                "$\\qquad \\qquad \\circ \\quad TotB=\\displaystyle\\sum_{i=1} ^{n_A} f_{ij}, \\qquad pour \\quad j \\in [1...n_B]$;", p(),
                "$\\qquad \\bullet \\quad $Fréquences Attendues:", p(),
                "$\\qquad \\qquad \\circ \\quad E_{ij}= \\frac{{TotA_i}{TotB_j}}{N}$", p(),
                "$\\qquad \\bullet $ Proportions", p(),
                "$\\qquad \\qquad \\circ \\quad \\%$ Total = $\\frac{f_{ij}}{N}$ ", p(),
                "$\\qquad \\qquad \\circ \\quad \\%$ Rangées = $ \\frac{f_{ij}}{TotA_i}$", p(),
                "$\\qquad \\qquad \\circ \\quad \\%$ Colonnes = $ \\frac{f_{ij}}{TotB_j}$", p(),
                "$\\qquad \\bullet $ Résidus", p(),
                "$\\qquad \\qquad \\circ \\quad$ Résidus = $O_{ij}-E_{ij}$", p(),
                "$\\qquad \\qquad \\circ \\quad$ Résidus Std = $ \\frac{O_{ij}-E_{ij}}{\\sqrt{E_{ij}}}$", p(),
                "$\\qquad \\qquad \\circ \\quad$ Résidus Ajustés Std  = $ \\frac{(O_{ij}-E_{ij}}{\\sqrt{E_{ij} \\left ( 1-\\frac{TotA_i}{N} \\right )\\left ( 1-\\frac{TotB_j}{N} \\right )}}$;", p()
              ),
              wellPanel(
                helpText(
                  h4(
                    "Dans le cas d'un tableau de contingence 2x2 $($2 rangées et 2 colonnes$)$, le test de Fisher et l'application ",
                    "de la correction de Yates nomment les 4 cellules du tableau de la manière suivante: "
                  )
                ),
                rHandsontableOutput("tbl2x2")
              )
            )
          )
        ),
        navbarMenu(
          "Graphiques",
          tabPanel(
            "Barres",
            plotOutput("bardiag")
          ),
          tabPanel(
            "Barres Superposées",
            plotOutput("barsup")
          ),
          tabPanel(
            "Barres Horizontales",
            plotOutput("barhoriz")
          ),
          tabPanel(
            "Mosaique",
            plotOutput("mosaicdiag")
          )
        ),
        navbarMenu(
          "Théorie",
          tabPanel(
            "Analyse globale",
            fluidRow(
              column(
                6,
                h4("Fréquences Observées"),
                tags$div(style = "line-height:25%;", br()),
                rHandsontableOutput("observedTable2")
              ),
              column(
                6,
                h4("Fréquences Attendues"),
                tags$div(style = "line-height:25%;", br()),
                rHandsontableOutput("cnjTable2")
              )
            ),
            tags$div(style = "line-height:50%;", br()),
            h2("Analyse de tableaux de contingence: Rappel théorique"),
            uiOutput("theory1"),
            plotOutput("chi2Dist")
          ),
          tabPanel(
            "Mesures d'association",
            conditionalPanel(
              condition = "input.typeR == 'Nominale' & input.typeC == 'Nominale'",
              uiOutput("NomNom")
            ),
            conditionalPanel(
              condition = "input.typeR == 'Ordinale' & input.typeC == 'Ordinale'",
              uiOutput("OrdOrd")
            ),
            conditionalPanel(
              condition = "(input.typeR == 'Nominale' & input.typeC == 'Ordinale') | (input.typeR == 'Ordinale' & input.typeC == 'Nominale')",
              uiOutput("NomOrd")
            )
          )
        ),
        tabPanel(
          "Analyse",
          conditionalPanel(
            condition = "input.testType == 'chi-squared'",
            uiOutput("chi2"),
            conditionalPanel(
              condition = "input.nRows == 2 & input.nCols == 2",
              uiOutput("yates")
            ),
            h3("Contributions relatives au $\\chi^2$:"),
            tags$div(style = "line-height:25%;", br()),
            rHandsontableOutput("chi2contrib"),
            tags$div(style = "line-height:50%;", br()),
            h3("Contributions au $G^2$:"),
            rHandsontableOutput("G2contrib"),
            tags$div(style = "line-height:50%;", br()),
            h3("Analyse Fréquentiste:"),
            helpText(
              h4(
                "$H_0:$ Les variables sont indépendantes l'une de l'autre", p(),
                "$H_1:$ Les variables sont associées l'une à l'autre", p(),
              )
            ),
            uiOutput("Freqtst"),
            tags$div(style = "line-height:50%;", br()),
            h3("Analyse Bayésienne:"),
            uiOutput("BayesTest"),
            tags$div(style = "line-height:50%;", br())
          ),
          conditionalPanel(
            condition = "input.testType == 'fisher'",
            uiOutput("fisher"),
            uiOutput("oddsratio")
          )
        ),
        tabPanel(
          "Simulation",
          fluidPage(
            numericInput(
              "k",
              "Nombre d'échantillons",
              min = 1000,
              max = 1000000,
              value = 10000,
              step = 1000,
              width = "200px"
            ),
            selectInput(
              "indice",
              "Statistique",
              choices = c(
                "Chi-Carré" = "chi2.stat",
                "Coefficient Phi" = "Phi",
                "Coefficient Q de Yule" = "YuleQ",
                "Coefficient de Contingence" = "ContCoef",
                "Information Mutuelle" = "MutInf",
                "Inf. Mutuelle Standardisée" = "MutInfStd",
                "V de Cramer" = "CramerV",
                "T de Tschuprow" = "TschuprowT",
                "Gamma de Goodman-Kruskal" = "GoodmanKruskalGamma",
                "Lambda de Goodman-Kruskal (Symétrique)" = "Lambda"
              ),
              selected = "chi2.stat",
              multiple = FALSE,
              width = "350px"
            )
          ),
          actionButton(
            "simule",
            "Lancer la simulation",
            width = "150px"
          ),
          plotOutput("simul") |>
            shinycssloaders::withSpinner(
              type = 1,
              color.background = "white"
            ),
          tags$div(style = "line-height:50%;", br()),
          uiOutput("simInt")
        ),
        navbarMenu(
          "Mise en oeuvre",
          tabPanel(
            "Exercice 1",
            uiOutput("ex1"),
            tabPanel(
              "Exercice 2",
              uiOutput("ex2")
            ),
            tabPanel(
              "Exercice 3",
              uiOutput("ex3")
            ),
            tabPanel(
              "Exercice 4",
              uiOutput("ex4"),
              tabPanel(
                "Exercice 5",
                uiOutput("ex5")
              ),
              tabPanel(
                "Exercice 6",
                uiOutput("ex6")
              ),
              tabPanel(
                "Exercice 7",
                uiOutput("ex7")
              ),
              tabPanel(
                "Exercice 8",
                uiOutput("ex8")
              ),
              tabPanel(
                "Exercice 9",
                uiOutput("ex9")
              ),
              tabPanel(
                "Exercice 10",
                uiOutput("ex10")
              )
            )
          )
        )
      )
    )
  )
)

#  tags$div(style="line-height:200%;", br())

server <- function(input, output, session) {
  values <- reactiveValues(
    tableData = emptyTable,
    observed = emptyTable,
    expected = emptyTable,
    residus = emptyTable,
    residustd = emptyTable,
    resstdaj = emptyTable,
    gtest = emptyTable,
    BFtest = emptyTable,
    simuldat = emptyTable,
    popul = emptyTable
  )

  idxvals <- reactiveValues(
    chi2 = vector(),
    phi = vector(),
    YuleQ = vector(),
    contcoef = vector(),
    mutinf = vector(),
    mutinfstd = vector(),
    Vcram = vector(),
    TschuprowT = vector(),
    GKgamma = vector(),
    GKlambdaSym = vector()
  )

  # Bayes Factor & Sampling plans:
  # https://stats.libretexts.org/Bookshelves/Applied_Statistics/Book%3A_Learning_Statistics_with_R_-_A_tutorial_for_Psychology_Students_and_other_Beginners_(Navarro)/17%3A_Bayesian_Statistics/17.06%3A_Bayesian_Analysis_of_Contingency_Tables

  contBF <- reactive({
    extractBF(contingencyTableBF(as.matrix(values$observed), sampleType = "jointMulti"))
  })

  contBFint <- reactive({
    if (contBF()$bf >= 100) {
      "évidence extrême pour $H_1$"
    } else if (contBF()$bf >= 30 & contBF()$bf < 100) {
      "évidence très élevée pour $H_1$"
    } else if (contBF()$bf >= 10 & contBF()$bf < 30) {
      "évidence élevée pour $H_1$"
    } else if (contBF()$bf >= 3 & contBF()$bf < 10) {
      "évidence modérée pour $H_1$"
    } else if (contBF()$bf > 1 & contBF()$bf < 3) {
      "évidence anectodique pour $H_1$"
    } else if (contBF()$bf == 1) {
      "aucune évidence, ni pour $H_1$, ni pour $H_0$"
    } else if (contBF()$bf >= 1 / 3 & contBF()$bf < 1) {
      "évidence anecdotique pour $H_0$"
    } else if (contBF()$bf >= 0.1 & contBF()$bf < 1 / 3) {
      "évidence modérée pour $H_0$"
    } else if (contBF()$bf >= 1 / 30 & contBF()$bf < 0.1) {
      "évidence élevée pour $H_0$"
    } else if (contBF()$bf > 0.01 & contBF()$bf < 1 / 30) {
      "évidence très élevée pour $H_0$"
    } else if (contBF()$bf < 0.01) {
      "évidence extrême pour $H_0$"
    }
  })


  fmt <- reactive(str_replace_all(string = paste("0.", strrep("0", input$digits)), pattern = " ", replacement = ""))

  output$contingencyTable <- renderRHandsontable({
    tableData <- values$tableData
    rhandsontable(
      tableData,
      colHeaders = paste0(rep("B", ncol(tableData)), 1:ncol(tableData)),
      rowHeaders = paste0(rep("A", nrow(tableData)), 1:nrow(tableData))
    ) %>%
      hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)
  })

  observe({
    if (!is.null(input$contingencyTable)) {
      newTableData <- hot_to_r(input$contingencyTable)
      if (!is.null(newTableData)) {
        isolate(values$tableData <- newTableData)
      }
    }
  })

  observe({
    tableData <- values$tableData
    if (is.null(tableData) || tableData %>%
      unlist() %>%
      sum(na.rm = TRUE) == 0) {
      shinyjs::disable("execute")
    } else {
      shinyjs::enable("execute")
    }
  })

  observe({
    numberOfRows <- as.integer(input$nRows)
    numberOfColumns <- as.integer(input$nCols)

    newTableData <- matrix(data = integer(0), nrow = numberOfRows, ncol = numberOfColumns) %>%
      as_tibble()

    currentTableData <- isolate(values$tableData)
    if (!is.null(currentTableData)) {
      nr <- min(numberOfRows, nrow(currentTableData))
      nc <- min(numberOfColumns, ncol(currentTableData))
      newTableData[1:nr, 1:nc] <- currentTableData[1:nr, 1:nc]
    }

    values$tableData <- newTableData
  })

  observeEvent(input$clearButton, {
    tableData <- values$tableData
    tableData[1:nrow(tableData), 1:ncol(tableData)] <- NA
    values$tableData <- tableData
  })

  observeEvent(input$execute, {
    contingency_table <- values$tableData %>%
      mutate_all(funs(ifelse(is.na(.), 0, .)))
    chisqRes <- chisq.test(contingency_table, correct = FALSE)
    values$observed <- as_tibble(values$tableData)
    values$gtest <- LRT.stat(contingency_table)
    #    values$BFtest <- contingencyTableBF(contingency_table, sampleType =  "jointMulti")
    values$chisqTestResult <- chisq.test(contingency_table, correct = FALSE)
    values$observed <- as.data.frame(chisqRes$observed)
    values$expected <- as.data.frame(chisqRes$expected)
    values$residus <- as.data.frame(chisqRes$residuals)
    values$residustd <- as.data.frame(chisqRes$stdres)
  })



  observeEvent(input$simule, {
    obs <- values$observed
    values$popul <- countsToCases(melt(as.matrix(values$observed)), countcol = "value")
    values$popul[] <- lapply(values$popul, factor)
    values$simuldat <- replicate(
      input$k,
      table(as.data.frame(values$popul[sample(1:sum(values$observed), sum(values$observed), replace = TRUE), ]))
    )
    for (i in 1:input$k) {
      MI <- MutInfStd(values$simuldat[, , i], ver = 4)
      idxvals$chi2[i] <- chi2.stat(values$simuldat[, , i])
      idxvals$phi[i] <- Phi(values$simuldat[, , i])
      idxvals$YuleQ[i] <- Yule(values$simuldat[, , i])
      idxvals$contcoef[i] <- ContCoef(values$simuldat[, , i])
      idxvals$mutinf[i] <- MI$MIxy
      idxvals$mutinfstd[i] <- MI$MIxySTD
      idxvals$Vcram[i] <- CramerV(values$simuldat[, , i])
      idxvals$TschuprowT[i] <- TschuprowT(values$simuldat[, , i])
      idxvals$GKgamma[i] <- GoodmanKruskalGamma(values$simuldat[, , i])
      idxvals$GKlambdaSym[i] <- Lambda(values$simuldat[, , i], direction = "sym")
    }
  })


  observe({
    if (input$nRows == 2 & input$nCols == 2) {
      selch <- c(
        "Chi-Carré" = "chi2.stat",
        "Coefficient Phi" = "Phi",
        "Coefficient Q de Yule" = "YuleQ",
        "Information Mutuelle" = "MutInf",
        "Inf. Mutuelle Standardisée" = "MutInfStd",
        "Gamma de Goodman-Kruskal" = "GoodmanKruskalGamma",
        "Lambda de Goodman-Kruskal (Symétrique)" = "Lambda"
      )
    } else {
      if (input$nRows == input$nCols & input$nRows > 2) {
        selch <- c(
          "Chi-Carré" = "chi2.stat",
          "Coefficient de Contingence" = "ContCoef",
          "Information Mutuelle" = "MutInf",
          "Inf. Mutuelle Standardisée" = "MutInfStd",
          "Gamma de Goodman-Kruskal" = "GoodmanKruskalGamma",
          "Lambda de Goodman-Kruskal (Symétrique)" = "Lambda"
        )
      } else {
        selch <- c(
          "Chi-Carré" = "chi2.stat",
          "V de Cramer" = "CramerV",
          "T de Tschuprow" = "TschuprowT",
          "Information Mutuelle" = "MutInf",
          "Inf. Mutuelle Standardisée" = "MutInfStd",
          "Gamma de Goodman-Kruskal" = "GoodmanKruskalGamma",
          "Lambda de Goodman-Kruskal (Symétrique)" = "Lambda"
        )
      }
    }

    updateSelectInput(session, "indice", choices = selch, selected = "chi2.stat")
  })

  observeEvent(input$simule2, {
    values$popul <- countsToCases(melt(as.matrix(values$observed)), countcol = "value")
    values$simuldat <- replicate(chi2.stat(input$k, table(as.data.frame(values$popul[sample(1:sum(values$observed), sum(values$observed), replace = TRUE), ]))))
  })

  output$tbl2x2 <- renderRHandsontable({
    tbl <- matrix(letters[1:4], nrow = 2, byrow = TRUE)
    tbl <- cbind(tbl, c("a + b", "c + d"))
    tbl <- rbind(tbl, c("a + c", "b + d", "N"))
    rownames(tbl) <- c("A1", "A2", "TotA")
    colnames(tbl) <- c("B1", "B2", "TotB")

    rhandsontable(
      tbl,
      #      colHeaders = c(paste0(rep("B", ncol(expected)), 1:ncol(expected))),
      #      rowHeaders = c(paste0(rep("A", nrow(expected)), 1:nrow(expected))),
      readOnly = TRUE
    ) %>%
      hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE) # %>%
    #      hot_cols(format = fmt())
  })

  output$observedTable <- output$observedTable2 <- renderRHandsontable({
    observed <- values$observed

    observedWithTotals <- observed %>%
      bind_rows(observed %>% summarize_all(funs(sum(., na.rm = TRUE)))) %>%
      mutate(Total = as.integer(rowSums(., na.rm = TRUE))) %>%
      mutate_all(funs(as.integer))

    rhandsontable(
      observedWithTotals,
      colHeaders = c(paste0(rep("B", ncol(observed)), (1:ncol(observed))), "TotA"),
      rowHeaders = c(paste0(rep("A", nrow(observed)), (1:nrow(observed))), "TotB"),
      readOnly = TRUE
    ) %>%
      hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)
  })

  output$cnjTable <- output$cnjTable2 <- renderRHandsontable({
    expected <- values$expected
    rhandsontable(
      expected,
      colHeaders = c(paste0(rep("B", ncol(expected)), 1:ncol(expected))),
      rowHeaders = c(paste0(rep("A", nrow(expected)), 1:nrow(expected))),
      readOnly = TRUE
    ) %>%
      hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE) %>%
      hot_cols(format = fmt())
  })

  output$cndBATable <- output$cndBATable2 <- renderRHandsontable({
    observed <- values$observed

    cndAB <- apply(observed, 2, function(x) x / sum(x))

    rhandsontable(
      cndAB,
      colHeaders = paste0(rep("B", ncol(observed)), 1:ncol(observed)),
      rowHeaders = paste0(rep("A", nrow(observed)), 1:nrow(observed)),
      readOnly = TRUE
    ) %>%
      hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE) %>%
      hot_cols(format = fmt())
  })

  output$cndABTable <- output$cndABTable2 <- renderRHandsontable({
    observed <- values$observed

    cndBA <- t(apply(observed, 1, function(x) x / sum(x)))

    rhandsontable(
      cndBA,
      colHeaders = paste0(rep("B", ncol(observed)), 1:ncol(observed)),
      rowHeaders = paste0(rep("A", nrow(observed)), 1:nrow(observed)),
      readOnly = TRUE
    ) %>%
      hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE) %>%
      hot_cols(format = fmt())
  })

  output$cndTOTTable <- output$cndTOTTable2 <- renderRHandsontable({
    observed <- values$observed

    cndTOT <- observed / sum(observed)

    rhandsontable(
      cndTOT,
      colHeaders = paste0(rep("B", ncol(observed)), 1:ncol(observed)),
      rowHeaders = paste0(rep("A", nrow(observed)), 1:nrow(observed)),
      readOnly = TRUE
    ) %>%
      hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE) %>%
      hot_cols(format = fmt())
  })

  output$RESTable <- output$cndRESTable2 <- renderRHandsontable({
    residus <- values$observed - values$expected

    rhandsontable(
      residus,
      colHeaders = c(paste0(rep("B", ncol(residus)), 1:ncol(residus))),
      rowHeaders = c(paste0(rep("A", nrow(residus)), 1:nrow(residus))),
      readOnly = TRUE
    ) %>%
      hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE) %>%
      hot_cols(format = fmt())
  })

  output$RESstdTable <- output$RESstdTable2 <- renderRHandsontable({
    residuSTD <- values$residus

    rhandsontable(
      residuSTD,
      colHeaders = c(paste0(rep("B", ncol(residuSTD)), 1:ncol(residuSTD))),
      rowHeaders = c(paste0(rep("A", nrow(residuSTD)), 1:nrow(residuSTD))),
      readOnly = TRUE
    ) %>%
      hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE) %>%
      hot_cols(format = fmt())
  })

  output$RESadjTable <- output$RESadjTable <- renderRHandsontable({
    observed <- values$observed
    expected <- values$expected
    N <- sum(observed)
    tr <- rowSums(observed)
    tc <- colSums(observed)

    residuADJ <- (observed - expected) / sqrt(expected * (1 - tr / N) * (1 - tc / N))

    rhandsontable(
      residuADJ,
      colHeaders = c(paste0(rep("B", ncol(residuADJ)), 1:ncol(residuADJ))),
      rowHeaders = c(paste0(rep("A", nrow(residuADJ)), 1:nrow(residuADJ))),
      readOnly = TRUE
    ) %>%
      hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE) %>%
      hot_cols(format = fmt())
  })

  output$bardiag <- renderPlot({
    observed <- as.matrix(values$observed)
    colnames(observed) <- paste0(rep("B", ncol(observed)), 1:ncol(observed))
    rownames(observed) <- paste0(rep("A", nrow(observed)), 1:nrow(observed))

    colors <- brewer.pal(as.integer(input$nRows), "Set2")
    barplot(observed, col = colors, beside = TRUE)
    legend("topleft", legend = row.names(observed), fill = colors)
  })

  output$barsup <- renderPlot({
    observed <- as.matrix(values$observed)
    colnames(observed) <- paste0(rep("B", ncol(observed)), 1:ncol(observed))
    rownames(observed) <- paste0(rep("A", nrow(observed)), 1:nrow(observed))

    colors <- brewer.pal(as.integer(input$nRows), "Set2")
    barplot(observed, col = colors, horiz = FALSE)
    legend("topleft", legend = row.names(observed), fill = colors)
  })

  output$barhoriz <- renderPlot({
    observed <- as.matrix(values$observed)
    colnames(observed) <- paste0(rep("B", ncol(observed)), 1:ncol(observed))
    rownames(observed) <- paste0(rep("A", nrow(observed)), 1:nrow(observed))

    colors <- brewer.pal(as.integer(input$nRows), "Set2")
    barplot(observed, col = colors, horiz = TRUE)
    legend("topleft", legend = row.names(observed), fill = colors)
  })

  output$mosaicdiag <- renderPlot({
    observed <- as.matrix(values$observed)
    colnames(observed) <- paste0(rep("B", ncol(observed)), 1:ncol(observed))
    rownames(observed) <- paste0(rep("A", nrow(observed)), 1:nrow(observed))

    mosaicplot(t(observed),
      main = "Relation entre A et B",
      xlab = "B",
      ylab = "A",
      las = 1,
      border = "chocolate",
      off = 5,
      shade = TRUE,
      type = "pearson",
      cex.axis = 1.2
    )
  })

  output$chi2 <- renderUI({
    res <- values$chisqTestResult
    res2 <- values$gtest
    obs <- values$observed
    #    if(is.na(obs[1, 1])){return(NULL)}
    expect <- values$expected
    nr <- as.integer(input$nRows)
    nc <- as.integer(input$nCols)
    dl <- (nr - 1) * (nc - 1)
    N <- sum(obs)

    txt1 <- matrix(, nrow = nr, ncol = nc)
    txt2 <- matrix(, nrow = nr, ncol = nc)
    for (i in 1:nrow(obs)) {
      for (j in 1:ncol(obs)) {
        txt1[i, j] <- paste0("\\frac{(", round(obs[i, j], input$digits), "-", round(expect[i, j], input$digits), ")^2}{", round(expect[i, j], input$digits), "}")
        txt2[i, j] <- paste0(round(obs[i, j], input$digits), "\\; ln \\left (\\frac{", round(obs[i, j], input$digits), "}{", round(expect[i, j], input$digits), "} \\right )")
      }
    }
    txt1b <- paste0(" = ", paste0(as.vector(txt1), collapse = " + "), " = \\color{red}{", round(res$statistic, input$digits), "}")
    txt2b <- paste0(" = 2 \\left [", paste0(as.vector(txt2), collapse = " + "), "\\right ] = \\color{red}{", round(res2, input$digits), "}")
    withMathJax(
      helpText(
        h3("$\\chi^2$ de Pearson:"),
        h4(
          "$$\\chi_{", res$parameter, "}^2=\\displaystyle \\sum_{i=1} ^R \\displaystyle \\sum_{j=1} ^C \\frac{(O_{ij}-E_{ij})^2}{E_{ij}}$$",
          "$$dl = (R-1)(C-1) $$",
          "$$\\chi_{", res$parameter, "}^2 ", toString(txt1b), ",\\qquad p=\\color{red}{", round(pchisq(res$statistic, dl, lower.tail = FALSE), input$digits), "}$$",
          "$$dl = (", nr, "- 1)(", nc, "-1) = ", res$parameter, "$$"
        ),
        h3("$G^2$: Rapport de vraisemblance:"),
        h4(
          "$$G_{", res$parameter, "}^2=2 \\displaystyle \\sum_{i=1} ^R \\displaystyle \\sum_{j=1} ^C O_{ij}ln \\left (\\frac{O_{ij}}{E_{ij}} \\right)$$",
          "$$dl = (R-1)(C-1) $$",
          "$$G_{", res$parameter, "}^2", toString(txt2b), ",\\qquad p=\\color{red}{", round(pchisq(res2, dl, lower.tail = FALSE), input$digits), "}$$",
          "$$dl = (", nr, "- 1)(", nc, "-1) = ", res$parameter, "$$"
        ),
      )
    )
  })

  output$yates <- renderUI({
    obs <- values$observed
    N <- sum(obs)
    Chi2Cor <- (N * (abs(obs[1, 1] * obs[2, 2] - obs[1, 2] * obs[2, 1]) - N / 2)^2) / prod(c(rowSums(obs), colSums(obs)))
    txtcor <- "$$\\chi^{2}=\\frac{N\\left(\\mid ad-bc \\mid-\\frac{N}{2}\\right)^{2}}{(a+b)(c+d)(a+c)(b+d)}$$"
    txtcor2 <- paste0(
      "$$\\chi^{2}=\\frac{", N, "\\left(\\mid ", obs[1, 1], "\\times ", obs[2, 2], " - ", obs[1, 2], "\\times", obs[2, 1],
      "\\mid-\\frac{", N, "}{2}\\right)^{2}}{(", obs[1, 1], "+", obs[1, 2], ")(", obs[2, 1], "+", obs[2, 2], ")(", obs[1, 1],
      "+", obs[2, 1], ")(", obs[1, 2], "+", obs[2, 2], ")}=\\color{red}{", round(Chi2Cor, input$digits),
      "}, \\qquad p=\\color{red}{", round(pchisq(Chi2Cor, 1, lower.tail = FALSE), input$digits), "}$$"
    )

    helpText(
      withMathJax(
        h3("Correction de Yates $($Continuité$)$:"), p(),
        h4(
          txtcor, p(),
          txtcor2
        )
      )
    )
  })

  output$chi2contrib <- renderRHandsontable({
    res <- values$chisqTestResult
    obs <- values$observed
    #    if(is.na(obs[1, 1])){return(NULL)}
    expect <- values$expected
    nr <- as.integer(input$nRows)
    nc <- as.integer(input$nCols)
    contrib <- ((obs - expect)^2 / expect) / res$statistic

    rhandsontable(
      contrib,
      colHeaders = c(paste0(rep("B", nc), 1:nc)),
      rowHeaders = c(paste0(rep("A", nr), 1:nr)),
      readOnly = TRUE
    ) %>%
      hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE) %>%
      hot_cols(format = fmt())
  })

  output$G2contrib <- renderRHandsontable({
    res <- values$gtest
    obs <- values$observed
    #    if(is.na(obs[1, 1])){return(NULL)}
    expect <- values$expected
    nr <- as.integer(input$nRows)
    nc <- as.integer(input$nCols)
    contrib <- 2 * obs * log(obs / expect) / res

    rhandsontable(
      contrib,
      colHeaders = c(paste0(rep("B", nc), 1:nc)),
      rowHeaders = c(paste0(rep("A", nr), 1:nr)),
      readOnly = TRUE
    ) %>%
      hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE) %>%
      hot_cols(format = fmt())
  })

  output$Freqtst <- renderUI({
    res <- values$chisqTestResult
    #    if(is.null(res)){return(NULL)}
    pval <- round(res$p.value, input$digits)
    helpText(
      withMathJax(
        h4(
          "Pour ", res$parameter, " degré", ifelse(res$parameter > 1, "s", ""), " de liberté, la probabilité d'observer ",
          "une valeur de $\\chi^2$ supérieure ou égale à ", round(res$statistic, input$digits), ", supposant l'indépendance ",
          "entre les deux variables $(H_0)$, est $p = ", pval, "$. Ce résultat conduit au ", ifelse(pval <= 0.05, " rejet  ", " non rejet "),
          "de l'hypothèse nulle selon laquelle les deux variables sont indépendantes, ou non associées, l'une par rapport à l'autre."
        )
      )
    )
  })

  output$BayesTest <- renderUI({
    obs <- as.matrix(values$observed)
    #    if(is.na(obs[1, 1])){return(NULL)}

    helpText(
      withMathJax(
        h4(
          "Le Facteur de Bayes pour ces données est égal à ", round(contBF()$bf, input$digits), " $\\pm$", round(contBF()$error, input$digits), " indiquant une ", contBFint(), "."
        )
      )
    )
  })

  output$chi2Dist <- renderPlot({
    result <- values$chisqTestResult

    statistic <- result$statistic
    if (is.null(statistic) || is.nan(statistic)) {
      return(NULL)
    }

    DF <- result$parameter

    xmax <- max(statistic, 4) * 1.1
    x <- seq(0, xmax, length.out = 10000)
    distribution <- tibble(X = x, Y = dchisq(x, DF))

    title <- paste("= ", round(statistic, input$digits), ", dl = ", DF, ",  p = ", round(result$p.value, input$digits), sep = "")
    title <- substitute(paste(chi^2, title))
    ggplot(distribution, aes(x = X, y = Y)) +
      geom_line() +
      geom_vline(xintercept = statistic, col = "red") +
      xlim(0, xmax) +
      ggtitle(title) +
      xlab(expression(chi^2)) +
      ylab(expression(Densité)) +
      theme_bw() +
      theme(
        title = element_text(size = 20, color = "red"),
        axis.title.x = element_text(size = 14),
        axis.text.x = element_text(size = 16),
        axis.title.y = element_text(size = 14),
        axis.text.y = element_text(size = 16)
      )
  })
  # http://personality-project.org/r/html/Yule.html --- Yule Q
  # https://en.wikipedia.org/wiki/Coefficient_of_colligation

  output$simul <- renderPlot({
    if (is.na(values$popul[1, 1])) {
      return(NULL)
    }

    if (input$indice == "chi2.stat") {
      idx <- "chi2"
    }
    if (input$indice == "Phi") {
      idx <- "phi"
    }
    if (input$indice == "YuleQ") {
      idx <- "YuleQ"
    }
    if (input$indice == "ContCoef") {
      idx <- "contcoef"
    }
    if (input$indice == "MutInf") {
      idx <- "mutinf"
    }
    if (input$indice == "MutInfStd") {
      idx <- "mutinfstd"
    }
    if (input$indice == "CramerV") {
      idx <- "Vcram"
    }
    if (input$indice == "TschuprowT") {
      idx <- "TschuprowT"
    }
    if (input$indice == "GoodmanKruskalGamma") {
      idx <- "GKgamma"
    }
    if (input$indice == "Lambda") {
      idx <- "GKlambdaSym"
    }

    intc <- eval(str2lang(paste0("quantile(idxvals$", idx, ", c(0.025, 0.975))")))

    gr1 <- eval(str2lang(paste0("hist(as.numeric(idxvals$", idx, "), breaks = 'FD', plot = FALSE)")))
    cuts <- cut(gr1$breaks, c(-Inf, intc[1], intc[2], Inf))
    cols <- c("red", "cadetblue1", "red")
    plot(gr1,
      main = paste0("Distribution des ", input$indice),
      cex.main = 1.5,
      xlab = input$indice,
      ylab = "Densité",
      cex.lab = 1.2,
      #         xlim = c(0, quantile(simdat, 0.999)),
      freq = FALSE,
      col = cols[cuts]
    )

    if (idx == "chi2") {
      curve(dchisq(x,
        df = values$chisqTestResult$parameter
      ),
      from = 0,
      to = quantile(idxvals$chi2, 0.999),
      n = 1000,
      add = TRUE,
      col = "red",
      lwd = 2
      )
    }
    #   abline(v=mean(simdat),
    #           lty = 2,
    #           lwd = 2,
    #           col = "red")
  })

  output$simInt <- renderUI({
    if (is.na(values$popul[1, 1])) {
      return(NULL)
    }

    if (input$indice == "chi2.stat") {
      idx <- "$\\chi^2$"
    }
    if (input$indice == "Phi") {
      idx <- "$\\phi$"
    }
    if (input$indice == "YuleQ") {
      idx <- "Q de Yule"
    }
    if (input$indice == "ContCoef") {
      idx <- "Coef. de Contingence"
    }
    if (input$indice == "MutInf") {
      idx <- "Information Mutuelle"
    }
    if (input$indice == "MutInfStd") {
      idx <- "Inf. Mutuelle Std"
    }
    if (input$indice == "CramerV") {
      idx <- "V de Cramer"
    }
    if (input$indice == "TschuprowT") {
      idx <- "TschuprowT"
    }
    if (input$indice == "GoodmanKruskalGamma") {
      idx <- "$\\gamma$ de Goodman-Kruskal"
    }
    if (input$indice == "Lambda") {
      idx <- "$\\lambda$ de Goodman-Kruskal"
    }

    res <- values$chisqTestResult
    pval <- round(res$p.value, input$digits)
    if (input$indice == "chi2.stat") {
      ci <- quantile(idxvals$chi2, c(0.025, 0.975))
    }
    if (input$indice == "Phi") {
      ci <- quantile(idxvals$phi, c(0.025, 0.975))
    }

    helpText(
      withMathJax(
        h3("Interprétation:"),
        h4(
          if (input$indice == "chi2.stat") {
            paste0(
              "Ce diagramme présente la distribution des $\\chi^2$ obtenus pour ", input$k, " ré-échantillons des données originales.", p(),
              "La courbe en rouge représente la distribution des $\\chi^2$ pour $dl = $", res$parameter, ", supposant $H_0$ vraie. Si les fréquences ",
              "observées dans chacune des cases étaient égales, la distribution des $\\chi^2$ observés $($l'histogramme$)$ suivrait de ",
              "très près cette courbe. On pourra vérifier ce fait en introduisant des fréquences égales dans le tableau de contingence ",
              "du tableau de bord.  Plus l'association entre les deux variables est grande, plus les écarts entre la distribution empirique ",
              "et la courbe théorique sont importants.",
              "La moyenne de la distribution empirique, $\\mu_{\\chi^2} =", round(mean(idxvals$chi2), input$digits), "$  est approximativement égale au ",
              " $\\chi^2$ observé pour le tableau de contingence original, plus le nombre de degrés de liberté: $\\chi_{", res$parameter, "}^2 + dl = ",
              round(res$statistic, input$digits), " + ", res$parameter, " = ", round(res$statistic + res$parameter, input$digits), "$.  ",
              "En effet, la distribution d'échantillonnage suit la distribution ", strong("non-centrale "), "du $\\chi^2$, dont la moyenne est égale au ",
              "nombre de degrés de liberté, $dl$, plus le paramètre de non-centralité, $\\lambda$.  Ce dernier paramètre réflète l'écart entre ",
              "la distribution du $\\chi^2$ et la distribution d'échantillonnage du $\\chi^2$ sous $H_1$. ",
              "Rappelons que le nombre de degrés de liberté pour ",
              "la distribution du $\\chi^2$ est égal à la moyenne de cette dernière.  Ainsi, pour un tableau de contingence dans lequel toutes les fréquences ",
              "observées sont égale, on trouvera $\\chi^2 \\approx 0$ et $\\mu_{\\chi^2}=", res$parameter, "$. Tout écart entre fréquences observées et fréquences théorique ",
              "se traduira par un déplacement de la distribution vers la droite, maintenant une différence égale au nombre de degrés de liberté entre ",
              "le $\\chi^2$ obtenu pour l'échantillon initial et la moyenne de la distribution des $\\chi^2$ obtenue par ré-échantillonnage.",
              "L'erreur standard de la distribution, $\\sigma_{\\chi^2}=", round(sd(idxvals$chi2), input$digits), "$ est une indication de l'étendue des valeurs ",
              "de $\\chi^2$ qu'on est susceptible de rencontrer pour un tel tableau de contingence. Formellement, pour un niveau ",
              "de confiance fixé à 95%, on trouve: ",
              "$$C \\left [", round(ci[1], input$digits), "\\le \\chi^2 \\le ", round(ci[2], input$digits), "\\right ] = 95\\%$$"
            )
          },
          if (input$indice == "Phi") {
            paste0(
              "Ce diagramme présente la distribution des $\\phi$ obtenus pour ", input$k, " ré-échantillons des données originales.  ",
              "La moyenne de la distribution empirique, $\\mu_{\\phi} =", round(m <- mean(idxvals$phi), input$digits), "$ correspond au degré ",
              "d'association observé pour le tableau de contingence étudié. Cette valeur devrait être approximativement égale au coefficient $\\phi$ ",
              "calculé à l'aide de la définition formelle de cet indice. ",
              "Cette quantité est indicatrice d'une relation ",
              if (m < 0.1) {
                "faible"
              } else if (m > 0.1 & m < 0.3) {
                "modérée"
              } else {
                "forte"
              }, ". ",
              "L'erreur standard de la distribution, $\\sigma_{\\phi}=", round(sd(idxvals$phi), input$digits), "$ est une indication de l'étendue des valeurs ",
              "de $\\phi$ qu'on est susceptible de rencontrer pour un tel tableau de contingence. Formellement, pour un niveau ",
              "de confiance fixé à 95%, on trouve: ",
              "$$C \\left [", round(ci[1], input$digits), "\\le \\phi \\le ", round(ci[2], input$digits), "\\right ] = 95\\%$$"
            )
          }
        )
      )
    )
  })

  #    simuldat <- replicate(1000, chi2.stat(contingency.data.break(obs, boot = FALSE)))
  # https://stats.stackexchange.com/questions/303939/bootstrap-resampling-for-contingency-table
  #    Categorical data
  #    R has a built-in permutation procedure for a contingency test of association when both of
  #    two variables are categorical (call them A1 and A2). To apply it, execute the usual command
  #    for the χ2 contingency test, but set the simulate.p.value option to TRUE. The number of
  #    replicates in the permutation is set by the option B (default is 2000). Each permutation
  #    rearranges the values in the contingency table while keeping all the row and column totals
  #    fixed to their observed values.
  #
  #    chisq.test(A1, A2, simulate.p.value = TRUE, B = 5000)

  output$fisher <- renderUI({
    # https://www.reneshbedre.com/blog/fisher-exact-test.html
    # http://vassarstats.net/odds2x2.html
    # https://rpkgs.datanovia.com/rstatix/reference/fisher_test.html
    # Astuce: convert table to long format:  m <- melt(Table)
    #         convert result of above to individual observations:  out <- m[rep(sequence(nrow(m)), m[["value"]]), ]
    # ou:  library(splitstackshape)
    #    m <- melt(Table)
    #   expandRows(m, "value")


    obs <- values$observed
    tr <- rowSums(obs)
    taux1 <- obs[1:2, 1] / tr
    taux2 <- obs[1:2, 2] / tr
    rr1 <- taux1[1] / taux1[2]
    compr1 <- ifelse(rr1 < 1, " moins ", " plus ")
    rr2 <- taux2[1] / taux2[2]
    compr2 <- ifelse(rr2 < 1, " plus ", " moins ")
    cote1 <- obs[1, 2] / obs[1, 1]
    cmpcote1 <- ifelse(cote1 < 1, " moins ", " plus ")
    cote2 <- obs[2, 2] / obs[2, 1]
    cmpcote2 <- ifelse(cote2 < 1, " moins ", " plus ")
    OR <- cote1 / cote2
    cmpOR <- ifelse(OR < 1, " petite ", " grande ")
    LOR <- log(OR)
    sLOR <- sqrt(sum(1 / obs))
    ME_LOR <- 1.96 * sLOR
    fishertst <- fisher.test(obs,
      alternative = "two.sided",
      conf.int = TRUE,
      conf.level = 0.95
    )
    oddsR <- odds.ratio(obs)

    withMathJax(
      helpText(
        h3(strong("Le test de Fisher")), p(),
        h4(
          "Le test de Fisher produit la probabilité d'obtenir un tableau de contingence donné ou un tableau ",
          "plus extrême obtenu en réduisant la plus petite fréquence d'une unité jusqu'à ce qu'elle atteigne 0, tout en ",
          "maintenant constants les totaux marginaux. Cette probabilité provient d'une distribution hypergéométrique qui, dans le cas présent, ",
          "est définie par: ", p(),
          "$$p=\\frac{C_{b}^{a+b} C_{d}^{c+d}}{C_{b+d}^{n}}=\\frac{(a+b) !(c+d) !(a+c) !(b+d) !}{a ! b ! c ! d ! N !}$$",
          "Pour le tableau sous analyse, p = ", round(fishertst$p.value, input$digits), ". Il semble donc qu'il ", ifelse(fishertst$p.value > 0.05, " n'y a pas d'", " y a une "),
          "association entre les deux variables. De ces observations, on trouve:", p(),
          "$$\\qquad \\bullet \\quad Taux_{A}= P(B|A)=\\frac{P(A \\cap B)}{P(A)}=\\frac{b}{a+b}$$",
          "$$\\qquad \\qquad \\circ \\quad Taux_{A_1}=\\frac{", obs[1, 1], "}{", tr[1], "}=", round(taux1[1], input$digits), "$$",
          "$$\\qquad \\qquad \\circ \\quad Taux_{A_2}=\\frac{", obs[2, 1], "}{", tr[2], "}=", round(taux1[2], input$digits), "$$",
          "Ces résultats font partie du tableau libellé ", strong("% Rangées"), " sous l'onglet ", strong("Définitions"),
          ". Ils indiquent que la probabilité que $B_1$ survienne étant donné $A_1$ est égale à ", round(taux1[1], 5),
          "alors que la probabilité que $B_1$ survienne étant donné $A_2$ est égale à ", round(taux2[2], input$digits), ". ", p(),
          "$\\qquad \\bullet \\quad$", strong("Risque Relatif $($RR$)$"), ":", p(),
          "Le rapport entre ces taux est le ", strong("Risque Relatif $($RR$)$"), ", défini par:", p(),
          "$$\\qquad \\qquad \\circ \\quad RR_{B_1}=\\frac{P(B_1|A_1)}{P(B_2|A_1)}=\\frac{\\frac{b}{a+b}}{\\frac{d}{c+d}}=\\frac{b(c+d)}{d(a+b)}$$",
          "$$\\qquad \\qquad \\circ \\quad RR_{B_2}=\\frac{P(B_2|A_1)}{P(B_1|A_1)}={\\frac{a}{a+b}}{\\frac{c}{c+d}}=\\frac{a(c+d)}{c(a+b)}$$",
          "Notez que $RR_{B_1} \\neq RR_{B_2}$!", p(),
          "Pour le tableau à l'étude, on trouve:", p(),
          "$$RR_{B_1}=\\frac{", round(taux1[1], input$digits), "}{", round(taux1[2], input$digits), "}= ", round(rr1, input$digits), "$$",
          "$$RR_{B_2}=\\frac{", round(taux2[1], input$digits), "}{", round(taux2[2], input$digits), "}= ", round(rr2, input$digits), "$$",
          "Ces résultats indiquent qu'il y a ", ifelse(rr1 < 1, paste0("$\\frac{1}{", round(rr1, input$digits), "}= $"), ""),
          ifelse(rr1 < 1, round(1 / rr1, input$digits), round(rr1, input$digits)), "fois ", compr1, " de chance que $B_1$ survienne pour $A_1$ que pour $A_2$, ",
          "et qu'il y a ", ifelse(rr2 < 1, paste0("$\\frac{1}{", round(rr2, input$digits), "}= $"), ""),
          ifelse(rr2 < 1, round(1 / rr2, input$digits), round(rr2, input$digits)), "fois ", compr2, " de chance que $B_2$ survienne pour $A_1$ que pour $A_2$. ", p(),
          "$\\qquad \\bullet \\quad$", strong("Cotes $($Odds$)$"), ":", p(),
          "Une cote est le rapport entre les fréquences observées sous $B_1$ et $B_2$, pour chaque niveau de A: ", p(),
          "$$\\qquad \\qquad \\circ \\quad Cote_{A_1}=\\frac{b}{a}$$",
          "$$\\qquad \\qquad \\circ \\quad Cote_{A_2}=\\frac{d}{c}$$",
          "Pour le tableau à l'étude, on trouve:", p(),
          "$$Cote_{A_1}=\\frac{", obs[1, 2], "}{", obs[1, 1], "}= ", round(obs[1, 2] / obs[1, 1], input$digits), "$$",
          "$$Cote_{A_2}=\\frac{", obs[2, 2], "}{", obs[2, 1], "}= ", round(obs[2, 2] / obs[2, 1], input$digits), "$$", p(),
          "Ces résultats indiquent que pour $A_1$, il y a ",
          ifelse(cote1 < 1, round(1 / cote1, input$digits), round(cote1, input$digits)), "fois ", cmpcote1, " de chance que $B_2$ survienne, par rapport à $B_1$",
          "alors que pour $A_2$, il y a ",
          ifelse(cote2 < 1, round(1 / cote2, input$digits), round(cote2, input$digits)), "fois ", cmpcote2, " de chance que $B_2$ survienne, par rapport à $B_1$.", p(),
          "$\\qquad \\bullet \\quad$", strong("Rapport de Cotes $($Odds Ratio$)$"), ":", p(),
          "Finalement, le rapport entre les cotes fournit un indice important: le ", strong("Rapport de Cotes"), "Formellement, ce rapport est défini par: ", p(),
          "$$RC=\\frac{\\frac{b}{a}}{\\frac{d}{c}}=\\frac{b c}{a d}$$",
          "Pour les données analysées, on trouve:", p(),
          "$$RC=\\frac{\\frac{", obs[1, 2], "}{", obs[1, 1], "}}{\\frac{", obs[2, 2], "}{", obs[2, 1], "}}=\\frac{", obs[1, 2], "\\times ", obs[2, 1], "}{", obs[1, 1], "\\times", obs[2, 2], "}=", round(OR, input$digits), "$$",
          "Ce résultat particulier indique que la proportion des observations dans la case $A_1B_2$ est ",
          ifelse(OR < 1, round(1 / OR, input$digits), round(OR, input$digits)), " fois plus ", cmpOR,
          " que la proportion des observations dans la case $A_2B_2$.", p(),
          "Le logarithme naturel du rapport de cotes est une quantité normalement distribuée avec $\\mu_{ln(RC)}=0$, et une erreur-standard égale à:", p(),
          "$$\\sigma_{\\ln (RC)}=\\sqrt{\\frac{1}{a}+\\frac{1}{b}+\\frac{1}{c}+\\frac{1}{d}}$$",
          "ce qui permet les inférences concernant le rapport de cotes. Pour les données analysées, on obtient:", p(),
          "$$\\sigma_{\\ln (RC)}=\\sqrt{\\frac{1}{", obs[1, 1], "}+\\frac{1}{", obs[1, 2], "}+\\frac{1}{", obs[2, 1], "}+\\frac{1}{", obs[2, 2], "}}=", round(sLOR, input$digits), "$$",
          "Ainsi, l'intervalle de confiance pour le $ln(RC)$ est:", p(),
          "$$C \\left [", round(LOR, input$digits), " - 1.96(", round(sLOR, input$digits), ")\\leq ln(RC) \\leq ", round(LOR, input$digits), " + 1.96(", round(sLOR, input$digits), ")\\right ] = 95 \\%$$",
          "$$C \\left [", round(LOR - ME_LOR, input$digits), "\\leq ln(RC) \\leq ", round(LOR + ME_LOR, input$digits), "\\right ] = 95 \\%$$", p(),
          "L'anti-logarithme $e^{Limite}$ permet de traduire cet intervalle de confiance en terme du rapport de cotes:", p(),
          "$$C \\left [e^{RCinf} \\leq RC \\leq e^{RCsup} \\right ] = 95 \\%$$", p(),
          "$$C \\left [", round(exp(LOR - ME_LOR), input$digits), " \\leq RC \\leq", round(exp(LOR + ME_LOR), input$digits), " \\right ] = 95 \\%$$", p(),
        )
      )
    )
  })
  output$selCase <- renderUI({
    fluidRow(
      column(
        2,
        sliderInput("row",
          "Rangée",
          min = 1,
          max = as.integer(input$nRows),
          value = 1,
          step = 1
        )
      ),
      column(
        2,
        sliderInput("col",
          "Colonne",
          min = 1,
          max = as.integer(input$nCols),
          value = 1,
          step = 1
        )
      )
    )
  })

  output$selCase2 <- renderUI({
    fluidRow(
      column(
        2,
        sliderInput("row2",
          "Rangée",
          min = 1,
          max = as.integer(input$nRows),
          value = 1,
          step = 1
        )
      ),
      column(
        2,
        sliderInput("col2",
          "Colonne",
          min = 1,
          max = as.integer(input$nCols),
          value = 1,
          step = 1
        )
      )
    )
  })

  output$theory1 <- renderUI({
    observed <- values$observed
    obsTot <- observed %>%
      bind_rows(observed %>% summarize_all(funs(sum(., na.rm = TRUE)))) %>%
      mutate(Total = as.integer(rowSums(., na.rm = TRUE))) %>%
      mutate_all(funs(as.integer))
    Fe <- values$expected

    withMathJax(
      tags$div(HTML("<script type='text/x-mathjax-config' >
                MathJax.Hub.Config({
                tex2jax: {inlineMath: [['$','$'], ['\\(','\\)']]}
                });
                </script >
                ")),
      tags$div(style = "line-height:50%;", br()),
      helpText(
        p(),
        h4(
          "L'analyse de tableaux de contingence vise à établir le degré d'association pouvant exister entre ",
          "deux variables qualitatives $($nominales ou ordinales$)$. Elle consiste à mettre en rapport les fréquences ",
          "observées, aux fréquences que l'on s'attend d'obtenir, s'il n'y a aucune association entre ces variables."
        ), p(),
        h4(
          "Les fréquences attendues sont définies par le produit des totaux marginaux, divisé par le nombre total d'observations. ",
          "Par exemple, pour le tableau étudié, la fréquence attendue dans la case $A_1 B_1$ est:", p(),
          "$$E_{11}=\\frac{", obsTot[1, ncol(obsTot)], "\\times", obsTot[nrow(obsTot), 1], "}{", sum(observed), "}=", round(values$expected[1, 1], input$digits), "$$", p(),
          "L'évaluation de l'écart pouvant exister entre les fréquences observées $O_{ij}$ et les fréquences attendues $E_{ij}$ se fait ",
          "à l'aide du $\\chi^2$, défini par:", p(),
          "$\\chi_{", values$chisqTestResult$parameter, "}^2=\\displaystyle \\sum_{i=1} ^R \\displaystyle \\sum_{j=1} ^C \\frac{(O_{ij}-E_{ij})^2}{E_{ij}}$, avec $dl = (R-1)(C-1) $", p(),
          "Le $G^2$, ou ", strong("Rapport de Vraisemblance"), ", est une alternative au $\\chi^2$, très utile pour des tableaux de contingence multidimensionnels. Il est défini par:", p(),
          "$G_{", values$chisqTestResult$parameter, "}^2=2 \\displaystyle \\sum_{i=1} ^R \\displaystyle \\sum_{j=1} ^C O_{ij}ln \\left (\\frac{O_{ij}}{E_{ij}} \\right)$, avec $dl = (R-1)(C-1) $", p(),
          "Dans tous les cas, la probabilité d'obtenir une valeur de $\\chi^2$ $($ ou de $G^2)$ égale ou plus extrême que celui observé s'obtient à partir ",
          "de la distribution du $\\chi^2$ pour le nombre de degrés de liberté approprié. La figure reproduite ci-dessous ",
          "illustre la distribution appropriée pour les données sous analyse."
        ), p(),
        if (input$nRows == 2 & input$nCols == 2) {
          helpText(
            h4(
              "Lorsque le tableau de contingence ne comporte que 2 rangées et 2 colonnes, la correction de Yates peut s'appliquer:", p(),
              "$$\\chi^{2}=\\frac{N\\left(\\mid ad-bc \\mid-\\frac{N}{2}\\right)^{2}}{(a+b)(c+d)(a+c)(b+d)}$$", p(),
              "où {a, b, c, d} correspondent aux fréquences observées dans chacune des cellules. Cette correction se traduit ",
              "généralement par une valeur plus faible du $\\chi^2$. ", p(),
              "Finalement, lorsque les fréquences observées sont faibles, le test de Fisher permet le calcul de la probabilité exacte ",
              "d'obtenir le tableau de contingence ou un tableau plus extrême. Cette probabilité est obtenue à partir de la distribution ",
              "hypergéométrique:", p(),
              "$$p=\\frac{C_{b}^{a+b} C_{d}^{c+d}}{C_{b+d}^{n}}=\\frac{(a+b) !(c+d) !(a+c) !(b+d) !}{a ! b ! c ! d ! N !}$$"
            )
          )
        }
      )
    )
  })

  output$NomNom <- renderUI({

    # https://en.wikipedia.org/wiki/Mutual_information
    # https://en.wikipedia.org/wiki/Entropy_(information_theory)
    # https://fr.wikipedia.org/wiki/Information_mutuelle
    # https://boowiki.info/art/theorie/information-mutuelle.html
    # https://www.techno-science.net/definition/6367.html
    # library(DescTools): Assocs()
    # https://quantdare.com/what-is-mutual-information/#:~:text=Properties%20of%20Mutual%20Information,x)%E2%8B%85P(y) ::: TZ
    # http://www.scholarpedia.org/article/Mutual_information
    # https://www.stat.cmu.edu/~cshalizi/dm/20/lectures/09/lecture-09.html
    # https://medium.com/@nyablk97/contingency-tables-in-r-3a5219f5cd38 ---- T de Tschuprow et V de Cramer
    # https://www.tidymodels.org/learn/statistics/xtabs/

    obs <- as.matrix(values$observed)
    nr <- as.integer(input$nRows)
    nc <- as.integer(input$nCols)

    if (nr == 2 & nc == 2) {
      txt1 <- paste0("$\\phi = $", round(Phi(obs), input$digits), "$\\qquad Q = $", round(Yule(obs), input$digits))
    }
    if (nr != 2 & nr == nc) {
      txt1 <- paste0("$C = $", round(ContCoef(obs), input$digits))
    }
    if (nr != nc) {
      txt1 <- paste0("$V = $", round(CramerV(obs), input$digits), "$\\qquad T = $", round(TschuprowT(obs), input$digits))
    }

    withMathJax(
      helpText(
        h4(
          "La valeur observée du $\\chi^2$ ne peut que nous renseigner sur l'association pouvant exister entre les deux variables. ",
          "Du fait que cette quantité dépend non seulement des écarts entre fréquences observées et attendue, mais aussi de la taille ",
          "de l'échantillon, elle ne peut être considérée comme indicatrice de la ", strong("vigueur"), " ou la ", strong("force"), " de cette association, ",
          "si elle existe. ", p(),
          "Lorsque les deux variables étudiées sont ", strong("nominales"), " plusieurs mesures du degré d'association sont disponibles. Certaines ",
          "s'obtiennent à partir du $\\chi^2$ calculé:", p(),
          "$\\qquad \\bullet \\quad$ Coefficient $\\phi$ et coefficient Q de Yule: pour R = C = 2:", p(),
          "$$\\qquad \\qquad \\circ \\; \\phi=\\frac{|a d-b c|}{\\sqrt{(a+b)(c+d)(a+c)(b+d)}} $$",
          "$$\\qquad \\qquad \\circ \\; Q=\\frac{a d-b c}{a d + b c} $$",
          "$\\qquad \\bullet \\quad$ Coefficient de Contingence: pour R = C:", p(),
          "$$\\qquad \\qquad \\circ \\; C=\\sqrt{\\frac{\\chi^2}{N+\\chi^2}} $$", p(),
          "$\\qquad \\bullet \\quad$ V de Cramer et T de Tschuprow: pour $R \\neq C$:", p(),
          "$$\\qquad \\qquad \\circ \\; V=\\sqrt{\\frac{\\chi^2}{N [min(R, C) - 1]}} $$", p(),
          "$$\\qquad \\qquad \\circ \\; T=\\sqrt{\\frac{\\chi^2/N}{\\sqrt{(R-1)(C-1)}}} $$", p(),
          "$\\qquad \\qquad * \\;$ Note: V et T coincident lorsque R = C", p(),
          "Le barème généralement utilisé pour l'interprétation de ces indices est:", p(),
          "$\\qquad idx < 0.10 $:  Association faible", p(),
          "$\\qquad 0.11 < idx < 0.30 $:  Association modérée", p(),
          "$\\qquad idx > 0.31 $:  Association forte", p(),
          "D'autres sont des mesures de la ", strong("réduction proportionnelle de l'erreur"), "dans la prédiction d'une variable ", strong("dépendante"),
          "connaissant la valeur d'une variable ", strong("indépendante"), ". On trouvera les détails relatifs à ces indices à la ",
          "section 11.5.3.1.4 du manuel ",
          tags$a(href = "http://biblio4haiti.ddns.net:8080/proportions-multiples-et-tableaux-de-contingence.html", "Probabilité & Statistiques"), p(),
          "$\\qquad \\bullet \\quad \\tau$ de Goodman & Kruskal  ", p(),
          "$\\qquad \\bullet \\quad \\lambda$ de Goodman & Kruskal  ", p(),
          "Le $\\lambda$ de Goodman-Kruskal permet l'examen d'associations symétriques $($aucune variable n'est considérée dépendante/indépendante$)$ ou ",
          "directionnelles $($une des variables est dépendante, l'autre indépendante$)$. Le $\\tau$ ne permet que des associations directionnelles. ", p(),
          "Pour le tableau de contingence à l'étude, on obtient: ", p(),
          txt1, p(),
          "$\\tau$ de Goodman-Kruskal $($R|C$)$ = ", round(GoodmanKruskalTau(obs, direction = "row"), input$digits), p(),
          "$\\tau$ de Goodman-Kruskal $($C|R$)$ = ", round(GoodmanKruskalTau(obs, direction = "column"), input$digits), p(),
          "$\\lambda$ de Goodman-Kruskal $($Symétrique$)$ = ", round(Lambda(obs, direction = "sym"), input$digits), p(),
          "$\\lambda$ de Goodman-Kruskal $($R|C$)$ = ", round(Lambda(obs, direction = "row"), input$digits), p(),
          "$\\lambda$ de Goodman-Kruskal $($C|R$)$ = ", round(Lambda(obs, direction = "column"), input$digits), p(),
          "Information Mutuelle = ", round(MutInf(obs), input$digits)
        )
      )
    )
  })

  output$OrdOrd <- renderUI({
    obs <- as.matrix(values$observed)
    nr <- as.integer(input$nRows)
    nc <- as.integer(input$nCols)

    withMathJax(
      helpText(
        h4(
          "La valeur observée du $\\chi^2$ ne peut que nous renseigner sur l'association pouvant exister entre les deux variables. ",
          "Du fait que cette quantité dépend non seulement des écarts entre fréquences observées et attendue, mais aussi de la taille ",
          "de l'échantillon, elle ne peut être considérée comme indicatrice de la ", strong("vigueur"), " ou la ", strong("force"), " de cette association, ",
          "si elle existe. ", p(),
          "Lorsque les deux variables étudiées sont ", strong("ordinales"), " plusieurs mesures du degré d'association sont disponibles. Ces mesures tiennent ",
          "compte du fait que les catégories de chacune des variables sont ordonnées. Elles se fondent sur le nombre de concordances et de discordances présentes ",
          "dans le tableau de contingence. On trouvera les détails des procédures au chapitre 11, section 11.5.4 du manuel ",
          tags$a(href = "http://biblio4haiti.ddns.net:8080/proportions-multiples-et-tableaux-de-contingence.html#mesures-dassociation-ordinal-ordinal", "Probabilité & Statistiques"), ".", p(),
          "$\\qquad \\bullet \\quad$ Coefficient $\\gamma$ de Goodman-Kruskal: $\\gamma=\\frac{C-D}{C+D}$", p(),
          "$\\qquad \\bullet \\quad$ Coefficient $\\tau$ de Kendall: ", p(),
          "$\\qquad \\qquad \\circ \\; \\tau_b$: pour des tableaux de contingence carrés $($R = C$)$", p(),
          "$\\qquad \\qquad \\qquad \\tau_b=\\frac{C-D}{\\sqrt{\\left [\\frac{N(N-1)}{2}-T_X \\right ] \\left [\\frac{N(N-1)}{2}-T_Y \\right ]}}$", p(),
          "$\\qquad \\qquad \\circ \\; \\tau_c$: pour des tableaux de contingence rectangulaires $(R \\neq C)$", p(),
          "$\\qquad \\qquad \\qquad \\; \\tau_c=\\frac{C-D}{0.5N^2 \\left [ \\frac{min(n_R,n_C)-1}{min(n_R,n_C)} \\right ]}$", p(),
          "$\\qquad \\bullet \\quad d$ de Somers : pour des associations directionnelles", p(),
          "$\\qquad \\qquad \\qquad \\; d_{R|C}= \\frac{2(C-D)}{N^2 - ∑(n_i.^2)} \\qquad d_{C|R}= \\frac{2(C-D)}{N^2 - ∑(n_{.j}^2)}$", p(),
          "Pour le tableau de contingence à l'étude, on obtient: ", p(),
          "$\\gamma$ de Goodman-Kruskal = ", round(GoodmanKruskalGamma(obs), input$digits), p(),
          ifelse(nr == nc, paste0("$\\tau_b $ de Kendall = ", round(KendallTauB(obs), input$digits)),
            paste0("$\\tau_c $ de Kendall = ", round(StuartTauC(obs), input$digits))
          ),
          p(),
          "$d$ de Somers $($R|C$)$ = ", round(SomersDelta(obs, direction = "row"), input$digits), p(),
          "$d$ de Somers $($C|R$)$ = ", round(SomersDelta(obs, direction = "column"), input$digits)
        )
      )
    )
  })

  output$NomOrd <- renderUI({
    helpText(
      h3("Aucune procédure disponible! Considérez les deux variables comme étant nominales...")
    )
  })

  output$compute <- renderUI({
    observed <- values$observed
    observedWithTotals <- observed %>%
      bind_rows(observed %>% summarize_all(funs(sum(., na.rm = TRUE)))) %>%
      mutate(Total = as.integer(rowSums(., na.rm = TRUE))) %>%
      mutate_all(funs(as.integer))
    cnj <- observedWithTotals / sum(observed)
    nr <- nrow(observed)
    nc <- ncol(observed)
    cndAB <- apply(observed, 2, function(x) x / sum(x))
    cndBA <- t(apply(observed, 1, function(x) x / sum(x)))
    pb <- cnj[nr + 1, 1:nc]
    pa <- cnj[1:nr, nc + 1]

    txt5 <- character(0)
    for (i in 1:nc) {
      txt5[i] <- paste0("P(B_", i, ")P(A_", input$row2, "|B_", i, ")")
    }
    txtden5 <- paste0(txt5, collapse = " + ")

    txt6 <- character(0)
    for (i in 1:nc) {
      txt6[i] <- paste0("(", round(pb[i], input$digits), ")(", round(cndAB[input$row2, i], input$digits), ")")
    }
    txtden6 <- paste0(txt6, collapse = " + ")

    txt7 <- character(0)
    for (i in 1:nc) {
      txt7[i] <- paste0("P(A_", i, ")P(B_", input$col2, "|A_", i, ")")
    }
    txtden7 <- paste0(txt7, collapse = " + ")

    txt8 <- character(0)
    for (i in 1:nr) {
      txt8[i] <- paste0("(", round(pa[i], input$digits), ")(", round(cndBA[i, input$col2], input$digits), ")")
    }
    txtden8 <- paste0(txt8, collapse = " + ")

    h4(
      withMathJax(),
      "Pour illustrer, définissez un tableau de contingence dans le tableau de bord, cliquez sur le bouton libellé ", strong("Exécuter"),
      "pour obtenir les probabilités marginales, conjointes et conditionnelles. Sélectionnez ensuite une case à l'aide des curseurs. Les ",
      "différentes équations sont illustrées numériquement ci-dessous:", p(),
      strong("Probabilités conditionnelles:"), p(),
      wellPanel(
        "$$P(A_", input$row2, "|B_", input$col2, ")=\\frac{P(A_", input$row2, "\\cap B_", input$col2, ")}{P(B_", input$col2, ")}=\\frac{", round(cnj[input$row2, input$col2], input$digits),
        "}{", round(pb[input$col2], input$digits), "}=\\color{darkgreen}{", round(cndAB[input$row2, input$col2], input$digits), "}$$",
        "et ", p(),
        "$$P(B_", input$col2, "|A_", input$row2, ")=\\frac{P(A_", input$row2, "\\cap B_", input$col2, ")}{P(A_", input$row2, ")}=\\frac{", round(cnj[input$row2, input$col2], input$digits),
        "}{", round(pa[input$row2], input$digits), "}= \\color{red}{", round(cndBA[input$row2, input$col2], input$digits), "}$$"
      ), p(),
      "ou", p(),
      wellPanel(
        "$$P(A_", input$row2, "|B_", input$col2, ")=\\frac{P(A_", input$row2, ")P(B_", input$col2, "| A_", input$row2,
        ")}{", txtden7, "}$$", p(),
        "$$ =\\frac{(", round(pa[input$row2], input$digits), ")(", round(cndBA[input$row2, input$col2], input$digits), ")}{",
        txtden8, "}=\\color{darkgreen}{", round(cndAB[input$row2, input$col2], input$digits), "}$$", p(),
        "et", p(),
        "$$P(B_", input$col2, "|A_", input$row2, ")=\\frac{P(B_", input$col2, ")P(A_", input$row2, "| B_", input$col2,
        ")}{", txtden5, "}$$", p(),
        "$$ =\\frac{(", round(pb[input$col2], input$digits), ")(", round(cndAB[input$row2, input$col2], input$digits), ")}{",
        txtden6, "}=\\color{red}{", round(cndBA[input$row2, input$col2], input$digits), "}$$",
      )
    )
  })
}

shinyApp(ui = ui, server = server)
