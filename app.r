library(shiny)
library(bslib)
library(dplyr)

# Clickable github icon + link
github_link <- tags$a(
    shiny::icon("github"), "GitHub",
    href = "https://github.com/mstanley-yo/satolab-codon-helper",
    target = "_blank"
)

# Codon data
codons <- tribble(
    ~Codon, ~`Amino Acid`, ~Abbreviation, ~Frequency,
    "UUU", "Phe", "F", 0.46,
    "UUC", "Phe", "F", 0.54,
    "UUA", "Leu", "L", 0.08,
    "UUG", "Leu", "L", 0.13,
    "UCU", "Ser", "S", 0.19,
    "UCC", "Ser", "S", 0.22,
    "UCA", "Ser", "S", 0.15,
    "UCG", "Ser", "S", 0.05,
    "UAU", "Tyr", "Y", 0.44,
    "UAC", "Tyr", "Y", 0.56,
    "UAA", "Stop", "*", 0.30,
    "UAG", "Stop", "*", 0.24,
    "UGU", "Cys", "C", 0.46,
    "UGC", "Cys", "C", 0.54,
    "UGA", "Stop", "*", 0.47,
    "UGG", "Trp", "W", 1.00,
    "CUU", "Leu", "L", 0.13,
    "CUC", "Leu", "L", 0.20,
    "CUA", "Leu", "L", 0.07,
    "CUG", "Leu", "L", 0.40,
    "CCU", "Pro", "P", 0.29,
    "CCC", "Pro", "P", 0.32,
    "CCA", "Pro", "P", 0.28,
    "CCG", "Pro", "P", 0.11,
    "CAU", "His", "H", 0.42,
    "CAC", "His", "H", 0.58,
    "CAA", "Gln", "Q", 0.27,
    "CAG", "Gln", "Q", 0.73,
    "CGU", "Arg", "R", 0.08,
    "CGC", "Arg", "R", 0.18,
    "CGA", "Arg", "R", 0.11,
    "CGG", "Arg", "R", 0.20,
    "AUU", "Ile", "I", 0.36,
    "AUC", "Ile", "I", 0.47,
    "AUA", "Ile", "I", 0.17,
    "AUG", "Met", "M", 1.00,
    "ACU", "Thr", "T", 0.25,
    "ACC", "Thr", "T", 0.36,
    "ACA", "Thr", "T", 0.28,
    "ACG", "Thr", "T", 0.11,
    "AAU", "Asn", "N", 0.47,
    "AAC", "Asn", "N", 0.53,
    "AAA", "Lys", "K", 0.43,
    "AAG", "Lys", "K", 0.57,
    "AGU", "Ser", "S", 0.15,
    "AGC", "Ser", "S", 0.24,
    "AGA", "Arg", "R", 0.21,
    "AGG", "Arg", "R", 0.21,
    "GUU", "Val", "V", 0.18,
    "GUC", "Val", "V", 0.24,
    "GUA", "Val", "V", 0.12,
    "GUG", "Val", "V", 0.46,
    "GCU", "Ala", "A", 0.27,
    "GCC", "Ala", "A", 0.40,
    "GCA", "Ala", "A", 0.23,
    "GCG", "Ala", "A", 0.11,
    "GAU", "Asp", "D", 0.46,
    "GAC", "Asp", "D", 0.54,
    "GAA", "Glu", "E", 0.42,
    "GAG", "Glu", "E", 0.58,
    "GGU", "Gly", "G", 0.16,
    "GGC", "Gly", "G", 0.34,
    "GGA", "Gly", "G", 0.25,
    "GGG", "Gly", "G", 0.25
)

# Amino acid data
amino_acids <- c(
    "Ala (A)" = "A",
    "Arg (R)" = "R",
    "Asn (N)" = "N",
    "Asp (D)" = "D",
    "Cys (C)" = "C",
    "Gln (Q)" = "Q",
    "Glu (E)" = "E",
    "Gly (G)" = "G",
    "His (H)" = "H",
    "Ile (I)" = "I",
    "Leu (L)" = "L",
    "Lys (K)" = "K",
    "Met (M)" = "M",
    "Phe (F)" = "F",
    "Pro (P)" = "P",
    "Ser (S)" = "S",
    "Thr (T)" = "T",
    "Trp (W)" = "W",
    "Tyr (Y)" = "Y",
    "Val (V)" = "V",
    "Stop (*)" = "*"
)

# ==== UI ====
ui <- page_navbar(
    title = "Codon Helper",
    theme = bs_theme(bootswatch = "flatly"),
    nav_panel(
        "AA to Codon",
        layout_sidebar(
            sidebar = sidebar(
                radioButtons(
                    "aa",
                    "Select amino acid:",
                    choices = amino_acids
                ),
                open = "always"
            ),
            value_box(
                title = textOutput("selected_aa"),
                value = textOutput("top_codon"),
                max_height = 150
            ),
            card(
                card_header = "Codon Table",
                tableOutput("codon_table"),
                p("Written in R Shiny by Maximilian Stanley Yo."),
                github_link
            )
        )
    )
)

# ==== Server ====
server <- function(input, output) {
    codon_table <- reactive(
        codons %>%
            filter(toupper(Abbreviation) == toupper(input$aa)) %>%
            arrange(desc(Frequency))
    )
    
    output$codon_table <- renderTable({
        codon_table()
    })
    
    output$selected_aa <- renderText({
        paste(
            "To get", 
            names(amino_acids)[amino_acids == input$aa],
            "use the codon:"
        )
    })
    
    output$top_codon <- renderText({
        codon_table() %>% slice(1) %>% pull(Codon)
    })
}

# ==== Run App ====
shinyApp(ui, server)
