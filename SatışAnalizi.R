library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(plotly)
library(DT)
library(scales)
library(dygraphs)
library(xts)
library(lubridate)

desk_tab  = readRDS("prep_data/deskriptiv_table.rds")
cat_sum   = readRDS("prep_data/category_summary.rds")
heat_uni  = readRDS("prep_data/heatmap_unified.rds")
na_prof   = readRDS("prep_data/na_profiling.rds")
heat_bak  = readRDS("prep_data/baku_heatmap_green.rds")
price_d   = readRDS("prep_data/price_dist.rds")
brand_f   = readRDS("prep_data/brand_final.rds")
data_plt  = readRDS("prep_data/data_plot.rds")
whales    = readRDS("prep_data/whales_report.rds")
chaos     = readRDS("prep_data/price_chaos.rds")
menfi_m   = readRDS("prep_data/menfi_m.rds")
h1=readRDS("prep_data/hypo_1.rds"); h2=readRDS("prep_data/hypo_2.rds"); h4=readRDS("prep_data/hypo_4.rds")
h5=readRDS("prep_data/hypo_5.rds"); h6=readRDS("prep_data/hypo_6.rds"); pareto=readRDS("prep_data/pareto_data.rds")


ui = dashboardPage(
  skin = "purple",
  dashboardHeader(
    title = span("Satış Analizi", style = "font-weight: bold; font-family: 'Arial';"),
    titleWidth = 200,
    tags$li(class = "dropdown", style = "float: left !important;",
            div(style = "display: flex; padding-top: 10px; padding-left: 10px; gap: 8px;",
                actionButton("go_desc", "Dashboard", icon = icon("th"), style="background:#605ca8; color:white; border:none; font-weight:bold;"),
                actionButton("go_anom", "Anomaliya", icon = icon("warning"), style="background:#605ca8; color:white; border:none; font-weight:bold;"),
                actionButton("go_prog", "Proqnoz", icon = icon("line-chart"), style="background:#605ca8; color:white; border:none; font-weight:bold;"),
                actionButton("go_hypo", "Hipotezlər", icon = icon("lightbulb-o"), style="background:#605ca8; color:white; border:none; font-weight:bold;")
            )
    )
  ),
  
  dashboardSidebar(
    width = 200,
    br(),
    div(style = "padding: 10px;",
        h5("Süzgəclər", style = "color: white; font-weight: bold; border-bottom: 1px solid #ffffff50;"),
        selectInput("reg_select", "Region:", choices = c("Hamısı", sort(unique(heat_uni$final_region))), selected = "Hamısı", multiple = TRUE),
        selectInput("cat_select", "Kateqoriya:", choices = c("Hamısı", sort(unique(cat_sum$category))), selected = "Hamısı", multiple = TRUE)
    ),
    sidebarMenu(id = "tabs", style = "display: none;",
                menuItem("desc", tabName = "desc"), menuItem("anom", tabName = "anom"),
                menuItem("prog", tabName = "prog"), menuItem("hypo", tabName = "hypo"))
  ),
  
  dashboardBody(
    tags$head(tags$style(HTML("
      .content-wrapper { background-color: #f8f9fd; }
      .box { border-radius: 10px; border-top: 4px solid #605ca8; }
      .main-header .navbar { background-color: #605ca8 !important; }
      g.xtick text, g.ytick text { font-size: 12px !important; font-weight: bold !important; }
    "))),
    
    tabItems(
      tabItem(tabName = "desc",
              fluidRow(
                valueBoxOutput("total_rev_box", width = 4),
                valueBoxOutput("avg_basket_box", width = 4),
                valueBoxOutput("top_cat_box", width = 4)
              ),
              fluidRow(
                box(title = "Kateqoriya Payı (%)", status = "primary", solidHeader = TRUE, plotlyOutput("cat_plot_plotly", height = "400px"), width = 7),
                box(title = "Göstəricilər", status = "info", solidHeader = TRUE, DTOutput("desc_table"), width = 5)
              ),
              fluidRow(
                box(title = "Regional Satış Matrisi", status = "primary", solidHeader = TRUE, plotOutput("reg_heat", height = "550px"), width = 6),
                box(title = "Bakı Rayonları Matrisi", status = "primary", solidHeader = TRUE, plotOutput("baku_heat", height = "550px"), width = 6)
              )
      ),
      
      tabItem(tabName = "anom",
              fluidRow(
                box(title = "Məlumat Boşluğu", plotlyOutput("na_pie"), width = 4),
                box(title = "Qiymət Dağılımı", plotOutput("price_spec_plot"), width = 8)
              ),
              fluidRow(
                tabBox(title = "Hesabatlar", width = 12,
                       tabPanel("Nəhəng Satışlar", DTOutput("whales_tab")),
                       tabPanel("Qiymət Chaosu", DTOutput("chaos_tab")),
                       tabPanel("Zərərli Məhsullar", DTOutput("loss_tab")))
              )
      ),
      
      tabItem(tabName = "prog",
              box(title = "90 Günlük Satış Proqnozu (Milyon AZN)", status = "primary", dygraphOutput("prognoz_plot", height = "400px"), width = 12),
              box(title = "Brendlərin Performansı (Ümumi Satış M-lə)", status = "info", plotOutput("brand_perf_plot", height = "650px"), width = 12)
      ),
      
      tabItem(tabName = "hypo",
              fluidRow(
                box(title = "H1: Marja", DTOutput("hypo1_tab"), width = 4),
                box(title = "H2: Səhiyyə/Supermarket", DTOutput("hypo2_tab"), width = 4),
                box(title = "H3: Pareto", div(style="text-align:center; padding:40px;", h3(textOutput("pareto_text"))), width = 4)
              ),
              fluidRow(
                box(title = "H4: Ayın Dövrü", DTOutput("hypo4_tab"), width = 4),
                box(title = "H5: Seqmentasiya", DTOutput("hypo5_tab"), width = 4),
                box(title = "H6: Həftəsonu", DTOutput("hypo6_tab"), width = 4)
              )
      )
    )
  )
)


server = function(input, output, session) {
  

  observeEvent(input$go_desc, { updateTabItems(session, "tabs", "desc") })
  observeEvent(input$go_anom, { updateTabItems(session, "tabs", "anom") })
  observeEvent(input$go_prog, { updateTabItems(session, "tabs", "prog") })
  observeEvent(input$go_hypo, { updateTabItems(session, "tabs", "hypo") })
  

  filtered_cat = reactive({
    df = cat_sum
    if (!is.null(input$cat_select) && !"Hamısı" %in% input$cat_select) {
      df = df %>% filter(category %in% input$cat_select)
    }
    df
  })
  

  output$total_rev_box = renderValueBox({
    data = filtered_cat()
    toplam = sum(data$total_rev, na.rm = TRUE)
    valueBox(
      paste0(round(toplam / 1e6, 1), "M"), 
      "Cəmi Satış", 
      icon = icon("money"), 
      color = "purple"
    )
  })
  
  output$avg_basket_box = renderValueBox({
    data = filtered_cat()
    orta = mean(data$avg_basket, na.rm = TRUE)
    valueBox(
      paste0(round(orta, 1), " AZN"), 
      "Orta Səbət", 
      icon = icon("shopping-cart"), 
      color = "blue"
    )
  })
  
  output$top_cat_box = renderValueBox({
    data = filtered_cat()
    lider = if(nrow(data) > 0) data$category[which.max(data$total_rev)] else "N/A"
    valueBox(
      lider, 
      "Lider Kateqoriya", 
      icon = icon("star"), 
      color = "teal"
    )
  })
  

  fmt_milli <- label_number(scale = 1e-6, suffix = "M", accuracy = 0.1)
  

  my_theme <- function() {
    theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, size = 11, face = "bold", color = "black"),
        axis.text.y = element_text(size = 11, face = "bold", color = "black"),
        axis.title = element_text(size = 13, face = "bold"),
        panel.grid.minor = element_blank()
      )
  }
  

  output$cat_plot_plotly = renderPlotly({
    p = ggplot(filtered_cat(), aes(x = reorder(category, rev_share), y = rev_share, fill = rev_share)) +
      geom_bar(stat = "identity") + coord_flip() +
      scale_fill_gradient(low = "#d1d1f0", high = "#5c2d91") +
      theme_minimal() + labs(x = "", y = "Gəlir Payı (%)")
    ggplotly(p)
  })
  

  output$reg_heat = renderPlot({
    data = heat_uni
    if (!is.null(input$reg_select) && !"Hamısı" %in% input$reg_select) {
      data = data %>% filter(final_region %in% input$reg_select)
    }
    if (!is.null(input$cat_select) && !"Hamısı" %in% input$cat_select) {
      data = data %>% filter(category %in% input$cat_select)
    }
    ggplot(data, aes(x = category, y = reorder(final_region, share), fill = share)) +
      geom_tile(color = "white") +
      scale_fill_gradient(low = "#f3e5f5", high = "#4a148c") +
      geom_text(aes(label = paste0(round(share, 0), "%")), size = 5, fontface = "bold", color = "black") +
      my_theme() + labs(x = "Kateqoriya", y = "Region")
  })
  

  output$baku_heat = renderPlot({
    data = heat_bak
    if (!is.null(input$cat_select) && !"Hamısı" %in% input$cat_select) {
      data = data %>% filter(category %in% input$cat_select)
    }
    ggplot(data, aes(x = category, y = reorder(district_name, share), fill = share)) +
      geom_tile(color = "white") +
      scale_fill_gradient(low = "#e3f2fd", high = "#1565c0") +
      geom_text(aes(label = paste0(round(share, 0), "%")), size = 4.5, fontface = "bold", color = "black") +
      my_theme() + labs(x = "Kateqoriya", y = "Rayon")
  })
  

  output$prognoz_plot = renderDygraph({
    data_xts = xts(data_plt[,-1], order.by = data_plt$ds)
    dygraph(data_xts) %>%
      dyOptions(colors = c("#2c3e50", "#605ca8", "#3498db"), strokeWidth = 2.5) %>%
      dyAxis("y", label = "Məbləğ (Milyon AZN)", 
             axisLabelFormatter = "function(v){return (v/1000000).toFixed(1) + 'M';}")
  })
  

  output$brand_perf_plot = renderPlot({
    ggplot(brand_f, aes(x = total_rev, y = reorder(brand, total_rev), fill = total_rev)) +
      geom_col() +
      scale_fill_gradient(low = "#b39ddb", high = "#311b92") +
      scale_x_continuous(labels = fmt_milli) + 
      my_theme() +
      theme(axis.text.y = element_text(size = 12)) +
      labs(x = "Ümumi Satış (Milyon AZN)", y = "Brend Adı")
  })
  

  output$price_spec_plot = renderPlot({
    ggplot(price_d, aes(x = price_bucket, y = rev_share)) + 
      geom_col(fill = "#5c2d91", width = 0.7) + 
      scale_y_continuous(labels = function(x) paste0(x, "%")) +
      my_theme() + labs(x = "Qiymət Aralığı (AZN)", y = "Gəlir Payı %")
  })
  

  render_fixed_dt = function(df) {
    datatable(df, options = list(dom = 'tp', pageLength = 7, scrollX = TRUE, autoWidth = TRUE), rownames = FALSE)
  }
  
  output$desc_table = renderDT({ render_fixed_dt(desk_tab) })
  output$whales_tab = renderDT({ render_fixed_dt(whales) })
  output$chaos_tab  = renderDT({ render_fixed_dt(chaos) })
  output$loss_tab   = renderDT({ render_fixed_dt(menfi_m) })
  output$hypo1_tab  = renderDT({ render_fixed_dt(h1) })
  output$hypo2_tab  = renderDT({ render_fixed_dt(h2) })
  output$hypo4_tab  = renderDT({ render_fixed_dt(h4) })
  output$hypo5_tab  = renderDT({ render_fixed_dt(h5) })
  output$hypo6_tab  = renderDT({ render_fixed_dt(h6) })
  
  output$pareto_text = renderText({ paste0("Top 10 məhsul gəlirin ", round(max(pareto$cum_pct[1:10]), 1), "%-ni təşkil edir.") })
  output$na_pie = renderPlotly({
    plot_ly(na_prof, labels = ~Status, values = ~Say, type = "pie", marker = list(colors = c("#605ca8", "#3498db")))
  })
}

shinyApp(ui, server)