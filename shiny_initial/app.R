library(shiny)

ui <- navbarPage("Menu",
                 tabPanel("Home",
                          titlePanel("Effects of Pitch Sequencing on MLB Pitcher Success"),
                          mainPanel(
                            p("This is the initial Shiny app, featuring the graphic from Milestone 5. 
                              More content will be added as the project progresses."),
                            img(src = "topsequences.png", height = 700, width = 700)
                            )
                          ),
                 tabPanel("About",
                          titlePanel("About"),
                          mainPanel(
                            p("There's more data in baseball than ever now. Whether you're interested 
                              in the probability a Mike Trout line drive will fall for a hit, the top 
                              speed Ronald Acuña Jr. reaches going from first to third, or the precise 
                              arm slot at which Josh Hader releases each fastball, these measurements 
                              are all available at quite literally the push of a button. Yet as the 
                              game becomes viewed by many as a series of sequences and estimations, 
                              there remain questions about what hasn't yet been quantified."
                              ),
                            p("Some of it is, of course, not truly measurable - one can't reduce a 
                              great teammate's impact on the clubhouse to fractions of a win or assess 
                              how many points of stolen base percentage a savvy first-base coach might 
                              add - but there's still room to improve analysis in certain areas. One of
                              these is pitch sequencing: how to optimize variation in pitch type, speed,
                              and location."
                              ),
                            p("In order to be able to properly analyze the effects of pitch sequencing,
                              pitch-level MLB data is of course required. Thankfully, when MLB 
                              introduced Statcast, its player-tracking camera system, in 2015, they 
                              made all the data publicly available and accessible via ",
                              a("Baseball Savant,",
                                href = "https://baseballsavant.mlb.com"),
                              "and for this project I collected it using Bill Petti's immensely useful",
                              a(code("baseballr"), "package.",
                                href = "http://billpetti.github.io/baseballr/"),
                              "The past two years of data have been found to be more accurate than 
                              2015-2017, but because we're primarily interested in basic pitch data and 
                              less so more advanced metrics like spin rate and exit velocity, I feel 
                              comfortable using all available Statcast measurements. In order to 
                              properly handle the multiple gigabytes worth of data, I used Git LFS and 
                              a modified form of Petti's", code("scrape_statcast_savant"), "function in
                              order to get the full five regular seasons of data."
                              ),
                            p("As of right now, I believe this will be the only data source required 
                              for this project, but should there be a material change I would consider 
                              using", 
                              a("Retrosheet play-by-play logs,", 
                                href = "https://www.retrosheet.org"), "Sean Lahman's", 
                              a("baseball database, ", 
                                href = "http://www.seanlahman.com/baseball-archive/statistics/"), 
                              "or the very detailed", 
                              a(code("openWAR"), "package.", 
                                href = "https://github.com/beanumber/openWAR")
                              ),
                            h3("Personal"),
                            p("My name is Tate Huffman, and I'm a junior at Harvard College studying 
                              Applied Mathematics and Economics with a secondary in Statistics. I row 
                              on the men's lightweight crew team, and I'm a member of the Harvard Sports
                              Analysis Collective."
                              ),
                            h4(a("Email",
                                 href = "mailto:thuffman@college.harvard.edu")),
                            h4(a("Link to GitHub Repository",
                                 href = "https://github.com/tjhuffman4246/final_project"))
                            
                          )),
                 tabPanel("More")
   
)

server <- function(input, output) {
   
  
}


# Run the application 

shinyApp(ui = ui, server = server)
