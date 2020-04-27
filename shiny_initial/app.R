library(shiny)
library(tidyverse)

ui <- navbarPage("Final Project",
                 tabPanel("Home",
                          titlePanel("Effects of Pitch Sequencing on MLB Pitcher Success"),
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
                            p("Using data from Baseball Savant, I set out to explore what sequencing in
                              MLB looks like, how it's changed over time, and whether it has significant
                              predictive on plate appearance outcomes beyond what one might expect. One
                              can play around with this modeling aspect in the Explore page and read more
                              about it in the Model section."
                              ), 
                            p("To set the stage, I'll quickly run through some descriptive analysis of the
                              data. The bar chart below shows the most commonly used sequences in the MLB
                              over the five years Statcast has been in place in ballparks (with a sequence
                              here defined as the pitch types thrown on consecutive pitches in the same
                              plate appearance)."
                              ),
                            img(src = "pctsequences.png", height = 600, width = 600),
                            p("The double-fastball sequence dominates this distribution, which is expected: 
                              four-seam fastballs have accounted for 35.6% of all pitches thrown, over twice
                              as many as the next closest pitch type (sliders, at 16.1%). Let's see whether 
                              these lesser-used pitches have become more or less used over time:"
                              ),
                            img(src = "pctpitches.png", height = 400, width = 600),
                            p("It's clear sliders, while always popular, have seen their usage skyrocket in
                              recent seasons, while the past two years have seen the two-seam fastball drop
                              from the second-most used pitch to the fourth, trailing the changeup and the
                              curveball. Sinkers, a fastball variant often grouped together with the
                              two-seamer, have also decreased in frequency, with these two together close to
                              being overtaken by the cut fastball. Now let's revisit our initial sequence
                              distribution, separating the data into starters and relievers, to see whether
                              one group or the other is adopting sequences that may be leading this changing
                              league-wide pitch mix."),
                            img(src = "sp_rp_sequences.png", height = 375, width = 600),
                            p("This indicates that relivers generally tend to have a less diversified pitch
                              portfolio in comparison to starters, relying more heavily on sequences
                              involving four-seam fastballs and sliders and less heavily on those with
                              two-seamers and changeups. This makes sense intuitively, given that reliever
                              outings are typically on the order of one to two innings and thus would be
                              expected to rely more heavily on pitch quality to defeat opposing hitters and
                              less on pitch mix and variation over the course of an extended appearance."),
                            p("That's my broad introduction to pitch sequencing - look at the rest of
                              this site to learn more!")
                            )
                          ),
                 tabPanel("Explore",
                          titlePanel("Explore"),
                          mainPanel(
                            p("More to be added soon!")
                          )),
                 tabPanel("Model",
                          titlePanel("Model"),
                          mainPanel(
                            
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
                                href = "https://baseballsavant.mlb.com",
                                target = "_blank"),
                              "and for this project I collected it using Bill Petti's immensely useful",
                              a(code("baseballr"), "package.",
                                href = "http://billpetti.github.io/baseballr/", 
                                target = "_blank"),
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
                                href = "https://www.retrosheet.org", 
                                target = "_blank"), "Sean Lahman's", 
                              a("baseball database, ", 
                                href = "http://www.seanlahman.com/baseball-archive/statistics/",
                                target = "_blank"), 
                              "or the very detailed", 
                              a(code("openWAR"), "package.", 
                                href = "https://github.com/beanumber/openWAR",
                                target = "_blank")
                              ),
                            h3("Personal"),
                            p("My name is Tate Huffman, and I'm a junior at Harvard College studying 
                              Applied Mathematics and Economics with a secondary in Statistics. I row 
                              on the men's lightweight crew team, and I'm a member of the Harvard Sports
                              Analysis Collective."
                              ),
                            h4(a("Email",
                                 href = "mailto:thuffman@college.harvard.edu",
                                 target = "_blank")),
                            h4(a("Link to GitHub Repository",
                                 href = "https://github.com/tjhuffman4246/final_project",
                                 target = "_blank"))
                            
                          ))
)

server <- function(input, output) {
   
  
}


# Run the application 

shinyApp(ui = ui, server = server)
