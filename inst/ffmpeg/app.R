library(shiny)

options(shiny.maxRequestSize=150*1024^2)
source("ffmpeg.R")

ui <- fluidPage(
    titlePanel("Simple Video Editing using FFMPEG"),
    tags$script("
    function load_src_video(x) {
      console.log(x);
      document.querySelector('#source_video').src = x;
    };
    function load_preview(x) {
      let file_ext = x.slice(-3);
      if (file_ext == 'gif') {
        document.querySelector('#preview_video').style.display = 'none';
        document.querySelector('#preview_gif').style.display = 'inline';
        document.querySelector('#preview_gif').src = x;
      } else {
        document.querySelector('#preview_video').style.display = 'inline';
        document.querySelector('#preview_gif').style.display = 'none';
        document.querySelector('#preview_video').src = x;
      }
    };
    Shiny.addCustomMessageHandler('load_src_video', load_src_video);
    Shiny.addCustomMessageHandler('load_pvw_video', load_preview);
    function enable_preview_btn(b) {
      document.querySelector('#preview').disabled = !b;
    };
    Shiny.addCustomMessageHandler('enable_preview_btn', enable_preview_btn);
    "),
    sidebarLayout(
        sidebarPanel(
          fileInput("file1", "Choose Video File",
                    multiple = FALSE,
                    accept = c(".mp4", ".mov")),

          radioButtons("format", "Output format",
                       choices = c("MP4" = "mp4", "GIF" = "gif"),
                       selected = "mp4",
                       inline = T),

          actionButton("preview", "Preview FFMPEG", diabled = TRUE),

          checkboxInput("skip", "skip", FALSE),
          numericInput("skipArg", "(number) seconds to skip", 0),

          checkboxInput("time", "length", FALSE),
          numericInput("timeArg", "(number) output video length in seconds", 5),

          checkboxInput("crop", "crop", FALSE),
          splitLayout(
            numericInput("cropWArg", "width", value = 1000, min = 0, step = 10),
            numericInput("cropHArg", "height", value = 1000, min = 0, step = 10),
            numericInput("cropXArg", "x", value = 0, min = 0, step = 10),
            numericInput("cropYArg", "y", value = 0, min = 0, step = 10)
          ),

          checkboxInput("framerate", "frame rate", FALSE),
          textInput("framerateArg", "(integer) the frame rate", 60),

          checkboxInput("codec", "Video codec", FALSE),
          textInput("codecArg", "(character) the codec", "h264"),

          checkboxInput("speed", "speed", FALSE),
          textInput("speedArg", "(number) video speed up", 1),

          checkboxInput("loop", "loop", FALSE),
          textInput("loopArg", "(number) (GIF only) number of times to repeat. -1 = no repeat, 0 = infinite loop, 1 = repeat once.", 0),

          checkboxInput("palette", "palette (GIF)", FALSE),

          width = 3
        ),
        mainPanel(
          fluidRow(
            tags$video(id = "source_video", width = "49%", controls = "controls"),
            tags$video(id = "preview_video", width = "49%", controls = "controls"),
            tags$img(id = "preview_gif", width = "49%", style = "display:none;vertical-align:top;")
          ),
          fluidRow(
            column(
              tags$hr(),
              tags$label("Command:"),
              textOutput("command"),
              tags$hr(),
              tags$label("FFMPEG log:"),
              verbatimTextOutput("log"),
              width = 12
            )
          ),
          width = 9
        )
    )
)

server <- function(input, output, session) {
  session$sendCustomMessage("enable_preview_btn", FALSE)
  observeEvent(input$file1, {
    servr::daemon_stop()
    handle <- servr::httd(dirname(input$file1$datapath), port = 5432)
    filename <- basename(input$file1$datapath)
    path <- paste0("http://127.0.0.1:5432/", filename)
    session$sendCustomMessage("load_src_video", path)

    param$file_in <- input$file1$datapath
    param$file_out <- file.path(
      dirname(input$file1$datapath),
      paste0(tools::file_path_sans_ext(filename), "_", timestamp(), ".", input$format)
    )
    session$sendCustomMessage("enable_preview_btn", TRUE)
  })

  param <- reactiveValues(
    file_in = "file.mp4",
    file_out = "file_2.mp4",
    skip = 0,
    time = 10,
    crop = "1000:500:0:0",
    speed = "1.0",
    codec = "h264",
    framerate = 20,
    loop = 0,
    palette = FALSE
  )

  observe({
    param$skip <- if (input$skip) input$skipArg else NULL
    param$time <- if (input$time) input$timeArg else NULL
    param$crop <- if (input$crop) {
      paste(input$cropWArg, input$cropHArg,
            input$cropXArg, input$cropYArg,
            sep = ":")
    } else NULL
    param$speed <- if (input$speed) input$speedArg else NULL
    param$loop <- if (input$loop) input$loopArg else NULL
    param$framerate <- if (input$framerate) input$framerateArg else NULL
    param$codec <- if (input$codec) input$codecArg else NULL
    param$palette <- input$palette
    param$file_out <- change_file_ext(param$file_out, input$format)
  })
  observeEvent(param, {
    output$command <- renderText({
      ffmpeg_command(param$file_in, param$file_out,
                     skip = param$skip,
                     time = param$time,
                     crop = param$crop,
                     speed = param$speed,
                     codec = param$codec,
                     frame_rate = param$framerate,
                     loop = param$loop,
                     palette = param$palette)
    })
  })

  observeEvent(input$preview, {
    temp_video <- tempfile(fileext = paste0(".", input$format),
                           tmpdir = dirname(param$file_in))

    # Convert the video and store it in the temporary folder
    cmd <- ffmpeg_command(
                   param$file_in,
                   temp_video,
                   skip = param$skip,
                   time = param$time,
                   crop = param$crop,
                   speed = param$speed,
                   codec = param$codec,
                   frame_rate = param$framerate,
                   loop = param$loop,
                   palette = param$palette)
    print(cmd)
    res <- system(cmd, wait = T, intern = T)
    output$log <- renderText({ paste(res, collapse = "\n") })

    # Serve the preview video
    servr::daemon_stop()
    handle <- servr::httd(dirname(param$file_in), port = 5432)
    path <- paste0("http://127.0.0.1:5432/", basename(temp_video))
    session$sendCustomMessage("load_pvw_video", path)
  })

  onStop(function() {
    servr::daemon_stop()
  })
}

timestamp <- function() {
  Sys.time() |>
    gsub(pattern = "[ ]", replacement = "_") |>
    gsub(pattern = "[-:]", replacement = "")
}

shinyApp(ui = ui, server = server)

