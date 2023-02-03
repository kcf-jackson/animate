#' 'FFmpeg'-based video editor ('Shiny' app)
ffmpeg_shiny <- function() {
  options(shiny.maxRequestSize = 1024^3) # 1GB limit for input

  ui <- shiny::fluidPage(
    shiny::titlePanel("Simple Video Editing using FFMPEG"),
    shiny::tags$script("
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
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::fileInput("file1", "Choose Video File",
          multiple = FALSE,
          accept = c(".mp4", ".mov")
        ),
        shiny::radioButtons("format", "Output format",
          choices = c("MP4" = "mp4", "GIF" = "gif"),
          selected = "mp4",
          inline = T
        ),
        shiny::actionButton("preview", "Preview FFMPEG", diabled = TRUE),
        shiny::checkboxInput("skip", "skip", FALSE),
        shiny::numericInput("skipArg", "(number) seconds to skip", 0),
        shiny::checkboxInput("time", "length", FALSE),
        shiny::numericInput("timeArg", "(number) output video length in seconds", 5),
        shiny::checkboxInput("crop", "crop", FALSE),
        shiny::splitLayout(
          shiny::numericInput("cropWArg", "width", value = 1000, min = 0, step = 10),
          shiny::numericInput("cropHArg", "height", value = 1000, min = 0, step = 10),
          shiny::numericInput("cropXArg", "x", value = 0, min = 0, step = 10),
          shiny::numericInput("cropYArg", "y", value = 0, min = 0, step = 10)
        ),
        shiny::checkboxInput("framerate", "frame rate", FALSE),
        shiny::textInput("framerateArg", "(integer) the frame rate", 60),
        shiny::checkboxInput("codec", "Video codec", FALSE),
        shiny::textInput("codecArg", "(character) the codec", "h264"),
        shiny::checkboxInput("speed", "speed", FALSE),
        shiny::textInput("speedArg", "(number) video speed up", 1),
        shiny::checkboxInput("loop", "loop", FALSE),
        shiny::textInput("loopArg", "(number) (GIF only) number of times to repeat. -1 = no repeat, 0 = infinite loop, 1 = repeat once.", 0),
        shiny::checkboxInput("palette", "palette (GIF)", FALSE),
        width = 3
      ),
      shiny::mainPanel(
        shiny::fluidRow(
          shiny::tags$video(id = "source_video", width = "49%", controls = "controls"),
          shiny::tags$video(id = "preview_video", width = "49%", controls = "controls"),
          shiny::tags$img(id = "preview_gif", width = "49%", style = "display:none;vertical-align:top;")
        ),
        shiny::fluidRow(
          shiny::column(
            shiny::tags$hr(),
            shiny::tags$label("Command:"),
            shiny::textOutput("command"),
            shiny::tags$hr(),
            shiny::tags$label("FFMPEG log:"),
            shiny::verbatimTextOutput("log"),
            width = 12
          )
        ),
        width = 9
      )
    )
  )

  server <- function(input, output, session) {
    session$sendCustomMessage("enable_preview_btn", FALSE)
    shiny::observeEvent(input$file1, {
      servr::daemon_stop()
      handle <- servr::httd(dirname(input$file1$datapath), port = 5432, browser = FALSE)
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

    param <- shiny::reactiveValues(
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

    shiny::observe({
      param$skip <- if (input$skip) input$skipArg else NULL
      param$time <- if (input$time) input$timeArg else NULL
      param$crop <- if (input$crop) {
        paste(input$cropWArg, input$cropHArg,
          input$cropXArg, input$cropYArg,
          sep = ":"
        )
      } else {
        NULL
      }
      param$speed <- if (input$speed) input$speedArg else NULL
      param$loop <- if (input$loop) input$loopArg else NULL
      param$framerate <- if (input$framerate) input$framerateArg else NULL
      param$codec <- if (input$codec) input$codecArg else NULL
      param$palette <- input$palette
      param$file_out <- change_file_ext(param$file_out, input$format)
    })
    shiny::observeEvent(param, {
      output$command <- shiny::renderText({
        ffmpeg_command(param$file_in, param$file_out,
          skip = param$skip,
          time = param$time,
          crop = param$crop,
          speed = param$speed,
          codec = param$codec,
          frame_rate = param$framerate,
          loop = param$loop,
          palette = param$palette
        )
      })
    })

    shiny::observeEvent(input$preview, {
      temp_video <- tempfile(
        fileext = paste0(".", input$format),
        tmpdir = dirname(param$file_in)
      )

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
        palette = param$palette
      )
      message("Running command: ", cmd)
      res <- system(cmd, wait = T, intern = T)
      output$log <- shiny::renderText({
        paste(res, collapse = "\n")
      })

      # Serve the preview video
      servr::daemon_stop()
      handle <- servr::httd(dirname(param$file_in), port = 5432, browser = FALSE)
      path <- paste0("http://127.0.0.1:5432/", basename(temp_video))
      session$sendCustomMessage("load_pvw_video", path)
    })

    shiny::onStop(function() {
      servr::daemon_stop()
    })
  }

  timestamp <- function() {
    Sys.time() |>
      gsub(pattern = "[ ]", replacement = "_") |>
      gsub(pattern = "[-:]", replacement = "")
  }

  shiny::shinyApp(ui = ui, server = server)
}
