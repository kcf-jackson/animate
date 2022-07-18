## Converting video

ffmpeg -i credit_score.mp4 -vf "fps=10,split[s0][s1];[s0]palettegen[p];[s1][p]paletteuse" -loop 0 credit_score.gif
ffmpeg -i hilbert_curve.mp4 -vf "fps=10,split[s0][s1];[s0]palettegen[p];[s1][p]paletteuse" -loop 0 hilbert_curve.gif
ffmpeg -i lorenz.mp4 -vf "fps=10,split[s0][s1];[s0]palettegen[p];[s1][p]paletteuse" -loop 0 lorenz.gif
ffmpeg -i parole.mp4 -vf "fps=10,split[s0][s1];[s0]palettegen[p];[s1][p]paletteuse" -loop 0 parole.gif
ffmpeg -i particle_system.mp4 -vf "fps=10,split[s0][s1];[s0]palettegen[p];[s1][p]paletteuse" -loop 0 particle_system.gif
ffmpeg -i sine_curve.mp4 -vf "fps=10,split[s0][s1];[s0]palettegen[p];[s1][p]paletteuse" -loop 0 sine_curve.gif

ffmpeg -ss 8 -i segregation_source.mp4 -vcodec h264 -vf "crop=450:260:480:60" segregation.mp4
ffmpeg -i segregation.mp4 -vf "fps=20,split[s0][s1];[s0]palettegen[p];[s1][p]paletteuse" -loop 0 segregation.gif

ffmpeg -ss 11 -i segregation_source_HD.mp4 -vcodec h264 -vf "crop=920:550:970:120" segregation_HD.mp4
ffmpeg -i segregation_HD.mp4 -vf "fps=20,split[s0][s1];[s0]palettegen[p];[s1][p]paletteuse" -loop 0 segregation_HD.gif


ffmpeg -ss 8 -i chess_source_HD.mp4 -vcodec h264 -vf "crop=540:540:970:140" chess_HD.mp4
ffmpeg -i chess_HD.mp4 -vf "fps=20,split[s0][s1];[s0]palettegen[p];[s1][p]paletteuse" -loop 0 chess_HD.gif

ffmpeg -ss 8 -i handdrawn_plot_source_HD.mp4 -vcodec h264 -vf "crop=580:450:960:110" handdrawn_plot_HD.mp4
ffmpeg -i handdrawn_plot_HD.mp4 -vf "fps=20,split[s0][s1];[s0]palettegen[p];[s1][p]paletteuse" -loop 0 handdrawn_plot_HD.gif

ffmpeg -ss 15 -i handdrawn_plot_2_source_HD.mp4 -vcodec h264 -vf "crop=680:510:1060:110" handdrawn_plot_2_HD.mp4
ffmpeg -i handdrawn_plot_2_HD.mp4 -vf "fps=20,split[s0][s1];[s0]palettegen[p];[s1][p]paletteuse" -loop 0 handdrawn_plot_2_HD.gif




ffmpeg -r 60 -t 60 -ss 4 -i predator_prey_source.mp4 -vcodec h264 -vf "crop=520:310:400:60" predator_prey.mp4 
ffmpeg -i predator_prey.mp4 -vf "fps=10,split[s0][s1];[s0]palettegen[p];[s1][p]paletteuse" -loop 0 predator_prey.gif

ffmpeg -r 60 -t 40 -ss 6 -i predator_prey_source_HD_2.mp4 -vcodec h264 -vf "crop=1080:640:800:120,scale=810:480" predator_prey_HD.mp4 
ffmpeg -i predator_prey_HD.mp4 -vf "fps=10,split[s0][s1];[s0]palettegen[p];[s1][p]paletteuse" -loop 0 predator_prey_HD.gif


ffmpeg -r 60 -ss 9 -i maze_generation_source.mp4 -vcodec h264 -vf "crop=820:450:240:400" maze_generation.mp4
ffmpeg -i maze_generation.mp4 -vf "fps=20,split[s0][s1];[s0]palettegen[p];[s1][p]paletteuse" -loop 0 maze_generation.gif

ffmpeg -r 60 -ss 5 -i marathon_source_HD.mp4 -vcodec h264 -vf "setpts=(PTS-STARTPTS)/2.0,crop=700:480:960:150" marathon_HD.mp4
ffmpeg -i marathon_HD.mp4 -vf "fps=15,split[s0][s1];[s0]palettegen[p];[s1][p]paletteuse" -loop 0 marathon_HD.gif

ffmpeg -i moving_points.mp4 -vf "fps=15,split[s0][s1];[s0]palettegen[p];[s1][p]paletteuse" -loop 0 moving_points.gif

