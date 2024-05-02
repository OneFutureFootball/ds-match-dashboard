from moviepy.editor import VideoFileClip, AudioFileClip, CompositeAudioClip
import os
import sys

dashboard_folder = os.getcwd()

# Define the paths to your input files (adjust these paths according to your actual file locations)
mp4_file = [os.path.join(dashboard_folder+'/output',f) for f in os.listdir(dashboard_folder+'/output') if 'mp4' in f][0]
HG_1 = AudioFileClip(dashboard_folder+"/sounds/home_goal_1.mp3")
HG_2 = AudioFileClip(dashboard_folder+"/sounds/home_goal_2.mp3")
HG_3 = AudioFileClip(dashboard_folder+"/sounds/home_goal_3.mp3")
HG_4 = AudioFileClip(dashboard_folder+"/sounds/home_goal_4.mp3")
AG_1 = AudioFileClip(dashboard_folder+"/sounds/away_goal_1.mp3")
BG = AudioFileClip(dashboard_folder+"/sounds/background.mp3")
WS_1 = AudioFileClip(dashboard_folder+"/sounds/whistle.mp3")

whistles = [float(x) for x in sys.argv[1].split('+')]
home_goals = [float(x) for x in sys.argv[2].split('+')]
away_goals = [float(x) for x in sys.argv[3].split('+')]

HG = []
for n in range(len(home_goals)):
    HG.append([HG_1,HG_2,HG_3,HG_4][n % 4].set_start(home_goals[n]))

AG = []
for n in range(len(away_goals)):
    AG.append(AG_1.set_start(away_goals[n]))

WS = []
for n in range(len(whistles)):
    WS.append(WS_1.set_start(whistles[n]))

BGS = [BG.set_start(0),
       BG.set_start(BG.duration),
       BG.set_start(BG.duration*2)]

audio_track = CompositeAudioClip(BGS + WS + HG + AG)
# Load the video clip

video_clip = VideoFileClip(mp4_file)

# Add the audio clip to the video clip at the specified timestamp
video_clip = video_clip.set_audio(audio_track)

# Write the video clip with the added audio to a new file
output_file = dashboard_folder+'/output/test_video.mp4'
video_clip.write_videofile(output_file, codec="libx264", audio_codec="aac")

# Close the clips
video_clip.close()