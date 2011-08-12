PROJECT_DIR = File.dirname(__FILE__)
#MY_BOT = "#{PROJECT_DIR}/MyBot"
MY_BOT = "#{PROJECT_DIR}/MyBot.sh"
LOG_DIR = "#{PROJECT_DIR}/logs"

desc "Play one game"
task :one => [:compile] do
  command = "cd ../aichallenge/ants && ./playgame.py --turntime=1000 --serial --player_seed 42 --end_wait=0.25 --verbose --log_error --log_stderr --log_dir game_logs --turns 1000 --map_file maps/symmetric_maps/symmetric_10.map \"$@\" \"#{MY_BOT}\" \"python dist/sample_bots/python/GreedyBot.py\" \"python dist/sample_bots/python/HunterBot.py\" \"python dist/sample_bots/python/GreedyBot.py\""
  puts "Running #{command}"
  puts `#{command}`
end

desc "compile"
task :compile do
  puts `ghc --make -prof -auto-all -caf-all -fforce-recomp -O MyBot`
end

desc "make"
task :make do
  `rm contest_package.zip`
  `zip -r contest_package.zip **/**/**/*.hs **/**/*.hs **/*.hs *.hs`
  puts "done"
end

