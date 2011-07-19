PROJECT_DIR = File.dirname(__FILE__)
MY_BOT = "#{PROJECT_DIR}/MyBot"
LOG_DIR = "#{PROJECT_DIR}/logs"

desc "Play one game"
task :one => [:compile] do
  puts `cd ../aichallenge/ants && ./playgame.py --turntime=10000 --serial --player_seed 42 --end_wait=0.25 --verbose --log_error --log_stderr --log_dir game_logs --turns 1000 --map_file maps/symmetric_maps/symmetric_10.map "$@" "#{MY_BOT}" "python dist/sample_bots/python/GreedyBot.py" "python dist/sample_bots/python/HunterBot.py" "python dist/sample_bots/python/GreedyBot.py 2>&1"`
end

desc "compile"
task :compile do
  puts `ghc --make -O MyBot`
end

task :make do
  `rm -rf staging/*`
  `mkdir -p staging/contest_package`
  `mkdir -p staging/contest_package/Data/Graph`
  `cp #{PROJECT_DIR}/*.hs staging/contest_package`
  `cp #{PROJECT_DIR}/Data/*.hs staging/contest_package/Data`
  `cp #{PROJECT_DIR}/Data/Graph/*.hs staging/contest_package/Data/Graph`
  `cd staging && zip -r contest_package.zip contest_package && rm -rf contest_package`
end

