PROJECT_DIR = File.dirname(__FILE__)
MY_BOT = "#{PROJECT_DIR}/MyBot"
LOG_DIR = "#{PROJECT_DIR}/logs"

desc "Play one game"
task :one => [:compile] do
  puts `cd ../aichallenge/ants && ./playgame.py --player_seed 42 --end_wait=0.25 --verbose --log_error --log_stderr --log_dir game_logs --turns 1000 --map_file maps/symmetric_maps/symmetric_10.map "$@" "#{MY_BOT}" "python dist/sample_bots/python/GreedyBot.py" "python dist/sample_bots/python/HunterBot.py" "python dist/sample_bots/python/GreedyBot.py"`
end

desc "compile"
task :compile do
  puts `ghc --make -O MyBot`
end

