desc "Play one game"
task :one do
  puts `ghc --make -O MyBot`
  puts `cd ../aichallenge/ants && ./playgame.py --player_seed 42 --end_wait=0.25 --verbose --log_dir game_logs --turns 1000 --map_file maps/symmetric_maps/symmetric_10.map "$@" "#{File.dirname(__FILE__)}/MyBot" "python dist/sample_bots/python/LeftyBot.py" "python dist/sample_bots/python/HunterBot.py" "python dist/sample_bots/python/LeftyBot.py"`
end
