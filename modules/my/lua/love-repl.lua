-- Loaded from a LÖVE game's conf.lua by +lua-love-hook-line when Emacs starts
-- the game. conf.lua runs before LÖVE's modules load, so everything that needs
-- them waits until love.run; REPL lines are then evaluated between frames on
-- the main thread, where love and the game's globals live.

local here = debug.getinfo(1, "S").source:match("^@(.*[/\\])") or "./"
local repl = dofile(here .. "evaluator.lua")

repl.set_interrupt_file(os.getenv("EMACS_LOVE_REPL_INTERRUPT"))

local CHANNEL = "emacs-repl"

local READER = [[
  local lines = love.thread.getChannel(...)
  for line in io.lines() do lines:push(line) end
]]

local function start_reader()
  love.thread.newThread(READER):start(CHANNEL)
end

local function love_version()
  local major, minor, revision = love.getVersion()
  return ("LOVE %d.%d.%d"):format(major, minor, revision)
end

local function feed_pending_lines(feed, channel)
  local line = channel:pop()
  while line do
    feed(line)
    line = channel:pop()
  end
end

local function with_repl(frame, feed, channel)
  return function()
    feed_pending_lines(feed, channel)
    return frame()
  end
end

local run_game = love.run

function love.run()
  local frame = run_game()
  start_reader()
  repl.say(love_version())
  repl.write_prompt("> ")
  return with_repl(frame, repl.new_session(), love.thread.getChannel(CHANNEL))
end
