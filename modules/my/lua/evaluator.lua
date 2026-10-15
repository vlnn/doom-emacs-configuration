-- Loaded into the REPL by modules/my/lua/repl.el; replies are framed as
-- STX status US value ETX so Emacs can split them from printed output.
-- Emacs treats ETX newline then a prompt as end of reply, so a stray prompt
-- (e.g. from lua-mode's init code) cannot end a redirect early. Code comes in
-- a file so command lines stay short whatever the REPL reads them from.
-- Library functions are captured here because evaluated code (a game's
-- `debug = false`, a custom print) may replace the globals.

local assert, error, getmetatable, ipairs, next, pcall, rawget, select, tostring, type =
      assert, error, getmetatable, ipairs, next, pcall, rawget, select, tostring, type
local format, rep = string.format, string.rep
local concat, sort = table.concat, table.sort
local open, lines, stdout = io.open, io.lines, io.stdout
local remove = os.remove
local sethook = debug.sethook
local load_chunk = loadstring or load

local STX, US, ETX = "\2", "\31", "\3"
local WIDTH = 60

local KEYWORDS = {}
for word in ([[and break do else elseif end false for function goto if in
               local nil not or repeat return then true until while]]):gmatch("%a+") do
  KEYWORDS[word] = true
end

local function pack(...)
  return { n = select("#", ...), ... }
end

local function quote_string(s)
  return (format("%q", s):gsub("\\\n", "\\n"))
end

local function is_identifier(key)
  return type(key) == "string" and key:match("^[%a_][%w_]*$") and not KEYWORDS[key]
end

local function sequence_length(t)
  local n = 0
  while rawget(t, n + 1) ~= nil do n = n + 1 end
  return n
end

local TYPE_ORDER = { number = 1, string = 2, boolean = 3 }

local function key_less(a, b)
  local ta, tb = TYPE_ORDER[type(a)] or 4, TYPE_ORDER[type(b)] or 4
  if ta ~= tb then return ta < tb end
  if ta == 1 or ta == 2 then return a < b end
  return tostring(a) < tostring(b)
end

local function record_keys(t, n)
  local keys = {}
  for key in next, t do
    local in_sequence = type(key) == "number" and key >= 1 and key <= n and key % 1 == 0
    if not in_sequence then keys[#keys + 1] = key end
  end
  sort(keys, key_less)
  return keys
end

local function has_tostring(value)
  local mt = getmetatable(value)
  return type(mt) == "table" and rawget(mt, "__tostring") ~= nil
end

local render

local function render_key(key, indent, path)
  if is_identifier(key) then return key end
  return "[" .. render(key, indent, path) .. "]"
end

local function table_entries(t, indent, path)
  local entries, n = {}, sequence_length(t)
  for i = 1, n do
    entries[#entries + 1] = render(rawget(t, i), indent, path)
  end
  for _, key in ipairs(record_keys(t, n)) do
    entries[#entries + 1] = render_key(key, indent, path) .. " = " .. render(rawget(t, key), indent, path)
  end
  return entries
end

local function layout_table(entries, indent)
  if #entries == 0 then return "{}" end
  local inline = "{ " .. concat(entries, ", ") .. " }"
  if #indent + #inline <= WIDTH and not inline:find("\n") then return inline end
  local child_indent = indent .. "  "
  return "{\n" .. child_indent .. concat(entries, ",\n" .. child_indent) .. "\n" .. indent .. "}"
end

local function render_table(t, indent, path)
  if path[t] then return "<cycle>" end
  path[t] = true
  local entries = table_entries(t, indent .. "  ", path)
  path[t] = nil
  return layout_table(entries, indent)
end

function render(value, indent, path)
  if type(value) == "string" then return quote_string(value) end
  if type(value) == "table" and not has_tostring(value) then
    return render_table(value, indent, path)
  end
  return tostring(value)
end

local function render_values(results, first)
  if results.n < first then return "nil" end
  local rendered = {}
  for i = first, results.n do
    rendered[#rendered + 1] = render(results[i], "", {})
  end
  return concat(rendered, ", ")
end

local function as_expression(code)
  -- passing code to an identity function keeps a top-level error() out of tail
  -- position, where LuaJIT drops its location, without relying on any global
  return "return (function(...) return ... end)(" .. code:gsub("[%s;]+$", "") .. "\n)"
end

local function compile(code, chunkname, line)
  local padding = rep("\n", line - 1)
  local name = "@" .. chunkname
  local expression = load_chunk(padding .. as_expression(code), name)
  if expression then return expression end
  return load_chunk(padding .. code, name)
end

local INTERRUPT_CHECK_EVERY = 100000
local interrupt_file

local function set_interrupt_file(path)
  interrupt_file = path
end

local function interrupt_requested()
  local file = interrupt_file and open(interrupt_file, "rb")
  if not file then return false end
  file:close()
  remove(interrupt_file)
  return true
end

local function check_interrupt()
  if interrupt_requested() then error("interrupted!", 2) end
end

local function interruptible_pcall(chunk)
  sethook(check_interrupt, "", INTERRUPT_CHECK_EVERY)
  local results = pack(pcall(chunk))
  sethook()
  return results
end

local function run(code, chunkname, line)
  local chunk, compile_error = compile(code, chunkname, line)
  if not chunk then return "err", tostring(compile_error) end
  local results = interruptible_pcall(chunk)
  if not results[1] then return "err", tostring(results[2]) end
  local rendered, text = pcall(render_values, results, 2)
  return rendered and "ok" or "err", tostring(text)
end

local function reply(status, text)
  stdout:write(STX, status, US, text, ETX, "\n")
  stdout:flush()
end

local function read_file(path)
  local file = assert(open(path, "rb"))
  local code = file:read("*a")
  file:close()
  return code
end

local function run_file(path, chunkname, line)
  return run(read_file(path), chunkname, line)
end

function __emacs_eval_file(path, chunkname, line)
  local ok, status, text = pcall(run_file, path, chunkname, line)
  if ok then return reply(status, text) end
  reply("err", tostring(status))
end

-- REPL fed one line at a time, by `lua evaluator.lua INTERRUPT-FILE` (plain
-- io.lines, so Lua never loads readline/libedit) or by love-repl.lua per frame.

local function write_prompt(prompt)
  stdout:write(prompt)
  stdout:flush()
end

local function say(text)
  stdout:write(text, "\n")
end

local function incomplete(compile_error)
  return compile_error ~= nil and compile_error:find("<eof>'?$") ~= nil
end

local function answer(chunk, compile_error)
  if not chunk then return say(compile_error) end
  local results = interruptible_pcall(chunk)
  if not results[1] then return say(tostring(results[2])) end
  if results.n > 1 then say(render_values(results, 2)) end
end

local function new_session()
  local pending
  return function(line)
    local source = pending and (pending .. "\n" .. line) or line
    local chunk, compile_error = compile(source, "stdin", 1)
    if not chunk and incomplete(compile_error) then
      pending = source
      return write_prompt(">> ")
    end
    pending = nil
    local ok, failure = pcall(answer, chunk, compile_error)
    if not ok then say(tostring(failure)) end
    write_prompt("> ")
  end
end

local function serve()
  say(_VERSION)
  write_prompt("> ")
  local feed = new_session()
  for line in lines() do feed(line) end
end

local script_interrupt_file = ...
if script_interrupt_file then
  set_interrupt_file(script_interrupt_file)
  serve()
end

return {
  new_session = new_session,
  say = say,
  set_interrupt_file = set_interrupt_file,
  write_prompt = write_prompt,
}
