-- Loaded into the REPL by modules/my/lua/repl.el; replies are framed as
-- STX status US value ETX so Emacs can split them from printed output.
-- Emacs treats ETX newline then a prompt as end of reply, so a stray prompt
-- (e.g. from lua-mode's init code) cannot end a redirect early.

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
  return (string.format("%q", s):gsub("\\\n", "\\n"))
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
  table.sort(keys, key_less)
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
  local inline = "{ " .. table.concat(entries, ", ") .. " }"
  if #indent + #inline <= WIDTH and not inline:find("\n") then return inline end
  local child_indent = indent .. "  "
  return "{\n" .. child_indent .. table.concat(entries, ",\n" .. child_indent) .. "\n" .. indent .. "}"
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
  return table.concat(rendered, ", ")
end

local function as_expression(code)
  -- select keeps a top-level error() out of tail position, where LuaJIT drops its location
  return "return select(1, " .. code:gsub("[%s;]+$", "") .. "\n)"
end

local function compile(code, chunkname, line)
  local padding = string.rep("\n", line - 1)
  local name = "@" .. chunkname
  local expression = load_chunk(padding .. as_expression(code), name)
  if expression then return expression end
  return load_chunk(padding .. code, name)
end

local function run(code, chunkname, line)
  local chunk, compile_error = compile(code, chunkname, line)
  if not chunk then return "err", tostring(compile_error) end
  local results = pack(pcall(chunk))
  if not results[1] then return "err", tostring(results[2]) end
  local rendered, text = pcall(render_values, results, 2)
  return rendered and "ok" or "err", tostring(text)
end

local function reply(status, text)
  io.stdout:write(STX, status, US, text, ETX, "\n")
  io.stdout:flush()
end

function __emacs_eval(code, chunkname, line)
  reply(run(code, chunkname, line))
end

function __emacs_eval_missing()
  reply("err", "Emacs evaluator is not loaded; restart the REPL")
end
