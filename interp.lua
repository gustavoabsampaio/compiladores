local lpeg = require "lpeg"

local pt = require "pt"

-----------------------------------------------------------
local function I (msg)
  return lpeg.P(function () print(msg); return true end)
end

-----------------------------------------------------------

local parser

do

local function node (tag, ...)
  local labels = {...}
  return function (...)
    local values = {...}
    local t = {tag = tag}
    for i = 1, #labels do
      t[labels[i]] = values[i]
    end
    return t
  end
end


local function binOpL (term, op)
  return lpeg.Cf(term * lpeg.Cg(op * term)^0,
                   node("binop", "e1", "op", "e2"))
end


local alpha = lpeg.R("az", "AZ", "__")
local digit = lpeg.R("09")
local alphanum = alpha + digit

local space = lpeg.V("space")

local opM = lpeg.C(lpeg.S("*/")) * space
local opA = lpeg.C(lpeg.S("+-")) * space
local opC = lpeg.C(lpeg.P(">=") + "<=" + "==" + "<>") * space

local maxpos = 0

-- create a token
local function T (t)
  return t * space
end

local reserved = {}

-- create a reserved word
local function Rw (w)
  reserved[w] = true
  return w * -alphanum * space
end

local comment = "#" * (lpeg.P(1) - "\n")^0

local function checkID (_, _, id)
  return not reserved[id], id
end

local ID = lpeg.Cmt(lpeg.C(alpha * alphanum^0), checkID) * space

local numeral = lpeg.C(digit^1) / tonumber * space


local primary = lpeg.V("primary")
local power = lpeg.V("power")
local product = lpeg.V("product")
local sum = lpeg.V("sum")
local comparison = lpeg.V("comparison")
local stat = lpeg.V("stat")
local stats = lpeg.V("stats")
local block = lpeg.V("block")

local exp = comparison

local grammar = lpeg.P{"prog",

  prog = space * stats * -1,

  stats = lpeg.Ct(stat^1) / node("stats", "stats"),

  block = T'{' * stats * T '}',

  stat = T";"
       + Rw"if" * exp * block * (Rw"else" * block)^-1
              / node("if", "cond", "th", "el")
       + Rw"return" * exp * T";" / node("return", "e")
       + ID * T"=" * exp * T";" / node("assg", "var", "exp"),

  primary = numeral / node("number", "val")
          + T"(" * exp * T")"
	  + ID / node("var", "id"),

  power = T"-" * power / node("neg", "e")
          + primary * (T"^" * power)^-1 /
	       function(e1, e2) return not e2 and e1 or
  		        {tag = "binop", op = "^", e1 = e1, e2 = e2} end,

  product = binOpL(power, opM),

  sum = binOpL(product, opA),

  comparison = binOpL(sum, opC),

  space = (lpeg.S(" \t\n") + comment)^0
            * lpeg.P(function (_,p) maxpos = math.max(maxpos, p); return true end)
}

function parser (source)
  local ast = grammar:match(source)
  if not ast then
    io.stderr:write("syntax error: ",
       string.sub(source, maxpos - 20, maxpos - 1),
       "|", string.sub(source, maxpos, maxpos + 20))
    os.exit(1)
  end
  return ast
end

end
-----------------------------------------------------------
local Compiler = { code = {}, vars = {}, nvars = 0 }

function Compiler:name2idx (name)
  local idx = self.vars[name]
  if not idx then
    idx = self.nvars + 1
    self.nvars = idx
    self.vars[name] = idx
  end
  return idx
end

function Compiler:addCode (op, ...)
  local params = {...}
  local code = self.code
  code[#code + 1] = op
  for i = 1, #params do
    code[#code + 1] = params[i]
  end
  return #code
end

function Compiler:fixjmp2here (jmp)
  self.code[jmp] = #self.code - jmp
end

function Compiler:codeStat (ast)
  local tag = ast.tag
  if tag == "return" then
    self:codeExp(ast.e)
    self:addCode("ret")
  elseif tag == "if" then
    self:codeExp(ast.cond)
    local jmp = self:addCode("IfZJmp", 0)
    self:codeStat(ast.th)
    if not ast.el then
      self:fixjmp2here(jmp)
    else
      local jmp1 = self:addCode("jmp", 0)
      self:fixjmp2here(jmp)
      self:codeStat(ast.el)
      self:fixjmp2here(jmp1)
    end
  elseif tag == "assg" then
    self:codeExp(ast.exp)
    self:addCode("store", self:name2idx(ast.var))
  elseif tag == "stats" then
    for i = 1, #ast.stats do
      self:codeStat(ast.stats[i])
    end
  else error("unknown tag " .. tag)
  end
end


function Compiler:codeExp (ast)
  local tag = ast.tag
  if tag == "number" then
    self:addCode("push", ast.val)
  elseif tag == "var" then
    self:addCode("load", self:name2idx(ast.id))
  elseif tag == "neg" then
    self:codeExp(ast.e)
    self:addCode("neg")
  elseif tag == "binop" then
    self:codeExp(ast.e1)
    self:codeExp(ast.e2)
    self:addCode("binop", ast.op)
  else error("unknown tag " .. tag)
  end
end


function compile (ast)
  Compiler:codeStat(ast)
  Compiler:addCode("push", 0)
  Compiler:addCode("ret")
  return Compiler.code
end


-----------------------------------------------------------
local binOps = {
	["+"] = function (a,b) return a + b end,
	["-"] = function (a,b) return a - b end,
	["*"] = function (a,b) return a * b end,
	["/"] = function (a,b) return a / b end,
	["^"] = function (a,b) return a ^ b end,
	[">="] = function (a,b) return a >= b and 1 or 0 end,
	[">"] = function (a,b) return a > b and 1 or 0 end,
	["<="] = function (a,b) return a <= b and 1 or 0 end,
	["<"] = function (a,b) return a < b and 1 or 0 end,
	["=="] = function (a,b) return a == b and 1 or 0 end,
	["~="] = function (a,b) return a ~= b and 1 or 0 end,
       }

local function run (code, stack, mem)
  local top = 0
  local pc = 1
  while true do
    local op = code[pc]
    -- [[
      io.write("---", pc, ":", op, ": ")
      for i = 1, top do io.write(stack[i] or "nil", " ") end
      io.write("\n")
    --]]
    if op == "ret" then
      return
    elseif op == "jmp" then
      pc = pc + 1
      pc = pc + code[pc]
    elseif op == "IfZJmp" then
      pc = pc + 1
      if stack[top] == 0 then
       pc = pc + code[pc]
     end
     top = top - 1
    elseif op == "push" then
      pc = pc + 1
      top = top + 1
      stack[top] = code[pc]
    elseif op == "load" then
      pc = pc + 1
      top = top + 1
      stack[top] = mem[code[pc]]
    elseif op == "store" then
      pc = pc + 1
      mem[code[pc]] = stack[top]
      top = top - 1
    elseif op == "neg" then
      stack[top] = -stack[top]
    elseif op == "binop" then
      pc = pc + 1
      stack[top - 1] = binOps[code[pc]](stack[top - 1], stack[top])
      top = top - 1
    else error("unknown instruction " .. op)
    end
    pc = pc + 1
  end
end
-----------------------------------------------------------
local input = io.read("*a")
local ast = parser(input)
print(pt.pt(ast))
local code = compile(ast)
print(pt.pt(code))
local stack = {}
local mem = {k0 = 0, k1 = 1, k10 = 10}
run(code, stack, mem)
print(stack[1])
