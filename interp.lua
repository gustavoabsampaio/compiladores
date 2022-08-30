local lpeg = require "lpeg"

local pt = require "pt"

local function foldBin (lst)
  local res = lst[1]
  for i = 2, #lst, 2 do
    res = {tag = "binop", op = lst[i], e1 = res, e2 = lst[i + 1]}
  end
  return res
end

local space = lpeg.V("space")

local opM = lpeg.C(lpeg.S("*/")) * space
local opA = lpeg.C(lpeg.S("+-")) * space
local opC = lpeg.C(lpeg.P(">=") + "<=" + "==" + "<>" + lpeg.S("<>")) * space

local alpha = lpeg.R("az", "AZ", "__")
local digit = lpeg.R("09")
local alphanum = alpha + digit

local function T (t)
  return t * space
end

local function Rw (w)
  return w * -alphanum * space
end

local comment = "#" * (lpeg.P(1) - "\n")^0

local multicomment = lpeg.P("#[") * (lpeg.P(1))^0 - lpeg.P("]#")

local ID = lpeg.C(alpha * alphanum^0) * space

local numeral = lpeg.C(digit^1) / tonumber * space


local primary = lpeg.V("primary")
local power = lpeg.V("power")
local product = lpeg.V("product")
local sum = lpeg.V("sum")
local comparison = lpeg.V("comparison")
local stat = lpeg.V("stat")
local stats = lpeg.V("stats")

local exp = comparison

local grammar = lpeg.P{"prog",

  prog = space * stats * -1,

  stats = lpeg.Ct(stat * stat^0)
             / function (lst) return {tag="stats", stats = lst} end,

  stat = T";"
       + Rw"return" * exp * T";" / function (e) return {tag = "return", e = e} end
       + ID * T"=" * exp * T";"
             / function (var, exp) return {tag = "assg", var = var, exp = exp} end,

  primary = numeral / function (n) return {tag = "number", val = n} end
          + T"(" * exp * T")" + T"{" * primary * "}"
	  + ID / function (id) return {tag = "var", id = id} end,

  power = T"-" * power / function (e) return {tag = "neg", e = e} end
          + primary * (T"^" * power)^-1 /
	       function(e1, e2) return not e2 and e1 or
  		        {tag = "binop", op = "^", e1 = e1, e2 = e2} end,

  product = lpeg.Ct(power * (opM * power)^0) / foldBin,

  sum = lpeg.Ct(product * (opA * product)^0) / foldBin,

  comparison = lpeg.Ct(sum * (opC * sum)^-1) / foldBin,

  space = (lpeg.S(" \t\n") + multicomment + comment)^0
}


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

function Compiler:addCode (op)
  local code = self.code
  code[#code + 1] = op
end


function Compiler:codeStat (ast)
  local tag = ast.tag
  if tag == "return" then
    self:codeExp(ast.e)
    self:addCode("ret")
  elseif tag == "assg" then
    self:codeExp(ast.exp)
    self:addCode("store")
    self:addCode(self:name2idx(ast.var))
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
    self:addCode("push")
    self:addCode(ast.val)
  elseif tag == "var" then
    self:addCode("load")
    self:addCode(self:name2idx(ast.id))
  elseif tag == "neg" then
    self:codeExp(ast.e)
    self:addCode("neg")
  elseif tag == "binop" then
    self:codeExp(ast.e1)
    self:codeExp(ast.e2)
    self:addCode("binop")
    self:addCode(ast.op)
  else error("unknown tag " .. tag)
  end
end


function compile (ast)
  Compiler:codeStat(ast)
  Compiler:addCode("push")
  Compiler:addCode(0)
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
      io.write("---", op, ": ")
      for i = 1, top do io.write(stack[i], " ") end
      io.write("\n")
    --]]
    if op == "ret" then
      return
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
local ast = grammar:match(input)
print(pt.pt(ast))
local code = compile(ast)
print(pt.pt(code))
local stack = {}
local mem = {k0 = 0, k1 = 1, k10 = 10}
run(code, stack, mem)
print(stack[1])
