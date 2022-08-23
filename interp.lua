local lpeg = require "lpeg"

local pt = require "pt"

local function foldBin (lst)
  local res = lst[1]
  for i = 2, #lst, 2 do
    res = {tag = "binop", op = lst[i], e1 = res, e2 = lst[i + 1]}
  end
  return res
end

local space = lpeg.S(" \t\n")^0

local opM = lpeg.C(lpeg.S("*/")) * space
local opA = lpeg.C(lpeg.S("+-")) * space

local OP = "(" * space
local CP = ")" * space

local numeral = (lpeg.C(lpeg.R("09")^1) / tonumber * space) /
                  function (n) return {tag = "number", val = n} end


local primary = lpeg.V("primary")
local prod = lpeg.V("prod")
local sum = lpeg.V("sum")

local grammar = lpeg.P{"sum",

  primary = numeral + OP * sum * CP,

  prod = lpeg.Ct(primary * (opM * primary)^0) / foldBin,

  sum = lpeg.Ct(prod * (opA * prod)^0) / foldBin

}

grammar = space * grammar * -1

-----------------------------------------------------------
local Compiler = { code = {} }

function Compiler:addCode (op)
  local code = self.code
  code[#code + 1] = op
end


local opcode = {["+"] = "add", ["-"] = "sub",
                ["*"] = "mul", ["/"] = "div"}

function Compiler:codeExp (ast)
  local tag = ast.tag
  if tag == "number" then
    self:addCode("push")
    self:addCode(ast.val)
  elseif tag == "binop" then
    self:codeExp(ast.e1)
    self:codeExp(ast.e2)
    self:addCode(opcode[ast.op])
  else error("unknown tag " .. tag)
  end
end


function compile (ast)
  Compiler:codeExp(ast)
  Compiler:addCode("ret")
  return Compiler.code
end


-----------------------------------------------------------
local function run (code, stack)
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
    elseif op == "add" then
      stack[top - 1] = stack[top - 1] + stack[top]
      top = top - 1
    elseif op == "sub" then
      stack[top - 1] = stack[top - 1] - stack[top]
      top = top - 1
    elseif op == "mul" then
      stack[top - 1] = stack[top - 1] * stack[top]
      top = top - 1
    elseif op == "div" then
      stack[top - 1] = stack[top - 1] / stack[top]
      top = top - 1
    else error("unknown instruction " .. op)
    end
    pc = pc + 1
  end
end
-----------------------------------------------------------
local input = io.read("a")
local ast = grammar:match(input)
print(pt.pt(ast))
local code = compile(ast)
print(pt.pt(code))
local stack = {}
run(code, stack)
print(stack[1])
