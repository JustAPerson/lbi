local lbi = require "src.lbi"
local argc, arg = select("#", ...), {...}

local file_name = assert(arg[1], "file expected")
local bytecode

if file_name:sub(-5) == ".luac" then
	local file = io.open(file_name, "r")
	bytecode = file:read("*all")
	file:close()
elseif file_name:sub(-4) == ".lua" then
	bytecode = string.dump(loadfile(file_name))
end

local inputs = {}
for i = 2, argc do
	inputs[i-1] = arg[i]
end

lbi.load_bytecode(bytecode)(unpack(inputs, 1, argc-1))
