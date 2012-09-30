require("main");

local test = require "input" 
local bytecode = ('').dump(test);

local func = load_bytecode(bytecode);
print("TEST:", test(10, 5))
print("FUNC:", func(10, 5))