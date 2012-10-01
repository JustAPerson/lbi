require("busted")

package.path = "/home/jason/workspace2/LBI/src/?.lua;" .. package.path
local lbi = require("main")
local lasm = require("MODS.src.current")

local loadb = lbi.load_bytecode

local function loadb(input)
	local bytecode = lasm.Assemble(input)
	return lbi.utils.debug_bytecode(bytecode)
end

local d, f, stack
describe("Running code", function()
	describe("General Instructions", function()
		it("MOVE", function()
			local d, f = loadb [[
			.options 0, 2, 0, 3
			move 0, 2
			move 1, 0
			move 2, 1
			return 0, 1
			]]
			f(1, 2)

			local stack = d.get_stack()
			assert.is.equal(stack[0], 2)
			assert.is.equal(stack[1], 1)
			assert.is.equal(stack[2], 1)
		end)

		describe("LOADK", function()
			it("LOADK - Boolean", function()
				d, f = loadb[[
				.options 0, 0, 0, 1
				loadk 0, <true>
				return 0, 1
				]]
				f()
				assert.is.equal(d.get_stack()[0], true)
			end)

			it("LOADK - Number", function()
				d, f = loadb[[
				.options 0, 0, 0, 1
				loadk	0, <3.14>
				return	0, 1
				]]
				f()
				assert.is.equal(d.get_stack()[0], 3.14)
			end)

			it("LOADK - String", function()
				d, f = loadb[[
				.options 0, 0, 0, 1
				loadk	0, <"string">
				return	0, 1
				]]
				f()
				assert.is.equal(d.get_stack()[0], "string")
			end)
		end)

		describe("LOADBOOL", function()
			it("LOADBOOL - False (0) without skip (0)", function()
				d, f = loadb[[
				.options 0, 0, 0, 2
				loadbool	0, 0, 0
				loadbool	1, 1, 0
				return	0, 1
				]]
				f()
				assert.is.equal(d.get_stack()[0], false)
				assert.is.equal(d.get_stack()[1], true)
			end)

			it("LOADBOOL - False (0) with skip (1)", function()
				d, f = loadb[[
				.options 0, 0, 0, 2
				loadbool	0, 1, 1
				loadbool	1, 1, 0
				return	0, 1
				]]
				f()
				assert.is.equal(d.get_stack()[0], true)
				assert.is.equal(d.get_stack()[1], nil)
			end)

			it("LOADBOOL - True (1) without skip (0)", function()
				d, f = loadb[[
				.options 0, 0, 0, 2
				loadbool	0, 1, 0
				loadbool	1, 1, 0
				return	0, 1
				]]
				f()
				assert.is.equal(d.get_stack()[0], true)
				assert.is.equal(d.get_stack()[1], true)
			end)

			it("LOADBOOL - True (1) with skip (1)", function()
				d, f = loadb[[
				.options 0, 0, 0, 2
				loadbool	0, 1, 1
				loadbool	1, 1, 0
				return	0, 1
				]]
				f()
				assert.is.equal(d.get_stack()[0], true)
				assert.is.equal(d.get_stack()[1], true)
			end)
		end)

		describe("LOADNIL", function()
			it("LOADNIL - Single", function()
				d, f = loadb[[
				.options 0, 0, 0, 1
				loadk	0, <"string">
				loadnil	0, 0, 0
				return	0, 1
				]]
				f()
				--assert.is_not.equal(d.get_stack()[0], "string")
				assert.is.equal(d.get_stack()[0], nil)
			end)

			it("LOADNIL - Single", function()
				d, f = loadb[[
				.options 0, 0, 0, 3
				loadk	0, <"string1">
				loadk	1, <"string2">
				loadk	2, <"string3">
				loadnil	0, 2, 0
				return	0, 1
				]]
				f()
				assert.is.equal(d.get_stack()[0], nil)
				assert.is.equal(d.get_stack()[1], nil)
				assert.is.equal(d.get_stack()[2], nil)
			end)
		end)
	end)

	describe("Mathematics", function()

	end)
end)