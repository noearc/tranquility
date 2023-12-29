--[[
Copyright (C) 2023 David Minnix

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
]]
--
require("tranquility.fraction")
require("tranquility.pattern")

local function Gcd(...)
	local list = ...
	return list:reduce(function(acc, value)
		return acc:gcd(value)
	end, list:at(1))
end

function Fraction:inverse()
	return Fraction:new(self._denominator, self._numerator)
end

function P(pat)
	print(Dump(pat))
end

function T(pat)
	print(Dump(Type(pat)))
end

function DrawLine(pat, chars)
	chars = chars or 60
	local cycle = 0
	local pos = Fraction:new(0)
	local lines = List:new({ "" })
	local emptyLine = ""
	-- while #lines:at(1) < chars do
	local events = pat:queryArc(cycle, cycle + 1)
	local durations = events
		-- filter ?
		:filter(function(event)
			return event:hasOnset()
		end)
		:map(function(event)
			return event:duration()
		end)
	local charFraction = Gcd(durations)
	local totalSlots = charFraction:inverse() -- TODO:
	lines = lines:map(function(line)
		return line .. "|"
	end)
	emptyLine = emptyLine .. "|"
	for i = 1, totalSlots:asFloat() do
		local _begin, _end = pos, pos + charFraction
		local matches = events:filter(function(event)
			-- return event.whole:beginTime() <= _begin and event.whole:endTime() >= _end
			return event:duration() >= _end - _begin
		end)
		local missingLines = #matches - #lines
		-- print(matches)
		-- 		if missingLines > 0 then
		-- 			lines = lines:concat(List:new():fill(missingLines, emptyLine))
		-- 		end
		-- lines = lines:map(function(line, index)
		local line = ""
		matches:foreach(function(_, event)
			if event ~= nil then
				-- local isOnset = event.whole._begin == _begin
				local isOnset = true
				local char = isOnset and "" .. event._value or "-"
				-- P(event._value)
				P(line .. char)
				return line .. char
			end
		end)
		P(line)
		lines = lines:map(function(li)
			return li .. line
		end)
		emptyLine = emptyLine .. "."
		pos = pos + charFraction
	end
	-- 	cycle = cycle + 1
	-- end
	P(lines)
	-- return lines:concat()
end

DrawLine(Fastcat({ Pure("45"), Pure("bd"), Pure("sd") }), 4)
