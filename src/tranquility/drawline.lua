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

--- drawline.lua
-- @module drawline

require("tranquility.fraction")
require("tranquility.pattern")

--- intended for debugging, drawline renders the pattern as a string, where each character represents the same time span.
-- should only be used with single characters as values, otherwise the character slots will be messed up.
-- character legend:
-- - "|" cycle separator
-- - "-" hold previous value
-- - "." silence
-- @tparam Pattern pattern to draw
-- @tparam number how many char to draw
-- @treturn string result string
-- @usage
-- print(Dump(DrawLine(Fastcat({ Pure("bd"):fast(3), Pure("hh"), Pure("sd") }), 40)))

function DrawLine(pat, chars)
	chars = chars or 60
	local cycle = 0
	local pos = Fraction:new(0)
	local lines = List:new({ "" })
	local emptyLine = ""
	while #lines:at(1) < chars do
		local events = pat:queryArc(cycle, cycle + 1)
		local durations = events
			:filter(function(event)
				return event:hasOnset()
			end)
			:map(function(event)
				return event:duration()
			end)
		local charFraction = Gcd(durations)
		local totalSlots = charFraction:inverse()
		lines = lines:map(function(line, _)
			return line .. "|"
		end)
		emptyLine = emptyLine .. "|"
		for _ = 1, totalSlots:asFloat() do
			local _begin, _end = pos, pos + charFraction
			local matches = events:filter(function(event)
				return event._whole:beginTime() <= _begin and event._whole:endTime() >= _end
			end)
			local missingLines = matches:length() - lines:length()
			if missingLines > 0 then
				lines = lines:concat(List:new():fill(emptyLine, missingLines))
			end
			lines = lines:map(function(line, index)
				local event = matches:at(index)
				if event ~= nil then
					local isOnset = event._whole._begin == _begin
					local char = isOnset and "" .. event._value or "-"
					return line .. char
				end
				return line .. "."
			end)
			emptyLine = emptyLine .. "."
			pos = pos + charFraction
		end
		cycle = cycle + 1
	end
	return table.concat(lines._list)
end
