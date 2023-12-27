require("tranquility.fraction")
require("tranquility.pattern")

-- TODO:
local function gcd(a, b)
	return (b == 0) and a or gcd(b, a % b)
end

function drawLine(pat, chars)
	chars = chars or 60
	local cycle = 0
	local pos = Fraction:new(0)
	local lines = List:new({ "" })
	local emptyLine = ""
	while #lines[1] < chars do
		local events = pat:queryArc(cycle, cycle + 1)
		local durations = events
			-- filter ?
			:filterEvents(function(event)
				return event:hasOnset()
			end)
			:map(function(event)
				return event.durations
			end)
		local charFraction = gcd(durations)
		local totalSlots = charFraction:inverse() -- TODO:
		lines = lines:map(function(line)
			return line .. "|"
		end)
		emptyLine = emptyLine .. "|"
		for _, i in pairs(totalSlots) do
			local _begin, _end = pos, pos + charFraction
			local matches = events:filterEvents(function(event)
				return event.whole._begin <= _begin and event.whole._end >= _end
			end)
			local missingLines = #matches - #lines
		end
	end
end
