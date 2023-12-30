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

--- pattern.lua
-- @module pattern

require("math")
require("tranquility.state")
require("tranquility.type")
require("tranquility.event")

function P(pat)
	print(Dump(pat))
end

function T(pat)
	print(Dump(Type(pat)))
end

Pattern = {
	_query = function(_)
		return List:new()
	end,
}

function Pattern:create(o)
	o = o or {}
	setmetatable(o, self)
	self.__index = self
	return o
end

function Pattern:new(query)
	return Pattern:create({ _query = query })
end

function Pattern:type()
	return "tranquility.Pattern"
end

function Pattern:query(state)
	return self._query(state)
end

--- Query haps inside the given time span
-- @param begin from time
-- @param end to time
-- @return Event
function Pattern:queryArc(beginTime, endTime)
	local span = TimeSpan:new(beginTime, endTime)
	local state = State:new(span)
	return self._query(state)
end

--- Returns a new pattern, with all haps without onsets filtered out. A hap with an onset is one with a `whole` timespan that begins at the same time as its `part` timespan
-- @return Pattern
function Pattern:onsetsOnly()
	return self:filterEvents(function(event)
		return event:hasOnset()
	end)
end

--- Returns a new Pattern, which only returns haps that meet the given test.
-- @tparam function function which returns false for haps to be removed from the pattern
-- @return Pattern
function Pattern:filterEvents(filterFunc)
	return Pattern:new(function(state)
		return self._query(state):filter(filterFunc)
	end)
end

--- Returns a new pattern, with queries split at cycle boundaries. This makes some calculations easier to express, as all haps are then constrained to happen within a cycle.
-- @return Pattern
function Pattern:splitQueries()
	local function splitQuery(state)
		return state
			:span()
			:spanCycles()
			:map(function(subspan)
				return self._query(state:setSpan(subspan))
			end)
			:flatten()
	end

	return Pattern:new(splitQuery)
end

--- Returns a new pattern, where the given function is applied to the query timespan before passing it to the original pattern.
--  @tparam Function function to apply
--  @treturn Pattern
function Pattern:withQuerySpan(func)
	return Pattern:new(function(state)
		return self._query(state:withSpan(func))
	end)
end

---  As with {@link Pattern#withQuerySpan}, but the function is applied to both the begin and end time of the query timespan.
--  @tparam Function function to apply
--  @return Pattern
function Pattern:withQueryTime(func)
	return Pattern:new(function(state)
		return self._query(state:withSpan(function(span)
			return span:withTime(func)
		end))
	end)
end

--- As with {@link Pattern#withHapSpan}, but the function is applied to both the begin and end time of the hap timespans.
-- @tparam Function func the function to apply
-- @return Pattern
function Pattern:withEventTime(func)
	local query = function(state)
		return self._query(state):map(function(event)
			--HACK: tmp fix?
			if Type(event) == "tranquility.Event" then
				return event:withSpan(function(span)
					return span:withTime(func)
				end)
			end
		end)
	end
	return Pattern:new(query)
end

function Pattern:_bindWhole(chooseWhole, func)
	local patVal = self
	local query = function(state)
		local withWhole = function(a, b)
			return Event:new(chooseWhole(a:whole(), b:whole()), b:part(), b:value())
		end

		local match = function(a)
			return func(a:value()):query(state:setSpan(a:part())):map(function(b)
				return withWhole(a, b)
			end)
		end

		return patVal:query(state):map(match):flatten()
	end
	return Pattern:new(query)
end

function Pattern:outerBind(func)
	local chooseOuterWhole = function(_, whole)
		return whole
	end
	return self:_bindWhole(chooseOuterWhole, func)
end

--- Flattens a pattern of patterns into a pattern, where wholes are taken from inner haps.
function Pattern:outerJoin()
	local function id(x)
		return x
	end

	return self:outerBind(id)
end

function Pattern:_patternify(method)
	local patterned = function(patSelf, ...)
		local patArg = Sequence(List:promote(...))
		-- print("PAT ARG")
		-- print(patArg)
		-- print(patSelf)
		-- print(List:promote(...))
		-- print(method)
		return patArg
			:fmap(function(arg)
				return method(patSelf, arg)
			end)
			:outerJoin()
	end
	return patterned
end

--- Returns a new pattern, with the function applied to the value of each event. synonyms fmap
-- @tparam function func to apply to the value
-- @treturn pattern
function Pattern:withValue(func)
	local query = function(state)
		return self:query(state):map(function(e)
			return e:withValue(func)
		end)
	end
	return Pattern:new(query)
end

function Pattern:fmap(func)
	return self:withValue(func)
end

function Pattern:_fast(value)
	local fastQuery = self:withQueryTime(function(t)
		return t * value
	end)
	local fastPattern = fastQuery:withEventTime(function(t)
		return t / value
	end)
	return fastPattern
end

function Pattern:_slow(value)
	return self:_fast(1 / value)
end

function Pattern:_early(offset)
	offset = Fraction:new(offset)
	local earlyQuery = self:withQueryTime(function(t)
		return t + offset
	end)
	local earlyPattern = earlyQuery:withEventTime(function(t)
		return t - offset
	end)
	return earlyPattern
end

function Pattern:_late(offset)
	return self:_early(0 - offset)
end

function Pattern:firstCycle()
	return self:queryArc(0, 1)
end

function Pattern:__tostring()
	return tostring(self:firstCycle())
end

function Pattern:show()
	return self:__tostring()
end

Pattern.fast = Pattern:_patternify(function(patSelf, val)
	return patSelf:_fast(val)
end)

Pattern.slow = Pattern:_patternify(function(patSelf, val)
	return patSelf:_slow(val)
end)

Pattern.early = Pattern:_patternify(function(patSelf, val)
	return patSelf:_early(val)
end)

Pattern.late = Pattern:_patternify(function(patSelf, val)
	return patSelf:_late(val)
end)

function Pure(value)
	local query = function(state)
		return state:span():spanCycles():map(function(subspan)
			local whole = TimeSpan:wholeCycle(subspan:beginTime())
			return Event:new(whole, subspan, value, List:new(), false)
		end)
	end
	return Pattern:new(query)
end
--def _sequence_count(x):
--    if type(x) == list or type(x) == tuple:
--        if len(x) == 1:
--            return _sequence_count(x[0])
--        else:
--            return (fastcat(*[sequence(x) for x in x]), len(x))
--    if isinstance(x, Pattern):
--        return (x,1)
--    else:
--        return (pure(x), 1)
function Reify(pat)
	if Type(pat) ~= "tranquility.Pattern" then
		return Pure(pat)
	end
	return pat
end

--- Concatenation: combines a list of patterns, switching between them successively, one per cycle. synonyms: {@link cat}
--  @return {Pattern}
--  @usage
--  slowcat("e5", "b4", ["d5", "c5"])

function Slowcat(pats)
	pats = List:promote(pats)
	pats = pats:map(Reify)
	local function query(state)
		local numPats = pats:length()
		local pat = pats:at((state:span():beginTime():floor() % numPats) + 1)
		return pat._query(state)
	end

	return Pattern:new(query):splitQueries()
end

function Fastcat(pats)
	pats = List:promote(pats)
	return Slowcat(pats):_fast(pats:length())
end

local function _sequenceCount(x)
	if Type(x) == "tranquility.List" then
		if x:length() == 1 then
			return _sequenceCount(x:at(1))
		else
			local pats = x:map(Sequence)
			return Fastcat(pats), x:length()
		end
	elseif Type(x) == "tranquility.Pattern" then
		return x, 1
	else
		return Pure(x), 1
	end
end

function Sequence(x)
	local seq, _ = _sequenceCount(x)
	return seq
end

function Silence()
	return Pattern:new(function() end)
end

--- Compress each cycle into the given timespan, leaving a gap
-- @usage
--   cat(
--     s("bd sd").compress(.25,.75),
--     s("~ bd sd ~")
--   )
function Pattern:compress(b, e)
	local _begin = b
	local _end = e
	if Type(b) ~= "tranquility.Fraction" then
		_begin = Fraction:new(b)
		_end = Fraction:new(e)
	end
	if
		_begin > _end
		or _end > Fraction:new(1)
		or _begin > Fraction:new(1)
		or _begin < Fraction:new(0)
		or _end < Fraction:new(0)
	then
		return Silence()
	end
	return self:fastgap(Fraction:new(1) / (_end - _begin)):late(b)
end

--- speeds up a pattern like fast, but rather than it playing multiple times as fast would it instead leaves a gap in the remaining space of the cycle. For example, the following will play the sound pattern "bd sn" only once but compressed into the first half of the cycle, i.e. twice as fast. synonyms fastgap
-- @tparam number factor of fast
-- @name fastGap
-- @usage
-- s("bd sd").fastGap(2)
function Pattern:fastgap(factor)
	if Type(factor) == "tranquility.Fraction" then
		factor = factor:asFloat()
	end

	if factor <= 0 then
		return Silence()
	end

	local factor_ = math.max(1, factor)

	local function munge_query(t)
		return t:sam() + ((t - t:sam()) * factor_):min(1)
	end

	local function event_span_func(span)
		local _begin = span._begin:sam() + (span._begin - span._begin:sam()) / factor_
		local _end = span._begin:sam() + (span._end - span._begin:sam()) / factor_
		return TimeSpan:new(_begin, _end)
	end

	local function query(state)
		local span = state:span()
		local new_span = TimeSpan:new(munge_query(span._begin), munge_query(span._end))
		if new_span._begin == span._begin:nextSam() then
			return {}
		end
		local res = List:new()
		local new_state = State:new(new_span)
		self:query(new_state):foreach(function(_, e)
			res:insert(e:withSpan(event_span_func))
		end)
		return res
	end
	return Pattern:new(query):splitQueries()
end
