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
]] --
require("math")
require('tranquility/time_span')
require('tranquility/event')
require('tranquility/state')
require('tranquility/map')
require('tranquility/filter')

Pattern = { _query = function(_) return {} end }

function Pattern:create(o)
    o = o or {}
    setmetatable(o, self)
    self.__index = self
    return o
end

function Pattern:new(query)
    return Pattern:create { _query = query }
end

function Pattern:query(state)
    return self._query(state)
end

function Pattern:queryArc(beginTime, endTime)
    return self._query(State:new(TimeSpan:new(beginTime, endTime)))
end

function Pattern:onsetsOnly()
    return self:filterEvents(function (event)
        return event:hasOnset()
    end)
end

function Pattern:filterEvents(filterFunc)
    return Pattern:new(function (state)
        return Filter(self:query(state), filterFunc)
    end)
end

function Pure(value)
    local query = function (state)
        return Map(
            function(subspan)
                local whole = TimeSpan:wholeCycle(subspan:beginTime())
                return Event:new(whole, subspan, value)
            end
            , state:span():spanCycles()
        )
    end
    return Pattern:new(query)
end
