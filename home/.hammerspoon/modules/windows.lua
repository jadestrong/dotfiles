require "hs.application"
local hotkey = require 'hs.hotkey'
local window = require 'hs.window'
local layout = require 'hs.layout'
local alert = require 'hs.alert'
local hints = require 'hs.hints'
local grid = require 'hs.grid'
local geometry = require 'hs.geometry'

---- hyper [ for left one half window
hotkey.bind(hyper, '[', function() window.focusedWindow():moveToUnit(layout.left50) end)

-- hyper ] for right one half window
hotkey.bind(hyper, ']', function() window.focusedWindow():moveToUnit(layout.right50) end)

-- hotkey.bind(hyper, 'Tab', function() window.focusedWindow():moveToUnit(layout.maximized) end)
hotkey.bind(hyperShift, 'Tab', function()
               local win = window.focusedWindow()
               local f = win:frame()
               local screen = win:screen()
               local max = screen:frame()

               f.x = max.x
               f.y = max.y
               f.w = max.w
               f.h = max.h

               win:setFrame(f)
end)
-- Hyper / to show window hints
hotkey.bind(hyper, '/', function()
    hints.windowHints()
end)

-- Hotkeys to interact with the window grid
-- hotkey.bind(hyper, ',', grid.show)
hotkey.bind(hyper, 'Left', grid.pushWindowLeft)
hotkey.bind(hyper, 'Right', grid.pushWindowRight)
hotkey.bind(hyper, 'Up', grid.pushWindowUp)
hotkey.bind(hyper, 'Down', grid.pushWindowDown)

-- switch focus to another display

hotkey.bind(hyperShift, 'o', function ()
    focusScreen(window.focusedWindow():screen():next())
end)

-- Predicate that checks if a window belongs to screen
function isInScreen(screen, win)
   return win:screen() == screen
end

function focusScreen(screen)
   -- Get windows within screen, ordered from front to back
   -- If no window exist, bring focus to desktop. Otherwise, set focus on front-most application window.
   local windows = hs.fnutils.filter(
      window.orderedWindows(),
      hs.fnutils.partial(isInScreen, screen)
   )
   local windowToFocus = #windows > 0 and windows[1] or window.desktop()
   windowToFocus:focus()

   -- Move mouse to center of screen
   local pt = geometry.rectMidPoint(screen:fullFrame())
   hs.mouse.setAbsolutePosition(pt)
end

-- hotkey.bind(hyper, 'h', function ()
--    window.switcher.nextWindow()
-- end)
