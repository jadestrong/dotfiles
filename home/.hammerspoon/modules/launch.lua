local hotkey = require 'hs.hotkey'
local window = require 'hs.window'
local application = require 'hs.application'

local key2App = {
    s = 'Finder',
    h = 'Arc',
    -- j = 'UTM',
    l = 'iTerm',
    -- m = 'UTM',
    -- v = 'code',
    -- s = 'PxCook',
    -- p = 'PDF Expert',
    d = '飞书',
    e = 'Emacs',
    k = '百度翻译',
    -- o = 'Outlook',
    -- x = 'XMind',
    -- n = 'PDF',
    -- g = 'Charles',
    -- u = 'CocosCreator',
}

local otherKey2App = {
  h = 'Chrome',
  v = 'code',
  p = 'PDF Expert',
}

for key, app in pairs(key2App) do
    hotkey.bind(hyper, key, function()
        --application.launchOrFocus(app)
        if app == 'CocosCreator' then
            toggle_cocos_creator(app)
        else
            toggle_application(app)
        end
    end)
end

for key, app in pairs(otherKey2App) do
    hotkey.bind(hyperShift, key, function()
        --application.launchOrFocus(app)
        if app == 'CocosCreator' then
            toggle_cocos_creator(app)
        else
            toggle_application(app)
        end
    end)
end

-- reload
hotkey.bind(hyper, 'escape', function() hs.reload() end )

---------------------------------------------------------------

-- Toggle an application between being the frontmost app, and being hidden
function toggle_application(_app)
    -- finds a running applications
    local app = application.find(_app)
    if not app then
        -- application not running, launch app
        application.launchOrFocus(_app)
        return
    end
    local wins = app:allWindows()
    table.sort(wins, function(a, b) return a:id() < b:id() end)

    -- hs.alert.show(wins[1])
    -- hs.alert.show(wins[2])
    -- hs.alert.show(wins[3])
    local len = get_table_length(wins)
    -- application running, toggle hide/unhide
    local focusedWindow = hs.window.focusedWindow()
    local mainwin = app:mainWindow()
    -- 如果当前的窗口和要打开的 app 的是一个且同时存在多个窗口时
    if focusedWindow:application():bundleID() == app:bundleID() and len > 1 then
      local idx = hs.fnutils.indexOf(wins, focusedWindow);
      -- hs.alert.show(wins[1])
      -- hs.alert.show(focusedWindow)
      local next = idx + 1 > len and idx + 1 - len or idx + 1
      -- hs.alert.show(next)
      local win = wins[next]
      -- win:becameMain()
      win:focus()
      -- hs.alert.show(wins[next] == hs.window.focusedWindow())
    end

    if mainwin then
        if true == app:isFrontmost() then
            mainwin:application():hide()
        else
            mainwin:application():activate(true)
            mainwin:application():unhide()
            mainwin:focus()
        end
    else
        -- no windows, maybe hide
        if true == app:hide() then
            -- focus app
            application.launchOrFocus(_app)
        else
            -- nothing to do
        end
    end
    -- focus app' screen
    local currentScreen = window.focusedWindow():screen();
    local pt = hs.geometry.rectMidPoint(currentScreen:fullFrame())
    hs.mouse.setAbsolutePosition(pt)
end

function toggle_cocos_creator(_app)
    local focusedWindow = hs.window.focusedWindow()
    local apps = application.applicationsForBundleID('com.cocos.creator')
    local len = get_table_length(apps)
    if focusedWindow == nil then
        if len >= 1 then
            apps[1]:activate(true)
            apps[1]:focus()
        else
            application.launchOrFocus(_app)
        end
    elseif focusedWindow:application():bundleID() == 'com.cocos.creator' then
        -- 如果当前窗口就是 cocos 则尝试切换下一个
        local app = focusedWindow:application()
        local idx = hs.fnutils.indexOf(apps, app);
        local next = idx + 1 > len and idx + 1 - len or idx + 1 
        apps[next]:activate(true)
        app[next]:focus()
    else
        if len >= 1 then
            apps[1]:activate(true)
            apps[1]:focus()
        else
            application.launchOrFocus(_app)
        end
    end
end

function get_table_length(tbl)
    local getN = 0
    for n in pairs(tbl) do 
      getN = getN + 1 
    end
    return getN
end

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
