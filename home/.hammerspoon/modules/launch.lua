local hotkey = require 'hs.hotkey'
local window = require 'hs.window'
local application = require 'hs.application'

local key2App = {
    s = 'Finder',
    h = 'Google Chrome',
    -- j = 'Min',
    l = 'iTerm',
    m = 'MailMaster',
    -- v = 'code',
    -- s = 'PxCook',
    -- p = 'PDF Expert',
    d = 'Kim',
    t = 'Terminal',
    e = 'Emacs',
    k = '百度翻译',
    -- o = 'Outlook',
    -- x = 'XMind',
    -- n = 'PDF',
    -- g = 'Charles',
    u = 'CocosCreator',
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

    -- application running, toggle hide/unhide
    local mainwin = app:mainWindow()
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
