local function Chinese()
    hs.keycodes.currentSourceID("im.rime.inputmethod.Squirrel.Rime")
end

local function English()
    hs.keycodes.currentSourceID("com.apple.keylayout.ABC")
end

-- app to expected ime config
local app2Ime = {
    { 'iTerm2', 'English' },
    { 'Emacs', 'English' },
    { 'Google Chrome', 'Chinese' },
    { 'Arc', 'Chinese' },
    { '访达', 'English' },
    { '飞书', 'Chinese' },
    { 'Code', 'English' },
    { '百度翻译', 'Chinese' }
}
-- local app2Ime = {
--     {'/Applications/iTerm.app', 'English'},
--     {'/Applications/Xcode.app', 'English'},
--     {'/Applications/Google Chrome.app', 'Chinese'},
--     {'/System/Library/CoreServices/Finder.app', 'English'},
--     {'/Applications/DingTalk.app', 'Chinese'},
--     {'/Applications/Kindle.app', 'English'},
--     {'/Applications/NeteaseMusic.app', 'Chinese'},
--     {'/Applications/微信.app', 'Chinese'},
--     {'/Applications/System Preferences.app', 'English'},
--     {'/Applications/Dash.app', 'English'},
--     {'/Applications/MindNode.app', 'Chinese'},
--     {'/Applications/Preview.app', 'Chinese'},
--     {'/Applications/wechatwebdevtools.app', 'English'},
--     {'/Applications/Sketch.app', 'English'},
-- }

function updateFocusAppInputMethod()
    -- local focusAppPath = hs.window.frontmostWindow():application():path()
    local focusAppName = hs.window.frontmostWindow():application():name();
    -- hs.alert.show(focusAppName);
    -- hs.alert.show(hs.keycodes.layouts());
    for index, app in pairs(app2Ime) do
        local appName = app[1]
        -- local appPath = app[1]
        local expectedIme = app[2]
        -- if focusAppPath == appPath then
        if focusAppName == appName then
            if expectedIme == 'Chinese' then
                Chinese()
            else
                English()
            end
            break
        end
    end
end

-- helper hotkey to figure out the app path and name of current focused window
hs.hotkey.bind({'ctrl', 'cmd'}, ".", function()
    hs.alert.show("App path:        "
    ..hs.window.focusedWindow():application():path()
    .."\n"
    .."App name:      "
    ..hs.window.focusedWindow():application():name()
    .."\n"
    .."IM source id:  "
    ..hs.keycodes.currentSourceID())
end)

-- Handle cursor focus and application's screen manage.
function applicationWatcher(appName, eventType, appObject)
    if (eventType == hs.application.watcher.activated) then
        updateFocusAppInputMethod()
    end
end

appWatcher = hs.application.watcher.new(applicationWatcher)
appWatcher:start()
