-- https://github.com/Hammerspoon/hammerspoon/issues/1462
function screenLayoutWatcher()
    local screens = hs.screen.allScreens()
    for key, screen in pairs(screens) do
        if ((screen:getUUID() == "52D0368A-4344-CE4A-53CC-4D451DFCE22D") and (screen:fullFrame().x ~= -2560)) then
            print(hs.execute('/usr/local/bin/displayplacer "id:173BF832-BA4F-E9DD-DBE9-A57FB16E961C res:1792x1120 hz:59 color_depth:4 scaling:on origin:(0,0) degree:0" "id:52D0368A-4344-CE4A-53CC-4D451DFCE22D res:2560x1440 hz:60 color_depth:8 scaling:off origin:(-2560,0) degree:0" "id:FF66CF66-62EC-B9DA-BA26-D76DE42396CF res:1440x2560 hz:60 color_depth:8 scaling:off origin:(1792,-638) degree:90"'))
        end
        -- if ((screen:getUUID() == "FF66CF66-62EC-B9DA-BA26-D76DE42396CF") and (screen:fullFrame().x ~= 1792)) then
        --     print(hs.execute("/usr/local/bin/displayplacer", "id:FF66CF66-62EC-B9DA-BA26-D76DE42396CF res:1440x2560 hz:60 color_depth:8 scaling:off origin:(1792,-638) degree:90"))
        -- end
    end
end

screenWatcher = hs.screen.watcher.new(screenLayoutWatcher)
screenWatcher:start()
screenLayoutWatcher()
