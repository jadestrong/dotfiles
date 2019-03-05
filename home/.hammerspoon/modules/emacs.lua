local task = require "hs.task"
local logger = hs.logger.new('emacs', 'debug')
local application = require "hs.application"

local mod = {}

local function run(params)
   local app = application.find("Emacs")

   if not app then
      application.launchOrFocus("Emacs")
      task.new('/usr/loacl/bin/emacsclient', nil, logger.d, params):start()
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
         application.launchOrFocus("Emacs")
      else
         -- nothing to do
      end
   end
end

local function eval(sexp)
   run({'--no-wait', '--quiet', '--eval', sexp})
end

function mod.capture()
   eval('(org-capture)')
end

function mod.inbox()
   eval('(jm/open-inbox)')
end

function mod.references()
   eval('(js/open-references)')
end

function mod.agenda()
   eval('(org-agenda-list)')
end

return mod
