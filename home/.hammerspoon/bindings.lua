local fnutils = require 'hs.fnutils'
local hotkey = require 'hs.hotkey'
local windows = require 'windows'
local emacs = require 'modules/emacs'

local logger = hs.logger.new('bindings', 'debug')
local hyper = { 'cmd', 'ctrl' }
local mod = {}

local hyperBindings = {
   { key = 'e', fn = emacs.capture }
}

local function buildBindFunction(binding)
  -- if binding.pos and binding.targetScreen then
  --   return windows.setPosition(binding.pos, binding.targetScreen, binding.reversable)
  -- elseif binding.pos then
  --   return windows.setPosition(binding.pos, 'primary', binding.reversable)
  -- elseif binding.layout then
  --   return windows.applyLayout(commonLayout, binding.layout)
   if binding.name then
      return windows.launchOrCycleFocus(binding.name)
   elseif binding.fn then
      return binding.fn
   end
end

local function bindToHyper(binding)
  local modifier = hyper

  if binding.shift then
    modifier = hyperShift
  end

  hotkey.bind(modifier, binding.key, buildBindFunction(binding))
end

function mod.init()
   -- hotkey.bind(hyper, 'e', function ()
   --                emasc.
   -- end)
  fnutils.each(hyperBindings, bindToHyper)
  -- local layoutBindings = fnutils.map(modeLayouts, buildLayoutBinding)
  -- mode.create({'option'}, 'space', 'Layout', layoutBindings)
  -- mode.create(hyper, 'space', 'General', generalBindings)

  -- hotkey.bind({'cmd'}, 'h', applications.slack)
  -- hotkey.bind({'cmd'}, 'm', windows.cycleScreen)
end

return mod
