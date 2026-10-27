require("hs.ipc")

local mediaControl = "/opt/homebrew/bin/media-control"
local resumeDelaySeconds = 2

local pausedByMic = false
local resumeTimer = nil
local watchedDevices = {}

local function isPlaying()
  local output = hs.execute(mediaControl .. " get")
  local ok, info = pcall(hs.json.decode, output or "")
  return ok and type(info) == "table" and info.playing == true
end

local function micInUse()
  for _, device in ipairs(hs.audiodevice.allInputDevices()) do
    if device:inUse() then
      return true
    end
  end
  return false
end

local function onMicStateChanged()
  if micInUse() then
    if resumeTimer then
      resumeTimer:stop()
      resumeTimer = nil
    end
    if not pausedByMic and isPlaying() then
      hs.execute(mediaControl .. " pause")
      pausedByMic = true
    end
  elseif pausedByMic and not resumeTimer then
    resumeTimer = hs.timer.doAfter(resumeDelaySeconds, function()
      resumeTimer = nil
      pausedByMic = false
      hs.execute(mediaControl .. " play")
    end)
  end
end

local function watchInputDevices()
  for _, device in pairs(watchedDevices) do
    device:watcherStop()
  end
  watchedDevices = {}
  for _, device in ipairs(hs.audiodevice.allInputDevices()) do
    device:watcherCallback(function(_, event)
      if event == "gone" then
        onMicStateChanged()
      end
    end)
    device:watcherStart()
    watchedDevices[device:uid()] = device
  end
end

hs.audiodevice.watcher.setCallback(function(event)
  if event == "dev#" then
    watchInputDevices()
  end
end)
hs.audiodevice.watcher.start()

watchInputDevices()
