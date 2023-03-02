#!/bin/bash

# quick and durty test
# exec xmonad

# xrandr --auto
# xrandr --output eDP-1-1 --auto --primary
# xrandr --setprovideroutputsource modesetting NVIDIA-0
# xrandr --auto

# echo "EXE XMONAD"
# logger "EXE XMONAD"

# exec urxvt
exec xmonad

xset +fp /usr/share/fonts/local
xset fp rehash

xsetroot -cursor_name left_ptr
xsetroot -solid black
# xscreensaver &
# make sure the tray runs after xmonad has started
{
  sleep 10
  # stalonetray &> /dev/null &
  trayer --edge top --align right \
    --widthtype pixel --width 100 \
    --heighttype pixel --height 20 \
    --tint 0x000000 --alpha 0 --transparent true &> /dev/null &
} &
# rsibreak &> /dev/null &
fix_screens force &
chromium &
urxvt &
# exec ck-launch-session dbus-launch xmonad
pulseaudio --check || pulseaudio --start -D
exec dbus-launch --exit-with-session xmonad
