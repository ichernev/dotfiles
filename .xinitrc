# quick and durty test
# exec xmonad

xsetroot -cursor_name left_ptr
xsetroot -solid black
# xscreensaver &
# make sure the tray runs after xmonad has started
{
  sleep 10
  stalonetray &> /dev/null &
} &
# rsibreak &> /dev/null &
fix_screens &
chromium &
urxvt &
exec ck-launch-session dbus-launch xmonad
