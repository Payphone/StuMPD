# StuMPD
A StumpWM module for interacting with MPD

## Installation
Clone the repository and move it to your StumpWM modules folder. The modules
folder has changed several times throughout the StumpWM releases, so the best
way to find it is to eval `*modules-dir*` in StumpWM (C-t :).
```
git clone https://github.com/Payphone/mpd.github
mv mpd ~/quicklisp/local-projects/

git clone https://github.com/Payphone/stumpd.git
mv stumpd ~/.stumpwm.d/modules/
```
Now update your .stumpwmrc file to include the following:
```
(load-module "stumpd")
(use-package :stumpd)
(define-key *root-map* (kbd "m") 'stumpd:*mpd-playback-map*)
```
You can change the key binding to whatever suites your tastes.
