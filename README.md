raymarcher
==========

Raymarcher written with gloss-raster and Accelerate.

![Animation - A yellow sphere and a small green cube casting shadows over a tiled plane.](Animation.gif)

Build
-----

Install `stack` using GHCUp, then

```
sudo apt install freeglut3-dev llvm-12
cd raymarcher
stack build
```

Then use `stack run` to run the program.

Controls
--------

- 'w', 'a', 's', 'd' : move forward/backward/left/right
- 'h', 'l' : turn left/right
- left/right arrow : change light direction
