# Nighttime Alleyway

A procedurally generated, dark, and atmospheric alleyway written in GLSL. This project showcases procedural scene rendering, dynamic lighting, and shader-based visual effects, designed to run in real-time on ShaderToy.

## Files
- `baseplate.glsl` – The starting point of the project, containing basic primitives and simple lighting experiments. This file reflects my early explorations into shader programming and lighting effects. [View on ShaderToy](https://www.shadertoy.com/view/t3lcRf)
- `alley.glsl` – The final, polished scene featuring a complete nighttime alleyway with walls, ground, dynamic lighting, shadows, and sky effects. [View on ShaderToy](https://www.shadertoy.com/view/wfSBD3)

## Features
- Procedural generation of textures, lamps, ground, and other elements.
- Realistic lighting and soft shadows.
- Nighttime atmosphere with starfield.
- Adjustable parameters.

## Notes
- The scene is performance-intensive due to ray marching; reducing `STEPS` and `SHADOWMAXSTEPS` in the shader can improve performance. <sub>but mostly bc i suck at coding</sub>
- Experiment with constants like alley width, wall height, and lighting parameters at the top of the shader for custom visuals.
