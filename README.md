# Super Haskell 🎮

A Mario-style functional 2D platformer written in Haskell using [Gloss](https://hackage.haskell.org/package/gloss).

> **Course:** Functional Programming (Group 57)  
> **Music Credits:** [Pixabay - Electronic Game Music](https://pixabay.com/music/electronic-game-game-music-508018/)

---

## TOC

- [System Dependencies](#system-dependencies-sdl2-audio)
- [Alternative: Run Without Music (`no_music` branch)](#alternative-run-without-music-no_music-branch)
- [Installation & Running](#installation--running)
- [Controls](#controls)

## System Dependencies (SDL2 Audio)

The main branch includes background music using `sdl2` and `sdl2-mixer`. These C libraries must be installed on your system before Cabal can build them.

### Ubuntu / Debian

```bash
sudo apt update
sudo apt install libsdl2-dev libsdl2-mixer-dev libgl1-mesa-dev libglu1-mesa-dev freeglut3-dev
```

### Arch Linux

```bash
sudo pacman -S sdl2 sdl2_mixer freeglut
```

### macOS (Homebrew)

```bash
brew install sdl2 sdl2_mixer
```

### Windows (GHCup + MSYS2)

For FreeGlut Font run:

```powershell
C:\ghcup\msys64\usr\bin\bash.exe -lc "pacman -S --noconfirm mingw-w64-x86_64-freeglut"
```

Besides this, windows requires the SDL2 native libraries AND a compatible MinGW/Clang toolchain.
Spent 3 hours debugging (because cabal, GHCup and MSYS2 do not play nice), cry, give up and then switch to the `no_music` branch.

## Alternative: Run Without Music (`no_music` branch)

If you do not want to install SDL2 C-libraries or are having issues with installation, you can switch to the `no_music` branch:

```bash
git checkout no_music
```

This branch removes the `sdl2` and `sdl2-mixer` C-dependencies, allowing the game to compile using only Haskell dependencies.

## Installation & Running

1. **Update package lists:**

   ```bash
   cabal update
   ```

2. **Build the project:**

   ```bash
   cabal build
   ```

3. **Run the game:**
   ```bash
   cabal run
   ```

## Controls

|     Key     | Action                                                       |
| :---------: | :----------------------------------------------------------- |
|   **`A`**   | Walk Left                                                    |
|   **`D`**   | Walk Right                                                   |
|   **`W`**   | Jump (only when grounded on a platform)                      |
|   **`S`**   | Fast-Fall (accelerate downwards)                             |
|   **`P`**   | Pause / Resume Game                                          |
|   **`K`**   | Save Game State (saves to `save.json`)                       |
|   **`L`**   | Load Game State (loads from `save.json`)                     |
|   **`B`**   | Toggle Debug Hitboxes                                        |
| **`Enter`** | Confirm menu selection / Retry on Game Over / Proceed        |
|   **`M`**   | Return to Main Menu (from Pause, Game Over, or Level Select) |
