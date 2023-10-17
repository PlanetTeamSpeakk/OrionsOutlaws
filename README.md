# Orions Outlaws
A spaceshooter-like game written in Haskell using Gloss.

## Installing SDL
This game requires SDL (for playing audio). Installing it can be quite a hassle, so here's a short guide:
1. Ensure you have installed [Stack](https://www.haskellstack.org/).
2. Ensure this repo is cloned and you are cd'd into it.
3. Ensure you have ran `stack setup`.
4. Run `stack exec -- pacman -Syu` to install the `pacman` package manager if it isn't installed already.
5. Close the terminal you ran the above command in and open a new one.
6. Run `stack exec -- pacman -S mingw-w64-x86_64-pkg-config` to install `pkg-config`.
7. Run `stack exec -- pacman -U mingw-w64-x86_64-SDL2-2.0.18-2-any.pkg.tar.zst` to install SDL2.  
   The package file should already be included in this repo, but if it isn't, [you can download it from here](https://repo.msys2.org/mingw/x86_64/).
8. Lastly, run either `stack build` or `stack run` to download and compile all dependencies and the project itself.  
   At this point, the SDL2 package should be able to compile properly, but keep your fingers crossed, just in case.
