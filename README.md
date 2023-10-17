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
7. Run `stack exec -- curl -O https://repo.msys2.org/mingw/x86_64/mingw-w64-x86_64-SDL2-2.0.18-2-any.pkg.tar.zst` to download SDL2.  
   Do note that by the time you read this, version 2.0.18-2 may no longer be available,
   in that case, browse to https://repo.msys2.org/mingw/x86_64/ and look for the oldest version of SDL2 still available.
   At the time of writing this, newer version do not compile using stack, so an older version must be used.
8. Run `stack exec -- pacman -U mingw-w64-x86_64-SDL2-2.0.18-2-any.pkg.tar.zst` to install SDL2.  
   Do not forget to replace the version in the above command in case you downloaded a different version.
9. Lastly, run either `stack build` or `stack run` to download and compile all dependencies and the project itself.  
   At this point, the SDL2 package should be able to compile properly, but keep your fingers crossed, just in case.
