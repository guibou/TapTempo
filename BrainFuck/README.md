This is TapTempo in brainfuck, with a Brainfuck VM written in Haskell.

# About the VM

The cell `-1` is special, any write on it will trigger a write on cell `0` of the time (in second * 5) since the last write (default to 1 for the first time)

# Install / Build

```
nix-build
```

The resulting derivation contains `Bf` which is the brainfuck virtual machine and `TapTempoBF` which just dump the brainfuck code.


# Exemple

```
$ nix-build
[...]

# Dump the brainfuck program
$ ./result/bin/TapTempoBF > bf

# Run the VM
$ ./result/bin/Bf bf
Appuyer sur une touche en cadence (q pour quitter).
Tempo : 300 bpm.
Tempo : 150 bpm.
Tempo : 300 bpm.
Tempo : 300 bpm.
Tempo : 300 bpm.
Tempo : 300 bpm.
Tempo : 300 bpm.
Tempo : 300 bpm.
Tempo : 300 bpm.
Tempo : 300 bpm.
Tempo : 27 bpm.
Tempo : 60 bpm.
Tempo : 60 bpm.
```
