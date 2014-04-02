# [hs2048][1]

[![Build Status][2]][3]

A [2048][4] clone in Haskell.

This implements the game logic as well as a console interface for playing it.

![Screenshot][5]

If you want to play the latest version, install it through Cabal.

``` sh
$ cabal install hs2048
```

If you want to work on the package, everything you need is provided by
[Vagrant][6].

``` sh
$ vagrant up
$ vagrant ssh
```

``` sh
$ cd /vagrant
$ make
```

If you want to check out other Haskell implementations of 2048, there are a few
on GitHub.

- <https://github.com/badamson/2048.hs>
- <https://github.com/chrismwendt/2048>
- <https://github.com/DavidMihola/2048>
- <https://github.com/egonSchiele/ones>
- <https://github.com/godel9/2048-hs>
- <https://github.com/itaibn/2048>
- <https://github.com/Javran/h2048>
- <https://github.com/jgallag88/2048>
- <https://github.com/KevinCotrone/TwentyFourtyEight-Simulator>
- <https://github.com/konn/FRP2048>
- <https://github.com/mitchellwrosen/hs2048-free>
- <https://github.com/robinp/solve-2048>
- <https://github.com/scturtle/2048AI>
- <https://github.com/techtangents/2048Solver>
- <https://github.com/uncleverone/haskell2048>
- <https://github.com/YawarRaza7349/2048Game.hs>

[1]: https://github.com/tfausak/hs2048
[2]: https://travis-ci.org/tfausak/hs2048.svg?branch=master
[3]: https://travis-ci.org/tfausak/hs2048
[4]: https://github.com/gabrielecirulli/2048
[5]: http://i.imgur.com/utw0O1h.png
[6]: http://www.vagrantup.com
