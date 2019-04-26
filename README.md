# Pukunui

Sound experiments; Pukunui

## TODOs

- [x] primitive tested signal functions
- [x] primitive wave generation
- [x] insert modulator as a unit slot value
- [x] WAV file playback
- [x] audio playback thread
- [x] timeline information (like 'bar')
- [x] experimental sequencers
- [ ] event thread (sequencer patterns comes from other thread)
- [ ] devices -- large and compound units
- [ ] tracks

### Future works...?

- Sound library abstruction
    - cl-portaudio driver, [JACK](http://jackaudio.org) driver, PCM file driver...
- KLOS (Kiwi Lightweight Object System)
- Unit graph modification API
- Unit graph representation DSL
- Programing sequencers with [MML](https://en.wikipedia.org/wiki/Music_Macro_Language)
- Stereo units are based on monoural units
- Connectintg to UI through TCP/IP

## Installation

*Pukunui* requires the libraries below:

- PortAudio

## Usage

Place *Pukunui* to the path of ASDF, and load it.

Then type `(pukunui:start)` start playing some noise. If you want to stop the noise, type `(pukunui:stop)`.

There are examples for use, see [`examples` directory](examples/).

## Author

- TANAKA Shinichi (shinichi.tanaka45@gmail.com)

## License

*Pukunui* is licensed under the Lisp Lesser GNU General Public License (LLGPL).
