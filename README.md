# md2man

Convert markdown to Unix manual page.

## Install

### Debian

Download the debian package form the [latest release on github]
(https://github.com/pylover/md2man/releases/latest)

```bash
sudo dpkg -i md2man_*-amd64.deb
```


### From source

1. Install [Stack](https://docs.haskellstack.org/en/stable/README/)
2. Clone and cd to `md2man` and run:

```bash
stack install
```


## Help

```bash
man 1 md2man
```

```bash
md2man -h
md2man --help
```


### Contributing

#### Debian package

```bash
cd deb
make clean build
```
