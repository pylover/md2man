# Create Debian package for md2man


## Prerequicites

```bash
sudo apt install autoconf automake autotools-dev \
                 debhelper and dh-make \
                 debmake
```

## Build

```bash
cd path/to/md2man/deb
make
ls md2man_*.deb
```


