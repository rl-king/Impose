# Impose

Create a [book layout](https://en.wikipedia.org/wiki/Imposition) from a list of image file paths


``` shell
# Input:
<@impose>-<⎇ main>-> tree example
example
├── 1.tif
├── 2.tif
├── 3.tif
├── 4.tif
├── 5.tif
├── 6.tif
├── 7.tif
├── 8.tif
├── 9.tif
└── index


# Run:
<@impose>-<⎇ main>-> cabal run impose -- -i example -s 2


# Output:
<@impose>-<⎇ main>-> tree book
book
├── signature-1
│   ├── sheet-1
│   │   ├── back-left-8.tif
│   │   ├── back-right-1.tif
│   │   ├── front-left-2.tif
│   │   └── front-right-7.tif
│   └── sheet-2
│       ├── back-left-6.tif
│       ├── back-right-3.tif
│       ├── front-left-4.tif
│       └── front-right-5.tif
└── signature-2
    └── sheet-1
        └── back-right-9.tif
```
