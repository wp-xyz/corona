# corona
[![Build Status](https://github.com/wp-xyz/corona/workflows/build-test/badge.svg?branch=master)](https://github.com/wp-xyz/corona/actions)

Downloads daily accumulated case counts of the corona virus disease from the 2019 Novel Coronavirus COVID-19 (2019-nCoV) data repository by Johns Hopkins University CSSE, and from Robert-Koch-Institut, Germany.

- Plots the data as a function of time: confirmed, death, recovered, sick counts. Cumulative cases, new cases per day.
- Plots the universal curve new cases vs cumulative cases.
- Estimates characteristic pandemic numbers: doubling time, reproduction number. (Disclaimer: due to various calculation methods, these numbers may differ from official data).

## Installation
This repository contains Lazarus source files only. Please use Lazarus v2.0.6 or
later to compile the binary.

No additional packages are required to compile the application. But the OpenSSL library must be available. In Windows, copy the files libeay32.dll and ssleay32.dll of the correct bitness to the application directory.

For Windows the OpenSSL dlls can be downloaded from
- 32 bit: https://packages.lazarus-ide.org/openssl-1.0.2j-i386-win32.zip
- 64 bit: https://packages.lazarus-ide.org/openssl-1.0.2j-x64_86-win64.zip

## Data source
- https://github.com/CSSEGISandData/COVID-19
- https://npgeo-corona-npgeo-de.hub.arcgis.com/
