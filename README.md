# shiny-twang

This project is a Shiny wrapper for the TWANG package.

### Building the executables

Clone the Shiny Electron proejct
```
git clone https://github.com/dirkschumacher/r-shiny-electron.git .
```

Change directory
```
cd r-shiny-electron
```

Close the Shiny rep into the `shiny/` folder:
```
rm app.R
git clone git@code.rand.org:causal-tools/shiny-twang.git shiny
```

Start Docker:
```
docker build -t shiny-electron .
docker run -d -e PASSWORD=pass1234 --name shiny_electron --mount type=bind,source="$(pwd)",target=/home/r-shiny-electron shiny-electron
docker exec -it shiny_electron bash
```

To stop
```
docker stop shiny_electron
```

To make
```
electron-forge make
```
