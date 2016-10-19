## Synopsis

This is a shiny (R) dashboard to present information extracted from a Jira (Atlassian) instance from projects under the Agile framework.

## Install instructions

This instructions assume you are **not** familiar to *R* programming language and just want to use this app. You may have *R* installed or not.

### General

 1. Clone the repo git clone https://github.com/rjof/jirar.git
 
 2. Insert your parameters in *app/shiny/R/parameters.R*

### Windows instructions in a system without *R* installed

 1. Download **R-portable** version 3.3.3.1.0 and unpacket in *app/R-portable*

### Windows instruccion in a system with *R* installed

 1. Modify the file *run.bat* to point to your path for Rscript (Rscript.exe in Windows)

 ```sh
 app\R-Portable\App\R-Portable\bin\Rscript.exe %ROPTS% app\launcher.R 1> app.log 2>&1
 ```

## Use instructions

Windows: double click *run.bat*

Linux:
```sh
cd /app/shiny/R

R -e "shiny::runApp('app.R')"
```

Open the url showed in the terminal

## Motivation

I started the project because in my workplace began to use the **Original estimate, time spent and logwork** fields. The default capacities of Jira for following up my own log are very limited.

## Contributors

Maintainer's twitter account: @olveraricardo72

## License

Creative Commons
