# `nimrod.R`

R functions to extract and visualise radar data from the NIMROD System.


# NIMROD

> "A collection of products from rain radars operated by the Met Office and
> other European agencies for the UK and Europe."

**Met Office (2003): Met Office Rain Radar Data from the NIMROD System. NCAS
British Atmospheric Data Centre.
<http://catalogue.ceda.ac.uk/uuid/82adec1f896af6169112d09cc1174499>**


# Example

```r
source('nimrod.R')

# read data (might have to specify the `nimrod_dir` option
img = nimrod_read(from='2020-02-01', to='2020-02-05', by='60 min')

# apply a bounding box (here: UK)
img = nimrod_apply_bbox(img, xlim=c(-2e5, 8e5), ylim=c(-1e5,12e5))

# get coast lines in nimrod region for plotting
coast = read.csv('coast.csv')

# animate the image collection
nimrod_animate(img, coast=coast, fps=10)

# plot a single image
nimrod_plot(img[[1]])
lines(coast)
```



# Details about NIMROD data

## Download

- The entry point to the NIMROD data on CEDA is
  <https://catalogue.ceda.ac.uk/uuid/82adec1f896af6169112d09cc1174499>
- You will have register a CEDA account and apply for permission to access the
  data.
- Ftp download with filezilla:
  - CEDA user name and password
  - Host: ftp.ceda.ac.uk 
  - Remote site: /badc/ukmo-nimrod/data/composite/uk-5km
  - drag and drop folders to local folders triggers download


## Local directory structure

- The R code assumes a local directory structure similar to how the files are
  stored on CEDA, i.e. daily `*.dat.gz.tar` files stored in subfolders per year
  
```
<nimrod_dir>
│
├── 2019
│   ├── metoffice-c-band-rain-radar_uk_20190101_5km-composite.dat.gz.tar
│   ├── metoffice-c-band-rain-radar_uk_20190102_5km-composite.dat.gz.tar
│   ├── ...
│
├── 2020
│   ├── metoffice-c-band-rain-radar_uk_20200101_5km-composite.dat.gz.tar
│   ├── metoffice-c-band-rain-radar_uk_20200102_5km-composite.dat.gz.tar
│   ├── ...
...
```





