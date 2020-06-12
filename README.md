Access Data from the USDA ARMS Data API: https://www.ers.usda.gov/developer/data-apis/arms-data-api/ 

## Description   
Agricultural Resource Management Survey (ARMS) is the U.S. Department of Agriculture (USDA)’s primary source of information on the production practices, resource use, and economic well-being of America’s farms and ranches. The results of this survey are the only source of information available for objective evaluation of many critical issues related to agriculture and the rural economy.

Recently, USDA’s Economic Research Service (ERS) is making data from USDA’s Agricultural Resource Management Survey (ARMS) available through an Application Programming Interface (API) to better serve customers. The data in the API are available in JSON format and provide attribute-based querying. 

The `rarms` package is built on the ARMS data API, and it allows users to access and download ARMS data directly. 

## Package Installation   
To install the package, use: 
``
devtools::install_github("cbw1243/rarms")
``   
Install the `devtools` package if you do not have it on your computer. 

## Note   
You need a valid key to access USDA-ARMS data and to use this package. If you do not have a key, go to this website (https://api.data.gov/signup/) to get one. 

If you encounter an issue or have any suggestion, please contact: Bowen Chen, PhD (bwchen0719@gmail.com)

**Acknowledgement**: The development of this package was supported by USDA-NRCS Agreement No. NR193A750016C001 through the Cooperative Ecosystem Studies Units network. Any opinions, findings, conclusions, or recommendations expressed are those of the author(s) and do not necessarily reflect the view of the U.S. Department of Agriculture. Dr. Benjamin Gramig is a contributor of this package. 

