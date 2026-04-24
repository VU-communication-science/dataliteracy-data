## Data Literacy Data

This repo contains the data used in the [VU Data Literacy](https://dataliteracy.cc) book. These files can be accessed via `https://data.dataliteracy.cc` using the same folder and filename. For instance, the file `simulated/practice_data.csv` can be downloaded from `https://data.dataliteracy.cc/simulated/practice_data.csv`.

Note that you do not have to clone the repo to add or change files. You can do this directly via the GitHub website.

## Website hosting

The website at `https://data.dataliteracy.cc` is hosted on [Cloudflare Pages](https://pages.cloudflare.com) rather than GitHub Pages. This is because the data needs to be accessible from [WebR](https://docs.r-wasm.org/webr/latest/) (R running in the browser via WebAssembly), which requires the server to set `Access-Control-Allow-Origin: *` CORS headers on all responses. GitHub Pages does not support setting custom response headers, while Cloudflare Pages does via a simple `_headers` file.