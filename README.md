# Fitbit heartrate graphs

Make heartrate graphs (and export to csv) from your exported fitbit user data.

## Usage

1. Make sure you have the `Packrat` library installed (or run `install.packages("packrat")`).
2. Run `packrat::init()` in the project's root folder.
3. Copy `.env-default` and rename it to `.env`.
4. Puth the path to `user-site-export` from your downloaded fitbit userdata in the `.env` file.
5. Enable what you want the script to do at the top of `getData.R`.
6. Run the script.
