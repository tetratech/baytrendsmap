NEWS
================
<Erik.Leppo@tetratech.com> and <jon.harcum@tetratech.com>

<!-- NEWS.md is generated from NEWS.Rmd. Please edit that file -->

    #> Last Update: 2022-05-12 12:07:27

# baytrendsmap 1.2.1.9020

Released - 2022-05-12

-   refactor: Update text on background tab, Issue #60

# baytrendsmap 1.2.1.9019

Released - 2022-05-12

-   refactor: Add pop up modal at start up, Issue #59
    -   Add shinyalert package to DESCRIPTION and global.R

# baytrendsmap 1.2.1.9018

Released - 2022-05-09

-   refactor: Update shiny help

# baytrendsmap 1.2.1.9017

Released - 2022-05-09

-   refactor: Update shiny help

# baytrendsmap 1.2.1.9016

Released - 2022-04-27

-   update: Replace rgdal and sp packages with sf package, Issue #58
    -   DESCRIPTION
    -   ProcessData_GIS_cbseg.R
    -   global.R
-   style: Remove “test” code at bottom of global.R

# baytrendsmap 1.2.1.9015

Released - 2022-03-23

-   refactor: Update shiny app
    -   leaflet maps, add missing space after “GAM:”
    -   Replace “trends” with “change” for map names

# baytrendsmap 1.2.1.9014

Released - 2022-03-07

-   refactor: Update shiny app
    -   Basic maps
        -   Load with default map when app opens
        -   Save buttons for range and trend moved to map tab

# baytrendsmap 1.2.1.9013

Released - 2022-03-03

-   refactor: Update shiny app

# baytrendsmap 1.2.1.9012

Released - 2022-01-19

-   fix: Update URL for plots for interactive map, Issue #56

# baytrendsmap 1.2.1.9011

Released - 2022-01-19

-   refactor: Complete updates to Shiny interface
    -   Basic download button
    -   Enable/disable save buttons in Shiny to guide order of
        operations
    -   DESCRIPTION, IMPORTS, add shinyjs package
-   style: Add style to some shiny code files

# baytrendsmap 1.2.1.9010

Released - 2022-01-18

-   refactor: Continued updates to Shiny interface
    -   Split interface into basic and advanced
    -   Link to Background PDF

# baytrendsmap 1.2.1.9009

Released - 2022-01-14

-   refactor: Update ‘filtered data summary’ table without rownames
-   style: Update outline of global.R
-   feature: Add default map
    -   Uses first remote file and first mapLayer
-   refactor: Redesign Shiny interface
    -   Interim version for testing online

# baytrendsmap 1.2.1.9008

Released - 2021-09-15

-   refactor: Update remote file directory structure, Issue #54 and
    Issue #50
    -   Update remove file pick_files.csv with plot directory name
-   refactor: Update shiny to use new files

# baytrendsmap 1.2.1.9007

Released - 2021-08-17

-   feat: Added plot creation script for “no data” in Shiny leaflet map
    pop up, Issue #49

# baytrendsmap 1.2.1.9006

Released - 2021-08-17

-   refactor: Add remote data URL to global.R
-   refactor: Update data and plot URL to reference global.R for base
    URL
-   refactor: Update Trends leaflet map to use all plots for import
    -   Previously tested only chla_S plots

# baytrendsmap 1.2.1.9005

Released - 2021-08-16

-   refactor: Update Shiny app to import data files from remote repo,
    Issue #54

# baytrendsmap 1.2.1.9004

Released - 2021-08-16

-   feat: Update Shiny app to auto update data import pick list from
    saved table, Issue #54

# baytrendsmap 1.2.1.9003

Released - 2021-08-09

-   fix: Update DESCRIPTION to accurately include second author
-   fix: Update range map in Shiny to use all categories when user
    supplies custom classes, Issue #51
-   docs: Update README to use `remotes` instead of `devtools` in
    install example

# baytrendsmap 1.2.1.9002

Released - 2021-06-09

-   refactor: Shiny lealfet map, change order of base layers
    -   Make Positron the default

# baytrendsmap 1.2.1.9001

Released - 2021-06-04

-   feat: Add leaflet map as demonstration for trend map.
-   chrore: Fix filtering
-   style: Trim lines to 80 characters

# baytrendsmap 1.2.1

Released - 2021-01-26

-   chore: Update to version 1.2.1 for release; Issue #45

# baytrendsmap 1.2.0.9003

Released - 2021-01-06

-   chore: Update trend (change) map color blind palette; Issue #46

# baytrendsmap 1.2.0.9002

Released - 2021-01-06

-   chore: Edit spelling of “palette” in shiny app map pages

# baytrendsmap 1.2.0.9001

Released - 2021-01-06

-   chore: Add ability to select colors for change (trend) map; Issue
    #46
-   chore: Add .groups = “drop_last” to summarize to avoid warning
    message

# baytrendsmap 1.2.0

Released - 2021-01-04

-   chore: Update version number and create release on GitHub

# baytrendsmap 1.1.0.9007

Released - 2021-01-04

-   chore: Move docker and buildspec.yml back to root folder
-   chore: Add docker and buildspec.yml to .Rbuildignore

# baytrendsmap 1.1.0.9006

Released - 2021-01-04

-   chore: Remove docs (pkgdown) folder from root

# baytrendsmap 1.1.0.9005

Released - 2021-01-04

-   refactor: lintr clean up
-   docs: DESCRIPTION add + file LICENSE
-   refactor: Add non-standard files/folders to .Rbuildignore
    -   folder data-raw
    -   NEWS.rmd
    -   README.html
    -   shiny rsconnect folder
-   chore: Move files from root to inst folder
    -   buildspec.yml
    -   Dockerfile
-   docs: Document data_GIS_cbpseg in Data.R
-   chore: Add GitHub Action, CMD check
-   chore: Add GitHub Action, pkgdown
-   chore: Remove \_pkgdown.yaml from root directory
-   docs: Add badges to README
-   style: Trim lines to 80 characters, run_shiny.R

# baytrendsmap 1.1.0.9004

Released - 2020-12-18

-   Replace “final” data with revised 2019 data; Issue #44
-   Update text on Shiny app sidebar (6 instead of 4 radio buttons);
    Issue #44
-   Widened sidebar for select data so no wrapping of radio button
    choices.

# baytrendsmap 1.1.0.9003

Released - 2020-07-16

-   Update help file for occassional bad file import; Issue #40

# baytrendsmap 1.1.0.9002

Released - 2020-07-16

-   Update column widths in data tables (import and filter); Issue #39

# baytrendsmap 1.1.0.9001

Released - 2020-07-15

-   Update maps to use defaults if no map options selected; Issue #23
    -   Avoids map creation error.
    -   Range and Change maps.

# baytrendsmap 1.1.0

Released - 2020-07-09

-   Update version number after merging in development branch; Issue #37
-   Create a “release” on github.
-   Upload latest version to Shiny.io.

# baytrendsmap 1.0.0.9046

Released - 2020-07-07

-   Update map legend code; Issue #27
    -   User/final files and range/change maps.

# baytrendsmap 1.0.0.9045

Released - 2020-07-06

-   Change name of change map file; Issue #33
    -   Replace “trend” with “change”.
    -   Internal and download file.

# baytrendsmap 1.0.0.9044

Released - 2020-07-06

-   Change map output size; Issue #26
    -   server.R
        -   Modified range and change maps to use values set in global.R
    -   global.R
        -   plot_h \<- 9
        -   plot_w \<- plot_h/map_coord_ratio
        -   plot_units \<- “in”
        -   plot_scale \<- 1.25
        -   Use map_coord_ratio so has the same value as coord_fixed
            when create plots.

# baytrendsmap 1.0.0.9043

Released - 2020-07-06

-   Unwind change to DESCRIPTION URL and packageVersion() in global.R.
    -   Unable to get Shinyapps.io to use the development branch when
        installing.

# baytrendsmap 1.0.0.9042

Released - 2020-07-06

-   DESCRIPTION
    -   Modify URL for Shiny since using development not main branch.

# baytrendsmap 1.0.0.9041

Released - 2020-07-06

-   global.R
    -   Use packageVersion() instead of manually setting the version.
    -   Tests failed before when on Shinyapps.io.

# baytrendsmap 1.0.0.9040

Released - 2020-07-06

-   Loaded file type text, official to final; Issue #35.
    -   server.R

# baytrendsmap 1.0.0.9039

Released - 2020-07-01

-   Map tabs move save button to top of column alongside apply button.

# baytrendsmap 1.0.0.9038

Released - 2020-07-01

-   Modify range map to included lowest value in legend; Issue #8 and
    #30

# baytrendsmap 1.0.0.9037

Released - 2020-06-30

-   Update filters to sort unique values; Issue #28

# baytrendsmap 1.0.0.9036

Released - 2020-06-30

-   Modify auto-generated map titles (range and change) for ‘user’
    files; Issue #27

# baytrendsmap 1.0.0.9035

Released - 2020-06-30

-   Range map; Issue #34
    -   Fix spelling for palette.

# baytrendsmap 1.0.0.9034

Released - 2020-06-30

-   Modify trend map tab; Issue #33

# baytrendsmap 1.0.0.9033

Released - 2020-06-29

-   Revised “final” data files; Issue #26

# baytrendsmap 1.0.0.9032

Released - 2020-06-29

-   Modify auto-generated map titles (range and change) for ‘final’
    files; Issue #27

# baytrendsmap 1.0.0.9031

Released - 2020-06-29

-   Modify help file; Issue #29

# baytrendsmap 1.0.0.9030

Released - 2020-06-29

-   Modify filter tab; Issue #32
-   Modify trend map tab; Issue #33
    -   Left variables beyond filename as ‘trend’.

# baytrendsmap 1.0.0.9029

Released - 2020-06-24

-   Modify help file; Issue #29

# baytrendsmap 1.0.0.9028

Released - 2020-06-24

-   Modify data tab; Issue #31
-   Modify filter tab title; Issue #32

# baytrendsmap 1.0.0.9027

Released - 2020-06-24

-   Modify filter tab; Issue #32

# baytrendsmap 1.0.0.9026

Released - 2020-06-17

Increase file size from 10 MB to 100 MB; Issue #25

# baytrendsmap 1.0.0.9025

Released - 2020-06-16

-   Interface changes complete; Issue #19

# baytrendsmap 1.0.0.9024

Released - 2020-06-16

-   Changes related to interface update; Issue #19
    -   Filter duplicates summary.
        -   Different table for different data file types.
    -   Works for each file type (official and user).
        -   Fails (filters and dups) to varying degrees when switch
            between file types.

# baytrendsmap 1.0.0.9023

Released - 2020-06-15

-   Modified Filtered Data Summary tab; Issue #19
    -   Individually official and user ok.
    -   Only have issues when change between them in the same session.

# baytrendsmap 1.0.0.9022

Released - 2020-06-15

-   Modified input data (official vs. user); Issue #19
    -   Removed “reset” button as handle internally with a
        reactiveValues.
    -   Filtered Data Summary tab (number of dups and table) still needs
        work; Issue #19

# baytrendsmap 1.0.0.9021

Released - 2020-06-12

-   Modify filters for user or official files; Issue #19
    -   Still some lingering issues with the changes.

# baytrendsmap 1.0.0.9020

Released - 2020-06-12

-   Modify file import to be able to use base files saved in-app; Issue
    #19
    -   Remaining issue of if select upload cannot select ‘official’
        file from radio buttons.
        -   Added ‘reset’ button but it is not active.

# baytrendsmap 1.0.0.9019

Released - 2020-06-11

-   Error checking for required columns in imported file; Issue #7
-   Revise Error message text for p-value for consistency; Issue #24

# baytrendsmap 1.0.0.9018

Released - 2020-06-10

-   Remove trend map p-value restriction for sum > 1; Issue #24
    -   Added in v1.0.0.9016

# baytrendsmap 1.0.0.9017

Released - 2020-06-10

-   Fix range map palatte selection; Issue #21

# baytrendsmap 1.0.0.9016

Released - 2020-06-09

-   Trend map, constrain p-value; Issue #24
    -   0.0 \<= p(significant) \< p(possible) \<= 1.0

# baytrendsmap 1.0.0.9015

Released - 2020-06-07

-   Add trend map p-values to auto-generated title; Issue #10

# baytrendsmap 1.0.0.9014

Released - 2020-06-07

-   Update “save map” button; Issue #9
    -   No changes other than formatting and comments.

# baytrendsmap 1.0.0.9013

Released - 2020-06-07

-   Modify range map variable pull-down to descriptive names; Issue #18

# baytrendsmap 1.0.0.9012

Released - 2020-06-07

-   Modify trend map p-values; Issue #10

# baytrendsmap 1.0.0.9011

Released - 2020-05-28

-   Add placeholder code to “open” collapsed filters and map options;
    Issue #23

# baytrendsmap 1.0.0.9010

Released - 2020-05-28

-   Filter data tab change “entries” to “stations”; Issue #17

# baytrendsmap 1.0.0.9009

Released - 2020-05-28

-   Fix reset filter button display text spelling; Issue #14

# baytrendsmap 1.0.0.9008

Released - 2020-05-28

-   Update map titles in case of multiple selections; Issue #11

# baytrendsmap 1.0.0.9007

Released - 2020-05-28

-   Update Table captions, Issue #15

# baytrendsmap 1.0.0.9006

Released - 2020-05-28

-   Shiny, Issue #16
    -   Remove numbers (3a and 3b) from map options headers.

# baytrendsmap 1.0.0.9005

Released - 2020-05-28

-   Shiny, Issue #20
    -   Rename ‘1. Import Data’ tab to ‘1. Select Data’.
    -   Default to ‘1. Import Data’ tab instead of ‘HELP’.

# baytrendsmap 1.0.0.9004

Released - 2020-05-28

-   README
    -   Update install example repo from leppottto tetratech
    -   Update install example to use dependencies = TRUE, Issue #22
        -   Default of NA does not include packages listed as Suggests
            in DESCRIPTION
    -   Commented out Set.sysenv() since on R v4 and only needed for R
        v3.6

# baytrendsmap 1.0.0.9003

Released - 2020-05-15

-   Moved GitHub repo from leppott to tetratech.
    -   Redo pkgdown.

# baytrendsmap 1.0.0.9002

Released - 2020-04-03

-   Added files for use with Docker.
    -   buildspec.yml
    -   Dockerfile

# baytrendsmap 1.0.0.9001

Released - 2020-01-29 (work in progress)

-   Updated from rhub::check_for_cran()

    -   Title to title case and no ending period.

    -   Update maintainer and author names to match.

    -   .github folder to .gitbuildignore.

More stuff from check

-   checking installed package size … NOTE installed size is 31.7Mb
    sub-directories of 1Mb or more: data 3.9Mb extdata 10.0Mb
    shiny-examples 17.7Mb

License components which are templates and need ‘+ file LICENSE’: MIT

-   checking top-level files … NOTE File LICENSE is not mentioned in the
    DESCRIPTION file.

Non-standard files/directories found at top level: ‘NEWS.rmd’
‘README.html’ ‘data-raw’

# baytrendsmap 1.0.0

Released - 2019-11-18

-   Release version with all updates and edits.

-   Update pkgdown website.

# baytrendsmap 0.0.3.9011

Released - 2019-11-18

-   Fix zoom level buffer. Issue #6.

    -   Both Trend and Range maps.

# baytrendsmap 0.0.3.9010

Released - 2019-11-18

-   server.R

    -   Remove leaflet references

# baytrendsmap 0.0.3.9009

Released - 2019-11-18

-   global.R

    -   Remove leaflet package.

# baytrendsmap 0.0.3.9008

Released - 2019-11-18

-   Fix Up Is Good, Issue #5

    -   Trend Map, change color only, not direction.

-   ui.R

    -   App title to use package version as defined in global.R.

# baytrendsmap 0.0.3.9007

Released - 2019-11-06

-   Fixed Zoom code. Issue #4.

-   Added vignette for use with pkgdown website.

-   DESCRIPTION

    -   Add URL of pkgdown website.

# baytrendsmap 0.0.3.9006

Released - 2019-11-06

-   Add custom breaks/classes for range map. Issue #3.

-   Add filtered data set for testing.

-   Zoom code not working so commented out. Issue #4

-   Restructured NEWS.

# baytrendsmap 0.0.3.9005

Released - 2019-11-05

-   Add a pkgdown website.

-   Replace map zoom code with plotly. Issue #4

# baytrendsmap 0.0.3.9004

Released - 2019-10-31

-   Shiny, Maps

    -   Add zoom feature, Issue #4

    -   Fixed ratio of 1.3 (better sizing).

-   Miscellaneous formatting.

# baytrendsmap 0.0.3.9003

Released - 2019-10-30

-   Shiny

    -   Map, Trend, Add point outlines, Issue #2.

-   NEWS

    -   Keep NEWS.md file rather than delete.

# baytrendsmap 0.0.3.9002

Released - 2019-10-30

-   Shiny

    -   Map, Range, Add point outlines, Issue #2.

    -   Update Help text., Issue #1

# baytrendsmap 0.0.3.9001

Released - 2019-10-29

-   Shiny

    -   Update Help screen, Issue #1.

# baytrendsmap 0.0.3

Released - 2019-09-24

-   Release stable version with updates.

# baytrendsmap 0.0.2.9002

Released - 2019-09-24

-   Filter

    -   Update “clearFilters” button.

# baytrendsmap 0.0.2.9001

Released - 2019-09-24

-   global.R

    -   Test map zoom in global.

    -   Make Shiny load silently as well.

-   Added 2nd test data file.

-   tab_Data.R

    -   Fix typo.

-   Trend map

    -   NS points

        -   Change “gray” to “dark gray”.

    -   Modified (removed debug mode):

        -   UpisGood

        -   p value significant threshold

        -   p value possible threshold

    -   Proper map is created for title and river names.

-   Range map

    -   “Pretty” option ignores number of breaks.

        -   Added error trapping to use number of breaks output from
            pretty option.

-   Filters

    -   Add “ClearFilters” button.

        -   Inactive at this point.

# baytrendsmap 0.0.2

Released - 2019-09-19

-   Interim release.

# baytrendsmap 0.0.1.9013

Released - 2019-09-19

-   Trends map

    -   Edits to global test.

    -   Create Trends tab; similar to Range tab.

    -   Create map.

-   Remove dynamic version number from title.

    -   Causes Shiny.io to fail.

-   global

    -   Base map declare fill color so don’t use scale_fill_manual.

-   tab_Filter

    -   Reorder tabs with filter summary before data.

    -   Rename data tabs.

# baytrendsmap 0.0.1.9012

Released - 2019-09-18

-   Trends map, legend to include range of values.

-   DESCRIPTION; add packages

    -   Add grid, lubridate, and sp

-   global.R

    -   Suppress messages

        -   library

        -   readOGR

        -   fortify

-   range map

    -   Add button to auto-generate map title based on filtered
        selections.

    -   Make auto-title wrap lines.

    -   Add cut (break) numbers to legend.

# baytrendsmap 0.0.1.9011

Released - 2019-09-18

-   Add number of non-“one” entries on “Data Filter Summary” tab.

-   Modifty title to include dynamic package version number.

# baytrendsmap 0.0.1.9010

Released - 2019-09-05

-   DESCRIPTION

    -   Reduced required R version from 3.6.0 to 3.2.0.

    -   Matches baytrends package.

# baytrendsmap 0.0.1.9009

Released - 2019-09-04

-   Returned to use of shp file.

# baytrendsmap 0.0.1.9008

Released - 2019-09-04

-   GIS shapefile (cbpseg) to RDA.

    -   Speed and file size improvement.

# baytrendsmap 0.0.1.9007

Released - 2019-09-03

-   Range map working.

# baytrendsmap 0.0.1.9006

Released - 2019-08-30

-   Range map selection buttons (not finished).

# baytrendsmap 0.0.1.9005

Released - 2019-08-29

-   Range map options.

-   Default Range map.

# baytrendsmap 0.0.1.9004

Released - 2019-08-29

-   Add dplyr package to global and DESCRIPTION.

    -   Need for filter summary table.

# baytrendsmap 0.0.1.9003

Released - 2019-08-29

-   Filters working.

-   Structure for maps.

-   Filter summary so can find non-uniques and re-apply filters.

-   Packages to global.R.

-   Package updates to DESCRIPTION.

# baytrendsmap 0.0.1.9002

Released - 2019-08-28

-   Filters and other additions.

# baytrendsmap 0.0.1.9001

Released - 2019-08-26

-   Redesign interface.

# baytrendsmap 0.0.0.9005

Released - 2019-08-23

-   Add test data to create new map.

-   Modify Shiny app to use imported data file.

# baytrendsmap 0.0.0.9004

Released - 2019-08-21

-   Update Readme with usage directions.

# baytrendsmap 0.0.0.9003

Released - 2019-08-21

-   Update Readme with installation directions.

# baytrendsmap 0.0.0.9002

Released - 2019-08-21

-   Updated repository with working files.

-   Rename without underscore to baytrendsmap.

# baytrendsmap 0.0.0.9001

Released - 2019-08-21

-   Initial commit to GitHub
