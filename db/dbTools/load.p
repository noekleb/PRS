CREATE ALIAS DICTDB FOR DATABASE sports.
DISPLAY LDBNAME("DICTDB").

RUN prodict/load_df.p ("sports.df").
/*
Input Parameters:
    df-file-name             : The name of the .df file to be loaded into DICTDB
*/

RUN prodict/load_d.p ("ALL",".").
/*
Input Parameters:
    file-name                : "ALL" or "<file-name>"
    dot-d-dir                : location of files to load; directory relative to working-directory

*/

DELETE ALIAS DICTDB. /* Optional */
