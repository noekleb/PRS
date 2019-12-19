CREATE ALIAS DICTDB FOR DATABASE sports.
DISPLAY LDBNAME("DICTDB").

RUN prodict/dump_df.p   ("ALL",
                         "sports.df",
                          "").
/*
Input parameters:
    file-name                : "ALL" or "<file-name> [,<filename>] ..." -> comma-delimited list of table names
    df-file-name             : Name of file to dump to
    code-page                : ?, "", "<code-page>"
               ?             = no conversion
               ""            = default conversion (SESSION:STREAM)
               "<code-page>" =  convert to <code-page>

*/

RUN prodict/dump_d.p ("ALL",".","").
/*
    Input parameters:
    file-name                : "ALL" or "<file-name>"
    dot-d-dir                : directory relative to working-directory
    code-page                : ?, "", "<code-page>"
               ?             = no conversion
               ""            = default conversion (SESSION:STREAM)
               "<code-page>" =  convert to <code-page>
*/

DELETE ALIAS DICTDB. /* Optional */
