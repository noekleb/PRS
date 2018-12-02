/************************************************************
    Program:  strtchar.P
    Created:  TN   25 Jun 98
Description: Program som starter Gamle Char programmer for
             SkoTex.

Last change:  TN   25 Jun 98   10:36 pm
************************************************************/

DEF INPUT PARAMETER parInnData as CHAR NO-UNDO.

IF SEARCH(SUBSTRING(parInnData,1,INDEX(parInnData,".") - 1) + ".r") <> ? then
  RUN VALUE(parInnData).

