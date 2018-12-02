
USING System.IO.*.

DEFINE VARIABLE cCurrentDirectory AS CHAR NO-UNDO. 
GET-KEY-VALUE SECTION "STARTUP" KEY "CURRENT-DIRECTORY" VALUE cCURRENTDIRECTORY.

IF  TRIM(cCurrentDirectory) NE "" AND 
    cCurrentDirectory NE ? THEN
    Directory:SetCurrentDirectory(cCurrentDirectory).

RUN c:\polygon\prs\win\START.p . 


