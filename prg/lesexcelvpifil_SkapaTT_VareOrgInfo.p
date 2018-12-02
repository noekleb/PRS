/* lesexcelvpifil_SkapaTT_VareOrgInfo.p */

DEFINE INPUT  PARAMETER iRad    AS INTEGER     NO-UNDO.
DEFINE INPUT  PARAMETER cTTfelt AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER cFelt   AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER cVerdi  AS CHARACTER   NO-UNDO.

{lesexcelvpifil.i &SHARED = "SHARED"}

CREATE TT_VareOrgInfo.
ASSIGN 
    TT_VareOrgInfo.Radnr   = iRad
    TT_VareOrgInfo.cTTfelt = cTTfelt
    TT_VareOrgInfo.cFelt   = cFelt
    TT_VareOrgInfo.cVerdi  = cVerdi.
