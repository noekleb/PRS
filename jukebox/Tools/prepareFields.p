DEF VAR cc AS CHAR NO-UNDO.

cc = "RegistrertAv RegistrertDato LeveringsDato ~
KOrdre_Id Telefon VaarRef DeresRef Referanse LevAdresse1 LevAdresse2 ~
LevPostNr LevPostSted FaktAdresse1 FaktAdresse2 FaktPostNr FaktPostSted ~
FaktLand".

CLIPBOARD:VALUE = REPLACE(cc," ",":HANDLE) + ',' + " + CHR(10) + "+ STRING(")
/* CLIPBOARD:VALUE = REPLACE(cc," ",",") */
