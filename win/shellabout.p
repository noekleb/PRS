/* Programnavn :   shellabout.p
   Laget av    :   SJ, 06.10.99
   Funksjoner  :   Viser ShellAbout for Windows
   Endringer   :
*/

DEF VAR wIcon   AS INTE NO-UNDO.
DEF VAR wRetVal AS INTE NO-UNDO.

RUN ShellAboutA(CURRENT-WINDOW:HWND,"Operativsystem info...#",
                "SkoTex kjører Progress " + 
                      PROVERSION + " (" + 
                      PROGRESS + "), "   + 
                      CHR(13)           + 
                      "          med serienummer " + STRING(_serial) +
".",
                wIcon,
                OUTPUT wRetval).
IF NOT wRetVal = 1 THEN 
   MESSAGE "Kunne ikke vise informasjonsboksen..." VIEW-AS ALERT-BOX
WARNING.

PROCEDURE ShellAboutA EXTERNAL "shell32.dll":
   DEF INPUT  PARAMETER hwnd         AS LONG.
   DEF INPUT  PARAMETER szApp        AS CHAR.
   DEF INPUT  PARAMETER szOtherStuff AS CHAR.
   DEF INPUT  PARAMETER hIcon        AS LONG.
   DEF RETURN PARAMETER RetVal       AS LONG.
END PROCEDURE.
