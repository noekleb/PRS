/*
   Following Code was added to determine local TimeZone on Windows based 
   platform.  The actual code was contributed to the freeframework by
   Jeff Pilant. 
  
  Description: Reads the time zone registry key
  Input Parameters: <none>
  Output Parameters: offset -- character, 6 long, as "+99:99"

  Author: Jeff Pilant

  Acknoledgements:
    This code used information found in the windows api files:
      windows.i, windows.p, winfunc.i, winfunc.p
    created by Jurjen Dijkstra, 1997
               mailto:jurjen@global-shared.com
               http://www.global-shared.com
    language: Progress 8.2A

  Revision History:
  Version Date         Description
  1.0     09-NOV-2001  Created
  1.0a    15-May-2002  Merged into smtpmail.p  (Paul Keary)
*/
  
/* registry */
&GLOBAL-DEFINE HKEY_LOCAL_MACHINE -2147483646
&GLOBAL-DEFINE ERROR_SUCCESS                0
&GLOBAL-DEFINE MAX_PATH                   260
&GLOBAL-DEFINE REG-KEY "SYSTEM\CurrentControlSet\Control\TimeZoneInformation":U
&GLOBAL-DEFINE REG-ATT               "ActiveTimeBias":U

PROCEDURE RegOpenKeyA EXTERNAL "advapi32" :
  DEFINE INPUT  PARAMETER hkey       AS LONG.
  DEFINE INPUT  PARAMETER lpszSubKey AS CHAR.
  DEFINE OUTPUT PARAMETER phkResult  AS LONG.
  DEFINE RETURN PARAMETER lpResult   AS LONG.
END PROCEDURE.

PROCEDURE RegCloseKey EXTERNAL "advapi32" :
  DEFINE INPUT  PARAMETER hkey     AS LONG.
  DEFINE RETURN PARAMETER lpresult AS LONG.
END PROCEDURE.

PROCEDURE RegQueryValueExA EXTERNAL "advapi32" :
  DEFINE INPUT        PARAMETER hkey         AS LONG.
  DEFINE INPUT        PARAMETER lpValueName  AS CHAR.
  DEFINE INPUT        PARAMETER lpdwReserved AS LONG.
  DEFINE OUTPUT       PARAMETER lpdwType     AS LONG.
  DEFINE INPUT        PARAMETER lpbData      AS LONG. /* memptr */
  DEFINE INPUT-OUTPUT PARAMETER lpcbData     AS LONG.
  DEFINE RETURN       PARAMETER lpresult     AS LONG.
END PROCEDURE.


DEFINE OUTPUT PARAMETER tzResult AS CHARACTER NO-UNDO.

RUN get-tz (OUTPUT tzResult).

PROCEDURE get-tz:
/*--------------------------------------------------------------------
  Purpose:     Get all top-level entries in main registry key
  Parameters:  output character -- Time Zone as +HH:MM
  Notes:       
---------------------------------------------------------------------*/
  DEFINE OUTPUT PARAMETER tzResult AS CHARACTER NO-UNDO.
  DEFINE VARIABLE tzStr      AS CHARACTER NO-UNDO.
  DEFINE VARIABLE tzBias     AS INTEGER   NO-UNDO.
  DEFINE VARIABLE tzHours    AS INTEGER   NO-UNDO.
  DEFINE VARIABLE tzMinutes  AS INTEGER   NO-UNDO.

  /* Registry read vars */
  DEFINE VARIABLE hKey       AS INTEGER   NO-UNDO.
  DEFINE VARIABLE reslt      AS INTEGER   NO-UNDO.
  DEFINE VARIABLE lth        AS INTEGER   NO-UNDO.
  DEFINE VARIABLE Bias       AS MEMPTR    NO-UNDO.
  DEFINE VARIABLE datatype   AS INTEGER   NO-UNDO.
  DEFINE VARIABLE hBiasKey   AS INTEGER   NO-UNDO.
  tzResult = "+0100 (CET)".
/*   tzResult = "+0200 (CEST)". */
/*   RUN RegOpenKeyA({&HKEY_LOCAL_MACHINE},                                */
/*                   {&Reg-Key},                                           */
/*                   OUTPUT hKey,                                          */
/*                   OUTPUT reslt).                                        */
/*   IF reslt = {&ERROR_SUCCESS} THEN                                      */
/*   DO:                                                                   */
/*     lth  = {&MAX_PATH} + 1.                                             */
/*     SET-SIZE(Bias) = lth.                                               */
/*     RUN RegQueryValueExA(hKey,                                          */
/*                          {&REG-ATT},                                    */
/*                          0,  /* reserved, must be 0 */                  */
/*                          OUTPUT datatype,                               */
/*                          GET-POINTER-VALUE(Bias),                       */
/*                          INPUT-OUTPUT lth,                              */
/*                          OUTPUT reslt).                                 */
/*     RUN RegCloseKey(hBiasKey,OUTPUT reslt).                             */
/*     /* Convert value from DWORD to INTEGER */                           */
/*     tzBias =                ASC(GET-BYTES(Bias, 4, 1)).                 */
/*     tzBias = tzBias * 256 + ASC(GET-BYTES(Bias, 3, 1)).                 */
/*     tzBias = tzBias * 256 + ASC(GET-BYTES(Bias, 2, 1)).                 */
/*     tzBias = tzBias * 256 + ASC(GET-BYTES(Bias, 1, 1)).                 */
/*     SET-SIZE(Bias)=0.                                                   */
/*     /* Convert value to +HHMM form */                                   */
/*     tzHours = INTEGER(- tzBias / 60).                                   */
/*     tzMinutes = - tzBias - 60 * tzHours.                                */
/*     tzStr = TRIM(STRING(tzHours, "-99":U) + STRING(tzMinutes, "99":U)). */
/*     IF tzHours >= 0 THEN tzStr = "+":U + tzStr.                         */
/*     tzResult = tzStr.                                                   */
/*   END.                                                                  */
/*   ELSE                                                                  */
/*     tzResult = "-0000":U. /* key not found in registry */               */
/*   RUN RegCloseKey(hKey,OUTPUT reslt).                                   */
END PROCEDURE.
