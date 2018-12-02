/*********************************************************************
* Copyright (C) 2000 by Progress Software Corporation ("PSC"),       *
* 14 Oak Park, Bedford, MA 01730, and other contributors as listed   *
* below.  All Rights Reserved.                                       *
*                                                                    *
* The Initial Developer of the Original Code is PSC.  The Original   *
* Code is Progress IDE code released to open source December 1, 2000.*
*                                                                    *
* The contents of this file are subject to the Possenet Public       *
* License Version 1.0 (the "License"); you may not use this file     *
* except in compliance with the License.  A copy of the License is   *
* available as of the date of this notice at                         *
* http://www.possenet.org/license.html                               *
*                                                                    *
* Software distributed under the License is distributed on an "AS IS"*
* basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. You*
* should refer to the License for the specific language governing    *
* rights and limitations under the License.                          *
*                                                                    *
* Contributors:                                                      *
*                                                                    *
*********************************************************************/
/*
BELL.
MESSAGE "No application help is available".
*/

  DEF VAR wHjelpeFil as CHAR NO-UNDO INITIAL ".\hlp\basis.hlp".

  GET-KEY-VALUE SECTION "SYSPARA" KEY "HjelpeFil" VALUE wHjelpeFil.
  IF wHjelpeFil = ? then
    DO:
      {syspara.i 1 1 2 wHjelpeFil}
    END.

  IF wHjelpefil <> "" THEN DO:
    IF SEARCH(wHjelpefil) = ? THEN DO:
      MESSAGE "Finner ikke hjelpefilen:" SKIP
              wHjelpefil        SKIP(1)
              "Kontakt systemansvarlig...."
        VIEW-AS ALERT-BOX ERROR TITLE "Feil".
      RETURN.
    END.
    SYSTEM-HELP wHjelpefil CONTENTS.
  END.
  ELSE MESSAGE "Navn på hjelpefil er ikke angitt i INI-Filen." SKIP(1)
              "Kontrakt systemansvarlig."
      VIEW-AS ALERT-BOX WARNING TITLE "Hjelp".

