
/*------------------------------------------------------------------------
    File        : JBoxFuncToMeth.i
    Purpose     : Body of wrappers for library functions so they can be re-used in classes with same name
                  Wrapper procs are JBoxCharFuncToMeth.p, JBoxIntFuncToMeth.p, JBoxDateFuncToMeth.p etc

    Author(s)   : Brynjar
    Created     : Thu Sep 15 15:21:54 CEST 2016
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEF VAR bb AS LOGICAL  NO-UNDO EXTENT 7.
DEF VAR cc AS CHAR     NO-UNDO EXTENT 7.
DEF VAR dd AS DATE     NO-UNDO EXTENT 7.
DEF VAR ii AS INT      NO-UNDO EXTENT 7.
DEF VAR hh AS HANDLE   NO-UNDO EXTENT 7.
DEF VAR ff AS DECIMAL  NO-UNDO EXTENT 7.
DEF VAR tt AS DATETIME NO-UNDO EXTENT 7.

DEF VAR ix      AS INT  NO-UNDO.
DEF VAR cParams AS CHAR NO-UNDO.

DO ix = 1 TO NUM-ENTRIES(icTypeList):
  IF ENTRY(ix,icTypeList) BEGINS "CHAR" THEN
    ASSIGN cParams = cParams + "c"
           cc[ix]  = ENTRY(ix,icValueList,CHR(1)).       
  ELSE IF ENTRY(ix,icTypeList) BEGINS "DATET" THEN
    ASSIGN cParams = cParams + "t"
           tt[ix]  = DATETIME(ENTRY(ix,icValueList,CHR(1))).       
  ELSE IF ENTRY(ix,icTypeList) BEGINS "DATE" THEN
    ASSIGN cParams = cParams + "d"
           dd[ix]  = DATE(ENTRY(ix,icValueList,CHR(1))).       
  ELSE IF ENTRY(ix,icTypeList) BEGINS "DEC" THEN
    ASSIGN cParams = cParams + "f"
           ff[ix]  = DECIMAL(ENTRY(ix,icValueList,CHR(1))).       
  ELSE IF ENTRY(ix,icTypeList) BEGINS "HAN" THEN
    ASSIGN cParams = cParams + "h"
           hh[ix]  = ihHandle.       
  ELSE IF ENTRY(ix,icTypeList) BEGINS "INT" THEN
    ASSIGN cParams = cParams + "i"
           ii[ix]  = INT(ENTRY(ix,icValueList,CHR(1))).       
  ELSE IF ENTRY(ix,icTypeList)BEGINS "LOG" THEN
    ASSIGN cParams = cParams + "b"
           bb[ix]  = LOGICAL(ENTRY(ix,icValueList,CHR(1))).       
END.

CASE cParams:
  WHEN ""        THEN {1} = DYNAMIC-FUNCTION(icFunctionName).    
  WHEN "b"       THEN {1} = DYNAMIC-FUNCTION(icFunctionName,bb[1]).    
  WHEN "c"       THEN {1} = DYNAMIC-FUNCTION(icFunctionName,cc[1]).    
  WHEN "d"       THEN {1} = DYNAMIC-FUNCTION(icFunctionName,dd[1]).    
  WHEN "f"       THEN {1} = DYNAMIC-FUNCTION(icFunctionName,ff[1]).    
  WHEN "h"       THEN {1} = DYNAMIC-FUNCTION(icFunctionName,hh[1]).    
  WHEN "i"       THEN {1} = DYNAMIC-FUNCTION(icFunctionName,ii[1]).
  WHEN "t"       THEN {1} = DYNAMIC-FUNCTION(icFunctionName,tt[1]).
      
  WHEN "bb"      THEN {1} = DYNAMIC-FUNCTION(icFunctionName,bb[1],bb[2]).    
  WHEN "bc"      THEN {1} = DYNAMIC-FUNCTION(icFunctionName,bb[1],cc[2]).    
  WHEN "bd"      THEN {1} = DYNAMIC-FUNCTION(icFunctionName,bb[1],dd[2]).    
  WHEN "bf"      THEN {1} = DYNAMIC-FUNCTION(icFunctionName,bb[1],ff[2]).    
  WHEN "bh"      THEN {1} = DYNAMIC-FUNCTION(icFunctionName,bb[1],hh[2]).    
  WHEN "bi"      THEN {1} = DYNAMIC-FUNCTION(icFunctionName,bb[1],ii[2]).    
  WHEN "bt"      THEN {1} = DYNAMIC-FUNCTION(icFunctionName,bb[1],tt[2]).
      
  WHEN "cb"      THEN {1} = DYNAMIC-FUNCTION(icFunctionName,cc[1],bb[2]).    
  WHEN "cc"      THEN {1} = DYNAMIC-FUNCTION(icFunctionName,cc[1],cc[2]).    
  WHEN "cd"      THEN {1} = DYNAMIC-FUNCTION(icFunctionName,cc[1],dd[2]).    
  WHEN "cf"      THEN {1} = DYNAMIC-FUNCTION(icFunctionName,cc[1],ff[2]).    
  WHEN "ch"      THEN {1} = DYNAMIC-FUNCTION(icFunctionName,cc[1],hh[2]).    
  WHEN "ci"      THEN {1} = DYNAMIC-FUNCTION(icFunctionName,cc[1],ii[2]).    
  WHEN "ct"      THEN {1} = DYNAMIC-FUNCTION(icFunctionName,cc[1],tt[2]).

  WHEN "db"      THEN {1} = DYNAMIC-FUNCTION(icFunctionName,dd[1],bb[2]).    
  WHEN "dc"      THEN {1} = DYNAMIC-FUNCTION(icFunctionName,dd[1],cc[2]).    
  WHEN "dd"      THEN {1} = DYNAMIC-FUNCTION(icFunctionName,dd[1],dd[2]).    
  WHEN "df"      THEN {1} = DYNAMIC-FUNCTION(icFunctionName,dd[1],ff[2]).    
  WHEN "dh"      THEN {1} = DYNAMIC-FUNCTION(icFunctionName,dd[1],hh[2]).    
  WHEN "di"      THEN {1} = DYNAMIC-FUNCTION(icFunctionName,dd[1],ii[2]).    
  WHEN "dt"      THEN {1} = DYNAMIC-FUNCTION(icFunctionName,dd[1],tt[2]).

  WHEN "fb"      THEN {1} = DYNAMIC-FUNCTION(icFunctionName,ff[1],bb[2]).    
  WHEN "fc"      THEN {1} = DYNAMIC-FUNCTION(icFunctionName,ff[1],cc[2]).    
  WHEN "fd"      THEN {1} = DYNAMIC-FUNCTION(icFunctionName,ff[1],dd[2]).    
  WHEN "ff"      THEN {1} = DYNAMIC-FUNCTION(icFunctionName,ff[1],ff[2]).    
  WHEN "fh"      THEN {1} = DYNAMIC-FUNCTION(icFunctionName,ff[1],hh[2]).    
  WHEN "fi"      THEN {1} = DYNAMIC-FUNCTION(icFunctionName,ff[1],ii[2]).    
  WHEN "ft"      THEN {1} = DYNAMIC-FUNCTION(icFunctionName,ff[1],tt[2]).
      
  WHEN "hb"      THEN {1} = DYNAMIC-FUNCTION(icFunctionName,hh[1],bb[2]).    
  WHEN "hc"      THEN {1} = DYNAMIC-FUNCTION(icFunctionName,hh[1],cc[2]).    
  WHEN "hd"      THEN {1} = DYNAMIC-FUNCTION(icFunctionName,hh[1],dd[2]).    
  WHEN "hf"      THEN {1} = DYNAMIC-FUNCTION(icFunctionName,hh[1],ff[2]).    
  WHEN "hi"      THEN {1} = DYNAMIC-FUNCTION(icFunctionName,hh[1],ii[2]).    
  WHEN "ht"      THEN {1} = DYNAMIC-FUNCTION(icFunctionName,hh[1],tt[2]).

  WHEN "ib"      THEN {1} = DYNAMIC-FUNCTION(icFunctionName,ii[1],bb[2]).    
  WHEN "ic"      THEN {1} = DYNAMIC-FUNCTION(icFunctionName,ii[1],cc[2]).    
  WHEN "id"      THEN {1} = DYNAMIC-FUNCTION(icFunctionName,ii[1],dd[2]).    
  WHEN "if"      THEN {1} = DYNAMIC-FUNCTION(icFunctionName,ii[1],ff[2]).    
  WHEN "ih"      THEN {1} = DYNAMIC-FUNCTION(icFunctionName,ii[1],hh[2]).    
  WHEN "ii"      THEN {1} = DYNAMIC-FUNCTION(icFunctionName,ii[1],ii[2]).    
  WHEN "it"      THEN {1} = DYNAMIC-FUNCTION(icFunctionName,ii[1],tt[2]).

  WHEN "tb"      THEN {1} = DYNAMIC-FUNCTION(icFunctionName,tt[1],bb[2]).    
  WHEN "tc"      THEN {1} = DYNAMIC-FUNCTION(icFunctionName,tt[1],cc[2]).    
  WHEN "td"      THEN {1} = DYNAMIC-FUNCTION(icFunctionName,tt[1],dd[2]).    
  WHEN "tf"      THEN {1} = DYNAMIC-FUNCTION(icFunctionName,tt[1],ff[2]).    
  WHEN "th"      THEN {1} = DYNAMIC-FUNCTION(icFunctionName,tt[1],hh[2]).    
  WHEN "ti"      THEN {1} = DYNAMIC-FUNCTION(icFunctionName,tt[1],ii[2]).    
  WHEN "tt"      THEN {1} = DYNAMIC-FUNCTION(icFunctionName,tt[1],tt[2]).
      
  WHEN "cbc"     THEN {1} = DYNAMIC-FUNCTION(icFunctionName,cc[1],bb[2],cc[3]).    
  WHEN "ccc"     THEN {1} = DYNAMIC-FUNCTION(icFunctionName,cc[1],cc[2],cc[3]).    
  WHEN "cic"     THEN {1} = DYNAMIC-FUNCTION(icFunctionName,cc[1],ii[2],cc[3]).    
  WHEN "cch"     THEN {1} = DYNAMIC-FUNCTION(icFunctionName,cc[1],cc[2],hh[3]).    
  WHEN "cci"     THEN {1} = DYNAMIC-FUNCTION(icFunctionName,cc[1],cc[2],ii[3]).    
  WHEN "hbc"     THEN {1} = DYNAMIC-FUNCTION(icFunctionName,hh[1],bb[2],cc[3]).    
  WHEN "hcc"     THEN {1} = DYNAMIC-FUNCTION(icFunctionName,hh[1],cc[2],cc[3]).    
  WHEN "hcf"     THEN {1} = DYNAMIC-FUNCTION(icFunctionName,hh[1],cc[2],ff[3]).    
  WHEN "icc"     THEN {1} = DYNAMIC-FUNCTION(icFunctionName,ii[1],cc[2],cc[3]). 

  WHEN "cccc"    THEN {1} = DYNAMIC-FUNCTION(icFunctionName,cc[1],cc[2],cc[3],cc[4]).    
  WHEN "ccbc"    THEN {1} = DYNAMIC-FUNCTION(icFunctionName,cc[1],cc[2],bb[3],cc[4]).    
  WHEN "ccic"    THEN {1} = DYNAMIC-FUNCTION(icFunctionName,cc[1],cc[2],ii[3],cc[4]).    
  WHEN "cibc"    THEN {1} = DYNAMIC-FUNCTION(icFunctionName,cc[1],ii[2],bb[3],cc[4]).    
  WHEN "hccc"    THEN {1} = DYNAMIC-FUNCTION(icFunctionName,hh[1],cc[2],cc[3],cc[4]).    

  WHEN "ccccb"   THEN {1} = DYNAMIC-FUNCTION(icFunctionName,cc[1],cc[2],cc[3],cc[4],bb[5]).    
  WHEN "hcccc"   THEN {1} = DYNAMIC-FUNCTION(icFunctionName,hh[1],cc[2],cc[3],cc[4],cc[5]).    
  WHEN "hcccb"   THEN {1} = DYNAMIC-FUNCTION(icFunctionName,hh[1],cc[2],cc[3],cc[4],bb[5]).    
  WHEN "icccc"   THEN {1} = DYNAMIC-FUNCTION(icFunctionName,ii[1],cc[2],cc[3],cc[4],cc[5]).    
  WHEN "iiccc"   THEN {1} = DYNAMIC-FUNCTION(icFunctionName,ii[1],ii[2],cc[3],cc[4],cc[5]).    

  WHEN "cccccc"  THEN {1} = DYNAMIC-FUNCTION(icFunctionName,cc[1],cc[2],cc[3],cc[4],cc[5],cc[6]).    
  WHEN "cccccb"  THEN {1} = DYNAMIC-FUNCTION(icFunctionName,cc[1],cc[2],cc[3],cc[4],cc[5],bb[6]).    
  WHEN "hccccc"  THEN {1} = DYNAMIC-FUNCTION(icFunctionName,hh[1],cc[2],cc[3],cc[4],cc[5],cc[6]).    
  WHEN "hicccc"  THEN {1} = DYNAMIC-FUNCTION(icFunctionName,hh[1],ii[2],cc[3],cc[4],cc[5],cc[6]).    
  WHEN "iccccc"  THEN {1} = DYNAMIC-FUNCTION(icFunctionName,ii[1],cc[2],cc[3],cc[4],cc[5],cc[6]).    

  WHEN "ccccccb" THEN {1} = DYNAMIC-FUNCTION(icFunctionName,cc[1],cc[2],cc[3],cc[4],cc[5],cc[6],bb[7]).    
  WHEN "hcccccc" THEN {1} = DYNAMIC-FUNCTION(icFunctionName,hh[1],cc[2],cc[3],cc[4],cc[5],cc[6],cc[7]).    
END.
