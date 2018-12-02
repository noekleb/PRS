/* PROCEDURE getMacAddress.p: */
/**
 * File: getMacAddress.p
 *
 * Description:
 *   This procedure is designed to return the MAC address (physical
address)
 *   associated with a given IP address.  It will be returned in the
format
 *   xx-xx-xx-xx-xx-xx.
 *
 * Parameters:
 *   input  cIpAddress  - the IP address for which to find the MAC
address
 *   output cMacAddress - the MAC address that was found for the IP
address
 *   output cErrorText  - the text of any errors that occurred when
trying to
 *                        discover the MAC address
 
/* replace xx's with ip address (not 127.0.0.1) */
RUN getMacAddress.p("xx.xx.xx.xx", OUTPUT cMacAddress, OUTPUT
cErrorText).
 *
 * Author:   Patrick Carlo-Hickman
 *           pcarlo@allegroconsultants.com
 *
 * Created:  April 29, 2010
 *  
 * Modified: xx/xx/xxxx - <initials>
 *           <description>
 *                     
 */
 
/* parameters */
DEFINE INPUT  PARAMETER cIpAddress  AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER cMacAddress AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER cErrorText  AS CHARACTER NO-UNDO.

/* globals */
&GLOBAL-DEFINE NO_ERROR                      0
&GLOBAL-DEFINE INADDR_ANY                    0
&GLOBAL-DEFINE INADDR_NONE                  -1
&GLOBAL-DEFINE FORMAT_MESSAGE_FROM_SYSTEM 4096 /* 0x00001000 */

/* forward declarations */
FUNCTION itoh RETURNS CHARACTER
  (INPUT iInteger AS INTEGER) FORWARD.

FUNCTION fnGetErrorMessage RETURNS CHARACTER
  (INPUT iError AS INTEGER) FORWARD.

FUNCTION fnGetMacAddrFromMemptr RETURNS CHARACTER
  (INPUT mMacAddr AS MEMPTR) FORWARD.

/***********************************************************************
*******/
/* Main Block */

DEFINE VARIABLE iBinaryIp     AS INTEGER    NO-UNDO.
DEFINE VARIABLE mMacAddr      AS MEMPTR     NO-UNDO.
DEFINE VARIABLE iMacAddrBytes AS INTEGER    NO-UNDO.
DEFINE VARIABLE iReturn       AS INTEGER    NO-UNDO.

/**
 * Turns an IP address into a binary representation of the address.  The
 * output is for use in the SendARP procedure.
 */
RUN inet_addr(cIpAddress, OUTPUT iBinaryIp).

IF iBinaryIp = {&INADDR_ANY} OR iBinaryIp = {&INADDR_NONE} THEN
DO:
  cErrorText = SUBSTITUTE("Error occurred converting IP
address.~n~ninet_addr returned &1.", TRIM(STRING(iBinaryIp =
{&INADDR_ANY}, "INADDR_ANY/INADDR_NONE"))).
  RETURN.
END.

/**
 * The MAC address will be stored in a 6 byte memory pointer.  One byte
per
 * address section.
 */
ASSIGN
  iMacAddrBytes = 6
  SET-SIZE(mMacAddr) = iMacAddrBytes
  .
RUN SendARP(iBinaryIp,                  /* destination ip (binary rep)
*/
            {&INADDR_ANY},              /* source ip */
            OUTPUT mMacAddr,            /* byte array storing mac
address */
            INPUT-OUTPUT iMacAddrBytes, /* number of bytes in byte array
*/
            OUTPUT iReturn).            /* return value */

IF iReturn = {&NO_ERROR} THEN
  cMacAddress = fnGetMacAddrFromMemptr(mMacAddr).
ELSE
  cErrorText = SUBSTITUTE("Error occurred sending ARP message.  Error
message:~n~n&1", fnGetErrorMessage(iReturn)).

SET-SIZE(mMacAddr) = 0.

/***********************************************************************
*******/
/* Function definitions */

FUNCTION itoh RETURNS CHARACTER
  (INPUT iInteger AS INTEGER):

  DEFINE VARIABLE iBase    AS INTEGER   NO-UNDO.
  DEFINE VARIABLE cHexList AS CHARACTER NO-UNDO.

  IF iInteger < 0 OR iInteger = ? THEN
    RETURN ?.

  ASSIGN
    iBase = 16
    cHexList = "0,1,2,3,4,5,6,7,8,9,A,B,C,D,E,F"
    .

  RETURN ((IF iInteger > 15
           THEN itoh(INTEGER(TRUNCATE(iInteger / iBase, 0)))
           ELSE "")
          + ENTRY((iInteger MODULO iBase) + 1, cHexList)).
END FUNCTION. /* itoh */

FUNCTION fnGetErrorMessage RETURNS CHARACTER
  (INPUT iErrorId AS INTEGER):

  DEFINE VARIABLE cMessage    AS CHARACTER NO-UNDO.
  DEFINE VARIABLE mMessage    AS MEMPTR    NO-UNDO.
  DEFINE VARIABLE iBufferSize AS INTEGER   NO-UNDO.
  DEFINE VARIABLE iFinalSize  AS INTEGER   NO-UNDO.

  /**
   * Increase buffer size if error message may be larger than this.
   * 64K is max.
   */
  ASSIGN
    iBufferSize = 1024
    SET-SIZE(mMessage) = iBufferSize
    .
  RUN FormatMessageA({&FORMAT_MESSAGE_FROM_SYSTEM}, /* Flags */
                     0,                             /* Source - ignored
*/
                     iErrorId,                      /* Id of requested
message */
                     0,                             /* Language id */
                     OUTPUT mMessage,               /* buffer to hold
message */
                     iBufferSize - 1,               /* allocated buffer
size */
                     0,                             /* arguments */
                     OUTPUT iFinalSize).            /* number of chars
written to buffer */
  cMessage = GET-STRING(mMessage, 1, iFinalSize).
  SET-SIZE(mMessage) = 0.

  RETURN cMessage.
END FUNCTION. /* fnGetErrorMessage */

FUNCTION fnGetMacAddrFromMemptr RETURNS CHARACTER
  (INPUT mMacAddr AS MEMPTR):

  DEFINE VARIABLE j       AS INTEGER   NO-UNDO.
  DEFINE VARIABLE iSize   AS INTEGER   NO-UNDO.
  DEFINE VARIABLE cEntry  AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cReturn AS CHARACTER NO-UNDO.

  iSize = GET-SIZE(mMacAddr).

  /**
   * Loop through the byte array storing the MAC address.  Each byte in
the
   * array stores an individual section of the MAC address.  Retrieve
each
   * byte, convert it to its hex value, and append to the MAC address
string.
   */
  DO j = 1 TO iSize:
    ASSIGN
      cEntry = itoh(GET-BYTE(mMacAddr, j))
      cEntry = SUBSTITUTE("&1&2", FILL("0", 2 - LENGTH(cEntry)), cEntry)
      cReturn = SUBSTITUTE("&1-&2", cReturn, cEntry)
      .
  END.

  RETURN TRIM(cReturn, "-").
END FUNCTION. /* fnGetMacAddrFromMemptr */

/***********************************************************************
*******/
/* Procedure definitions */

PROCEDURE inet_addr EXTERNAL "wsock32.dll":
  DEFINE INPUT  PARAMETER cp      AS CHARACTER.
  DEFINE RETURN PARAMETER iReturn AS LONG.
END.

PROCEDURE SendARP EXTERNAL "iphlpapi.dll":
  DEFINE INPUT        PARAMETER DestIP     AS LONG.
  DEFINE INPUT        PARAMETER SrcIP      AS LONG.
  DEFINE OUTPUT       PARAMETER pMacAddr   AS MEMPTR.
  DEFINE INPUT-OUTPUT PARAMETER PhyAddrLen AS LONG.
  DEFINE RETURN       PARAMETER iReturn    AS LONG.
END.

PROCEDURE FormatMessageA EXTERNAL "kernel32.dll".
  DEFINE INPUT  PARAMETER dwFlags      AS LONG.
  DEFINE INPUT  PARAMETER lpSource     AS LONG.
  DEFINE INPUT  PARAMETER dwMessageId  AS LONG.
  DEFINE INPUT  PARAMETER dwLanguageId AS LONG.
  DEFINE OUTPUT PARAMETER lpBuffer     AS MEMPTR.
  DEFINE INPUT  PARAMETER nSize        AS LONG.
  DEFINE INPUT  PARAMETER Arguments    AS LONG.
  DEFINE RETURN PARAMETER iReturn      AS LONG.
END.
