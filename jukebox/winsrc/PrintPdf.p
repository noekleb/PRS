/**
 *  File:              PrintPdf.p
 *
 *  Description:       This procedure is used to print a PDF document
and
 *                     close the Adobe process after the print job is
 *                     is finished spooling.  It uses Win32 API calls to
 *                     accomplish this.
 *
 *  Input Parameters:
 *    character - cPdfFile - the full path to the PDF to print
 *    character - cPrinter - the name of the printer to use
 *
 *  Output Parameters:
 *    <none>
 *
 *  Author:            Patrick Carlo-Hickman
 *                     Allegro Consultants
 *                     pcarlo@allegroconsultants.com
 *
 *  Created:           June 1, 2009
 *  
 *  Modified:          mm/dd/yyyy - <initials>
 *                     <description>
 */
define input parameter cPdfFile as character no-undo.
define input parameter cPrinter as character no-undo.

run ipPrintPdf(cPdfFile, cPrinter).

function fnIsJobSpooling returns logical
  (input cPrinter as character, input cPdfFile as character):

  define variable lReturn        as logical    no-undo.
  define variable cPdfName       as character  no-undo.
  define variable iPrinterHandle as integer    no-undo.
  define variable iReturnValue   as integer    no-undo.
  define variable iNeeded        as integer    no-undo.
  define variable iReturned      as integer    no-undo.
  define variable j              as integer    no-undo.
  define variable iJobSize       as integer    no-undo initial 64.
  define variable iOffset        as integer    no-undo.
  define variable mJobEnum       as memptr     no-undo.
  define variable mJobInfo       as memptr     no-undo.
  define variable mDocument      as memptr     no-undo.
  define variable cDocument      as character  no-undo.
  define variable cStatus        as character  no-undo.
  define variable iStatus        as integer    no-undo.

  /**
   * Open the printer to get the printer handle.
   */
  run OpenPrinterA(cPrinter, output iPrinterHandle, 0, output
iReturnValue).

  /**
   * How this works:
   * 1. Initialize the JobEnum memory pointer to some dummy value.
   * 2. Run EnumJobsA.  iNeeded will be set to the size of memory needed
   *    to hold the JOB_INFO_n array.
   * 3. If there are jobs, the return value will be 0, denoting the call
   *    failed (since we need more memory).
   * 4. Reinitialize the JobEnum memory pointer to the correct size.
   * 5. Run EnumJobsA again.  Now the JobEnum pointer should point to an
   *    array of JOB_INFO_n structures.
   */
  assign
    cPdfName = entry(num-entries(cPdfFile, "~\"), cPdfFile, "~\")
    set-size(mJobEnum) = 1
    .
  run EnumJobsA(iPrinterHandle,              /* Handle to printer object
*/
                0,                           /* 0 based first job to
enum */
                -1,                          /* Number of jobs to enum
(-1 = all) */
                1,                           /* Structure level to
return (1,2,3) */
                get-pointer-value(mJobEnum), /* points to JOB_INFO_n
structures */
                get-size(mJobEnum),          /* Tells function the size
of mJobEnum */
                output iNeeded,              /* Number of bytes copied
or required */
                output iReturned,            /* Number of JOB_INFO_n
structures returned */
                output iReturnValue).        /* A Bool value = zero if
failure */
  if iReturnValue = 0 then
  RERUN-ENUM:
  do:
    /**
     * If the needed value is 0 here, there's been some type of error.
     */
    if iNeeded = 0 then
    do:
      assign
        iReturned = 0
        lReturn = false
        .
      leave RERUN-ENUM.
    end.

    assign
      set-size(mJobEnum) = 0
      set-size(mJobEnum) = iNeeded
      .
    run EnumJobsA(iPrinterHandle,              /* Handle to printer
object */
                  0,                           /* 0 based first job to
enum */
                  -1,                          /* Number of jobs to enum
(-1 = all) */
                  1,                           /* Structure level to
return (1,2,3) */
                  get-pointer-value(mJobEnum), /* points to JOB_INFO_n
structures */
                  get-size(mJobEnum),          /* Tells function the
size of mJobEnum */
                  output iNeeded,              /* Number of bytes copied
or required */
                  output iReturned,            /* Number of JOB_INFO_n
structures returned */
                  output iReturnValue).        /* A Bool value = zero if
failure */
    /**
     * If the return value is 0 here, there's been some type of error.
     */
    if iReturnValue = 0 then
    do:
      assign
        iReturned = 0
        lReturn = false
        .
      leave RERUN-ENUM.
    end.
  end.
  /**
   * If the return value from the first EnumJobs call wasn't 0, then
there
   * are no jobs in the queue.  Therefore, the job isn't spooling.
   */
  else
    assign
      iReturned = 0
      lReturn = false
      .
  
  /**
   * Loop through the JobEnum array.
   */
  do j = 1 to iReturned:
    /**
     * The JobInfo memory pointer will point to the individual elements
     * in the JobEnum array.  Update the offset each incrementation to
     * move to the next element.
     */
    assign
      iOffset = (j - 1) * iJobSize
      set-pointer-value(mJobInfo) = get-pointer-value(mJobEnum) +
iOffset
      set-pointer-value(mDocument) = get-long(mJobInfo, 17)
      cDocument = get-string(mDocument, 1)
      .

    /**
     * Use the name of the document we're printing to make sure we are
     * looking at the correct JobInfo structure.
     *
     * An 8 in the one's digit of the status from the JobInfo structure
     * denotes that the print job is spooling.
     */
    if index(cDocument, cPdfName) > 0 then
      assign
        iStatus = get-long(mJobInfo, 29)
        iStatus = iStatus - (truncate(iStatus / 10, 0) * 10)
        lReturn = (iStatus >= 8)
        .

  end.

  /**
   * Close the handle to the printer.
   */
  run ClosePrinter(iPrinterHandle, output iReturnValue).

  /**
   * Free our memory.
   */
  set-size(mJobEnum) = 0.

  return lReturn.

end function.

procedure ipPrintPdf:
  define input  parameter cPdfFile as character  no-undo.
  define input  parameter cPrinter as character  no-undo.

  /*
  define variable cReaderPath as character  no-undo.
  load "SOFTWARE" base-key "HKEY_LOCAL_MACHINE".
  use  "SOFTWARE".
  get-key-value section "Microsoft\Windows\CurrentVersion\App
Paths\AcroRd32.exe" key default value cReaderPath.
  unload "SOFTWARE".
  
  os-command silent value('call "' + cReaderPath + '" /t "' + cPdfFile +
'" "' + cPrinter + '"').
  */

  define variable mVerb        as memptr  no-undo.
  define variable mFile        as memptr  no-undo.
  define variable mPrinter     as memptr  no-undo.
  define variable mExecInfo    as memptr  no-undo.
  define variable iReturnValue as integer no-undo.
  define variable j            as integer no-undo.
  define variable iProcess     as integer no-undo.

  assign
    /**
     * Set up the pointers to strings.  Strings pointed to by mem
pointers
     * must be NULL terminated.
     */
    set-size(mVerb) = length("printto") + 1
    put-string(mVerb, 1) = "printto"
    set-size(mFile) = length(cPdfFile) + 1
    put-string(mFile, 1) = cPdfFile
    set-size(mPrinter) = length(cPrinter) + 1
    put-string(mPrinter, 1) = cPrinter

    /**
     * Build the SHELLEXECUTEINFO structure.
     */
    set-size(mExecInfo) = 60
    put-long(mExecInfo, 1) = get-size(mExecInfo)
    /**
     * SEE_MASK_NOCLOSEPROCESS   - 0x00000040.
     * Use to indicate that the hProcess member receives the process
handle.
     * This handle is typically used to allow an application to find out
when
     * a process created with ShellExecuteEx terminates. The calling
     * application is responsible for closing the handle when it is no
longer
     * needed.
     * SEE_MASK_NOASYNC          - 0x00000100.
     * Wait for the execute operation to complete before returning.
     * If the SEE_MASK_WAITFORINPUTIDLE flag is specified, then
ShellExecuteEx
     * calls WaitForInputIdle and waits for the new process to idle
before
     * returning, with a maximum timeout of 1 minute.
     * SEE_MASK_NO_CONSOLE       - 0x00008000.
     * Use to create a console for the new process instead of having it
     * inherit the parent's console.
     * SEE_MASK_WAITFORINPUTIDLE - 0x02000000.
     * After the new process is created, wait for the process to become
idle
     * before returning, with a one minute timeout.
     */
    put-long(mExecInfo, 5) = 33587520 /* combiniation of above hex
values */
    put-long(mExecInfo, 9) = 0 /* hwnd                    */
    put-long(mExecInfo, 13) = get-pointer-value(mVerb)
    put-long(mExecInfo, 17) = get-pointer-value(mFile)
    put-long(mExecInfo, 21) = get-pointer-value(mPrinter)
    put-long(mExecInfo, 25) = 0 /* current directory       */
    put-long(mExecInfo, 29) = 2 /* wCmdShow                */
    .
 
  /**
   * Execute the printto command using the structure we just built.
Since
   * we're printing a PDF, this will open Adobe Reader.
   */
  run ShellExecuteExA(get-pointer-value(mExecInfo), output
iReturnValue).

  /**
   * Make sure the job finishes spooling to the printer.  Wait a max of
   * 30 seconds.
   */
  do j = 1 to 30:
    if not fnIsJobSpooling(cPrinter, cPdfFile) then
      leave.
    pause 1 no-message.
  end.

  /**
   * Get the handle to the process created from the ShellExecuteExA
command
   * so that we can terminate it.  This will kill the Adobe process that
is
   * started.
   */
  iProcess = get-long(mExecInfo, 57).
  run TerminateProcess(iProcess, 0, output iReturnValue).

  /**
   * Release the memory.
   */
  assign
    set-size(mExecInfo) = 0
    set-size(mFile) = 0
    set-size(mVerb) = 0
    set-size(mPrinter) = 0
    .

end procedure.

procedure ShellExecuteExA external "shell32.dll" :
  define input  parameter mExecInfo  as long. /* Pointer to a
SHELLEXECUTEINFO struct */
  define return parameter ReturnValue as long. /* A Bool value = zero if
failure */
end procedure.

procedure TerminateProcess external "kernel32" :
  define input  parameter hProcess    as long. /* Handle to process
object */
  define input  parameter uExitCode   as long. /* Exit code for the
process to use */
  define return parameter ReturnValue as long. /* A Bool value = zero if
failure */
end procedure.

procedure OpenPrinterA external "winspool.drv" :
  define input  parameter pPrinterName as character. /* Name of the
printer */
  define output parameter phPrinter    as long.      /* Handle to
printer object */
  define input  parameter pDefault     as long.      /* Pointer to
PRINTER_DEFAULTS struct */
  define return parameter ReturnValue  as long.      /* A Bool value =
zero if failure */
end procedure.

procedure ClosePrinter external "winspool.drv" :
  define input  parameter hPrinter     as long. /* Handle to printer
object */
  define return parameter ReturnValue  as long. /* A Bool value = zero
if failure */
end procedure.

procedure EnumJobsA external "winspool.drv" :
  define input  parameter hPrinter    as long. /* Handle to printer
object */
  define input  parameter FirstJob    as long. /* 0 based first job to
enum */
  define input  parameter NoJobs      as long. /* Number of jobs to enum
*/
  define input  parameter Level       as long. /* Structure level to
return (1,2,3) */
  define input  parameter pJob        as long. /* points to JOB_INFO_n
structures */
  define input  parameter cbBuf       as long. /* Tells function the
size of pJob */
  define output parameter pcbNeeded   as long. /* Number of bytes copied
or required */
  define output parameter pcReturned  as long. /* Number of JOB_INFO_n
structures returned */
  define return parameter ReturnValue as long. /* A Bool value = zero if
failure */
end procedure.
