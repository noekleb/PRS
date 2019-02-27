 
 /*------------------------------------------------------------------------
    File        : JBoxCompile
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : goo_000
    Created     : Wed Nov 09 09:41:24 CET 2016
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING OpenEdge.DataAdmin.ISequence FROM PROPATH.

BLOCK-LEVEL ON ERROR UNDO, THROW.

CLASS JBoxCompile: 
    def stream stIn.
      
    def temp-table ttDirectory no-undo
      field iDirectorySeq as int
      field cSourceDirectory as char 
      field cTargetDirectory as char 
      index iDirectorySeq is primary unique iDirectorySeq
      .
    def temp-table ttFile no-undo 
      field iFileSeq as int
      field iDirectorySeq as int
      field cFileName as char format 'X(72)'
      field cFullFileName as char format 'X(72)'
      field cFileExtent as char  
      field bCompiled as log
      field cFileStatus as char format "x(72)"
       
      index iFileSeq is primary unique iFileSeq
      index iDirectorySeq iDirectorySeq
      .  

    def dataset dsCompile 
      for ttDirectory, ttFile
      data-relation relttDirectory for ttDirectory, ttFile relation-field(iDirectorySeq,iDirectorySeq).


    def var iSequence as int no-undo.
    def var cTmp as char no-undo.
    
	DEFINE PUBLIC PROPERTY TargetPath AS CHARACTER NO-UNDO 
	GET.
	SET. 
    DEFINE PUBLIC PROPERTY SourcePath AS CHARACTER NO-UNDO 
    GET.
    SET. 

    DEFINE PUBLIC PROPERTY ttDirecotry AS handle NO-UNDO 
    GET():
      return temp-table ttDirectory:HANDLE.  
    end get.
    SET. 

    DEFINE PUBLIC PROPERTY ttFile AS handle NO-UNDO 
    GET():
      return temp-table ttFile:HANDLE.  
    end get.
    SET. 

    DEFINE PUBLIC PROPERTY dsCompile AS handle NO-UNDO 
    GET():
      return dataset dsCompile:HANDLE.  
    end get.
    SET. 
    
    DEFINE PUBLIC PROPERTY LogFile AS char NO-UNDO GET. SET.

    DEFINE PUBLIC PROPERTY Logg AS longchar NO-UNDO GET. SET.
     
	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
		
	CONSTRUCTOR PUBLIC JBoxCompile (input icLogFile as char, input iiLogLevel as int  ):
		SUPER ().
		  
/*		LOG-MANAGER:LOGFILE-NAME  = icLogFile.       */
/*        log-manager:LOGGING-LEVEL = iiLogLevel.*/
            this-object:LogFile = icLogFile.
            create ttFile.
            assign
              iSequence            = iSequence + 1 
              ttFile.iFileSeq      = iSequence
              ttFile.cFileName     = ''
              ttFile.cFileExtent   = ''
              ttFile.bCompiled     = ?
              ttFile.iDirectorySeq = ?
              .           
		
		CATCH e AS Progress.Lang.Error:

		END CATCH.

		FINALLY:

		END FINALLY.

	END CONSTRUCTOR.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/

	METHOD PUBLIC LOGICAL addDirectory( input icSourceDirectory as char, input icTargetDirectory as char ):
      create ttDirectory.
      assign
        iSequence                    = iSequence + 1 
        ttDirectory.iDirectorySeq    = iSequence
        ttDirectory.cSourceDirectory = icSourceDirectory
        ttDirectory.cTargetDirectory = icTargetDirectory
        .		
      return true.  

	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/

	METHOD PUBLIC LOGICAL compileDirectory( input icSourceDirectory as char  ):
	  def var cFilename as char no-undo.
	  def var cFileType as char no-undo.
	  def var cFullname as char no-undo.
	  def var iFileSize as char no-undo.
    def var cFileExtent as char no-undo.
    def var cTmp as char no-undo.
      
/*      cTmp = SESSION:TEMP-DIRECTORY.*/
	  input stream stIn from os-dir(icSourceDirectory).
	  repeat:
	    import stream stIn cFilename cFullname cFileType.
	    if cFileType = 'F' and index(cFilename,'.') gt 0 then
	    do:
	      if can-do('cls,p,w',entry(2,cFileName,'.')) then 
	      do:
	        cFileExtent = entry(2,cFileName,'.').
	        create ttFile.
	        assign
	          iSequence            = iSequence + 1 
	          ttFile.iFileSeq      = iSequence
	          cTmp                 = replace(cFilename,'/','\')
	          cTmp                 = if r-index(cTmp,'\') gt 0 then substring(cTmp,r-index(cTmp,'\') + 1) else cTmp
	          ttFile.cFileName     = cTmp
            ttFile.cFullFileName = cFullname
	          ttFile.cFileExtent   = cFileExtent
	          ttFile.bCompiled     = ?
	          ttFile.iDirectorySeq = ttDirectory.iDirectorySeq
	          .  
	      end.  
	    end. 
	  end.
	  
      for each ttFile, first ttDirectory of ttFile:
        compile value(ttFile.cFullFileName) save into value(ttDirectory.cTargetDirectory) no-error.
        if error-status:error then 
        do:
            assign 
              ttFile.cFileStatus = ERROR-STATUS:GET-MESSAGE (1)
              ttFile.bCompiled   = false
              .
            undo, next.                 
        end.
        else do:
          ttFile.bCompiled = true.
        end.
      end.
      CATCH eAny AS Progress.Lang.Error :
        JBoxLogging:Instance:LOG('Error.....' + quoter(eAny:GetMessage(1))).
      END CATCH.
	END METHOD.

    METHOD PUBLIC LOGICAL compileCommand( input icSourceFile as char, input icTargetDirectory as char  ):
      def var cTmp as char no-undo.
      file-info:file-name = icSourceFile.
      if file-info:file-size ne ? then 
        compile value(icSourceFile) save into value(icTargetDirectory) no-error.
      else do:
       this-object:Logg = this-object:Logg + icSourceFile + " ble ikke funnet!" + chr(13) + chr(10).
        JBoxLogging:Instance:LOG('Error.....' + quoter(error-status:GET-MESSAGE (1))).            
        return false.        
      end. 
      if error-status:error then 
      do:
        this-object:Logg = this-object:Logg + icSourceFile + " " + error-status:GET-MESSAGE (1) + chr(13) + chr(10).
        JBoxLogging:Instance:LOG('Error.....' + quoter(error-status:GET-MESSAGE (1))).
        return false.            
      end.
      else do:
        this-object:Logg = this-object:Logg + icSourceFile + " Kompilert OK!:" +  chr(13) + chr(10).
/*        assign                                                                                                 */
/*          cTmp                 = replace(icSourceFile,'/','\')                                                 */
/*          cTmp                 = if r-index(cTmp,'\') gt 0 then substring(cTmp,r-index(cTmp,'\') + 1) else cTmp*/
/*        .                                                                                                      */
      end.
      return true.
    end method.


	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
  
	METHOD PUBLIC void startCompile ():
      for each ttDirectory:
        compileDirectory(ttDirectory.cSourceDirectory).
      end.
      this-object:Logg = 'START kompilering:' + chr(13) + chr(10).
      for each ttFile where ttFile.bCompiled: 
        this-object:Logg = this-object:Logg + ttFile.cFileName + chr(13) + chr(10) + chr(10).
       end.
      if can-find(first ttFile where not ttFile.bCompiled) then this-object:Logg = this-object:Logg + 'Feil ved kompilering:' + chr(13) + chr(10).
      for each ttFile where not ttFile.bCompiled: 
        this-object:Logg = this-object:Logg + ttFile.cFileName + ':' + chr(9) + ttFile.cFileStatus + chr(13) + chr(10).
       end.
      this-object:Logg = this-object:Logg + 'FERDIG:' + chr(13) + chr(10).
      COPY-LOB object this-object:Logg to file this-object:LogFile.
       
	END METHOD.

END CLASS.
