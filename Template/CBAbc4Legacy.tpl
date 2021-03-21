#TEMPLATE(CBAbc4Legacy,'ABC Templates for Legacy Coexistence (Carl Barnes)'),FAMILY('ABC') 
#!  By Carl Barnes see https://github.com/CarlTBarnes/Legacy-ABC-Coexist
#!  Allow a DATA DLL built with ABC to service Legacy APPs 
#!      by providing File::Used Global LONGs for File Opened Counts
#!      by Exporting File::used in the .EXP
#!      by configuring each File in FileManager.Init()
#!          to set .Opened &= Legacy::Used
#!          to set .DoNotAssertOnCloseMisuse = True (CHECK Optio - not needed so far)
#!          to error if LazyOpen=True (Defer File Opening)
#!
#! ABFile.INC - FileManager CLASS - requires simple changes
#!      1. Find      "Opened UNSIGNED"
#!      2. Change to "Opened &LONG       !CBAbc4Legacy change to &LONG Reference"
#!      3. Add below "Opened::Used LONG  !CBAbc4Legacy true .Opened Count"  
#!      4. Add below "CONSTRUCT PROCEDURE() !CBAbc4Legacy Sets Opened &= Opened::Used"  
#!
#! ABFile.CLW - FileManager CLASS - requires simple changes
#!      1. Add below for new CONSTRUCT 
#!            FileManager.CONSTRUCT PROCEDURE
#!                CODE 
#!                SELF.Opened &= SELF.Opened::Used  !CBAbc4Legacy &Reference, allows &= File::Used in  Legacy
#!                RETURN
#!
#!-------------------------------------------------------------------------------
#EXTENSION(CBAbc4LegacyFileUsed,'Generate Legacy File::used and Assign to ABC File Managers'),APPLICATION
#! ----------------------------------------------------------------
  #DISPLAY ('Make an ABC DATA DLL usable as a Legacy Data DLL')
  #DISPLAY ('')
  #DISPLAY ('Template does the following:')
  #DISPLAY ('  1. Declares Global Data: FILE::Used LONG')
  #DISPLAY ('  2. Exports FILE::Used ')
  #DISPLAY ('  3. FileManager.Init sets .Opened &= FILE::Used')
  #DISPLAY ('')
  #PROMPT('Set DoNotAssertOnCloseMisuse=True',CHECK),%DoNotAssertMisuse,AT(10),DEFAULT(%FALSE)  
  #DISPLAY ('')
  #DISPLAY ('')
  #DISPLAY ('Requires FileManager changes in ABFile.INC/CLW')
  #DISPLAY ('  1. INC: .Opened change UNSIGNED to &LONG')
  #DISPLAY ('  2. INC: Add .Opended::Used LONG')
  #DISPLAY ('  3. INC: Add CONSTRUCT PROCEDURE') 
  #DISPLAY ('')
  #DISPLAY ('  4b. CLW: Add FileManager.CONSTRUCT PROCEDURE')
  #DISPLAY ('  4a. CLW: CONSTRUCT Code SELF.Opened &= SELF.Opened::Used')
  #DISPLAY ('')
  #PROMPT('Do Not Generate Code',CHECK),%DoNotGenerate,AT(10),DEFAULT(%FALSE)    
#!==============================================================================
#! If Globals are external then don't allow. Could restrict more, like only DLL
#RESTRICT
  #IF(%GlobalExternal = %False)
    #ACCEPT
  #ELSE
    #REJECT
  #ENDIF
#ENDRESTRICT
#!==============================================================================
#! For Each File's FileManager.Init() add .Opened &= File::Used for Legacy 
#!-------------------------------------------------------------------------------
#AT(%FileManagerCodeSection,,'Init','()'),PRIORITY(4500),WHERE(~%DoNotGenerate)
 SELF.Opened &= %File::Used  !CBAbc4Legacy
   #IF(%DoNotAssertMisuse)
 SELF.DoNotAssertOnCloseMisuse=True  !CBAbc4Legacy
   #ENDIF
  #IF(%OverrideLazyOpen='Yes')
    #ERROR('"Defer Opening File" CANNOT be "Yes" for ' & %File &' in "Individual File Overrides" (No LazyOpen - CBAbc4LegacyFileUsed)')
  #ENDIF   
#ENDAT
#!==============================================================================
#! Global Data declares "File::Used LONG" for Legacy 
#!-------------------------------------------------------------------------------
#AT(%BeforeFileDeclarations),WHERE(~%DoNotGenerate)
  #IF(%DefaultLazyOpen)
    #ERROR('UN-Check "Defer Opening files" in Global Actions "File Control" (No LazyOpen - CBAbc4LegacyFileUsed)')
  #ENDIF
!Region CBAbc4Legacy File::Used Counters
  #FOR(%File),WHERE(%FileIsUsedABC() AND %GetFileDeclareModeABC()='FILE')
    #IF(%FileThreadedFlag)
%File::Used LONG,THREAD
    #ELSE
%File::Used LONG
    #ENDIF
  #ENDFOR
!endregion  
#ENDAT   
#!==============================================================================
#! Add Exports for Legacy Global to EXP file as "File::Used @?" 
#!-------------------------------------------------------------------------------
#AT(%DLLExportList),WHERE(~%DoNotGenerate)
  #IF(~%GlobalExternal)
    #IF(~%NoGenerateGlobals)
 ; CBAbc4Legacy File::Used Counters
      #FOR(%File),WHERE(%FileIsUsedABC() AND (%OverrideExport OR (%DefaultExport AND %DefaultExternal='None External'))) 
         #IF(%GetFileDeclareModeABC()='FILE')
#INSERT(%AddExpItem(ABC),'$'&%File&'::Used')
         #ENDIF
      #ENDFOR
    #ENDIF
  #ENDIF
#ENDAT
#!=======================================
#! GROUPs 
#GROUP(%FileIsUsedABC),AUTO
 #DECLARE(%IsUsed)
 #CALL(%FileIsUsed(ABC)),%IsUsed
 #RETURN(%IsUsed)
#GROUP(%GetFileDeclareModeABC),AUTO    #! I want Return of 'FILE'
 #DECLARE(%RetVal)
 #CALL(%GetFileDeclareMode(ABC)),%RetVal
 #RETURN(%RetVal)
#!=======================================