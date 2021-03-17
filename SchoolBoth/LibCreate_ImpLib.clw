!To make the LIBs from the EXPs for School Both just build this  
!See https://clarionhub.com/t/msbuild-example-for-clarion/274/9?u=carlbarnes
!DOS Cmd: -- DEL ImpLib.TXT & FOR %f IN (*.exp) DO IF not EXIST %~nf.EXE ECHO #implib %~nf.LIB %f >> ImpLib.TXT

  PROGRAM
!  MAP.
  CODE

  !Based on https://clarionmag.jira.com/wiki/spaces/clarion/pages/399918/Handling+circular+references+with+implib  
  !
  !The TopSpeed / Clarion Project Laanguage is described in the AdvancedTopicsReferenceGuide.pdf
  !  Note this is WRONG syntax: #implib <ExpFilename> <LibFilename>  
  !           The Right Syntax: #implib <LibFilename> <ExpFilename>  
  !  Search #implib finds all examples with the right syntax: #implib %%drvname%%.lib %%drvname%%.exp
  
!This SLN includes a LibCreate_ImpLib.PR Project file with lines like below 
!-------------------------------------------------
!    #implib ALLFILES.LIB  ALLFILES.EXP
!    #implib REPORTS.LIB   REPORTS.EXP
!    #implib UPDATES.LIB   UPDATES.EXP 
!    -- No need to keep the useless files made by this
!    #file delete LibCreate_ImpLib.LIB 
!-------------------------------------------------
!Each line of the prj is simply an #implib statement followed by the name of the LIB to be created and the EXP to use when creating the LIB.
!
!So while this CLW has no code that does anything, it is set to create a LIB that can be disposed
!So ths CwProj has Post Build Event to:  DEL LibCreate_ImpLib.LIB
!If this makes an EXE then: DEL LibCreate_ImpLib.EXE & LibCreate_ImpLib.LIB
  

    OMIT('**END**')

    Excerpt From "Basic Compiling and Linking"

    #implib

      #implib <libfilename>

    The #implib command is used to create (if necessary) a dynamic link library file. There are two 
    forms of this command, which operate in slightly different ways. If a single filename is specified, 
    this names an import library file, which is created (if not up-to-date) from the object files in 
    the link list. The object files are scanned and each public procedure or variable is exported. 
    For example:

        #pragma link( fred.obj, joe.obj )

        #implib mylib.lib
        
    In the second form of the #implib command, an import library filename and a module definition file 
    (.exp - see Module Definition File below) are both specified, and the library file is created 
    (if not up-to-date) from the symbols named in the definition file. This form of the command 
    is equivalent to using the tsimplib utility that comes with SoftVelocity C, C++, and Modula-2.

        #implib <filename.EXP> <filename.LIB>  <--- WRONG .Lib .Exp
        #implib <filename.LIB> <filename.EXP>  <--- RIGHT .Exp .Lib
        
    Using #implib in the second form requires you to create and maintain the list of exports 
    'by hand', whereas the first form exports all public names automatically. The use of a 
    module definition file is an advantage if you need to maintain compatibility with previous 
    versions of an interface, and it also allows you to export only the procedures which need 
    to be exported.

    When #implib is used with a module definition file, the link list is cleared.    
    
    !end of OMIT('**END**')
      