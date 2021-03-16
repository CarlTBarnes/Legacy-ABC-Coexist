   PROGRAM

   INCLUDE('CbWndPrvHelpHook.INC'),ONCE   !From GitHub CarlTBarnes
   INCLUDE('CbWndPreview.INC'),ONCE       !<><><><><><><> CB Wnd Preview <><><><><><><>

Help2WndPreviewCls  &CbWndPrvHelpHookClass  !Declare as Ref so no chance affects live APP
   INCLUDE('Equates.CLW')
   INCLUDE('TplEqu.CLW')
   INCLUDE('Keycodes.CLW')
   INCLUDE('Errors.CLW')
include('prnprop.clw')
   MAP
     MODULE('BothSchool001.clw')
       Main
       SplashIt
       SplashIt2
     END
     MODULE('ProcLegacy.lib')
       BrowseStudents,DLL(dll_mode)
       AttendanceSheets,DLL(dll_mode)
       BrowseClasses,DLL(dll_mode)
       UpdateGrades,DLL(dll_mode)
       BrowseCourses,DLL(dll_mode)
       BrowseEnrollment,DLL(dll_mode)
       BrowseTeachers,DLL(dll_mode)
       CourseEnrollment,DLL(dll_mode)
       BrowseMajors,DLL(dll_mode)
       ClassTree,DLL(dll_mode)
       FinalGrades,DLL(dll_mode)
       StudentTree,DLL(dll_mode)
       ClassSchedules1,DLL(dll_mode)
       ClassSchedules2,DLL(dll_mode)
       EnrollSummary,DLL(dll_mode)
       StudentIDs,DLL(dll_mode)
     END
     MODULE('AbcInitDLL.lib')
       Abc_Init_Kill(BYTE KIllMe=0),DLL(dll_mode)
     END
     MODULE('ProcABC.lib')
       AbcBrowseStudents,DLL(dll_mode)
       ABCBrowseClasses,DLL(dll_mode)
       ABCBrowseTeachers,DLL(dll_mode)
       ABCBrowseCourses,DLL(dll_mode)
       ABCBrowseEnrollment,DLL(dll_mode)
       ABCBrowseMajors,DLL(dll_mode)
       ABCClassTree,DLL(dll_mode)
       AbcCourseEnrollment,DLL(dll_mode)
       ABCStudentTree,DLL(dll_mode)
     END
     MODULE('BothS_SF.CLW')
       CheckOpen(FILE File,<BYTE OverrideCreate>,<BYTE OverrideOpenMode>)
       ReportPreview(QUEUE PrintPreviewQueue)
       Preview:JumpToPage(LONG Input:CurrentPage, LONG Input:TotalPages),LONG
       Preview:SelectDisplay(*LONG Input:PagesAcross, *LONG Input:PagesDown)
       StandardWarning(LONG WarningID),LONG,PROC
       StandardWarning(LONG WarningID,STRING WarningText1),LONG,PROC
       StandardWarning(LONG WarningID,STRING WarningText1,STRING WarningText2),LONG,PROC
       SetupStringStops(STRING ProcessLowLimit,STRING ProcessHighLimit,LONG InputStringSize,<LONG ListType>)
       NextStringStop,STRING
       SetupRealStops(REAL InputLowLimit,REAL InputHighLimit)
       NextRealStop,REAL
       INIRestoreWindow(STRING ProcedureName,STRING INIFileName)
       INISaveWindow(STRING ProcedureName,STRING INIFileName)
       RISaveError
     END
     MODULE('BothS_RU.CLW')
     END
     MODULE('BothS_RD.CLW')
     END
   END

GLO:FileName         STRING(64)
AppFrameRef          &WINDOW
GLO:DropThread       LONG
GLO:DropControl      LONG
GLO:ThreadRef        &LONG
GLO:CameFrom         STRING(20)

SaveErrorCode        LONG
SaveError            CSTRING(255)
SaveFileErrorCode    CSTRING(255)
SaveFileError        CSTRING(255)
GlobalRequest        LONG(0),EXTERNAL,DLL(dll_mode),THREAD
GlobalResponse       LONG(0),EXTERNAL,DLL(dll_mode),THREAD
VCRRequest           LONG(0),EXTERNAL,DLL(dll_mode),THREAD
!region File Declaration
!endregion

Sort:Name            STRING(ScrollSort:Name)
Sort:Name:Array      STRING(3),DIM(100),OVER(Sort:Name)
Sort:Alpha           STRING(ScrollSort:Alpha)
Sort:Alpha:Array     STRING(2),DIM(100),OVER(Sort:Alpha)


  CODE
  HELP('SCHOOL.HLP')
  Main
!---------------------------------------------------------------------------
