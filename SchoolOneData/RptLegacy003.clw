

                     MEMBER('RptLegacy.clw')               ! This is a MEMBER module

!!! <summary>
!!! Generated from procedure template - Frame
!!! Frame Not Used in DLL
!!! </summary>
Main PROCEDURE

LocalRequest         LONG                                  ! 
OriginalRequest      LONG                                  ! 
LocalResponse        LONG                                  ! 
FilesOpened          LONG                                  ! 
WindowOpened         LONG                                  ! 
WindowInitialized    LONG                                  ! 
ForceRefresh         LONG                                  ! 
CurrentTab           STRING(80)                            ! 
DisplayDayString STRING('Sunday   Monday   Tuesday  WednesdayThursday Friday   Saturday ')
DisplayDayText   STRING(9),DIM(7),OVER(DisplayDayString)
AppFrame             APPLICATION('TopSpeed University'),AT(,,400,243),FONT('MS Sans Serif',8,COLOR:Black),RESIZE, |
  MAXIMIZE,ALRT(MouseLeft2),ICON('_TPSPDU.ICO'),MAX,HLP('~TopSpeedUniversity'),STATUS(-1,80, |
  120,45),SYSTEM,IMM
                       MENUBAR
                         MENU('&File')
                           ITEM('&Print Setup ...'),USE(?PrintSetup),MSG('Setup printer'),STD(STD:PrintSetup)
                           ITEM,SEPARATOR
                           ITEM('E&xit'),USE(?Exit),MSG('Exit this application'),STD(STD:Close)
                         END
                         MENU('&Edit')
                           ITEM('Cu&t'),USE(?Cut),KEY(CtrlX),MSG('Remove item to Windows Clipboard'),STD(STD:Cut)
                           ITEM('&Copy'),USE(?Copy),KEY(CtrlC),MSG('Copy item to Windows Clipboard'),STD(STD:Copy)
                           ITEM('&Paste'),USE(?Paste),KEY(CtrlV),MSG('Paste contents of Windows Clipboard'),STD(STD:Paste)
                         END
                         MENU('&Reports'),USE(?ReportMenu),MSG('Report data')
                           ITEM('Student Class Schedules'),USE(?PrintENR:StuSeq),MSG('Print ordered by by Student Number')
                           ITEM('Teacher Class Schedules'),USE(?ReportsTeacherClassSchedules)
                           ITEM('Attendance Sheets'),USE(?PrintENR:SeqStu),MSG('Print ordered by by Class Number')
                           ITEM('Course Enrollment'),USE(?PrintCOU:KeyDescription),MSG('Print ordered by by Course' & |
  ' Description')
                           ITEM('Course Enrollment Summary'),USE(?ReportsCourseEnrollmentSummary)
                           ITEM('Final Grades'),USE(?ReportsFinalGrades)
                           ITEM('Student IDs'),USE(?ReportsStudentIDs)
                         END
                         MENU('&Window'),MSG('Create and Arrange windows'),STD(STD:WindowList)
                           ITEM('T&ile'),USE(?Tile),MSG('Make all open windows visible'),STD(STD:TileWindow)
                           ITEM('&Cascade'),USE(?Cascade),MSG('Stack all open windows'),STD(STD:CascadeWindow)
                           ITEM('&Arrange Icons'),USE(?Arrange),MSG('Align all window icons'),STD(STD:ArrangeIcons)
                         END
                         MENU('&Help'),MSG('Windows Help')
                           ITEM('&Contents'),USE(?Helpindex),MSG('View the contents of the help file'),STD(STD:HelpIndex)
                           ITEM('&Search for Help On...'),USE(?HelpSearch),MSG('Search for help on a subject'),STD(STD:HelpSearch)
                           ITEM('&How to Use Help'),USE(?HelpOnHelp),MSG('How to use Windows Help'),STD(STD:HelpOnHelp)
                           ITEM,SEPARATOR
                         END
                       END
                       TOOLBAR,AT(0,0,399,16)
                         BUTTON,AT(2,1,16,14),USE(?TBarBrwTop,TBarBrwTop),ICON('VCRFIRST.ICO'),DISABLE,TIP('Go to the ' & |
  'First Page')
                         BUTTON,AT(18,1,16,14),USE(?TBarBrwPageUp,TBarBrwPageUp),ICON('VCRPRIOR.ICO'),DISABLE,TIP('Go to the ' & |
  'Prior Page')
                         BUTTON,AT(34,1,16,14),USE(?TBarBrwUp,TBarBrwUp),ICON('VCRUP.ICO'),DISABLE,TIP('Go to the ' & |
  'Prior Record')
                         BUTTON,AT(50,1,16,14),USE(?TBarBrwLocate,TBarBrwLocate),ICON('FIND.ICO'),DISABLE,TIP('Locate record')
                         BUTTON,AT(66,1,16,14),USE(?TBarBrwDown,TBarBrwDown),ICON('VCRDOWN.ICO'),DISABLE,TIP('Go to the ' & |
  'Next Record')
                         BUTTON,AT(82,1,16,14),USE(?TBarBrwPageDown,TBarBrwPageDown),ICON('VCRNEXT.ICO'),DISABLE,TIP('Go to the Next Page')
                         BUTTON,AT(98,1,16,14),USE(?TBarBrwBottom,TBarBrwBottom),ICON('VCRLAST.ICO'),DISABLE,TIP('Go to the Last Page')
                         BUTTON,AT(118,1,16,14),USE(?TBarBrwSelect,TBarBrwSelect),ICON('MARK.ICO'),DISABLE,TIP('Select This Record')
                         BUTTON,AT(134,1,16,14),USE(?TBarBrwInsert,TBarBrwInsert),ICON('INSERT.ICO'),DISABLE,TIP('Insert a New Record')
                         BUTTON,AT(150,1,16,14),USE(?TBarBrwChange,TBarBrwChange),ICON('EDIT.ICO'),DISABLE,TIP('Edit This Record')
                         BUTTON,AT(166,1,16,14),USE(?TBarBrwDelete,TBarBrwDelete),ICON('DELETE.ICO'),DISABLE,TIP('Delete This Record')
                         BUTTON,AT(186,1,16,14),USE(?TbarBrwHistory,TBarBrwHistory),ICON('DITTO.ICO'),DISABLE,TIP('Previous value')
                         BUTTON,AT(202,1,16,14),USE(?TbarBrwHelp,TBarBrwHelp),ICON('HELP.ICO'),DISABLE,TIP('Get Help')
                       END
                     END
  CODE
  PUSHBIND
  LocalRequest    = GlobalRequest
  OriginalRequest = GlobalRequest
  LocalResponse   = RequestCancelled
  ForceRefresh    = False
  CLEAR(GlobalRequest)
  CLEAR(GlobalResponse)
  IF KEYCODE() = MouseRight
    SETKEYCODE(0)
  END
  DO PrepareProcedure
  SYSTEM{PROP:Icon} = '~_TpSpdU.ICO'
  IF NOT INRANGE(AppFrame{Prop:Timer},1,100)
    AppFrame{Prop:Timer} = 100
  END
    AppFrame{Prop:StatusText,3} = CLIP(DisplayDayText[(TODAY()%7)+1]) & ', ' & FORMAT(TODAY(),@D4)
    AppFrame{Prop:StatusText,4} = FORMAT(CLOCK(),@T3)
  ACCEPT
    CASE EVENT()
    OF EVENT:DoResize
      ForceRefresh = True
      DO RefreshWindow
    OF EVENT:GainFocus
      ForceRefresh = True
      IF NOT WindowInitialized
        DO InitializeWindow
        WindowInitialized = True
      ELSE
        DO RefreshWindow
      END
    OF EVENT:OpenWindow
      IF NOT WindowInitialized
        DO InitializeWindow
        WindowInitialized = True
      END
      SELECT(1)
    OF EVENT:Sized
      POST(EVENT:DoResize,0,THREAD())
    OF Event:Rejected
      BEEP
      DISPLAY(?)
      SELECT(?)
    ELSE
      IF INRANGE(ACCEPTED(),TBarBrwFirst,TBarBrwLast) THEN            !Toolbar Browse box navigation control handler
        POST(EVENT:Accepted,ACCEPTED(),SYSTEM{PROP:Active})
        CYCLE
      END
      IF EVENT() = Event:Timer
          AppFrame{Prop:StatusText,3} = CLIP(DisplayDayText[(TODAY()%7)+1]) & ', ' & FORMAT(TODAY(),@D4)
          AppFrame{Prop:StatusText,4} = FORMAT(CLOCK(),@T3)
      END
    END
    CASE ACCEPTED()
    OF ?PrintENR:StuSeq
      START(LegClassSchedules1, 050000)
    OF ?ReportsTeacherClassSchedules
      START(LegClassSchedules2, 25000)
    OF ?PrintENR:SeqStu
      START(LegAttendanceSheets, 050000)
    OF ?PrintCOU:KeyDescription
      START(LegCourseEnrollment, 050000)
    OF ?ReportsCourseEnrollmentSummary
      START(LegEnrollSummary, 25000)
    OF ?ReportsFinalGrades
      START(LegFinalGrades, 25000)
    OF ?ReportsStudentIDs
      START(LegStudentIDs, 25000)
    END
  END
  DO ProcedureReturn
!---------------------------------------------------------------------------
PrepareProcedure ROUTINE
  FilesOpened = TRUE
  DO BindFields
  OPEN(AppFrame)
  WindowOpened=True
      AppFrame{PROP:TabBarVisible}  = False
  Do DefineListboxStyle

!---------------------------------------------------------------------------
BindFields ROUTINE
!---------------------------------------------------------------------------
UnBindFields ROUTINE
!---------------------------------------------------------------------------
ProcedureReturn ROUTINE
!|
!| This routine provides a common procedure exit point for all template
!| generated procedures.
!|
!| First, all of the files opened by this procedure are closed.
!|
!| Next, if it was opened by this procedure, the window is closed.
!|
!| Next, GlobalResponse is assigned a value to signal the calling procedure
!| what happened in this procedure.
!|
!| Next, we replace the BINDings that were in place when the procedure initialized
!| (and saved with PUSHBIND) using POPBIND.
!|
!| Finally, we return to the calling procedure.
!|
  IF FilesOpened
  END
  IF WindowOpened
    CLOSE(AppFrame)
  END
  Do UnBindFields
  IF LocalResponse
    GlobalResponse = LocalResponse
  ELSE
    GlobalResponse = RequestCancelled
  END
  POPBIND
  RETURN
!---------------------------------------------------------------------------
InitializeWindow ROUTINE
!|
!| This routine is used to prepare any control templates for use. It should be called once
!| per procedure.
!|
  DO RefreshWindow
!---------------------------------------------------------------------------
RefreshWindow ROUTINE
!|
!| This routine is used to keep all displays and control templates current.
!|
  IF AppFrame{Prop:AcceptAll} THEN EXIT.
  Do LookupRelated
  DISPLAY()
  ForceRefresh = False
!---------------------------------------------------------------------------
SyncWindow ROUTINE
!|
!| This routine is used to insure that any records pointed to in control
!| templates are fetched before any procedures are called via buttons or menu
!| options.
!|
  Do LookupRelated
!---------------------------------------------------------------------------
LookupRelated  ROUTINE
!|
!| This routine fetch all related records.
!| It's called from SyncWindow and RefreshWindow
!|
!---------------------------------------------------------------------------
DefineListboxStyle ROUTINE
!|
!| This routine create all the styles to be shared in this window
!| It's called after the window open
!|
!---------------------------------------------------------------------------
