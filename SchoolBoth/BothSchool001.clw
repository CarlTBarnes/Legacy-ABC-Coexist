

                     MEMBER('BothSchool.clw')              ! This is a MEMBER module

!!! <summary>
!!! Generated from procedure template - Frame
!!! Clarion for Windows Wizard Application
!!! </summary>
Main PROCEDURE

LocalRequest         LONG                         ! 
OriginalRequest      LONG                         ! 
LocalResponse        LONG                         ! 
FilesOpened          LONG                         ! 
WindowOpened         LONG                         ! 
WindowInitialized    LONG                         ! 
ForceRefresh         LONG                         ! 
CurrentTab           STRING(80)                   ! 
SplashProcedureThread LONG
DisplayDayString STRING('Sunday   Monday   Tuesday  WednesdayThursday Friday   Saturday ')
DisplayDayText   STRING(9),DIM(7),OVER(DisplayDayString)
AppFrame             APPLICATION('TopSpeed University'),AT(,,400,243),FONT('MS Sans Serif',8,COLOR:Black),RESIZE, |
  MAXIMIZE,ALRT(MouseLeft2),ICON('_TPSPDU.ICO'),MAX,HLP('~TopSpeedUniversity'),STATUS(-1,80, |
  120,45),SYSTEM,IMM
                       MENUBAR,USE(?MENUBAR1)
                         MENU('&File'),USE(?MENU1)
                           ITEM('&Print Setup ...'),USE(?PrintSetup),MSG('Setup printer'),STD(STD:PrintSetup)
                           ITEM,USE(?SEPARATOR1),SEPARATOR
                           ITEM('E&xit'),USE(?Exit),MSG('Exit this application'),STD(STD:Close)
                         END
                         MENU('&Edit'),USE(?MENU2)
                           ITEM('Cu&t'),USE(?Cut),KEY(CtrlX),MSG('Remove item to Windows Clipboard'),STD(STD:Cut)
                           ITEM('&Copy'),USE(?Copy),KEY(CtrlC),MSG('Copy item to Windows Clipboard'),STD(STD:Copy)
                           ITEM('&Paste'),USE(?Paste),KEY(CtrlV),MSG('Paste contents of Windows Clipboard'),STD(STD:Paste)
                         END
                         MENU('&Trees'),USE(?Trees)
                           ITEM('&Class Tree'),USE(?BrowseClassTree)
                           ITEM('&Students Tree'),USE(?BrowseStudentsTree)
                         END
                         MENU('&Browse'),USE(?MENU3)
                           ITEM('Students '),USE(?BrowseStudents),MSG('Browse Students')
                           ITEM('Teachers '),USE(?BrowseTeachers),MSG('Browse Teachers')
                           ITEM('Classes '),USE(?BrowseClasses),MSG('Browse Classes')
                           ITEM('Enrollment '),USE(?BrowseEnrollment),MSG('Browse Enrollment')
                           ITEM('Courses '),USE(?BrowseCourses),MSG('Browse Courses')
                           ITEM('Majors '),USE(?BrowseMajors),MSG('Browse Majors')
                           ITEM('Update Grades'),USE(?BrowseUpdateGrades)
                         END
                         MENU('BrowseABC'),USE(?ABCBrowseMenu)
                           ITEM('ABC Init'),USE(?AbcInitItem)
                           ITEM,USE(?SeparatorABC1),SEPARATOR
                           ITEM('Students ABC'),USE(?ABCBrowseStudents)
                           ITEM('Teachers '),USE(?ABCBrowseTeachers)
                           ITEM('Classes '),USE(?ABCBrowseClasses)
                           ITEM('Enrollment '),USE(?ABCBrowseEnrollment)
                           ITEM('Courses '),USE(?ABCBrowseCourses)
                           ITEM('Majors '),USE(?ABCBrowseMajors)
                           ITEM,USE(?SeparatorABC2),SEPARATOR
                           ITEM('&Class Tree'),USE(?ABCBrowseClassTree)
                           ITEM('&Students Tree'),USE(?ABCBrowseStudentsTree)
                           ITEM('Update Grades'),USE(?ABCBrowseUpdateGrades)
                           MENU('Reports'),USE(?ABCReportMenu)
                             ITEM('Course Enrollment'),USE(?ABCPrintCOU:KeyDescription)
                           END
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
                         MENU('&Window'),USE(?MENU4),MSG('Create and Arrange windows'),STD(STD:WindowList)
                           ITEM('T&ile'),USE(?Tile),MSG('Make all open windows visible'),STD(STD:TileWindow)
                           ITEM('&Cascade'),USE(?Cascade),MSG('Stack all open windows'),STD(STD:CascadeWindow)
                           ITEM('&Arrange Icons'),USE(?Arrange),MSG('Align all window icons'),STD(STD:ArrangeIcons)
                         END
                         MENU('&Help'),USE(?MENU5),MSG('Windows Help')
                           ITEM('&Contents'),USE(?Helpindex),MSG('View the contents of the help file'),STD(STD:HelpIndex)
                           ITEM('&Search for Help On...'),USE(?HelpSearch),MSG('Search for help on a subject'),STD(STD:HelpSearch)
                           ITEM('&How to Use Help'),USE(?HelpOnHelp),MSG('How to use Windows Help'),STD(STD:HelpOnHelp)
                           ITEM,USE(?SEPARATOR2),SEPARATOR
                           ITEM('&About School...'),USE(?HelpAbout)
                         END
                         MENU('CarlWndPreview'),USE(?CarlWndPreviewMenu)
                           ITEM('Enable F1 Hook to show CB Window Preview on Any Window '),USE(?CbHlpHookEnableItem)
                           ITEM('(Hide the above item for non-developer users'),USE(?CbHlpHookEnableItem2)
                         END
                       END
                       TOOLBAR,AT(0,0,400,16),USE(?TOOLBAR1)
                         BUTTON,AT(2,1,16,14),USE(?TBarBrwTop, TBarBrwTop),ICON('VCRFIRST.ICO'),DISABLE,TIP('Go to the ' & |
  'First Page')
                         BUTTON,AT(18,1,16,14),USE(?TBarBrwPageUp, TBarBrwPageUp),ICON('VCRPRIOR.ICO'),DISABLE,TIP('Go to the ' & |
  'Prior Page')
                         BUTTON,AT(34,1,16,14),USE(?TBarBrwUp, TBarBrwUp),ICON('VCRUP.ICO'),DISABLE,TIP('Go to the ' & |
  'Prior Record')
                         BUTTON,AT(50,1,16,14),USE(?TBarBrwLocate, TBarBrwLocate),ICON('FIND.ICO'),DISABLE,TIP('Locate record')
                         BUTTON,AT(66,1,16,14),USE(?TBarBrwDown, TBarBrwDown),ICON('VCRDOWN.ICO'),DISABLE,TIP('Go to the ' & |
  'Next Record')
                         BUTTON,AT(82,1,16,14),USE(?TBarBrwPageDown, TBarBrwPageDown),ICON('VCRNEXT.ICO'),DISABLE,TIP('Go to the Next Page')
                         BUTTON,AT(98,1,16,14),USE(?TBarBrwBottom, TBarBrwBottom),ICON('VCRLAST.ICO'),DISABLE,TIP('Go to the Last Page')
                         BUTTON,AT(118,1,16,14),USE(?TBarBrwSelect, TBarBrwSelect),ICON('MARK.ICO'),DISABLE,TIP('Select This Record')
                         BUTTON,AT(134,1,16,14),USE(?TBarBrwInsert, TBarBrwInsert),ICON('INSERT.ICO'),DISABLE,TIP('Insert a New Record')
                         BUTTON,AT(150,1,16,14),USE(?TBarBrwChange, TBarBrwChange),ICON('EDIT.ICO'),DISABLE,TIP('Edit This Record')
                         BUTTON,AT(166,1,16,14),USE(?TBarBrwDelete, TBarBrwDelete),ICON('DELETE.ICO'),DISABLE,TIP('Delete This Record')
                         BUTTON,AT(186,1,16,14),USE(?TbarBrwHistory, TBarBrwHistory),ICON('DITTO.ICO'),DISABLE,TIP('Previous value')
                         BUTTON,AT(202,1,16,14),USE(?TbarBrwHelp, TBarBrwHelp),ICON('HELP.ICO'),DISABLE,TIP('Get Help')
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
   Disable(?ABCBrowseStudents,?ABCReportMenu)
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
      SplashProcedureThread = START(SplashIt)
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
      IF SplashProcedureThread
        IF EVENT() = Event:Accepted
          POST(Event:CloseWindow,,SplashProcedureThread)
          SplashPRocedureThread = 0
        END
      END
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
    OF ?BrowseClassTree
      START(ClassTree, 25000)
    OF ?BrowseStudentsTree
      START(StudentTree, 25000)
    OF ?BrowseStudents
      START(BrowseStudents, 050000)
    OF ?BrowseTeachers
      START(BrowseTeachers, 050000)
    OF ?BrowseClasses
      START(BrowseClasses, 050000)
    OF ?BrowseEnrollment
      START(BrowseEnrollment, 050000)
    OF ?BrowseCourses
      START(BrowseCourses, 050000)
    OF ?BrowseMajors
      START(BrowseMajors, 050000)
    OF ?AbcInitItem
      Abc_Init_Kill()
      Disable(?AbcInitItem)
      Enable(?ABCBrowseStudents,?ABCReportMenu)
    OF ?ABCBrowseStudents
      START(AbcBrowseStudents, 25000)
    OF ?ABCBrowseTeachers
      START(ABCBrowseTeachers, 25000)
    OF ?ABCBrowseClasses
      START(ABCBrowseClasses, 25000)
    OF ?ABCBrowseEnrollment
      START(ABCBrowseEnrollment, 25000)
    OF ?ABCBrowseCourses
      START(ABCBrowseCourses, 25000)
    OF ?ABCBrowseMajors
      START(ABCBrowseMajors, 25000)
    OF ?ABCBrowseClassTree
      START(ABCClassTree, 25000)
    OF ?ABCBrowseStudentsTree
      START(ABCStudentTree, 25000)
    OF ?ABCBrowseUpdateGrades
      START(UpdateGrades, 25000)
    OF ?ABCPrintCOU:KeyDescription
      START(AbcCourseEnrollment, 25000)
    OF ?PrintENR:StuSeq
      START(ClassSchedules1, 050000)
    OF ?ReportsTeacherClassSchedules
      START(ClassSchedules2, 25000)
    OF ?PrintENR:SeqStu
      START(AttendanceSheets, 050000)
    OF ?PrintCOU:KeyDescription
      START(CourseEnrollment, 050000)
    OF ?ReportsCourseEnrollmentSummary
      START(EnrollSummary, 25000)
    OF ?ReportsFinalGrades
      START(FinalGrades, 25000)
    OF ?ReportsStudentIDs
      START(StudentIDs, 25000)
    OF ?HelpAbout
      START(SplashIt2, 25000)
    OF ?CbHlpHookEnableItem
        DO CbHlpHookEnableRtn      
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
CbHlpHookEnableRtn  ROUTINE     !<><><><><><><> CB Window Preview Hook <><><><><><>
    IF Help2WndPreviewCls &= NULL THEN   
       Help2WndPreviewCls &= NEW(CbWndPrvHelpHookClass) 
       IF ~Help2WndPreviewCls.IsInited THEN
          RV# = Help2WndPreviewCls.Init('YourHelp.CHM')  !Help file does NOT need to exist, but need a Name
          IF RV# THEN 
             Message('Help2WndPreviewCls failed reason ' & RV# )
             DISABLE(?) 
             EXIT
          END
       END
    END    
!    Message('Press Ctrl+Shift+F1 on Open Windows to see CB Window Preview Reflection List.' & |
!            '||Note: Any windows open now must be closed to work.', | 
!            'CbWndPrvHelpHookClass.Init()')

    Message('Press Ctrl+Shift+F1 on Open Windows to see CB Window Preview Reflection List.' & |
            '||Note: Any windows open now must be closed to work.' & | 
            '||If you compiled in 11.13505 you can view List Queues, before that you need a Global Extension.' & |
             '||On any window with a LIST presss Ctrl+Shift+F1 to open CB Window Preview. ' & |
             '|On the CB wInspect Controls window press the LIST button' & |
             '|then press the From(Q) button and you can see the QUEUE that feeds the list.' & |
             '|Press the "View From(Q)" to see all the data.','CbWndPrvHelpHookClass.Init()')
    
    !Is declared as Ref so no chance affects live APP until it is NEW() on the secret menu
    !Help2WndPreviewCls  &CbWndPrvHelpHookClass 
!!! <summary>
!!! Generated from procedure template - Splash
!!! </summary>
SplashIt PROCEDURE

LocalRequest         LONG                                  ! 
OriginalRequest      LONG                                  ! 
LocalResponse        LONG                                  ! 
FilesOpened          LONG                                  ! 
WindowOpened         LONG                                  ! 
WindowInitialized    LONG                                  ! 
ForceRefresh         LONG                                  ! 
window               WINDOW,AT(,,209,189),FONT('MS Sans Serif',8,,FONT:regular),NOFRAME,CENTER,GRAY,MDI,PALETTE(256)
                       PANEL,AT(0,0,209,189),BEVEL(6)
                       PANEL,AT(7,6,195,175),BEVEL(-2,1)
                       STRING('TopSpeed University'),AT(11,12,185,20),USE(?String2),FONT('Arial',18,COLOR:Red,FONT:bold+FONT:italic), |
  CENTER
                       IMAGE('TPSPD-U.BMP'),AT(15,68,178,106),USE(?Image1)
                       PANEL,AT(12,30,182,12),BEVEL(-1,1,9)
                       STRING('Written using Clarion for Windows 2.0'),AT(13,54,182,10),USE(?String1),CENTER
                       PANEL,AT(12,66,185,111),USE(?Panel4),BEVEL(2)
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
  ACCEPT
    CASE EVENT()
    OF EVENT:AlertKey
      CASE KEYCODE()
      OF MouseLeft
      OROF MouseLeft2
      OROF MouseRight
        POST(Event:CloseWindow)
      END
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
      SELECT(?String2)
    OF EVENT:Sized
      POST(EVENT:DoResize,0,THREAD())
    OF Event:Rejected
      BEEP
      DISPLAY(?)
      SELECT(?)
    ELSE
      IF EVENT() = Event:Timer
        POST(Event:CloseWindow)
      END
      IF EVENT() = Event:AlertKey
        CASE KEYCODE()
        OF MouseLeft
        OROF MouseLeft2
        OROF MouseRight
          POST(Event:CloseWindow)
        END
      END
    END
  END
  DO ProcedureReturn
!---------------------------------------------------------------------------
PrepareProcedure ROUTINE
  FilesOpened = TRUE
  DO BindFields
  OPEN(window)
  WindowOpened=True
  TARGET{Prop:Timer} = 1000
  TARGET{Prop:Alrt,255} = MouseLeft
  TARGET{Prop:Alrt,254} = MouseLeft2
  TARGET{Prop:Alrt,253} = MouseRight
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
    CLOSE(window)
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
  IF window{Prop:AcceptAll} THEN EXIT.
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
!!! <summary>
!!! Generated from procedure template - Splash
!!! </summary>
SplashIt2 PROCEDURE

LocalRequest         LONG                                  ! 
OriginalRequest      LONG                                  ! 
LocalResponse        LONG                                  ! 
FilesOpened          LONG                                  ! 
WindowOpened         LONG                                  ! 
WindowInitialized    LONG                                  ! 
ForceRefresh         LONG                                  ! 
BioText              STRING(2000)                          ! 
window               WINDOW,AT(,,353,226),FONT('MS Sans Serif',8,COLOR:Black,FONT:regular),NOFRAME,CENTER,GRAY, |
  MDI,PALETTE(256)
                       PANEL,AT(0,-1,355,227),BEVEL(6)
                       PANEL,AT(7,5,147,214),BEVEL(4,3)
                       IMAGE('RICHARDT.BMP'),AT(12,9,137,206),USE(?Image1)
                       STRING('TopSpeed University'),AT(165,4),USE(?String4),FONT(,18,COLOR:Red,FONT:bold+FONT:italic)
                       STRING('Written by'),AT(176,21,143,12),USE(?String2),FONT(,11,COLOR:Blue),CENTER,TRN
                       STRING('Richard Taylor'),AT(176,31,143,16),USE(?String2:2),FONT(,14,COLOR:Blue),CENTER,TRN
                       STRING('Director, Technical Communications'),AT(176,45,143,10),USE(?String1),FONT(,10,COLOR:Blue), |
  CENTER
                       TEXT,AT(159,62,185,156),USE(BioText),VSCROLL,COLOR(COLOR:BTNFACE),READONLY,SKIP
                     END
  CODE
  PUSHBIND
  LocalRequest    = GlobalRequest
  OriginalRequest = GlobalRequest
  LocalResponse   = RequestCancelled
  ForceRefresh    = False
  CLEAR(GlobalRequest)
  CLEAR(GlobalResponse)
  BioText = 'Richard''s background is, in a word, "eclectic." His Bachelors degree is in Computer Information Science, and he also holds a Masters degree in Counseling Psychology, and a PhD in Humanities. ' |
      & 'His collegiate work all came after a successful career as a musician and actor. '  |
      & '<13,10,13,10>Richard started at Clarion Software (now TopSpeed Corp.) in the Technical Support department, moved to in-house MIS programming, then transferred ' |
      & 'to the Education Department, where he taught advanced Clarion language classes.'  |
      & '<13,10,13,10>During his stint in MIS and Education he wrote his first book: "Tips, Tricks, and Techniques for Clarion Professional Developer" (version 2.1) published by PC Information Group, and was subsequently asked to take over Clarion''s Documentation. ' |
      & '<13,10,13,10>Since moving to Documentation, Richard has also found time to write "Mastering Clarion Database Developer: Understanding Clarion Templates" and co-author "Tips, Tricks, and Templates for CDD" -- both published by PCIG. ' |
      & 'He was also the Technical Reviewer of "Clarion for Windows for Dummies" by Tom Moseley and Jim DeFabia, published by IDG Books.'
  IF KEYCODE() = MouseRight
    SETKEYCODE(0)
  END
  DO PrepareProcedure
  ACCEPT
    CASE EVENT()
    OF EVENT:AlertKey
      CASE KEYCODE()
      OF MouseLeft
      OROF MouseLeft2
      OROF MouseRight
        POST(Event:CloseWindow)
      END
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
      SELECT(?Image1)
    OF EVENT:Sized
      POST(EVENT:DoResize,0,THREAD())
    OF Event:Rejected
      BEEP
      DISPLAY(?)
      SELECT(?)
    ELSE
      IF EVENT() = Event:Timer
        POST(Event:CloseWindow)
      END
      IF EVENT() = Event:AlertKey
        CASE KEYCODE()
        OF MouseLeft
        OROF MouseLeft2
        OROF MouseRight
          POST(Event:CloseWindow)
        END
      END
    END
  END
  DO ProcedureReturn
!---------------------------------------------------------------------------
PrepareProcedure ROUTINE
  FilesOpened = TRUE
  DO BindFields
  OPEN(window)
  WindowOpened=True
  TARGET{Prop:Timer} = 0
  TARGET{Prop:Alrt,255} = MouseLeft
  TARGET{Prop:Alrt,254} = MouseLeft2
  TARGET{Prop:Alrt,253} = MouseRight
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
    CLOSE(window)
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
  IF window{Prop:AcceptAll} THEN EXIT.
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
