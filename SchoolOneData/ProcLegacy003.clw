

                     MEMBER('ProcLegacy.clw')              ! This is a MEMBER module

!!! <summary>
!!! Generated from procedure template - Form
!!! Update the Students File
!!! </summary>
UpdateStudents PROCEDURE

CurrentTab           STRING(80)                            ! 
LocalRequest         LONG                                  ! 
OriginalRequest      LONG                                  ! 
LocalResponse        LONG                                  ! 
FilesOpened          LONG                                  ! 
WindowOpened         LONG                                  ! 
WindowInitialized    LONG                                  ! 
ForceRefresh         LONG                                  ! 
RecordFiltered       LONG                                  ! 
ActionMessage        CSTRING(40)                           ! 
RecordChanged        BYTE,AUTO                             ! 
DOSDialogHeader      CSTRING(40)                           ! 
DOSExtParameter      CSTRING(250)                          ! 
DOSTargetVariable    CSTRING(256)                          ! 
SelectMajorsThread   LONG                                  ! 
PhotoChanged         BYTE                                  ! 

BRW2::View:Browse    VIEW(Enrollment)
                       PROJECT(ENR:ClassNumber)
                       PROJECT(ENR:MidtermExam)
                       PROJECT(ENR:FinalExam)
                       PROJECT(ENR:TermPaper)
                       PROJECT(ENR:StudentNumber)
                       JOIN(CLA:KeyClassNumber,ENR:ClassNumber)
                         PROJECT(CLA:ScheduledTime)
                         PROJECT(CLA:ClassNumber)
                            PROJECT(CLA:CourseNumber)
                            JOIN(COU:KeyNumber,CLA:CourseNumber)
                              PROJECT(COU:Description)
                              PROJECT(COU:Number)
                            END
                       END
                     END
Queue:Browse:2       QUEUE,PRE()                           ! Browsing Queue
BRW2::ENR:ClassNumber  LIKE(ENR:ClassNumber)               ! Queue Display field
BRW2::CLA:ScheduledTime LIKE(CLA:ScheduledTime)            ! Queue Display field
BRW2::COU:Description  LIKE(COU:Description)               ! Queue Display field
BRW2::ENR:MidtermExam  LIKE(ENR:MidtermExam)               ! Queue Display field
BRW2::ENR:FinalExam    LIKE(ENR:FinalExam)                 ! Queue Display field
BRW2::ENR:TermPaper    LIKE(ENR:TermPaper)                 ! Queue Display field
BRW2::ENR:StudentNumber LIKE(ENR:StudentNumber)            ! Queue Display field
BRW2::CLA:ClassNumber  LIKE(CLA:ClassNumber)               ! Queue Display field
BRW2::COU:Number       LIKE(COU:Number)                    ! Queue Display field
BRW2::Mark             BYTE                                ! Record mark flag
BRW2::Position         STRING(1024)                        ! Queue POSITION information
                     END                                   ! END (Browsing Queue)
BRW2::UsingAdditionalSortOrder BYTE                        ! When true the view is using PROP:Order
BRW2::CurrentScroll  BYTE                                  ! Queue position of scroll thumb
BRW2::ScrollRecordCount LONG                               ! Queue position of scroll thumb
BRW2::SkipFirst      BYTE                                  ! Skip first retrieved record in page fill
BRW2::Sort1:KeyDistribution LIKE(ENR:ClassNumber),DIM(100)
BRW2::Sort1:LowValue LIKE(ENR:ClassNumber)                 ! Queue position of scroll thumb
BRW2::Sort1:HighValue LIKE(ENR:ClassNumber)                ! Queue position of scroll thumb
BRW2::Sort1:Reset:STU:Number LIKE(STU:Number)
BRW2::CurrentEvent   LONG                                  !
BRW2::CurrentChoice  LONG                                  !
BRW2::RecordCount    LONG                                  !
BRW2::SortOrder      BYTE                                  !
BRW2::LocateMode     BYTE                                  !
BRW2::RefreshMode    BYTE                                  !
BRW2::LastSortOrder  BYTE                                  !
BRW2::FillDirection  BYTE                                  !
BRW2::AddQueue       BYTE                                  !
BRW2::Changed        BYTE                                  !
BRW2::RecordStatus   BYTE                                  ! Flag for Range/Filter test
BRW2::ItemsToFill    LONG                                  ! Controls records retrieved
BRW2::MaxItemsInList LONG                                  ! Retrieved after window opened
BRW2::HighlightedPosition STRING(1024)                     ! POSITION of located record
BRW2::NewSelectPosted BYTE                                 ! Queue position of located record
BRW2::PopupText      CSTRING(10000)                        !
BRW2::ActiveInvisible BYTE(1)                              !
BRW2::LoadPending    BYTE                                  !
ToolBarMode          UNSIGNED,AUTO
BrowseButtons        GROUP                      !info for current browse with focus
ListBox                SIGNED                   !Browse list control
InsertButton           SIGNED                   !Browse insert button
ChangeButton           SIGNED                   !Browse change button
DeleteButton           SIGNED                   !Browse delete button
SelectButton           SIGNED                   !Browse select button
                     END
Update::Reloop  BYTE
Update::Error   BYTE
History::STU:Record LIKE(STU:Record),STATIC
SAV::STU:Record      LIKE(STU:Record)
Save:LastInsertedPosition STRING(512)
WindowXPos      SIGNED,AUTO,STATIC
WindowYPos      SIGNED,AUTO,STATIC
WindowPosInit   BOOL(False),STATIC
WinResize            WindowResizeType
QuickWindow          WINDOW('Update the Students File'),AT(,,160,204),FONT('MS Sans Serif',8,COLOR:Black),RESIZE, |
  GRAY,IMM,MDI,HLP('~UpdateStudents'),PALETTE(256),SYSTEM
                       SHEET,AT(3,4,151,173),USE(?CurrentTab)
                         TAB('General')
                           PROMPT('&First Name:'),AT(7,20),USE(?STU:FirstName:Prompt)
                           ENTRY(@S20),AT(47,20,84,10),USE(STU:FirstName)
                           PROMPT('&Last Name:'),AT(7,34),USE(?STU:LastName:Prompt)
                           ENTRY(@S20),AT(47,34,84,10),USE(STU:LastName)
                           PROMPT('&Address:'),AT(7,48),USE(?STU:Address:Prompt)
                           ENTRY(@S20),AT(47,48,84,10),USE(STU:Address)
                           PROMPT('Address 2:'),AT(7,62),USE(?STU:Address2:Prompt)
                           ENTRY(@s20),AT(47,62,84,10),USE(STU:Address2)
                           PROMPT('&City:'),AT(7,76),USE(?STU:City:Prompt)
                           ENTRY(@S20),AT(47,76,84,10),USE(STU:City)
                           PROMPT('&State:'),AT(7,90),USE(?STU:State:Prompt)
                           ENTRY(@S2),AT(47,90,40,10),USE(STU:State)
                           PROMPT('&Zip:'),AT(7,104),USE(?STU:Zip:Prompt)
                           ENTRY(@n05),AT(47,104,40,10),USE(STU:Zip)
                           PROMPT('&Telephone:'),AT(7,118),USE(?STU:Telephone:Prompt)
                           ENTRY(@s12),AT(47,118,52,10),USE(STU:Telephone)
                           PROMPT('Grad Year:'),AT(7,132),USE(?STU:GradYear:Prompt)
                           ENTRY(@n4),AT(47,132,40,10),USE(STU:GradYear)
                           PROMPT('&Number:'),AT(7,146),USE(?STU:Number:Prompt)
                           ENTRY(@P###-##-####P),AT(47,146,,10),USE(STU:Number),RIGHT(1),REQ
                           STRING(@s20),AT(74,160,77,10),USE(MAJ:Description)
                           PROMPT('Major:'),AT(7,160),USE(?STU:Major:Prompt)
                           ENTRY(@n4),AT(47,160,13,10),USE(STU:Major),ALRT(F10Key),DROPID('Majors')
                           BUTTON('...'),AT(61,159,12,12),USE(?Button8)
                         END
                         TAB('Enrollment'),USE(?EnrollmentTab)
                           LIST,AT(7,22,143,148),USE(?Browse:2),HVSCROLL,FORMAT('[52L(2)|M~Class Number~@p##-#####' & |
  'p@80L(2)|M~Scheduled Time~@s20@/120L(2)|M~Description~@S30@]|M~Class~[52R(2)|M~Midte' & |
  'rm Exam~C(0)@n3@44R(2)|M~Final Exam~C(0)@n3@44R(2)|M~Term Paper~C(0)@n3@]|M~Grades~'),FROM(Queue:Browse:2), |
  IMM,MSG('Browsing Records')
                           BUTTON('&Insert'),AT(7,128,45,14),USE(?Insert:3),HIDE
                           BUTTON('&Change'),AT(57,128,45,14),USE(?Change:3),HIDE
                           BUTTON('&Delete'),AT(105,128,45,14),USE(?Delete:3),HIDE
                         END
                         TAB('Photo'),USE(?Tab3)
                           IMAGE,AT(37,21,80,120),USE(?Image1)
                           ENTRY(@s64),AT(7,158,129,12),USE(GLO:FileName)
                           PROMPT('Image File Name:'),AT(7,147),USE(?GLO:FileName:Prompt)
                           BUTTON('...'),AT(137,158,12,12),USE(?LookupFile)
                         END
                       END
                       BUTTON('OK'),AT(12,185,45,14),USE(?OK),DEFAULT
                       BUTTON('Cancel'),AT(61,185,45,14),USE(?Cancel)
                       BUTTON('Help'),AT(110,185,45,14),USE(?Help),STD(STD:Help)
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
  IF LocalRequest = InsertRecord
    ?EnrollmentTab{PROP:Hide} = TRUE
  END
  CASE LocalRequest
  OF ViewRecord
    ActionMessage = 'View Record'
  OF InsertRecord
    CLEAR(Save:LastInsertedPosition)
    ActionMessage = 'Adding a Students Record'
  OF ChangeRecord
    ActionMessage = 'Changing a Students Record'
  OF DeleteRecord
  END
  QuickWindow{PROP:Text} = ActionMessage
  IF LocalRequest <> ViewRecord THEN
    ENABLE(TBarBrwHistory)
  END
  ACCEPT
    CASE EVENT()
    OF EVENT:AlertKey
      IF KEYCODE() = 734 AND LocalRequest <> ViewRecord THEN
        DO HistoryField
      END
    OF EVENT:CloseDown
      DO ClosingWindow
      IF Update::Reloop THEN CYCLE.
      WinResize.Kill()
    OF EVENT:CloseWindow
      !Close drag toolbox
      IF SelectMajorsThread
        POST(EVENT:CloseWindow,,SelectMajorsThread)
      END
      DO ClosingWindow
      IF Update::Reloop THEN CYCLE.
      WinResize.Kill()
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
    OF EVENT:Moved
      GETPOSITION(0,WindowXPos,WindowYPos)
    OF EVENT:OpenWindow
      DO BRW2::AssignButtons
      DO FORM::AssignButtons
      IF NOT WindowInitialized
        DO InitializeWindow
        WindowInitialized = True
      END
      SELECT(?STU:FirstName:Prompt)
    OF EVENT:Sized
      POST(EVENT:DoResize,0,THREAD())
    OF Event:Rejected
      BEEP
      DISPLAY(?)
      SELECT(?)
    ELSE
      IF ToolBarMode=BrowseMode THEN
        DO ListBoxDispatch
      END
      IF ToolBarMode = BrowseMode THEN
        DO UpdateDispatch
      END
      IF LocalRequest <> ViewRecord THEN
        IF ACCEPTED() = TbarBrwHistory
          DO HistoryField
        END
      END
      IF EVENT() = EVENT:Completed
        !Update Photo BLOB on disk
        STU:Photograph{PROP:size} = 0
        STU:Photograph{PROP:Handle} = ?Image1{PROP:ImageBlob}
        History::STU:Record = STU:Record
        CASE LocalRequest
        OF InsertRecord
          ADD(Students)
          CASE ERRORCODE()
          OF NoError
            IF StandardWarning(Warn:NewRecordAdded) = Button:Yes
              Save:LastInsertedPosition = POSITION(Students)
              DO PrimeFields
              DISPLAY
              SELECT(?STU:FirstName:Prompt)
              CYCLE
            ELSE
              LocalResponse = RequestCompleted
              POST(EVENT:CloseWindow)
            END
          OF DupKeyErr
            IF DUPLICATE(STU:KeyStudentNumber)
              IF StandardWarning(Warn:DuplicateKey,'STU:KeyStudentNumber')
                SELECT(?STU:FirstName:Prompt)
                VCRRequest = VCRNone
                CYCLE
              END
            END
          ELSE
            IF StandardWarning(Warn:InsertError)
              SELECT(?STU:FirstName:Prompt)
              VCRRequest = VCRNone
              CYCLE
            END
          END
        OF ChangeRecord
          LOOP
            LocalResponse = RequestCancelled
            SETCURSOR(CURSOR:Wait)
            RecordChanged = FALSE
            IF (SAV::STU:Record <> STU:Record)  OR STU:Photograph{PROP:Touched} = True
               RecordChanged = TRUE
            END
            IF PhotoChanged
              RecordChanged = TRUE
            END
            IF RecordChanged THEN
              Update::Error = RIUpdate:Students(1)
            ELSE
              Update::Error = 0
            END
            SETCURSOR()
            IF Update::Error THEN
              IF Update::Error = 1 THEN
                CASE StandardWarning(Warn:UpdateError)
                OF Button:Yes
                  CYCLE
                OF Button:No
                  POST(EVENT:CloseWindow)
                  BREAK
                END
              END
              DISPLAY
              SELECT(?STU:FirstName:Prompt)
              VCRRequest = VCRNone
            ELSE
              IF RecordChanged OR VCRRequest = VCRNone THEN
                LocalResponse = RequestCompleted
              END
              POST(EVENT:CloseWindow)
            END
            BREAK
          END
        END
      END
      IF ToolbarMode = FormMode THEN
        CASE ACCEPTED()
        OF TBarBrwBottom TO TBarBrwUp
        OROF TBarBrwInsert
          VCRRequest=ACCEPTED()
          POST(EVENT:Completed)
        OF TBarBrwHelp
          PRESSKEY(F1Key)
        END
      END
    END
    CASE FIELD()
    OF ?CurrentTab
      CASE EVENT()
      OF EVENT:Accepted
        DO RefreshWindow
      OF EVENT:Selected
        DO RefreshWindow
      OF EVENT:Selecting
        DO RefreshWindow
      OF EVENT:NewSelection
          !Code to assign button control based upon current tab selection
          CASE CHOICE(?CurrentTab)
          OF 1
            DO FORM::AssignButtons
          OF 2
            DO BRW2::AssignButtons
          END
        DO RefreshWindow
      OF EVENT:TabChanging
        DO RefreshWindow
      END
    OF ?STU:Major
      CASE EVENT()
      OF EVENT:Accepted
        MAJ:Number = STU:Major
        GET(Majors,MAJ:KeyNumber)
        IF ERRORCODE()
          IF StandardWarning(Warn:NotInFile,'STU:Major','Majors')
            SELECT(?STU:Major)
            QuickWindow{PROP:AcceptAll} = False
            CYCLE
          END
        END
      OF EVENT:Selected
        !Call drag toolbox
        IF OriginalRequest = InsertRecord AND NOT STU:Major
          GLO:DropThread = THREAD()
          GLO:DropControl = ?STU:Major
          SelectMajorsThread = START(SelectMajors)
          GLO:ThreadRef &= SelectMajorsThread
        END
      OF EVENT:AlertKey
        !Call drag toolbox
        IF NOT SelectMajorsThread
          GLO:DropThread = THREAD()
          GLO:DropControl = ?STU:Major
          SelectMajorsThread = START(SelectMajors)
          GLO:ThreadRef &= SelectMajorsThread
        END
      OF EVENT:Drop
        !Receive dropped data and close drag toolbox
        STU:Major = DROPID()
        DISPLAY
        POST(EVENT:CloseWindow,,SelectMajorsThread)
        SelectMajorsThread = 0
        DO RefreshWindow
        PRESSKEY(TabKey)
      END
    OF ?Button8
      CASE EVENT()
      OF EVENT:Accepted
        !Call drag toolbox
        POST(EVENT:AlertKey,?STU:Major)
        DO SyncWindow
      END
    OF ?Browse:2
      CASE EVENT()
      OF EVENT:NewSelection
        DO BRW2::NewSelection
      OF EVENT:ScrollUp
        DO BRW2::ProcessScroll
      OF EVENT:ScrollDown
        DO BRW2::ProcessScroll
      OF EVENT:PageUp
        DO BRW2::ProcessScroll
      OF EVENT:PageDown
        DO BRW2::ProcessScroll
      OF EVENT:ScrollTop
        DO BRW2::ProcessScroll
      OF EVENT:ScrollBottom
        DO BRW2::ProcessScroll
      OF EVENT:ScrollDrag
        DO BRW2::ScrollDrag
      OF EVENT:AlertKey
        DO BRW2::AlertKey
      END
    OF ?Insert:3
      CASE EVENT()
      OF EVENT:Accepted
        DO SyncWindow
        DO BRW2::ButtonInsert
      END
    OF ?Change:3
      CASE EVENT()
      OF EVENT:Accepted
        DO SyncWindow
        DO BRW2::ButtonChange
      END
    OF ?Delete:3
      CASE EVENT()
      OF EVENT:Accepted
        DO SyncWindow
        DO BRW2::ButtonDelete
      END
    OF ?GLO:FileName
      CASE EVENT()
      OF EVENT:Accepted
        !Display photo from selected disk file 
        IF NOT TARGET{PROP:AcceptAll}
          ?Image1{PROP:Text} = GLO:FileName
        END
        PhotoChanged = TRUE
      END
    OF ?LookupFile
      CASE EVENT()
      OF EVENT:Accepted
        DO SyncWindow
        DOSTargetVariable = GLO:FileName
        
        DOSDialogHeader = 'Choose a File'
        DOSExtParameter = 'Image Files|*.GIF;*.BMP;*.JPG;*.PCX|All Files|*.*'
        IF FILEDIALOG(DOSDialogHeader,DOSTargetVariable,DOSExtParameter,FILE:KeepDIR+FILE:LongName)
          GLO:FileName = DOSTargetVariable
          DO RefreshWindow
        END
        !Display photo from selected disk file 
        IF NOT TARGET{PROP:AcceptAll}
          ?Image1{PROP:Text} = GLO:FileName
        END
        PhotoChanged = TRUE
      END
    OF ?OK
      CASE EVENT()
      OF EVENT:Accepted
        DO SyncWindow
        IF LocalRequest <> ViewRecord THEN
          IF OriginalRequest = ChangeRecord OR OriginalRequest = InsertRecord
            SELECT()
          ELSE
            POST(EVENT:Completed)
          END
        END
      END
    OF ?Cancel
      CASE EVENT()
      OF EVENT:Accepted
        DO SyncWindow
        LocalResponse = RequestCancelled
        VCRRequest = VCRNone
        POST(EVENT:CloseWindow)
      END
    END
  END
  DO ProcedureReturn
!---------------------------------------------------------------------------
PrepareProcedure ROUTINE
  IF Classes::Used = 0
    CheckOpen(Classes,1)
  END
  Classes::Used += 1
  IF Courses::Used = 0
    CheckOpen(Courses,1)
  END
  Courses::Used += 1
  IF Enrollment::Used = 0
    CheckOpen(Enrollment,1)
  END
  Enrollment::Used += 1
  IF Majors::Used = 0
    CheckOpen(Majors,1)
  END
  Majors::Used += 1
  IF Students::Used = 0
    CheckOpen(Students,1)
  END
  Students::Used += 1
  FilesOpened = TRUE
  DO BindFields
  RISnap:Students
  SAV::STU:Record = STU:Record
  IF LocalRequest = InsertRecord
    LocalResponse = RequestCompleted
    DO PrimeFields
    IF LocalResponse = RequestCancelled
      DO ProcedureReturn
    END
    LocalResponse = RequestCancelled
  END
  IF LocalRequest = DeleteRecord
    IF StandardWarning(Warn:StandardDelete) = Button:OK
      LOOP
        LocalResponse = RequestCancelled
        SETCURSOR(CURSOR:Wait)
        IF RIDelete:Students()
          SETCURSOR()
          CASE StandardWarning(Warn:DeleteError)
          OF Button:Yes
            CYCLE
          OF Button:No OROF Button:Cancel
            BREAK
          END
        ELSE
          SETCURSOR()
          LocalResponse = RequestCompleted
        END
        BREAK
      END
    END
    DO ProcedureReturn
  END
  OPEN(QuickWindow)
  WindowOpened=True
  !Display photo from stored BLOB
  IF OriginalRequest <> InsertRecord
    ?Image1{PROP:ImageBlob} = STU:Photograph{PROP:Handle}
  ELSE
    ?Image1{PROP:Text} = 'NoPhoto.BMP'
  END
  GLO:FileName = ''
  BRW2::AddQueue = True
  BRW2::RecordCount = 0
  IF WindowPosInit THEN
    SETPOSITION(0,WindowXPos,WindowYPos)
  ELSE
    GETPOSITION(0,WindowXPos,WindowYPos)
    WindowPosInit=True
  END
  QuickWindow{PROP:MinWidth}=160                           ! Restrict the minimum window width
  QuickWindow{PROP:MinHeight}=204                          ! Restrict the minimum window height
  WinResize.Init(AppStrategy:Spread)
  Do DefineListboxStyle
  ?Browse:2{PROP:Alrt,252} = MouseLeft2
  ?Browse:2{PROP:Alrt,255} = AppsKey
  ?Browse:2{PROP:Alrt,253} = MouseRight
  ?Browse:2{PROP:Alrt,255} = InsertKey
  ?Browse:2{PROP:Alrt,254} = DeleteKey
  ?Browse:2{PROP:Alrt,253} = CtrlEnter
  ?Browse:2{PROP:Alrt,252} = MouseLeft2
  ?STU:FirstName{PROP:Alrt,255} = 734
  ?STU:LastName{PROP:Alrt,255} = 734
  ?STU:Address{PROP:Alrt,255} = 734
  ?STU:Address2{PROP:Alrt,255} = 734
  ?STU:City{PROP:Alrt,255} = 734
  ?STU:State{PROP:Alrt,255} = 734
  ?STU:Zip{PROP:Alrt,255} = 734
  ?STU:Telephone{PROP:Alrt,255} = 734
  ?STU:GradYear{PROP:Alrt,255} = 734
  ?STU:Number{PROP:Alrt,255} = 734
  ?STU:Major{PROP:Alrt,255} = 734

!---------------------------------------------------------------------------
BindFields ROUTINE
  BIND(CLA:RECORD)
  BIND(COU:RECORD)
  BIND('COU:CompleteDescription',COU:CompleteDescription)
  BIND(ENR:RECORD)
  BIND(MAJ:RECORD)
  BIND(STU:RECORD)
  BIND('BRW2::Sort1:Reset:STU:Number',BRW2::Sort1:Reset:STU:Number) ! Added by: BrowseBox(Clarion)
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
    Classes::Used -= 1
    IF Classes::Used = 0 THEN CLOSE(Classes).
    Courses::Used -= 1
    IF Courses::Used = 0 THEN CLOSE(Courses).
    Enrollment::Used -= 1
    IF Enrollment::Used = 0 THEN CLOSE(Enrollment).
    Majors::Used -= 1
    IF Majors::Used = 0 THEN CLOSE(Majors).
    Students::Used -= 1
    IF Students::Used = 0 THEN CLOSE(Students).
  END
  IF WindowOpened
    CLOSE(QuickWindow)
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
  IF QuickWindow{Prop:AcceptAll} THEN EXIT.
  MAJ:Number = STU:Major                                   ! Assign linking field value
  GET(Majors,MAJ:KeyNumber)                                ! Lookup record
  IF ERRORCODE()
    CLEAR(MAJ:Record)                                      ! Clear record if unsuccessful
  END
  Do LookupRelated
  DO BRW2::SelectSort
  ?Browse:2{Prop:VScrollPos} = BRW2::CurrentScroll
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
  DO BRW2::GetRecord
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
BRW2::AssignButtons ROUTINE
  CLEAR(BrowseButtons)
  BrowseButtons.ListBox = ?Browse:2
  BrowseButtons.InsertButton = ?Insert:3
  BrowseButtons.ChangeButton = ?Change:3
  BrowseButtons.DeleteButton = ?Delete:3
  DO DisplayBrowseToolbar
!----------------------------------------------------------------------
BRW2::SelectSort ROUTINE
!|
!| This routine is called during the RefreshWindow ROUTINE present in every window procedure.
!| The purpose of this routine is to make certain that the BrowseBox is always current with your
!| user's selections. This routine...
!|
!| 1. Checks to see if any of your specified sort-order conditions are met, and if so, changes the sort order.
!| 2. If no sort order change is necessary, this routine checks to see if any of your Reset Fields has changed.
!| 3. If the sort order has changed, or if a reset field has changed, or if the ForceRefresh flag is set...
!|    a. The current record is retrieved from the disk.
!|    b. If the BrowseBox is accessed for the first time, and the Browse has been called to select a record,
!|       the page containing the current record is loaded.
!|    c. If the BrowseBox is accessed for the first time, and the Browse has not been called to select a
!|       record, the first page of information is loaded.
!|    d. If the BrowseBox is not being accessed for the first time, and the Browse sort order has changed, the
!|       new "first" page of information is loaded.
!|    e. If the BrowseBox is not being accessed for the first time, and the Browse sort order hasn't changes,
!|       the page containing the current record is reloaded.
!|    f. The record buffer is refilled from the currently highlighted BrowseBox item.
!|    f. The BrowseBox is reinitialized (BRW2::InitializeBrowse ROUTINE).
!| 4. If step 3 is not necessary, the record buffer is refilled from the currently highlighted BrowseBox item.
!|
  BRW2::LastSortOrder = BRW2::SortOrder
  BRW2::Changed = False
  IF BRW2::SortOrder = 0
    BRW2::SortOrder = 1
  END
  IF BRW2::SortOrder = BRW2::LastSortOrder
    CASE BRW2::SortOrder
    OF 1
      IF BRW2::Sort1:Reset:STU:Number <> STU:Number
        BRW2::Changed = True
      END
    END
  ELSE
  END
  IF BRW2::SortOrder <> BRW2::LastSortOrder OR BRW2::Changed OR ForceRefresh OR (BRW2::LoadPending AND ?Browse:2{PROP:VISIBLE})
    CASE BRW2::SortOrder
    OF 1
      BRW2::Sort1:Reset:STU:Number = STU:Number
    END
    DO BRW2::GetRecord
    DO BRW2::Reset
    IF BRW2::LastSortOrder = 0
      IF LocalRequest = SelectRecord
        BRW2::LocateMode = LocateOnValue
        DO BRW2::LocateRecord
      ELSE
        FREE(Queue:Browse:2)
        BRW2::RefreshMode = RefreshOnTop
        DO BRW2::RefreshPage
        DO BRW2::PostNewSelection
      END
    ELSE
      IF BRW2::Changed
        FREE(Queue:Browse:2)
        BRW2::RefreshMode = RefreshOnTop
        DO BRW2::RefreshPage
        DO BRW2::PostNewSelection
      ELSE
        BRW2::LocateMode = LocateOnValue
        DO BRW2::LocateRecord
      END
    END
    IF BRW2::RecordCount
      GET(Queue:Browse:2,BRW2::CurrentChoice)
      DO BRW2::FillBuffer
    END
    DO BRW2::InitializeBrowse
  ELSE
    IF BRW2::RecordCount
      GET(Queue:Browse:2,BRW2::CurrentChoice)
      DO BRW2::FillBuffer
    END
  END
!----------------------------------------------------------------------
BRW2::InitializeBrowse ROUTINE
!|
!| This routine initializes the BrowseBox control template. This routine is called when...
!|
!| The BrowseBox sort order has changed. This includes the first time the BrowseBox is accessed.
!| The BrowseBox returns from a record update.
!|
!| This routine performs two main functions.
!|   1. Computes all BrowseBox totals. All records that satisfy the current selection criteria
!|      are read, and totals computed. If no totals are present, this section is not generated,
!|      and may not be present in the code below.
!|   2. Calculates any runtime scrollbar positions. Again, if runtime scrollbars are not used,
!|      the code for runtime scrollbar computation will not be present.
!|
  IF NOT BRW2::ActiveInvisible THEN
     IF NOT ?Browse:2{PROP:Visible} THEN
        BRW2::LoadPending = True
        EXIT
     END
  END
  DO BRW2::Reset
  PREVIOUS(BRW2::View:Browse)
  IF ERRORCODE()
    IF ERRORCODE() = BadRecErr
      DO BRW2::RestoreResetValues
    ELSE
      StandardWarning(Warn:RecordFetchError,'Enrollment')
      POST(Event:CloseWindow)
    END
    EXIT
  END
  CASE BRW2::SortOrder
  OF 1
    BRW2::Sort1:HighValue = ENR:ClassNumber
  END
  DO BRW2::Reset
  NEXT(BRW2::View:Browse)
  IF ERRORCODE()
    IF ERRORCODE() = BadRecErr
      DO BRW2::RestoreResetValues
    ELSE
      StandardWarning(Warn:RecordFetchError,'Enrollment')
      POST(Event:CloseWindow)
    END
    EXIT
  END
  CASE BRW2::SortOrder
  OF 1
    BRW2::Sort1:LowValue = ENR:ClassNumber
    SetupRealStops(BRW2::Sort1:LowValue,BRW2::Sort1:HighValue)
    LOOP BRW2::ScrollRecordCount = 1 TO 100
      BRW2::Sort1:KeyDistribution[BRW2::ScrollRecordCount] = NextRealStop()
    END
  END
!----------------------------------------------------------------------
BRW2::FillBuffer ROUTINE
!|
!| This routine fills the record buffer from the BrowseBox queue. This gives the appearance
!| that the record is "fresh" from the disk, without the disk access required.
!|
  ENR:ClassNumber = BRW2::ENR:ClassNumber
  CLA:ScheduledTime = BRW2::CLA:ScheduledTime
  COU:Description = BRW2::COU:Description
  ENR:MidtermExam = BRW2::ENR:MidtermExam
  ENR:FinalExam = BRW2::ENR:FinalExam
  ENR:TermPaper = BRW2::ENR:TermPaper
  ENR:StudentNumber = BRW2::ENR:StudentNumber
  CLA:ClassNumber = BRW2::CLA:ClassNumber
  COU:Number = BRW2::COU:Number
!----------------------------------------------------------------------
BRW2::FillQueue ROUTINE
!|
!| This routine is used to fill the BrowseBox QUEUE from several sources.
!|
!| First, all Format Browse formulae are processed.
!|
!| Next, each field of the BrowseBox is processed. For each field...
!|
!|    The value of the field is placed in the BrowseBox queue.
!|
!| Finally, the POSITION of the current VIEW record is added to the QUEUE
!|
  BRW2::ENR:ClassNumber = ENR:ClassNumber
  BRW2::CLA:ScheduledTime = CLA:ScheduledTime
  BRW2::COU:Description = COU:Description
  BRW2::ENR:MidtermExam = ENR:MidtermExam
  BRW2::ENR:FinalExam = ENR:FinalExam
  BRW2::ENR:TermPaper = ENR:TermPaper
  BRW2::ENR:StudentNumber = ENR:StudentNumber
  BRW2::CLA:ClassNumber = CLA:ClassNumber
  BRW2::COU:Number = COU:Number
  BRW2::Position = POSITION(BRW2::View:Browse)
!----------------------------------------------------------------------
BRW2::PostNewSelection ROUTINE
!|
!| This routine is used to post the NewSelection EVENT to the window. Because we only want this
!| EVENT processed once, and becuase there are several routines that need to initiate a NewSelection
!| EVENT, we keep a flag that tells us if the EVENT is already waiting to be processed. The EVENT is
!| only POSTed if this flag is false.
!|
  IF NOT BRW2::NewSelectPosted
    BRW2::NewSelectPosted = True
    POST(EVENT:NewSelection,?Browse:2)
  END
!----------------------------------------------------------------------
BRW2::NewSelection ROUTINE
!|
!| This routine performs any window bookkeeping necessary when a new record is selected in the
!| BrowseBox.
!| 1. If the new selection is made with the right mouse button, the popup menu (if applicable) is
!|    processed.
!| 2. The current record is retrieved into the buffer using the BRW2::FillBuffer ROUTINE.
!|    After this, the current vertical scrollbar position is computed, and the scrollbar positioned.
!|
  IF NOT BRW2::ActiveInvisible THEN
     IF NOT ?Browse:2{PROP:Visible} THEN
        BRW2::LoadPending = True
        EXIT
     END
  END
  BRW2::NewSelectPosted = False
  IF KEYCODE() = MouseRight OR KEYCODE() = AppsKey
     SETKEYCODE(0)
    BRW2::PopupText = ''
    IF BRW2::RecordCount
      IF BRW2::PopupText
        BRW2::PopupText = '&Insert|&Change|&Delete|-|' & BRW2::PopupText
      ELSE
        BRW2::PopupText = '&Insert|&Change|&Delete'
      END
    ELSE
      IF BRW2::PopupText
        BRW2::PopupText = '&Insert|~&Change|~&Delete|-|' & BRW2::PopupText
      ELSE
        BRW2::PopupText = '&Insert|~&Change|~&Delete'
      END
    END
    EXECUTE(POPUP(BRW2::PopupText))
      POST(EVENT:Accepted,?Insert:3)
      POST(EVENT:Accepted,?Change:3)
      POST(EVENT:Accepted,?Delete:3)
    END
  ELSIF BRW2::RecordCount
    BRW2::CurrentChoice = CHOICE(?Browse:2)
    GET(Queue:Browse:2,BRW2::CurrentChoice)
    DO BRW2::FillBuffer
    IF BRW2::RecordCount = ?Browse:2{PROP:Items}
      IF NOT ?Browse:2{PROP:VScroll}
        ?Browse:2{PROP:VScroll} = True
      END
      CASE BRW2::SortOrder
      OF 1
        LOOP BRW2::CurrentScroll = 1 TO 100
          IF BRW2::Sort1:KeyDistribution[BRW2::CurrentScroll] => ENR:ClassNumber
            IF BRW2::CurrentScroll <= 1
              BRW2::CurrentScroll = 0
            ELSIF BRW2::CurrentScroll = 100
              BRW2::CurrentScroll = 100
            ELSE
            END
            BREAK
          END
        END
      END
    ELSE
      IF ?Browse:2{PROP:VScroll}
        ?Browse:2{PROP:VScroll} = False
      END
    END
    DO RefreshWindow
  END
!---------------------------------------------------------------------
BRW2::ProcessScroll ROUTINE
!|
!| This routine processes any of the six scrolling EVENTs handled by the BrowseBox.
!| If one record is to be scrolled, the ROUTINE BRW2::ScrollOne is called.
!| If a page of records is to be scrolled, the ROUTINE BRW2::ScrollPage is called.
!| If the first or last page is to be displayed, the ROUTINE BRW2::ScrollEnd is called.
!|
!| If an incremental locator is in use, the value of that locator is cleared.
!| Finally, if a Fixed Thumb vertical scroll bar is used, the thumb is positioned.
!|
  IF BRW2::RecordCount
    BRW2::CurrentEvent = EVENT()
    CASE BRW2::CurrentEvent
    OF EVENT:ScrollUp OROF EVENT:ScrollDown
      DO BRW2::ScrollOne
    OF EVENT:PageUp OROF EVENT:PageDown
      DO BRW2::ScrollPage
    OF EVENT:ScrollTop OROF EVENT:ScrollBottom
      DO BRW2::ScrollEnd
    END
    ?Browse:2{PROP:SelStart} = BRW2::CurrentChoice
    DO BRW2::PostNewSelection
  END
!----------------------------------------------------------------------
BRW2::ScrollOne ROUTINE
!|
!| This routine is used to scroll a single record on the BrowseBox. Since the BrowseBox is an IMM
!| listbox, all scrolling must be handled in code. When called, this routine...
!|
!| 1. Sees if scrolling in the intended direction will cause the listbox display to shift. If not,
!|    the routine moves the list box cursor and exits.
!| 2. Calls BRW2::FillRecord to retrieve one record in the direction required.
!|
  IF BRW2::CurrentEvent = EVENT:ScrollUp AND BRW2::CurrentChoice > 1
    BRW2::CurrentChoice -= 1
    EXIT
  ELSIF BRW2::CurrentEvent = EVENT:ScrollDown AND BRW2::CurrentChoice < BRW2::RecordCount
    BRW2::CurrentChoice += 1
    EXIT
  END
  BRW2::ItemsToFill = 1
  BRW2::FillDirection = BRW2::CurrentEvent - 2
  DO BRW2::FillRecord
!----------------------------------------------------------------------
BRW2::ScrollPage ROUTINE
!|
!| This routine is used to scroll a single page of records on the BrowseBox. Since the BrowseBox is
!| an IMM listbox, all scrolling must be handled in code. When called, this routine...
!|
!| 1. Calls BRW2::FillRecord to retrieve one page of records in the direction required.
!| 2. If BRW2::FillRecord doesn't fill a page (BRW2::ItemsToFill > 0), then
!|    the list-box cursor ia shifted.
!|
  BRW2::ItemsToFill = ?Browse:2{PROP:Items}
  BRW2::FillDirection = BRW2::CurrentEvent - 4
  DO BRW2::FillRecord                           ! Fill with next read(s)
  IF BRW2::ItemsToFill
    IF BRW2::CurrentEvent = EVENT:PageUp
      BRW2::CurrentChoice -= BRW2::ItemsToFill
      IF BRW2::CurrentChoice < 1
        BRW2::CurrentChoice = 1
      END
    ELSE
      BRW2::CurrentChoice += BRW2::ItemsToFill
      IF BRW2::CurrentChoice > BRW2::RecordCount
        BRW2::CurrentChoice = BRW2::RecordCount
      END
    END
  END
!----------------------------------------------------------------------
BRW2::ScrollEnd ROUTINE
!|
!| This routine is used to load the first or last page of the displayable set of records.
!| Since the BrowseBox is an IMM listbox, all scrolling must be handled in code. When called,
!| this routine...
!|
!| 1. Resets the BrowseBox VIEW to insure that it reads from the end of the current sort order.
!| 2. Calls BRW2::FillRecord to retrieve one page of records.
!| 3. Selects the record that represents the end of the view. That is, if the first page was loaded,
!|    the first record is highlighted. If the last was loaded, the last record is highlighted.
!|
  FREE(Queue:Browse:2)
  BRW2::RecordCount = 0
  DO BRW2::Reset
  BRW2::ItemsToFill = ?Browse:2{PROP:Items}
  IF BRW2::CurrentEvent = EVENT:ScrollTop
    BRW2::FillDirection = FillForward
  ELSE
    BRW2::FillDirection = FillBackward
  END
  DO BRW2::FillRecord                           ! Fill with next read(s)
  IF BRW2::CurrentEvent = EVENT:ScrollTop
    BRW2::CurrentChoice = 1
  ELSE
    BRW2::CurrentChoice = BRW2::RecordCount
  END
!----------------------------------------------------------------------
BRW2::AlertKey ROUTINE
!|
!| This routine processes any KEYCODEs experienced by the BrowseBox.
!| NOTE: The cursor movement keys are not processed as KEYCODEs. They are processed as the
!|       appropriate BrowseBox scrolling and selection EVENTs.
!| This routine includes handling for double-click. Actually, this handling is in the form of
!| EMBEDs, which are filled by child-control templates.
!| This routine also includes the BrowseBox's locator handling.
!| After a value is entered for locating, this routine sets BRW2::LocateMode to a value
!| of 2 -- EQUATEd to LocateOnValue -- and calls the routine BRW2::LocateRecord.
!|
  IF BRW2::RecordCount
    CASE KEYCODE()                                         ! What keycode was hit
    OF AppsKey
    OROF MouseRight
       IF ?Browse:2{PROPLIST:MouseDownRow}>0
         ?Browse:2{PROP:Selected} = ?Browse:2{PROPLIST:MouseDownRow}
         BRW2::CurrentChoice = CHOICE(?Browse:2)
       END
       DO BRW2::NewSelection
    OF MouseLeft2
      POST(EVENT:Accepted,?Change:3)
      DO BRW2::FillBuffer
    OF InsertKey
      POST(EVENT:Accepted,?Insert:3)
    OF DeleteKey
      POST(EVENT:Accepted,?Delete:3)
    OF CtrlEnter
      POST(EVENT:Accepted,?Change:3)
    END                                                    ! END (What keycode was hit)
  ELSE
    CASE KEYCODE()                                         ! What keycode was hit
    OF AppsKey
    OROF MouseRight
       DO BRW2::NewSelection
    OF InsertKey
      POST(EVENT:Accepted,?Insert:3)
    ELSE                                                   ! ELSE (What keycode was hit)
      CASE BRW2::SortOrder
      OF 1
      END
    END
  END
  DO BRW2::PostNewSelection
!----------------------------------------------------------------------
BRW2::ScrollDrag ROUTINE
!|
!| This routine processes the Vertical Scroll Bar arrays to find the free key field value
!| that corresponds to the current scroll bar position.
!|
!| After the scroll position is computed, and the scroll value found, this routine sets
!| BRW2::LocateMode to that scroll value of 2 -- EQUATEd to LocateOnValue --
!| and calls the routine BRW2::LocateRecord.
!|
  IF ?Browse:2{PROP:VScrollPos} <= 1
    POST(EVENT:ScrollTop,?Browse:2)
  ELSIF ?Browse:2{PROP:VScrollPos} = 100
    POST(EVENT:ScrollBottom,?Browse:2)
  ELSE
    CASE BRW2::SortOrder
    OF 1
      ENR:ClassNumber = BRW2::Sort1:KeyDistribution[?Browse:2{PROP:VScrollPos}]
      BRW2::LocateMode = LocateOnValue
      DO BRW2::LocateRecord
    END
  END
!----------------------------------------------------------------------
BRW2::FillRecord ROUTINE
!|
!| This routine is used to retrieve a number of records from the VIEW. The number of records
!| retrieved is held in the variable BRW2::ItemsToFill. If more than one record is
!| to be retrieved, QuickScan is used to minimize reads from the disk.
!|
!| If records exist in the queue (in other words, if the browse has been used before), the record
!| at the appropriate end of the list box is retrieved, and the VIEW is reset to read starting
!| at that record.
!|
!| Next, the VIEW is accessed to retrieve BRW2::ItemsToFill records. Normally, this will
!| result in BRW2::ItemsToFill records being read from the VIEW, but if custom filtering
!| or range limiting is used (via the BRW2::ValidateRecord routine) then any number of records
!| might be read.
!|
!| For each good record, if BRW2::AddQueue is true, the queue is filled using the BRW2::FillQueue
!| routine. The record is then added to the queue. If adding this record causes the BrowseBox queue
!| to contain more records than can be displayed, the record at the opposite end of the queue is
!| deleted.
!|
!| The only time BRW2::AddQueue is false is when the BRW2::LocateRecord routine needs to
!| get the closest record to its record to be located. At this time, the record doesn't need to be
!| added to the queue, so it isn't.
!|
  IF BRW2::RecordCount
    IF BRW2::FillDirection = FillForward
      GET(Queue:Browse:2,BRW2::RecordCount)                ! Get the first queue item
    ELSE
      GET(Queue:Browse:2,1)                                ! Get the first queue item
    END
    RESET(BRW2::View:Browse,BRW2::Position)                ! Reset for sequential processing
    BRW2::SkipFirst = TRUE
  ELSE
    BRW2::SkipFirst = FALSE
  END
  LOOP WHILE BRW2::ItemsToFill
    IF BRW2::View:Browse{PROP:IPRequestCount} = 0
       BRW2::View:Browse{PROP:IPRequestCount} = BRW2::ItemsToFill
    END
    IF BRW2::FillDirection = FillForward
      NEXT(BRW2::View:Browse)
    ELSE
      PREVIOUS(BRW2::View:Browse)
    END
    IF ERRORCODE()
      IF ERRORCODE() = BadRecErr
        DO BRW2::RestoreResetValues
        BREAK
      ELSE
        StandardWarning(Warn:RecordFetchError,'Enrollment')
        POST(EVENT:CloseWindow)
        EXIT
      END
    END
    IF BRW2::SkipFirst
       BRW2::SkipFirst = FALSE
       IF POSITION(BRW2::View:Browse) = BRW2::Position
          CYCLE
       END
    END
    IF BRW2::AddQueue
      IF BRW2::RecordCount = ?Browse:2{PROP:Items}
        IF BRW2::FillDirection = FillForward
          GET(Queue:Browse:2,1)                            ! Get the first queue item
        ELSE
          GET(Queue:Browse:2,BRW2::RecordCount)            ! Get the first queue item
        END
        DELETE(Queue:Browse:2)
        BRW2::RecordCount -= 1
      END
      DO BRW2::FillQueue
      IF BRW2::FillDirection = FillForward
        ADD(Queue:Browse:2)
      ELSE
        ADD(Queue:Browse:2,1)
      END
      BRW2::RecordCount += 1
    END
    BRW2::ItemsToFill -= 1
  END
  BRW2::AddQueue = True
  EXIT
!----------------------------------------------------------------------
BRW2::LocateRecord ROUTINE
!|
!| This routine is used to find a record in the VIEW, and to display that record
!| in the BrowseBox.
!|
!| This routine has three different modes of operation, which are invoked based on
!| the setting of BRW2::LocateMode. These modes are...
!|
!|   LocateOnPosition (1) - This mode is still supported for 1.5 compatability. This mode
!|                          is the same as LocateOnEdit.
!|   LocateOnValue    (2) - The values of the current sort order key are used. This mode
!|                          used for Locators and when the BrowseBox is called to select
!|                          a record.
!|   LocateOnEdit     (3) - The current record of the VIEW is used. This mode assumes
!|                          that there is an active VIEW record. This mode is used when
!|                          the sort order of the BrowseBox has changed
!|
!| If an appropriate record has been located, the BRW2::RefreshPage routine is
!| called to load the page containing the located record.
!|
!| If an appropriate record is not locate, the last page of the BrowseBox is loaded.
!|
  IF BRW2::LocateMode = LocateOnPosition
    BRW2::LocateMode = LocateOnEdit
  END
  CLOSE(BRW2::View:Browse)
  CASE BRW2::SortOrder
  OF 1
    IF BRW2::UsingAdditionalSortOrder THEN
       BRW2::UsingAdditionalSortOrder = False
       BRW2::View:Browse{PROP:Order} = '+ENR:StudentNumber,+ENR:ClassNumber'
    END
    IF BRW2::LocateMode = LocateOnEdit
      BRW2::HighlightedPosition = POSITION(ENR:StuSeq)
      RESET(ENR:StuSeq,BRW2::HighlightedPosition)
    ELSE
      ENR:StudentNumber = STU:Number
      SET(ENR:StuSeq,ENR:StuSeq)
    END
    BRW2::View:Browse{Prop:Filter} = 'ENR:StudentNumber = BRW2::Sort1:Reset:STU:Number'
  END
  IF ERRORCODE()
    StandardWarning(Warn:ViewOpenError)
  END
  OPEN(BRW2::View:Browse)
  IF ERRORCODE()
    StandardWarning(Warn:ViewOpenError)
  END
  IF BRW2::UsingAdditionalSortOrder = True
    CASE BRW2::SortOrder
    OF 1
       SET(BRW2::View:Browse,2)
    END
  END
  FREE(Queue:Browse:2)
  BRW2::RecordCount = 0
  BRW2::ItemsToFill = 1
  BRW2::FillDirection = FillForward                        ! Fill with next read(s)
  BRW2::AddQueue = False
  DO BRW2::FillRecord                                      ! Fill with next read(s)
  BRW2::AddQueue = True
  IF BRW2::ItemsToFill
    BRW2::RefreshMode = RefreshOnBottom
    DO BRW2::RefreshPage
  ELSE
    BRW2::RefreshMode = RefreshOnPosition
    DO BRW2::RefreshPage
  END
  DO BRW2::PostNewSelection
  BRW2::LocateMode = 0
  EXIT
!----------------------------------------------------------------------
BRW2::RefreshPage ROUTINE
!|
!| This routine is used to load a single page of the BrowseBox.
!|
!| If this routine is called with a BRW2::RefreshMode of RefreshOnPosition,
!| the active VIEW record is loaded at the top of the page. Otherwise, if there are
!| records in the browse queue (Queue:Browse:2), then the current page is reloaded, and the
!| currently selected item remains selected.
!|
  IF NOT BRW2::ActiveInvisible THEN
     IF NOT ?Browse:2{PROP:Visible} THEN
        BRW2::LoadPending = True
        EXIT
     END
  END
  SETCURSOR(Cursor:Wait)
  IF BRW2::RefreshMode = RefreshOnPosition
    BRW2::HighlightedPosition = POSITION(BRW2::View:Browse)
    RESET(BRW2::View:Browse,BRW2::HighlightedPosition)
    BRW2::RefreshMode = RefreshOnTop
  ELSIF RECORDS(Queue:Browse:2)
    GET(Queue:Browse:2,BRW2::CurrentChoice)
    IF ERRORCODE()
      GET(Queue:Browse:2,RECORDS(Queue:Browse:2))
    END
    BRW2::HighlightedPosition = BRW2::Position
    GET(Queue:Browse:2,1)
    RESET(BRW2::View:Browse,BRW2::Position)
    BRW2::RefreshMode = RefreshOnCurrent
  ELSE
    BRW2::HighlightedPosition = ''
    DO BRW2::Reset
  END
  FREE(Queue:Browse:2)
  BRW2::RecordCount = 0
  BRW2::ItemsToFill = ?Browse:2{PROP:Items}
  IF BRW2::RefreshMode = RefreshOnBottom
    BRW2::FillDirection = FillBackward
  ELSE
    BRW2::FillDirection = FillForward
  END
  DO BRW2::FillRecord                                      ! Fill with next read(s)
  IF BRW2::HighlightedPosition
    IF BRW2::ItemsToFill
      IF NOT BRW2::RecordCount
        DO BRW2::Reset
      END
      IF BRW2::RefreshMode = RefreshOnBottom
        BRW2::FillDirection = FillForward
      ELSE
        BRW2::FillDirection = FillBackward
      END
      DO BRW2::FillRecord
    END
  END
  IF BRW2::RecordCount
    IF BRW2::HighlightedPosition
      LOOP BRW2::CurrentChoice = 1 TO BRW2::RecordCount
        GET(Queue:Browse:2,BRW2::CurrentChoice)
        IF BRW2::Position = BRW2::HighlightedPosition THEN BREAK.
      END
      IF BRW2::CurrentChoice > BRW2::RecordCount
        BRW2::CurrentChoice = BRW2::RecordCount
      END
    ELSE
      IF BRW2::RefreshMode = RefreshOnBottom
        BRW2::CurrentChoice = RECORDS(Queue:Browse:2)
      ELSE
        BRW2::CurrentChoice = 1
      END
    END
    ?Browse:2{Prop:Selected} = BRW2::CurrentChoice
    DO BRW2::FillBuffer
    ?Change:3{PROP:Disable} = 0
    ?Delete:3{PROP:Disable} = 0
  ELSE
    CLEAR(ENR:Record)
    BRW2::CurrentChoice = 0
    ?Change:3{PROP:Disable} = 1
    ?Delete:3{PROP:Disable} = 1
  END
  SETCURSOR()
  BRW2::RefreshMode = 0
  EXIT
BRW2::Reset ROUTINE
!|
!| This routine is used to reset the VIEW used by the BrowseBox.
!|
  IF NOT BRW2::ActiveInvisible THEN
     IF NOT ?Browse:2{PROP:Visible} THEN
        BRW2::LoadPending = True
        EXIT
     END
  END
  BRW2::LoadPending = False
  CLOSE(BRW2::View:Browse)
  CASE BRW2::SortOrder
  OF 1
    IF BRW2::UsingAdditionalSortOrder THEN
       BRW2::UsingAdditionalSortOrder = False
       BRW2::View:Browse{PROP:Order} = '+ENR:StudentNumber,+ENR:ClassNumber'
    END
    ENR:StudentNumber = STU:Number
    SET(ENR:StuSeq)
    BRW2::View:Browse{Prop:Filter} = 'ENR:StudentNumber = BRW2::Sort1:Reset:STU:Number'
  END
  IF ERRORCODE()
    StandardWarning(Warn:ViewOpenError)
  END
  OPEN(BRW2::View:Browse)
  IF ERRORCODE()
    StandardWarning(Warn:ViewOpenError)
  END
  SET(BRW2::View:Browse)
!----------------------------------------------------------------------
BRW2::GetRecord ROUTINE
!|
!| This routine is used to retrieve the VIEW record that corresponds to a
!| chosen listbox record.
!|
  IF BRW2::RecordCount
    BRW2::CurrentChoice = CHOICE(?Browse:2)
    GET(Queue:Browse:2,BRW2::CurrentChoice)
    WATCH(BRW2::View:Browse)
    REGET(BRW2::View:Browse,BRW2::Position)
  END
!----------------------------------------------------------------------
BRW2::RestoreResetValues ROUTINE
!|
!| This routine is used to restore reset values to their saved value
!| after a bad record access from the VIEW.
!|
  CASE BRW2::SortOrder
  OF 1
    STU:Number = BRW2::Sort1:Reset:STU:Number
  END
!--------------------------------------------------------------------------
DisplayBrowseToolbar      ROUTINE
  ENABLE(TBarBrwBottom,TBarBrwLocate)
  TBarBrwHelp{PROP:DISABLE}=?Help{PROP:DISABLE}
  IF BrowseButtons.InsertButton THEN
    TBarBrwInsert{PROP:Disable} = BrowseButtons.InsertButton{PROP:Disable}
  END
  IF BrowseButtons.ChangeButton THEN
    TBarBrwChange{PROP:Disable} = BrowseButtons.ChangeButton{PROP:Disable}
  END
  IF BrowseButtons.DeleteButton THEN
    TBarBrwDelete{PROP:Disable} = BrowseButtons.DeleteButton{PROP:Disable}
  END
  DISABLE(TBarBrwHistory)
  ToolBarMode = BrowseMode
  TBarBrwDown{PROP:ToolTip} = 'Go to the Next Record'
  TBarBrwBottom{PROP:ToolTip} = 'Go to the Last Page'
  TBarBrwTop{PROP:ToolTip} = 'Go to the First Page'
  TBarBrwPageDown{PROP:ToolTip} = 'Go to the Next Page'
  TBarBrwPageUp{PROP:ToolTip} = 'Go to the Prior Page'
  TBarBrwDown{PROP:ToolTip} = 'Go to the Next Record'
  TBarBrwUP{PROP:ToolTip} = 'Go to the Prior Record'
  TBarBrwInsert{PROP:ToolTip} = 'Insert a new Record'
  DISPLAY(TBarBrwFirst,TBarBrwLast)
!--------------------------------------------------------------------------
ListBoxDispatch ROUTINE
  DO DisplayBrowseToolbar
  IF FOCUS() <> BrowseButtons.ListBox THEN  ! List box must have focus when on update form
    EXIT
  END
  IF ACCEPTED() THEN            !trap remote browse box control calls
    EXECUTE(ACCEPTED()-TBarBrwBottom+1)
      POST(EVENT:ScrollBottom,BrowseButtons.ListBox)
      POST(EVENT:ScrollTop,BrowseButtons.ListBox)
      POST(EVENT:PageDown,BrowseButtons.ListBox)
      POST(EVENT:PageUp,BrowseButtons.ListBox)
      POST(EVENT:ScrollDown,BrowseButtons.ListBox)
      POST(EVENT:ScrollUp,BrowseButtons.ListBox)
      POST(EVENT:Locate,BrowseButtons.ListBox)
      BEGIN                     !EXECUTE Place Holder - Ditto has no effect on a browse
      END
      PRESSKEY(F1Key)
    END
  END

UpdateDispatch ROUTINE
  DISABLE(TBarBrwDelete)
  DISABLE(TBarBrwChange)
  IF BrowseButtons.DeleteButton AND BrowseButtons.DeleteButton{PROP:Disable} = 0 THEN
    ENABLE(TBarBrwDelete)
  END
  IF BrowseButtons.ChangeButton AND BrowseButtons.ChangeButton{PROP:Disable} = 0 THEN
    ENABLE(TBarBrwChange)
  END
  IF FOCUS() <> BrowseButtons.ListBox THEN  ! List box must have focus when on update form
    EXIT
  END
  IF INRANGE(ACCEPTED(),TBarBrwInsert,TBarBrwDelete) THEN         !trap remote browse update control calls
    EXECUTE(ACCEPTED()-TBarBrwInsert+1)
      POST(EVENT:Accepted,BrowseButtons.InsertButton)
      POST(EVENT:Accepted,BrowseButtons.ChangeButton)
      POST(EVENT:Accepted,BrowseButtons.DeleteButton)
    END
  END
!----------------------------------------------------------------
BRW2::ButtonInsert ROUTINE
!|
!| This routine calls the BrowseBox's update procedure (as specified in the
!| BrowseUpdateButtons control template) to insert a new record.
!|
!| First, the primary file's record  buffer is cleared, as well as any memos
!| or BLOBs. Next, any range-limit values are restored so that the inserted
!| record defaults to being added to the current display set.
!|
!| Next, LocalRequest is set to InsertRecord, and the BRW2::CallRecord routine
!| is called. This routine performs the actual call to the update procedure.
!|
!| If the insert is successful (GlobalRequest = RequestCompleted) then the newly added
!| record is displayed in the BrowseBox, at the top of the listbox.
!|
!| If the insert is not successful, the current page of the browse is refreshed.
!|
!| Finally, The BrowseBox is re-initialized, resetting scroll bars and totals.
!|
  GET(Enrollment,0)
  CLEAR(ENR:Record,0)
  CASE BRW2::SortOrder
  OF 1
    ENR:StudentNumber = BRW2::Sort1:Reset:STU:Number
  END
  LocalRequest = InsertRecord
  DO BRW2::CallUpdate
  IF GlobalResponse = RequestCompleted
    BRW2::LocateMode = LocateOnEdit
    DO BRW2::LocateRecord
  ELSE
    BRW2::RefreshMode = RefreshOnQueue
    DO BRW2::RefreshPage
  END
  DO BRW2::InitializeBrowse
  DO BRW2::PostNewSelection
  SELECT(?Browse:2)
  LocalRequest = OriginalRequest
  LocalResponse = RequestCancelled
  DO RefreshWindow
!----------------------------------------------------------------
BRW2::ButtonChange ROUTINE
!|
!| This routine calls the BrowseBox's update procedure (as specified in the
!| BrowseUpdateButtons control template) to change a selected record.
!|
!| Whenever a button is pressed, the first thing that happens is that the
!| SyncWindow routine is called. This routine insures that the BrowseBox's
!| VIEW corresponds to the highlighted record by calling the BRW2::GetRecord routine.
!|
!| First, LocalRequest is set to ChangeRecord, and the BRW2::CallRecord routine
!| is called. This routine performs the actual call to the update procedure.
!|
!| If the change is successful (GlobalRequest = RequestCompleted) then the newly changed
!| record is displayed in the BrowseBox.
!|
!| If the change is not successful, the current page of the browse is refreshed.
!|
!| Finally, The BrowseBox is re-initialized, resetting scroll bars and totals.
!|
  LocalRequest = ChangeRecord
  DO BRW2::CallUpdate
  IF GlobalResponse = RequestCompleted
    BRW2::LocateMode = LocateOnEdit
    DO BRW2::LocateRecord
  ELSE
    BRW2::RefreshMode = RefreshOnQueue
    DO BRW2::RefreshPage
  END
  DO BRW2::InitializeBrowse
  DO BRW2::PostNewSelection
  SELECT(?Browse:2)
  LocalRequest = OriginalRequest
  LocalResponse = RequestCancelled
  DO RefreshWindow
!----------------------------------------------------------------
BRW2::ButtonDelete ROUTINE
!|
!| This routine calls the BrowseBox's update procedure (as specified in the
!| BrowseUpdateButtons control template) to delete a selected record.
!|
!| Whenever a button is pressed, the first thing that happens is that the
!| SyncWindow routine is called. This routine insures that the BrowseBox's
!| VIEW corresponds to the highlighted record by calling the BRW2::GetRecord routine.
!|
!| First, LocalRequest is set to DeleteRecord, and the BRW2::CallRecord routine
!| is called. This routine performs the actual call to the update procedure.
!|
!| If the delete is successful (GlobalRequest = RequestCompleted) then the deleted record is
!| removed from the queue.
!|
!| Next, the BrowseBox is refreshed, redisplaying the current page.
!|
!| Finally, The BrowseBox is re-initialized, resetting scroll bars and totals.
!|
  LocalRequest = DeleteRecord
  DO BRW2::CallUpdate
  IF GlobalResponse = RequestCompleted
    DELETE(Queue:Browse:2)
    BRW2::RecordCount -= 1
  END
  BRW2::RefreshMode = RefreshOnQueue
  DO BRW2::RefreshPage
  DO BRW2::InitializeBrowse
  DO BRW2::PostNewSelection
  SELECT(?Browse:2)
  LocalRequest = OriginalRequest
  LocalResponse = RequestCancelled
  DO RefreshWindow
!----------------------------------------------------------------
BRW2::CallUpdate ROUTINE
!|
!| This routine performs the actual call to the update procedure.
!|
!| The first thing that happens is that the VIEW is closed. This is performed just in case
!| the VIEW is still open.
!|
!| Next, GlobalRequest is set the the value of LocalRequest, and the update procedure
!| (UpdateEnrollment) is called.
!|
!| Upon return from the update, the routine BRW2::Reset is called to reset the VIEW
!| and reopen it.
!|
  CLOSE(BRW2::View:Browse)
  LOOP
    GlobalRequest = LocalRequest
    VCRRequest = VCRNone
    UpdateEnrollment
    LocalResponse = GlobalResponse
    CASE VCRRequest
    OF VCRNone
      BREAK
    OF VCRInsert
      IF LocalRequest = ChangeRecord THEN
        LocalRequest = InsertRecord
      END
    OROF VCRForward
      IF LocalRequest = InsertRecord THEN
        GET(Enrollment,0)
        CLEAR(ENR:Record,0)
      ELSE
        DO BRW2::PostVCREdit1
        BRW2::CurrentEvent = EVENT:ScrollDown
        DO BRW2::ScrollOne
        DO BRW2::PostVCREdit2
      END
    OF VCRBackward
      DO BRW2::PostVCREdit1
      BRW2::CurrentEvent = EVENT:ScrollUp
      DO BRW2::ScrollOne
      DO BRW2::PostVCREdit2
    OF VCRPageForward
      DO BRW2::PostVCREdit1
      BRW2::CurrentEvent = EVENT:PageDown
      DO BRW2::ScrollPage
      DO BRW2::PostVCREdit2
    OF VCRPageBackward
      DO BRW2::PostVCREdit1
      BRW2::CurrentEvent = EVENT:PageUp
      DO BRW2::ScrollPage
      DO BRW2::PostVCREdit2
    OF VCRFirst
      DO BRW2::PostVCREdit1
      BRW2::CurrentEvent = EVENT:ScrollTop
      DO BRW2::ScrollEnd
      DO BRW2::PostVCREdit2
    OF VCRLast
      DO BRW2::PostVCREdit1
      BRW2::CurrentEvent = EVENT:ScrollBottom
      DO BRW2::ScrollEND
      DO BRW2::PostVCREdit2
    END
  END
  DO BRW2::Reset

BRW2::PostVCREdit1 ROUTINE
  DO BRW2::Reset
  BRW2::LocateMode = LocateOnEdit
  DO BRW2::LocateRecord
  DO RefreshWindow

BRW2::PostVCREdit2 ROUTINE
  ?Browse:2{PROP:SelStart} = BRW2::CurrentChoice
  DO BRW2::NewSelection
  REGET(BRW2::View:Browse,BRW2::Position)
  CLOSE(BRW2::View:Browse)

!|
!| Copies a field from save buffer to actual buffer switched on current field
!|
HistoryField  ROUTINE
  CASE FOCUS()
  OF ?STU:FirstName
    STU:FirstName = History::STU:Record.FirstName
  OF ?STU:LastName
    STU:LastName = History::STU:Record.LastName
  OF ?STU:Address
    STU:Address = History::STU:Record.Address
  OF ?STU:Address2
    STU:Address2 = History::STU:Record.Address2
  OF ?STU:City
    STU:City = History::STU:Record.City
  OF ?STU:State
    STU:State = History::STU:Record.State
  OF ?STU:Zip
    STU:Zip = History::STU:Record.Zip
  OF ?STU:Telephone
    STU:Telephone = History::STU:Record.Telephone
  OF ?STU:GradYear
    STU:GradYear = History::STU:Record.GradYear
  OF ?STU:Number
    STU:Number = History::STU:Record.Number
  OF ?STU:Major
    STU:Major = History::STU:Record.Major
  END
  DISPLAY()
!---------------------------------------------------------------
PrimeFields ROUTINE
!|
!| This routine is called whenever the procedure is called to insert a record.
!|
!| This procedure performs three functions. These functions are..
!|
!|   1. Prime the new record with initial values specified in the dictionary
!|      and under the Field priming on Insert button.
!|   2. Generates any Auto-Increment values needed.
!|   3. Saves a copy of the new record, as primed, for use in batch-adds.
!|
!| If an auto-increment value is generated, this routine will add the new record
!| at this point, keeping its place in the file.
!|
  STU:Record = SAV::STU:Record
  STU:Photograph{PROP:Size} = 0
  SAV::STU:Record = STU:Record
ClosingWindow ROUTINE
  Update::Reloop = 0
  IF LocalResponse <> RequestCompleted
    DO CancelAutoIncrement
    IF Save:LastInsertedPosition
      LocalResponse = RequestCompleted
      REGET(Students,Save:LastInsertedPosition)
    END
  END

CancelAutoIncrement ROUTINE
  IF LocalResponse <> RequestCompleted
  END
FORM::AssignButtons ROUTINE
  ToolBarMode=FormMode
  DISABLE(TBarBrwFirst,TBarBrwLast)
  ENABLE(TBarBrwHistory)
  CASE OriginalRequest
  OF InsertRecord
    ENABLE(TBarBrwDown)
    ENABLE(TBarBrwInsert)
    TBarBrwDown{PROP:ToolTip}='Save record and add another'
    TBarBrwInsert{PROP:ToolTip}=TBarBrwDown{PROP:ToolTip}
  OF ChangeRecord
    ENABLE(TBarBrwBottom,TBarBrwUp)
    ENABLE(TBarBrwInsert)
    TBarBrwBottom{PROP:ToolTip}='Save changes and go to last record'
    TBarBrwTop{PROP:ToolTip}='Save changes and go to first record'
    TBarBrwPageDown{PROP:ToolTip}='Save changes and page down to record'
    TBarBrwPageUp{PROP:ToolTip}='Save changes and page up to record'
    TBarBrwDown{PROP:ToolTip}='Save changes and go to next record'
    TBarBrwUP{PROP:ToolTip}='Save changes and go to previous record'
    TBarBrwInsert{PROP:ToolTip}='Save this record and add a new one'
  END
  TBarBrwHelp{PROP:Disable}=?Help{PROP:Disable}
  DISPLAY(TBarBrwFirst,TBarBrwLast)

!!! <summary>
!!! Generated from procedure template - Form
!!! Update the Majors File
!!! </summary>
UpdateMajors PROCEDURE

CurrentTab           STRING(80)                            ! 
LocalRequest         LONG                                  ! 
OriginalRequest      LONG                                  ! 
LocalResponse        LONG                                  ! 
FilesOpened          LONG                                  ! 
WindowOpened         LONG                                  ! 
WindowInitialized    LONG                                  ! 
ForceRefresh         LONG                                  ! 
RecordFiltered       LONG                                  ! 
ActionMessage        CSTRING(40)                           ! 
RecordChanged        BYTE,AUTO                             ! 

BRW2::View:Browse    VIEW(Teachers)
                       PROJECT(TEA:FirstName)
                       PROJECT(TEA:LastName)
                       PROJECT(TEA:Address)
                       PROJECT(TEA:City)
                       PROJECT(TEA:State)
                       PROJECT(TEA:Zip)
                       PROJECT(TEA:Telephone)
                       PROJECT(TEA:Number)
                       PROJECT(TEA:Department)
                     END
Queue:Browse:2       QUEUE,PRE()                           ! Browsing Queue
BRW2::TEA:FirstName    LIKE(TEA:FirstName)                 ! Queue Display field
BRW2::TEA:LastName     LIKE(TEA:LastName)                  ! Queue Display field
BRW2::TEA:Address      LIKE(TEA:Address)                   ! Queue Display field
BRW2::TEA:City         LIKE(TEA:City)                      ! Queue Display field
BRW2::TEA:State        LIKE(TEA:State)                     ! Queue Display field
BRW2::TEA:Zip          LIKE(TEA:Zip)                       ! Queue Display field
BRW2::TEA:Telephone    LIKE(TEA:Telephone)                 ! Queue Display field
BRW2::TEA:Number       LIKE(TEA:Number)                    ! Queue Display field
BRW2::TEA:Department   LIKE(TEA:Department)                ! Queue Display field
BRW2::Mark             BYTE                                ! Record mark flag
BRW2::Position         STRING(1024)                        ! Queue POSITION information
                     END                                   ! END (Browsing Queue)
BRW2::UsingAdditionalSortOrder BYTE                        ! When true the view is using PROP:Order
BRW2::CurrentScroll  BYTE                                  ! Queue position of scroll thumb
BRW2::ScrollRecordCount LONG                               ! Queue position of scroll thumb
BRW2::SkipFirst      BYTE                                  ! Skip first retrieved record in page fill
BRW2::Sort1:KeyDistribution LIKE(TEA:Department),DIM(100)
BRW2::Sort1:LowValue LIKE(TEA:Department)                  ! Queue position of scroll thumb
BRW2::Sort1:HighValue LIKE(TEA:Department)                 ! Queue position of scroll thumb
BRW2::Sort1:Reset:MAJ:Number LIKE(MAJ:Number)
BRW2::CurrentEvent   LONG                                  !
BRW2::CurrentChoice  LONG                                  !
BRW2::RecordCount    LONG                                  !
BRW2::SortOrder      BYTE                                  !
BRW2::LocateMode     BYTE                                  !
BRW2::RefreshMode    BYTE                                  !
BRW2::LastSortOrder  BYTE                                  !
BRW2::FillDirection  BYTE                                  !
BRW2::AddQueue       BYTE                                  !
BRW2::Changed        BYTE                                  !
BRW2::RecordStatus   BYTE                                  ! Flag for Range/Filter test
BRW2::ItemsToFill    LONG                                  ! Controls records retrieved
BRW2::MaxItemsInList LONG                                  ! Retrieved after window opened
BRW2::HighlightedPosition STRING(1024)                     ! POSITION of located record
BRW2::NewSelectPosted BYTE                                 ! Queue position of located record
BRW2::PopupText      CSTRING(10000)                        !
BRW2::ActiveInvisible BYTE(1)                              !
BRW2::LoadPending    BYTE                                  !
ToolBarMode          UNSIGNED,AUTO
BrowseButtons        GROUP                      !info for current browse with focus
ListBox                SIGNED                   !Browse list control
InsertButton           SIGNED                   !Browse insert button
ChangeButton           SIGNED                   !Browse change button
DeleteButton           SIGNED                   !Browse delete button
SelectButton           SIGNED                   !Browse select button
                     END

BRW4::View:Browse    VIEW(Students)
                       PROJECT(STU:FirstName)
                       PROJECT(STU:LastName)
                       PROJECT(STU:Address)
                       PROJECT(STU:Address2)
                       PROJECT(STU:City)
                       PROJECT(STU:State)
                       PROJECT(STU:Zip)
                       PROJECT(STU:Telephone)
                       PROJECT(STU:GradYear)
                       PROJECT(STU:Number)
                       PROJECT(STU:Major)
                     END
Queue:Browse:4       QUEUE,PRE()                           ! Browsing Queue
BRW4::STU:FirstName    LIKE(STU:FirstName)                 ! Queue Display field
BRW4::STU:LastName     LIKE(STU:LastName)                  ! Queue Display field
BRW4::STU:Address      LIKE(STU:Address)                   ! Queue Display field
BRW4::STU:Address2     LIKE(STU:Address2)                  ! Queue Display field
BRW4::STU:City         LIKE(STU:City)                      ! Queue Display field
BRW4::STU:State        LIKE(STU:State)                     ! Queue Display field
BRW4::STU:Zip          LIKE(STU:Zip)                       ! Queue Display field
BRW4::STU:Telephone    LIKE(STU:Telephone)                 ! Queue Display field
BRW4::STU:GradYear     LIKE(STU:GradYear)                  ! Queue Display field
BRW4::STU:Number       LIKE(STU:Number)                    ! Queue Display field
BRW4::STU:Major        LIKE(STU:Major)                     ! Queue Display field
BRW4::Mark             BYTE                                ! Record mark flag
BRW4::Position         STRING(1024)                        ! Queue POSITION information
                     END                                   ! END (Browsing Queue)
BRW4::UsingAdditionalSortOrder BYTE                        ! When true the view is using PROP:Order
BRW4::CurrentScroll  BYTE                                  ! Queue position of scroll thumb
BRW4::ScrollRecordCount LONG                               ! Queue position of scroll thumb
BRW4::SkipFirst      BYTE                                  ! Skip first retrieved record in page fill
BRW4::Sort1:KeyDistribution LIKE(STU:LastName),DIM(100)
BRW4::Sort1:LowValue LIKE(STU:LastName)                    ! Queue position of scroll thumb
BRW4::Sort1:HighValue LIKE(STU:LastName)                   ! Queue position of scroll thumb
BRW4::Sort1:Reset:MAJ:Number LIKE(MAJ:Number)
BRW4::CurrentEvent   LONG                                  !
BRW4::CurrentChoice  LONG                                  !
BRW4::RecordCount    LONG                                  !
BRW4::SortOrder      BYTE                                  !
BRW4::LocateMode     BYTE                                  !
BRW4::RefreshMode    BYTE                                  !
BRW4::LastSortOrder  BYTE                                  !
BRW4::FillDirection  BYTE                                  !
BRW4::AddQueue       BYTE                                  !
BRW4::Changed        BYTE                                  !
BRW4::RecordStatus   BYTE                                  ! Flag for Range/Filter test
BRW4::ItemsToFill    LONG                                  ! Controls records retrieved
BRW4::MaxItemsInList LONG                                  ! Retrieved after window opened
BRW4::HighlightedPosition STRING(1024)                     ! POSITION of located record
BRW4::NewSelectPosted BYTE                                 ! Queue position of located record
BRW4::PopupText      CSTRING(10000)                        !
BRW4::ActiveInvisible BYTE(1)                              !
BRW4::LoadPending    BYTE                                  !
Update::Reloop  BYTE
Update::Error   BYTE
History::MAJ:Record LIKE(MAJ:Record),STATIC
SAV::MAJ:Record      LIKE(MAJ:Record)
Save:LastInsertedPosition STRING(512)
WindowXPos      SIGNED,AUTO,STATIC
WindowYPos      SIGNED,AUTO,STATIC
WindowPosInit   BOOL(False),STATIC
QuickWindow          WINDOW('Update the Majors File'),AT(,,159,138),FONT('MS Sans Serif',8,COLOR:Black),GRAY,IMM, |
  MDI,HLP('~UpdateMajors'),SYSTEM
                       SHEET,AT(4,4,151,112),USE(?CurrentTab)
                         TAB('General')
                           PROMPT('&Description:'),AT(8,20),USE(?MAJ:Description:Prompt)
                           ENTRY(@S20),AT(64,20,84,10),USE(MAJ:Description)
                         END
                         TAB('Teachers'),USE(?TeachersTab)
                           LIST,AT(8,20,143,94),USE(?Browse:2),HVSCROLL,FORMAT('80L(2)|M~First Name~L(2)@S20@80L(2' & |
  ')|M~Last Name~L(2)@S20@80L(2)|M~Address~L(2)@S20@80L(2)|M~City~L(2)@S20@24L(2)|M~Sta' & |
  'te~L(2)@S2@24R(2)|M~Zip~C(0)@n05@52L(2)|M~Telephone~L(2)@s12@'),FROM(Queue:Browse:2),IMM, |
  MSG('Browsing Records')
                           BUTTON('&Insert'),AT(8,98,45,14),USE(?Insert:3),HIDE
                           BUTTON('&Change'),AT(57,98,45,14),USE(?Change:3),HIDE
                           BUTTON('&Delete'),AT(106,98,45,14),USE(?Delete:3),HIDE
                         END
                         TAB('Students'),USE(?StudentsTab)
                           LIST,AT(8,20,143,94),USE(?Browse:4),HVSCROLL,FORMAT('80L(2)|M~First Name~L(2)@S20@80L(2' & |
  ')|M~Last Name~L(2)@S20@80L(2)|M~Address~L(2)@S20@80L(2)|M~Address 2~L(2)@s20@80L(2)|' & |
  'M~City~L(2)@S20@24L(2)|M~State~L(2)@S2@24R(2)|M~Zip~C(0)@n05@52L(2)|M~Telephone~L(2)' & |
  '@s12@40R(2)|M~Grad Year~C(0)@n4@'),FROM(Queue:Browse:4),IMM,MSG('Browsing Records')
                           BUTTON('&Insert'),AT(8,98,45,14),USE(?Insert:5),HIDE
                           BUTTON('&Change'),AT(57,98,45,14),USE(?Change:5),HIDE
                           BUTTON('&Delete'),AT(106,98,45,14),USE(?Delete:5),HIDE
                         END
                       END
                       BUTTON('OK'),AT(12,120,45,14),USE(?OK),DEFAULT
                       BUTTON('Cancel'),AT(61,120,45,14),USE(?Cancel)
                       BUTTON('Help'),AT(110,120,45,14),USE(?Help),STD(STD:Help)
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
  IF LocalRequest = InsertRecord
    ?TeachersTab{PROP:Hide} = TRUE
    ?StudentsTab{PROP:Hide} = TRUE
  END
  CASE LocalRequest
  OF ViewRecord
    ActionMessage = 'View Record'
  OF InsertRecord
    CLEAR(Save:LastInsertedPosition)
    ActionMessage = 'Adding a Majors Record'
  OF ChangeRecord
    ActionMessage = 'Changing a Majors Record'
  OF DeleteRecord
  END
  QuickWindow{PROP:Text} = ActionMessage
  IF LocalRequest <> ViewRecord THEN
    ENABLE(TBarBrwHistory)
  END
  ACCEPT
    CASE EVENT()
    OF EVENT:AlertKey
      IF KEYCODE() = 734 AND LocalRequest <> ViewRecord THEN
        DO HistoryField
      END
    OF EVENT:CloseDown
      DO ClosingWindow
      IF Update::Reloop THEN CYCLE.
    OF EVENT:CloseWindow
      DO ClosingWindow
      IF Update::Reloop THEN CYCLE.
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
    OF EVENT:Moved
      GETPOSITION(0,WindowXPos,WindowYPos)
    OF EVENT:OpenWindow
      DO BRW2::AssignButtons
      DO FORM::AssignButtons
      IF NOT WindowInitialized
        DO InitializeWindow
        WindowInitialized = True
      END
      SELECT(?MAJ:Description:Prompt)
    OF EVENT:Sized
      POST(EVENT:DoResize,0,THREAD())
    OF Event:Rejected
      BEEP
      DISPLAY(?)
      SELECT(?)
    ELSE
      IF ToolBarMode=BrowseMode THEN
        DO ListBoxDispatch
      END
      IF ToolBarMode = BrowseMode THEN
        DO UpdateDispatch
      END
      IF LocalRequest <> ViewRecord THEN
        IF ACCEPTED() = TbarBrwHistory
          DO HistoryField
        END
      END
      IF EVENT() = EVENT:Completed
        History::MAJ:Record = MAJ:Record
        CASE LocalRequest
        OF InsertRecord
          ADD(Majors)
          CASE ERRORCODE()
          OF NoError
            IF StandardWarning(Warn:NewRecordAdded) = Button:Yes
              Save:LastInsertedPosition = POSITION(Majors)
              DO PrimeFields
              DISPLAY
              SELECT(?MAJ:Description:Prompt)
              CYCLE
            ELSE
              LocalResponse = RequestCompleted
              POST(EVENT:CloseWindow)
            END
          OF DupKeyErr
            IF DUPLICATE(MAJ:KeyNumber)
              IF StandardWarning(Warn:DuplicateKey,'MAJ:KeyNumber')
                SELECT(?MAJ:Description:Prompt)
                VCRRequest = VCRNone
                CYCLE
              END
            END
            IF DUPLICATE(MAJ:KeyDescription)
              IF StandardWarning(Warn:DuplicateKey,'MAJ:KeyDescription')
                SELECT(?MAJ:Description:Prompt)
                VCRRequest = VCRNone
                CYCLE
              END
            END
          ELSE
            IF StandardWarning(Warn:InsertError)
              SELECT(?MAJ:Description:Prompt)
              VCRRequest = VCRNone
              CYCLE
            END
          END
        OF ChangeRecord
          LOOP
            LocalResponse = RequestCancelled
            SETCURSOR(CURSOR:Wait)
            RecordChanged = FALSE
            IF (SAV::MAJ:Record <> MAJ:Record) 
               RecordChanged = TRUE
            END
            IF RecordChanged THEN
              Update::Error = RIUpdate:Majors(1)
            ELSE
              Update::Error = 0
            END
            SETCURSOR()
            IF Update::Error THEN
              IF Update::Error = 1 THEN
                CASE StandardWarning(Warn:UpdateError)
                OF Button:Yes
                  CYCLE
                OF Button:No
                  POST(EVENT:CloseWindow)
                  BREAK
                END
              END
              DISPLAY
              SELECT(?MAJ:Description:Prompt)
              VCRRequest = VCRNone
            ELSE
              IF RecordChanged OR VCRRequest = VCRNone THEN
                LocalResponse = RequestCompleted
              END
              POST(EVENT:CloseWindow)
            END
            BREAK
          END
        END
      END
      IF ToolbarMode = FormMode THEN
        CASE ACCEPTED()
        OF TBarBrwBottom TO TBarBrwUp
        OROF TBarBrwInsert
          VCRRequest=ACCEPTED()
          POST(EVENT:Completed)
        OF TBarBrwHelp
          PRESSKEY(F1Key)
        END
      END
    END
    CASE FIELD()
    OF ?CurrentTab
      CASE EVENT()
      OF EVENT:Accepted
        DO RefreshWindow
      OF EVENT:Selected
        DO RefreshWindow
      OF EVENT:Selecting
        DO RefreshWindow
      OF EVENT:NewSelection
          !Code to assign button control based upon current tab selection
          CASE CHOICE(?CurrentTab)
          OF 1
            DO FORM::AssignButtons
          OF 2
            DO BRW2::AssignButtons
          OF 3
            DO BRW4::AssignButtons
          END
        DO RefreshWindow
      OF EVENT:TabChanging
        DO RefreshWindow
      END
    OF ?Browse:2
      CASE EVENT()
      OF EVENT:NewSelection
        DO BRW2::NewSelection
      OF EVENT:ScrollUp
        DO BRW2::ProcessScroll
      OF EVENT:ScrollDown
        DO BRW2::ProcessScroll
      OF EVENT:PageUp
        DO BRW2::ProcessScroll
      OF EVENT:PageDown
        DO BRW2::ProcessScroll
      OF EVENT:ScrollTop
        DO BRW2::ProcessScroll
      OF EVENT:ScrollBottom
        DO BRW2::ProcessScroll
      OF EVENT:ScrollDrag
        DO BRW2::ScrollDrag
      OF EVENT:AlertKey
        DO BRW2::AlertKey
      END
    OF ?Insert:3
      CASE EVENT()
      OF EVENT:Accepted
        DO SyncWindow
        DO BRW2::ButtonInsert
      END
    OF ?Change:3
      CASE EVENT()
      OF EVENT:Accepted
        DO SyncWindow
        DO BRW2::ButtonChange
      END
    OF ?Delete:3
      CASE EVENT()
      OF EVENT:Accepted
        DO SyncWindow
        DO BRW2::ButtonDelete
      END
    OF ?Browse:4
      CASE EVENT()
      OF EVENT:NewSelection
        DO BRW4::NewSelection
      OF EVENT:ScrollUp
        DO BRW4::ProcessScroll
      OF EVENT:ScrollDown
        DO BRW4::ProcessScroll
      OF EVENT:PageUp
        DO BRW4::ProcessScroll
      OF EVENT:PageDown
        DO BRW4::ProcessScroll
      OF EVENT:ScrollTop
        DO BRW4::ProcessScroll
      OF EVENT:ScrollBottom
        DO BRW4::ProcessScroll
      OF EVENT:ScrollDrag
        DO BRW4::ScrollDrag
      OF EVENT:AlertKey
        DO BRW4::AlertKey
      END
    OF ?Insert:5
      CASE EVENT()
      OF EVENT:Accepted
        DO SyncWindow
        DO BRW4::ButtonInsert
      END
    OF ?Change:5
      CASE EVENT()
      OF EVENT:Accepted
        DO SyncWindow
        DO BRW4::ButtonChange
      END
    OF ?Delete:5
      CASE EVENT()
      OF EVENT:Accepted
        DO SyncWindow
        DO BRW4::ButtonDelete
      END
    OF ?OK
      CASE EVENT()
      OF EVENT:Accepted
        DO SyncWindow
        IF LocalRequest <> ViewRecord THEN
          IF OriginalRequest = ChangeRecord OR OriginalRequest = InsertRecord
            SELECT()
          ELSE
            POST(EVENT:Completed)
          END
        END
      END
    OF ?Cancel
      CASE EVENT()
      OF EVENT:Accepted
        DO SyncWindow
        LocalResponse = RequestCancelled
        VCRRequest = VCRNone
        POST(EVENT:CloseWindow)
      END
    END
  END
  DO ProcedureReturn
!---------------------------------------------------------------------------
PrepareProcedure ROUTINE
  IF Majors::Used = 0
    CheckOpen(Majors,1)
  END
  Majors::Used += 1
  IF Students::Used = 0
    CheckOpen(Students,1)
  END
  Students::Used += 1
  IF Teachers::Used = 0
    CheckOpen(Teachers,1)
  END
  Teachers::Used += 1
  FilesOpened = TRUE
  DO BindFields
  RISnap:Majors
  SAV::MAJ:Record = MAJ:Record
  IF LocalRequest = InsertRecord
    LocalResponse = RequestCompleted
    DO PrimeFields
    IF LocalResponse = RequestCancelled
      DO ProcedureReturn
    END
    LocalResponse = RequestCancelled
  END
  IF LocalRequest = DeleteRecord
    IF StandardWarning(Warn:StandardDelete) = Button:OK
      LOOP
        LocalResponse = RequestCancelled
        SETCURSOR(CURSOR:Wait)
        IF RIDelete:Majors()
          SETCURSOR()
          CASE StandardWarning(Warn:DeleteError)
          OF Button:Yes
            CYCLE
          OF Button:No OROF Button:Cancel
            BREAK
          END
        ELSE
          SETCURSOR()
          LocalResponse = RequestCompleted
        END
        BREAK
      END
    END
    DO ProcedureReturn
  END
  OPEN(QuickWindow)
  WindowOpened=True
  BRW2::AddQueue = True
  BRW2::RecordCount = 0
  BRW4::AddQueue = True
  BRW4::RecordCount = 0
  IF WindowPosInit THEN
    SETPOSITION(0,WindowXPos,WindowYPos)
  ELSE
    GETPOSITION(0,WindowXPos,WindowYPos)
    WindowPosInit=True
  END
  Do DefineListboxStyle
  ?Browse:2{PROP:Alrt,252} = MouseLeft2
  ?Browse:2{PROP:Alrt,255} = AppsKey
  ?Browse:2{PROP:Alrt,253} = MouseRight
  ?Browse:2{PROP:Alrt,255} = InsertKey
  ?Browse:2{PROP:Alrt,254} = DeleteKey
  ?Browse:2{PROP:Alrt,253} = CtrlEnter
  ?Browse:2{PROP:Alrt,252} = MouseLeft2
  ?Browse:4{PROP:Alrt,252} = MouseLeft2
  ?Browse:4{PROP:Alrt,255} = AppsKey
  ?Browse:4{PROP:Alrt,253} = MouseRight
  ?Browse:4{PROP:Alrt,255} = InsertKey
  ?Browse:4{PROP:Alrt,254} = DeleteKey
  ?Browse:4{PROP:Alrt,253} = CtrlEnter
  ?Browse:4{PROP:Alrt,252} = MouseLeft2
  ?MAJ:Description{PROP:Alrt,255} = 734

!---------------------------------------------------------------------------
BindFields ROUTINE
  BIND(MAJ:RECORD)
  BIND(STU:RECORD)
  BIND(TEA:RECORD)
  BIND('BRW2::Sort1:Reset:MAJ:Number',BRW2::Sort1:Reset:MAJ:Number) ! Added by: BrowseBox(Clarion)
  BIND('BRW4::Sort1:Reset:MAJ:Number',BRW4::Sort1:Reset:MAJ:Number) ! Added by: BrowseBox(Clarion)
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
    Majors::Used -= 1
    IF Majors::Used = 0 THEN CLOSE(Majors).
    Students::Used -= 1
    IF Students::Used = 0 THEN CLOSE(Students).
    Teachers::Used -= 1
    IF Teachers::Used = 0 THEN CLOSE(Teachers).
  END
  IF WindowOpened
    CLOSE(QuickWindow)
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
  IF QuickWindow{Prop:AcceptAll} THEN EXIT.
  Do LookupRelated
  DO BRW2::SelectSort
  DO BRW4::SelectSort
  ?Browse:2{Prop:VScrollPos} = BRW2::CurrentScroll
  ?Browse:4{Prop:VScrollPos} = BRW4::CurrentScroll
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
  DO BRW2::GetRecord
  DO BRW4::GetRecord
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
BRW2::AssignButtons ROUTINE
  CLEAR(BrowseButtons)
  BrowseButtons.ListBox = ?Browse:2
  BrowseButtons.InsertButton = ?Insert:3
  BrowseButtons.ChangeButton = ?Change:3
  BrowseButtons.DeleteButton = ?Delete:3
  DO DisplayBrowseToolbar
!----------------------------------------------------------------------
BRW2::SelectSort ROUTINE
!|
!| This routine is called during the RefreshWindow ROUTINE present in every window procedure.
!| The purpose of this routine is to make certain that the BrowseBox is always current with your
!| user's selections. This routine...
!|
!| 1. Checks to see if any of your specified sort-order conditions are met, and if so, changes the sort order.
!| 2. If no sort order change is necessary, this routine checks to see if any of your Reset Fields has changed.
!| 3. If the sort order has changed, or if a reset field has changed, or if the ForceRefresh flag is set...
!|    a. The current record is retrieved from the disk.
!|    b. If the BrowseBox is accessed for the first time, and the Browse has been called to select a record,
!|       the page containing the current record is loaded.
!|    c. If the BrowseBox is accessed for the first time, and the Browse has not been called to select a
!|       record, the first page of information is loaded.
!|    d. If the BrowseBox is not being accessed for the first time, and the Browse sort order has changed, the
!|       new "first" page of information is loaded.
!|    e. If the BrowseBox is not being accessed for the first time, and the Browse sort order hasn't changes,
!|       the page containing the current record is reloaded.
!|    f. The record buffer is refilled from the currently highlighted BrowseBox item.
!|    f. The BrowseBox is reinitialized (BRW2::InitializeBrowse ROUTINE).
!| 4. If step 3 is not necessary, the record buffer is refilled from the currently highlighted BrowseBox item.
!|
  BRW2::LastSortOrder = BRW2::SortOrder
  BRW2::Changed = False
  IF BRW2::SortOrder = 0
    BRW2::SortOrder = 1
  END
  IF BRW2::SortOrder = BRW2::LastSortOrder
    CASE BRW2::SortOrder
    OF 1
      IF BRW2::Sort1:Reset:MAJ:Number <> MAJ:Number
        BRW2::Changed = True
      END
    END
  ELSE
  END
  IF BRW2::SortOrder <> BRW2::LastSortOrder OR BRW2::Changed OR ForceRefresh OR (BRW2::LoadPending AND ?Browse:2{PROP:VISIBLE})
    CASE BRW2::SortOrder
    OF 1
      BRW2::Sort1:Reset:MAJ:Number = MAJ:Number
    END
    DO BRW2::GetRecord
    DO BRW2::Reset
    IF BRW2::LastSortOrder = 0
      IF LocalRequest = SelectRecord
        BRW2::LocateMode = LocateOnValue
        DO BRW2::LocateRecord
      ELSE
        FREE(Queue:Browse:2)
        BRW2::RefreshMode = RefreshOnTop
        DO BRW2::RefreshPage
        DO BRW2::PostNewSelection
      END
    ELSE
      IF BRW2::Changed
        FREE(Queue:Browse:2)
        BRW2::RefreshMode = RefreshOnTop
        DO BRW2::RefreshPage
        DO BRW2::PostNewSelection
      ELSE
        BRW2::LocateMode = LocateOnValue
        DO BRW2::LocateRecord
      END
    END
    IF BRW2::RecordCount
      GET(Queue:Browse:2,BRW2::CurrentChoice)
      DO BRW2::FillBuffer
    END
    DO BRW2::InitializeBrowse
  ELSE
    IF BRW2::RecordCount
      GET(Queue:Browse:2,BRW2::CurrentChoice)
      DO BRW2::FillBuffer
    END
  END
!----------------------------------------------------------------------
BRW2::InitializeBrowse ROUTINE
!|
!| This routine initializes the BrowseBox control template. This routine is called when...
!|
!| The BrowseBox sort order has changed. This includes the first time the BrowseBox is accessed.
!| The BrowseBox returns from a record update.
!|
!| This routine performs two main functions.
!|   1. Computes all BrowseBox totals. All records that satisfy the current selection criteria
!|      are read, and totals computed. If no totals are present, this section is not generated,
!|      and may not be present in the code below.
!|   2. Calculates any runtime scrollbar positions. Again, if runtime scrollbars are not used,
!|      the code for runtime scrollbar computation will not be present.
!|
  IF NOT BRW2::ActiveInvisible THEN
     IF NOT ?Browse:2{PROP:Visible} THEN
        BRW2::LoadPending = True
        EXIT
     END
  END
  DO BRW2::Reset
  PREVIOUS(BRW2::View:Browse)
  IF ERRORCODE()
    IF ERRORCODE() = BadRecErr
      DO BRW2::RestoreResetValues
    ELSE
      StandardWarning(Warn:RecordFetchError,'Teachers')
      POST(Event:CloseWindow)
    END
    EXIT
  END
  CASE BRW2::SortOrder
  OF 1
    BRW2::Sort1:HighValue = TEA:Department
  END
  DO BRW2::Reset
  NEXT(BRW2::View:Browse)
  IF ERRORCODE()
    IF ERRORCODE() = BadRecErr
      DO BRW2::RestoreResetValues
    ELSE
      StandardWarning(Warn:RecordFetchError,'Teachers')
      POST(Event:CloseWindow)
    END
    EXIT
  END
  CASE BRW2::SortOrder
  OF 1
    BRW2::Sort1:LowValue = TEA:Department
    SetupRealStops(BRW2::Sort1:LowValue,BRW2::Sort1:HighValue)
    LOOP BRW2::ScrollRecordCount = 1 TO 100
      BRW2::Sort1:KeyDistribution[BRW2::ScrollRecordCount] = NextRealStop()
    END
  END
!----------------------------------------------------------------------
BRW2::FillBuffer ROUTINE
!|
!| This routine fills the record buffer from the BrowseBox queue. This gives the appearance
!| that the record is "fresh" from the disk, without the disk access required.
!|
  TEA:FirstName = BRW2::TEA:FirstName
  TEA:LastName = BRW2::TEA:LastName
  TEA:Address = BRW2::TEA:Address
  TEA:City = BRW2::TEA:City
  TEA:State = BRW2::TEA:State
  TEA:Zip = BRW2::TEA:Zip
  TEA:Telephone = BRW2::TEA:Telephone
  TEA:Number = BRW2::TEA:Number
  TEA:Department = BRW2::TEA:Department
!----------------------------------------------------------------------
BRW2::FillQueue ROUTINE
!|
!| This routine is used to fill the BrowseBox QUEUE from several sources.
!|
!| First, all Format Browse formulae are processed.
!|
!| Next, each field of the BrowseBox is processed. For each field...
!|
!|    The value of the field is placed in the BrowseBox queue.
!|
!| Finally, the POSITION of the current VIEW record is added to the QUEUE
!|
  BRW2::TEA:FirstName = TEA:FirstName
  BRW2::TEA:LastName = TEA:LastName
  BRW2::TEA:Address = TEA:Address
  BRW2::TEA:City = TEA:City
  BRW2::TEA:State = TEA:State
  BRW2::TEA:Zip = TEA:Zip
  BRW2::TEA:Telephone = TEA:Telephone
  BRW2::TEA:Number = TEA:Number
  BRW2::TEA:Department = TEA:Department
  BRW2::Position = POSITION(BRW2::View:Browse)
!----------------------------------------------------------------------
BRW2::PostNewSelection ROUTINE
!|
!| This routine is used to post the NewSelection EVENT to the window. Because we only want this
!| EVENT processed once, and becuase there are several routines that need to initiate a NewSelection
!| EVENT, we keep a flag that tells us if the EVENT is already waiting to be processed. The EVENT is
!| only POSTed if this flag is false.
!|
  IF NOT BRW2::NewSelectPosted
    BRW2::NewSelectPosted = True
    POST(EVENT:NewSelection,?Browse:2)
  END
!----------------------------------------------------------------------
BRW2::NewSelection ROUTINE
!|
!| This routine performs any window bookkeeping necessary when a new record is selected in the
!| BrowseBox.
!| 1. If the new selection is made with the right mouse button, the popup menu (if applicable) is
!|    processed.
!| 2. The current record is retrieved into the buffer using the BRW2::FillBuffer ROUTINE.
!|    After this, the current vertical scrollbar position is computed, and the scrollbar positioned.
!|
  IF NOT BRW2::ActiveInvisible THEN
     IF NOT ?Browse:2{PROP:Visible} THEN
        BRW2::LoadPending = True
        EXIT
     END
  END
  BRW2::NewSelectPosted = False
  IF KEYCODE() = MouseRight OR KEYCODE() = AppsKey
     SETKEYCODE(0)
    BRW2::PopupText = ''
    IF BRW2::RecordCount
      IF BRW2::PopupText
        BRW2::PopupText = '&Insert|&Change|&Delete|-|' & BRW2::PopupText
      ELSE
        BRW2::PopupText = '&Insert|&Change|&Delete'
      END
    ELSE
      IF BRW2::PopupText
        BRW2::PopupText = '&Insert|~&Change|~&Delete|-|' & BRW2::PopupText
      ELSE
        BRW2::PopupText = '&Insert|~&Change|~&Delete'
      END
    END
    EXECUTE(POPUP(BRW2::PopupText))
      POST(EVENT:Accepted,?Insert:3)
      POST(EVENT:Accepted,?Change:3)
      POST(EVENT:Accepted,?Delete:3)
    END
  ELSIF BRW2::RecordCount
    BRW2::CurrentChoice = CHOICE(?Browse:2)
    GET(Queue:Browse:2,BRW2::CurrentChoice)
    DO BRW2::FillBuffer
    IF BRW2::RecordCount = ?Browse:2{PROP:Items}
      IF NOT ?Browse:2{PROP:VScroll}
        ?Browse:2{PROP:VScroll} = True
      END
      CASE BRW2::SortOrder
      OF 1
        LOOP BRW2::CurrentScroll = 1 TO 100
          IF BRW2::Sort1:KeyDistribution[BRW2::CurrentScroll] => TEA:Department
            IF BRW2::CurrentScroll <= 1
              BRW2::CurrentScroll = 0
            ELSIF BRW2::CurrentScroll = 100
              BRW2::CurrentScroll = 100
            ELSE
            END
            BREAK
          END
        END
      END
    ELSE
      IF ?Browse:2{PROP:VScroll}
        ?Browse:2{PROP:VScroll} = False
      END
    END
    DO RefreshWindow
  END
!---------------------------------------------------------------------
BRW2::ProcessScroll ROUTINE
!|
!| This routine processes any of the six scrolling EVENTs handled by the BrowseBox.
!| If one record is to be scrolled, the ROUTINE BRW2::ScrollOne is called.
!| If a page of records is to be scrolled, the ROUTINE BRW2::ScrollPage is called.
!| If the first or last page is to be displayed, the ROUTINE BRW2::ScrollEnd is called.
!|
!| If an incremental locator is in use, the value of that locator is cleared.
!| Finally, if a Fixed Thumb vertical scroll bar is used, the thumb is positioned.
!|
  IF BRW2::RecordCount
    BRW2::CurrentEvent = EVENT()
    CASE BRW2::CurrentEvent
    OF EVENT:ScrollUp OROF EVENT:ScrollDown
      DO BRW2::ScrollOne
    OF EVENT:PageUp OROF EVENT:PageDown
      DO BRW2::ScrollPage
    OF EVENT:ScrollTop OROF EVENT:ScrollBottom
      DO BRW2::ScrollEnd
    END
    ?Browse:2{PROP:SelStart} = BRW2::CurrentChoice
    DO BRW2::PostNewSelection
  END
!----------------------------------------------------------------------
BRW2::ScrollOne ROUTINE
!|
!| This routine is used to scroll a single record on the BrowseBox. Since the BrowseBox is an IMM
!| listbox, all scrolling must be handled in code. When called, this routine...
!|
!| 1. Sees if scrolling in the intended direction will cause the listbox display to shift. If not,
!|    the routine moves the list box cursor and exits.
!| 2. Calls BRW2::FillRecord to retrieve one record in the direction required.
!|
  IF BRW2::CurrentEvent = EVENT:ScrollUp AND BRW2::CurrentChoice > 1
    BRW2::CurrentChoice -= 1
    EXIT
  ELSIF BRW2::CurrentEvent = EVENT:ScrollDown AND BRW2::CurrentChoice < BRW2::RecordCount
    BRW2::CurrentChoice += 1
    EXIT
  END
  BRW2::ItemsToFill = 1
  BRW2::FillDirection = BRW2::CurrentEvent - 2
  DO BRW2::FillRecord
!----------------------------------------------------------------------
BRW2::ScrollPage ROUTINE
!|
!| This routine is used to scroll a single page of records on the BrowseBox. Since the BrowseBox is
!| an IMM listbox, all scrolling must be handled in code. When called, this routine...
!|
!| 1. Calls BRW2::FillRecord to retrieve one page of records in the direction required.
!| 2. If BRW2::FillRecord doesn't fill a page (BRW2::ItemsToFill > 0), then
!|    the list-box cursor ia shifted.
!|
  BRW2::ItemsToFill = ?Browse:2{PROP:Items}
  BRW2::FillDirection = BRW2::CurrentEvent - 4
  DO BRW2::FillRecord                           ! Fill with next read(s)
  IF BRW2::ItemsToFill
    IF BRW2::CurrentEvent = EVENT:PageUp
      BRW2::CurrentChoice -= BRW2::ItemsToFill
      IF BRW2::CurrentChoice < 1
        BRW2::CurrentChoice = 1
      END
    ELSE
      BRW2::CurrentChoice += BRW2::ItemsToFill
      IF BRW2::CurrentChoice > BRW2::RecordCount
        BRW2::CurrentChoice = BRW2::RecordCount
      END
    END
  END
!----------------------------------------------------------------------
BRW2::ScrollEnd ROUTINE
!|
!| This routine is used to load the first or last page of the displayable set of records.
!| Since the BrowseBox is an IMM listbox, all scrolling must be handled in code. When called,
!| this routine...
!|
!| 1. Resets the BrowseBox VIEW to insure that it reads from the end of the current sort order.
!| 2. Calls BRW2::FillRecord to retrieve one page of records.
!| 3. Selects the record that represents the end of the view. That is, if the first page was loaded,
!|    the first record is highlighted. If the last was loaded, the last record is highlighted.
!|
  FREE(Queue:Browse:2)
  BRW2::RecordCount = 0
  DO BRW2::Reset
  BRW2::ItemsToFill = ?Browse:2{PROP:Items}
  IF BRW2::CurrentEvent = EVENT:ScrollTop
    BRW2::FillDirection = FillForward
  ELSE
    BRW2::FillDirection = FillBackward
  END
  DO BRW2::FillRecord                           ! Fill with next read(s)
  IF BRW2::CurrentEvent = EVENT:ScrollTop
    BRW2::CurrentChoice = 1
  ELSE
    BRW2::CurrentChoice = BRW2::RecordCount
  END
!----------------------------------------------------------------------
BRW2::AlertKey ROUTINE
!|
!| This routine processes any KEYCODEs experienced by the BrowseBox.
!| NOTE: The cursor movement keys are not processed as KEYCODEs. They are processed as the
!|       appropriate BrowseBox scrolling and selection EVENTs.
!| This routine includes handling for double-click. Actually, this handling is in the form of
!| EMBEDs, which are filled by child-control templates.
!| This routine also includes the BrowseBox's locator handling.
!| After a value is entered for locating, this routine sets BRW2::LocateMode to a value
!| of 2 -- EQUATEd to LocateOnValue -- and calls the routine BRW2::LocateRecord.
!|
  IF BRW2::RecordCount
    CASE KEYCODE()                                         ! What keycode was hit
    OF AppsKey
    OROF MouseRight
       IF ?Browse:2{PROPLIST:MouseDownRow}>0
         ?Browse:2{PROP:Selected} = ?Browse:2{PROPLIST:MouseDownRow}
         BRW2::CurrentChoice = CHOICE(?Browse:2)
       END
       DO BRW2::NewSelection
    OF MouseLeft2
      POST(EVENT:Accepted,?Change:3)
      DO BRW2::FillBuffer
    OF InsertKey
      POST(EVENT:Accepted,?Insert:3)
    OF DeleteKey
      POST(EVENT:Accepted,?Delete:3)
    OF CtrlEnter
      POST(EVENT:Accepted,?Change:3)
    END                                                    ! END (What keycode was hit)
  ELSE
    CASE KEYCODE()                                         ! What keycode was hit
    OF AppsKey
    OROF MouseRight
       DO BRW2::NewSelection
    OF InsertKey
      POST(EVENT:Accepted,?Insert:3)
    ELSE                                                   ! ELSE (What keycode was hit)
      CASE BRW2::SortOrder
      OF 1
      END
    END
  END
  DO BRW2::PostNewSelection
!----------------------------------------------------------------------
BRW2::ScrollDrag ROUTINE
!|
!| This routine processes the Vertical Scroll Bar arrays to find the free key field value
!| that corresponds to the current scroll bar position.
!|
!| After the scroll position is computed, and the scroll value found, this routine sets
!| BRW2::LocateMode to that scroll value of 2 -- EQUATEd to LocateOnValue --
!| and calls the routine BRW2::LocateRecord.
!|
  IF ?Browse:2{PROP:VScrollPos} <= 1
    POST(EVENT:ScrollTop,?Browse:2)
  ELSIF ?Browse:2{PROP:VScrollPos} = 100
    POST(EVENT:ScrollBottom,?Browse:2)
  ELSE
    CASE BRW2::SortOrder
    OF 1
      TEA:Department = BRW2::Sort1:KeyDistribution[?Browse:2{PROP:VScrollPos}]
      BRW2::LocateMode = LocateOnValue
      DO BRW2::LocateRecord
    END
  END
!----------------------------------------------------------------------
BRW2::FillRecord ROUTINE
!|
!| This routine is used to retrieve a number of records from the VIEW. The number of records
!| retrieved is held in the variable BRW2::ItemsToFill. If more than one record is
!| to be retrieved, QuickScan is used to minimize reads from the disk.
!|
!| If records exist in the queue (in other words, if the browse has been used before), the record
!| at the appropriate end of the list box is retrieved, and the VIEW is reset to read starting
!| at that record.
!|
!| Next, the VIEW is accessed to retrieve BRW2::ItemsToFill records. Normally, this will
!| result in BRW2::ItemsToFill records being read from the VIEW, but if custom filtering
!| or range limiting is used (via the BRW2::ValidateRecord routine) then any number of records
!| might be read.
!|
!| For each good record, if BRW2::AddQueue is true, the queue is filled using the BRW2::FillQueue
!| routine. The record is then added to the queue. If adding this record causes the BrowseBox queue
!| to contain more records than can be displayed, the record at the opposite end of the queue is
!| deleted.
!|
!| The only time BRW2::AddQueue is false is when the BRW2::LocateRecord routine needs to
!| get the closest record to its record to be located. At this time, the record doesn't need to be
!| added to the queue, so it isn't.
!|
  IF BRW2::RecordCount
    IF BRW2::FillDirection = FillForward
      GET(Queue:Browse:2,BRW2::RecordCount)                ! Get the first queue item
    ELSE
      GET(Queue:Browse:2,1)                                ! Get the first queue item
    END
    RESET(BRW2::View:Browse,BRW2::Position)                ! Reset for sequential processing
    BRW2::SkipFirst = TRUE
  ELSE
    BRW2::SkipFirst = FALSE
  END
  LOOP WHILE BRW2::ItemsToFill
    IF BRW2::View:Browse{PROP:IPRequestCount} = 0
       BRW2::View:Browse{PROP:IPRequestCount} = BRW2::ItemsToFill
    END
    IF BRW2::FillDirection = FillForward
      NEXT(BRW2::View:Browse)
    ELSE
      PREVIOUS(BRW2::View:Browse)
    END
    IF ERRORCODE()
      IF ERRORCODE() = BadRecErr
        DO BRW2::RestoreResetValues
        BREAK
      ELSE
        StandardWarning(Warn:RecordFetchError,'Teachers')
        POST(EVENT:CloseWindow)
        EXIT
      END
    END
    IF BRW2::SkipFirst
       BRW2::SkipFirst = FALSE
       IF POSITION(BRW2::View:Browse) = BRW2::Position
          CYCLE
       END
    END
    IF BRW2::AddQueue
      IF BRW2::RecordCount = ?Browse:2{PROP:Items}
        IF BRW2::FillDirection = FillForward
          GET(Queue:Browse:2,1)                            ! Get the first queue item
        ELSE
          GET(Queue:Browse:2,BRW2::RecordCount)            ! Get the first queue item
        END
        DELETE(Queue:Browse:2)
        BRW2::RecordCount -= 1
      END
      DO BRW2::FillQueue
      IF BRW2::FillDirection = FillForward
        ADD(Queue:Browse:2)
      ELSE
        ADD(Queue:Browse:2,1)
      END
      BRW2::RecordCount += 1
    END
    BRW2::ItemsToFill -= 1
  END
  BRW2::AddQueue = True
  EXIT
!----------------------------------------------------------------------
BRW2::LocateRecord ROUTINE
!|
!| This routine is used to find a record in the VIEW, and to display that record
!| in the BrowseBox.
!|
!| This routine has three different modes of operation, which are invoked based on
!| the setting of BRW2::LocateMode. These modes are...
!|
!|   LocateOnPosition (1) - This mode is still supported for 1.5 compatability. This mode
!|                          is the same as LocateOnEdit.
!|   LocateOnValue    (2) - The values of the current sort order key are used. This mode
!|                          used for Locators and when the BrowseBox is called to select
!|                          a record.
!|   LocateOnEdit     (3) - The current record of the VIEW is used. This mode assumes
!|                          that there is an active VIEW record. This mode is used when
!|                          the sort order of the BrowseBox has changed
!|
!| If an appropriate record has been located, the BRW2::RefreshPage routine is
!| called to load the page containing the located record.
!|
!| If an appropriate record is not locate, the last page of the BrowseBox is loaded.
!|
  IF BRW2::LocateMode = LocateOnPosition
    BRW2::LocateMode = LocateOnEdit
  END
  CLOSE(BRW2::View:Browse)
  CASE BRW2::SortOrder
  OF 1
    IF BRW2::UsingAdditionalSortOrder THEN
       BRW2::UsingAdditionalSortOrder = False
       BRW2::View:Browse{PROP:Order} = '+TEA:Department'
    END
    IF BRW2::LocateMode = LocateOnEdit
      BRW2::HighlightedPosition = POSITION(TEA:KeyDepartment)
      RESET(TEA:KeyDepartment,BRW2::HighlightedPosition)
    ELSE
      TEA:Department = MAJ:Number
      SET(TEA:KeyDepartment,TEA:KeyDepartment)
    END
    BRW2::View:Browse{Prop:Filter} = 'TEA:Department = BRW2::Sort1:Reset:MAJ:Number'
  END
  IF ERRORCODE()
    StandardWarning(Warn:ViewOpenError)
  END
  OPEN(BRW2::View:Browse)
  IF ERRORCODE()
    StandardWarning(Warn:ViewOpenError)
  END
  IF BRW2::UsingAdditionalSortOrder = True
    CASE BRW2::SortOrder
    OF 1
       SET(BRW2::View:Browse,1)
    END
  END
  FREE(Queue:Browse:2)
  BRW2::RecordCount = 0
  BRW2::ItemsToFill = 1
  BRW2::FillDirection = FillForward                        ! Fill with next read(s)
  BRW2::AddQueue = False
  DO BRW2::FillRecord                                      ! Fill with next read(s)
  BRW2::AddQueue = True
  IF BRW2::ItemsToFill
    BRW2::RefreshMode = RefreshOnBottom
    DO BRW2::RefreshPage
  ELSE
    BRW2::RefreshMode = RefreshOnPosition
    DO BRW2::RefreshPage
  END
  DO BRW2::PostNewSelection
  BRW2::LocateMode = 0
  EXIT
!----------------------------------------------------------------------
BRW2::RefreshPage ROUTINE
!|
!| This routine is used to load a single page of the BrowseBox.
!|
!| If this routine is called with a BRW2::RefreshMode of RefreshOnPosition,
!| the active VIEW record is loaded at the top of the page. Otherwise, if there are
!| records in the browse queue (Queue:Browse:2), then the current page is reloaded, and the
!| currently selected item remains selected.
!|
  IF NOT BRW2::ActiveInvisible THEN
     IF NOT ?Browse:2{PROP:Visible} THEN
        BRW2::LoadPending = True
        EXIT
     END
  END
  SETCURSOR(Cursor:Wait)
  IF BRW2::RefreshMode = RefreshOnPosition
    BRW2::HighlightedPosition = POSITION(BRW2::View:Browse)
    RESET(BRW2::View:Browse,BRW2::HighlightedPosition)
    BRW2::RefreshMode = RefreshOnTop
  ELSIF RECORDS(Queue:Browse:2)
    GET(Queue:Browse:2,BRW2::CurrentChoice)
    IF ERRORCODE()
      GET(Queue:Browse:2,RECORDS(Queue:Browse:2))
    END
    BRW2::HighlightedPosition = BRW2::Position
    GET(Queue:Browse:2,1)
    RESET(BRW2::View:Browse,BRW2::Position)
    BRW2::RefreshMode = RefreshOnCurrent
  ELSE
    BRW2::HighlightedPosition = ''
    DO BRW2::Reset
  END
  FREE(Queue:Browse:2)
  BRW2::RecordCount = 0
  BRW2::ItemsToFill = ?Browse:2{PROP:Items}
  IF BRW2::RefreshMode = RefreshOnBottom
    BRW2::FillDirection = FillBackward
  ELSE
    BRW2::FillDirection = FillForward
  END
  DO BRW2::FillRecord                                      ! Fill with next read(s)
  IF BRW2::HighlightedPosition
    IF BRW2::ItemsToFill
      IF NOT BRW2::RecordCount
        DO BRW2::Reset
      END
      IF BRW2::RefreshMode = RefreshOnBottom
        BRW2::FillDirection = FillForward
      ELSE
        BRW2::FillDirection = FillBackward
      END
      DO BRW2::FillRecord
    END
  END
  IF BRW2::RecordCount
    IF BRW2::HighlightedPosition
      LOOP BRW2::CurrentChoice = 1 TO BRW2::RecordCount
        GET(Queue:Browse:2,BRW2::CurrentChoice)
        IF BRW2::Position = BRW2::HighlightedPosition THEN BREAK.
      END
      IF BRW2::CurrentChoice > BRW2::RecordCount
        BRW2::CurrentChoice = BRW2::RecordCount
      END
    ELSE
      IF BRW2::RefreshMode = RefreshOnBottom
        BRW2::CurrentChoice = RECORDS(Queue:Browse:2)
      ELSE
        BRW2::CurrentChoice = 1
      END
    END
    ?Browse:2{Prop:Selected} = BRW2::CurrentChoice
    DO BRW2::FillBuffer
    ?Change:3{PROP:Disable} = 0
    ?Delete:3{PROP:Disable} = 0
  ELSE
    CLEAR(TEA:Record)
    BRW2::CurrentChoice = 0
    ?Change:3{PROP:Disable} = 1
    ?Delete:3{PROP:Disable} = 1
  END
  SETCURSOR()
  BRW2::RefreshMode = 0
  EXIT
BRW2::Reset ROUTINE
!|
!| This routine is used to reset the VIEW used by the BrowseBox.
!|
  IF NOT BRW2::ActiveInvisible THEN
     IF NOT ?Browse:2{PROP:Visible} THEN
        BRW2::LoadPending = True
        EXIT
     END
  END
  BRW2::LoadPending = False
  CLOSE(BRW2::View:Browse)
  CASE BRW2::SortOrder
  OF 1
    IF BRW2::UsingAdditionalSortOrder THEN
       BRW2::UsingAdditionalSortOrder = False
       BRW2::View:Browse{PROP:Order} = '+TEA:Department'
    END
    TEA:Department = MAJ:Number
    SET(TEA:KeyDepartment)
    BRW2::View:Browse{Prop:Filter} = 'TEA:Department = BRW2::Sort1:Reset:MAJ:Number'
  END
  IF ERRORCODE()
    StandardWarning(Warn:ViewOpenError)
  END
  OPEN(BRW2::View:Browse)
  IF ERRORCODE()
    StandardWarning(Warn:ViewOpenError)
  END
  SET(BRW2::View:Browse)
!----------------------------------------------------------------------
BRW2::GetRecord ROUTINE
!|
!| This routine is used to retrieve the VIEW record that corresponds to a
!| chosen listbox record.
!|
  IF BRW2::RecordCount
    BRW2::CurrentChoice = CHOICE(?Browse:2)
    GET(Queue:Browse:2,BRW2::CurrentChoice)
    WATCH(BRW2::View:Browse)
    REGET(BRW2::View:Browse,BRW2::Position)
  END
!----------------------------------------------------------------------
BRW2::RestoreResetValues ROUTINE
!|
!| This routine is used to restore reset values to their saved value
!| after a bad record access from the VIEW.
!|
  CASE BRW2::SortOrder
  OF 1
    MAJ:Number = BRW2::Sort1:Reset:MAJ:Number
  END
!--------------------------------------------------------------------------
DisplayBrowseToolbar      ROUTINE
  ENABLE(TBarBrwBottom,TBarBrwLocate)
  TBarBrwHelp{PROP:DISABLE}=?Help{PROP:DISABLE}
  IF BrowseButtons.InsertButton THEN
    TBarBrwInsert{PROP:Disable} = BrowseButtons.InsertButton{PROP:Disable}
  END
  IF BrowseButtons.ChangeButton THEN
    TBarBrwChange{PROP:Disable} = BrowseButtons.ChangeButton{PROP:Disable}
  END
  IF BrowseButtons.DeleteButton THEN
    TBarBrwDelete{PROP:Disable} = BrowseButtons.DeleteButton{PROP:Disable}
  END
  DISABLE(TBarBrwHistory)
  ToolBarMode = BrowseMode
  TBarBrwDown{PROP:ToolTip} = 'Go to the Next Record'
  TBarBrwBottom{PROP:ToolTip} = 'Go to the Last Page'
  TBarBrwTop{PROP:ToolTip} = 'Go to the First Page'
  TBarBrwPageDown{PROP:ToolTip} = 'Go to the Next Page'
  TBarBrwPageUp{PROP:ToolTip} = 'Go to the Prior Page'
  TBarBrwDown{PROP:ToolTip} = 'Go to the Next Record'
  TBarBrwUP{PROP:ToolTip} = 'Go to the Prior Record'
  TBarBrwInsert{PROP:ToolTip} = 'Insert a new Record'
  DISPLAY(TBarBrwFirst,TBarBrwLast)
!--------------------------------------------------------------------------
ListBoxDispatch ROUTINE
  DO DisplayBrowseToolbar
  IF FOCUS() <> BrowseButtons.ListBox THEN  ! List box must have focus when on update form
    EXIT
  END
  IF ACCEPTED() THEN            !trap remote browse box control calls
    EXECUTE(ACCEPTED()-TBarBrwBottom+1)
      POST(EVENT:ScrollBottom,BrowseButtons.ListBox)
      POST(EVENT:ScrollTop,BrowseButtons.ListBox)
      POST(EVENT:PageDown,BrowseButtons.ListBox)
      POST(EVENT:PageUp,BrowseButtons.ListBox)
      POST(EVENT:ScrollDown,BrowseButtons.ListBox)
      POST(EVENT:ScrollUp,BrowseButtons.ListBox)
      POST(EVENT:Locate,BrowseButtons.ListBox)
      BEGIN                     !EXECUTE Place Holder - Ditto has no effect on a browse
      END
      PRESSKEY(F1Key)
    END
  END

UpdateDispatch ROUTINE
  DISABLE(TBarBrwDelete)
  DISABLE(TBarBrwChange)
  IF BrowseButtons.DeleteButton AND BrowseButtons.DeleteButton{PROP:Disable} = 0 THEN
    ENABLE(TBarBrwDelete)
  END
  IF BrowseButtons.ChangeButton AND BrowseButtons.ChangeButton{PROP:Disable} = 0 THEN
    ENABLE(TBarBrwChange)
  END
  IF FOCUS() <> BrowseButtons.ListBox THEN  ! List box must have focus when on update form
    EXIT
  END
  IF INRANGE(ACCEPTED(),TBarBrwInsert,TBarBrwDelete) THEN         !trap remote browse update control calls
    EXECUTE(ACCEPTED()-TBarBrwInsert+1)
      POST(EVENT:Accepted,BrowseButtons.InsertButton)
      POST(EVENT:Accepted,BrowseButtons.ChangeButton)
      POST(EVENT:Accepted,BrowseButtons.DeleteButton)
    END
  END
!----------------------------------------------------------------
BRW2::ButtonInsert ROUTINE
!|
!| This routine calls the BrowseBox's update procedure (as specified in the
!| BrowseUpdateButtons control template) to insert a new record.
!|
!| First, the primary file's record  buffer is cleared, as well as any memos
!| or BLOBs. Next, any range-limit values are restored so that the inserted
!| record defaults to being added to the current display set.
!|
!| Next, LocalRequest is set to InsertRecord, and the BRW2::CallRecord routine
!| is called. This routine performs the actual call to the update procedure.
!|
!| If the insert is successful (GlobalRequest = RequestCompleted) then the newly added
!| record is displayed in the BrowseBox, at the top of the listbox.
!|
!| If the insert is not successful, the current page of the browse is refreshed.
!|
!| Finally, The BrowseBox is re-initialized, resetting scroll bars and totals.
!|
  GET(Teachers,0)
  CLEAR(TEA:Record,0)
  CASE BRW2::SortOrder
  OF 1
    TEA:Department = BRW2::Sort1:Reset:MAJ:Number
  END
  LocalRequest = InsertRecord
  DO BRW2::CallUpdate
  IF GlobalResponse = RequestCompleted
    BRW2::LocateMode = LocateOnEdit
    DO BRW2::LocateRecord
  ELSE
    BRW2::RefreshMode = RefreshOnQueue
    DO BRW2::RefreshPage
  END
  DO BRW2::InitializeBrowse
  DO BRW2::PostNewSelection
  SELECT(?Browse:2)
  LocalRequest = OriginalRequest
  LocalResponse = RequestCancelled
  DO RefreshWindow
!----------------------------------------------------------------
BRW2::ButtonChange ROUTINE
!|
!| This routine calls the BrowseBox's update procedure (as specified in the
!| BrowseUpdateButtons control template) to change a selected record.
!|
!| Whenever a button is pressed, the first thing that happens is that the
!| SyncWindow routine is called. This routine insures that the BrowseBox's
!| VIEW corresponds to the highlighted record by calling the BRW2::GetRecord routine.
!|
!| First, LocalRequest is set to ChangeRecord, and the BRW2::CallRecord routine
!| is called. This routine performs the actual call to the update procedure.
!|
!| If the change is successful (GlobalRequest = RequestCompleted) then the newly changed
!| record is displayed in the BrowseBox.
!|
!| If the change is not successful, the current page of the browse is refreshed.
!|
!| Finally, The BrowseBox is re-initialized, resetting scroll bars and totals.
!|
  LocalRequest = ChangeRecord
  DO BRW2::CallUpdate
  IF GlobalResponse = RequestCompleted
    BRW2::LocateMode = LocateOnEdit
    DO BRW2::LocateRecord
  ELSE
    BRW2::RefreshMode = RefreshOnQueue
    DO BRW2::RefreshPage
  END
  DO BRW2::InitializeBrowse
  DO BRW2::PostNewSelection
  SELECT(?Browse:2)
  LocalRequest = OriginalRequest
  LocalResponse = RequestCancelled
  DO RefreshWindow
!----------------------------------------------------------------
BRW2::ButtonDelete ROUTINE
!|
!| This routine calls the BrowseBox's update procedure (as specified in the
!| BrowseUpdateButtons control template) to delete a selected record.
!|
!| Whenever a button is pressed, the first thing that happens is that the
!| SyncWindow routine is called. This routine insures that the BrowseBox's
!| VIEW corresponds to the highlighted record by calling the BRW2::GetRecord routine.
!|
!| First, LocalRequest is set to DeleteRecord, and the BRW2::CallRecord routine
!| is called. This routine performs the actual call to the update procedure.
!|
!| If the delete is successful (GlobalRequest = RequestCompleted) then the deleted record is
!| removed from the queue.
!|
!| Next, the BrowseBox is refreshed, redisplaying the current page.
!|
!| Finally, The BrowseBox is re-initialized, resetting scroll bars and totals.
!|
  LocalRequest = DeleteRecord
  DO BRW2::CallUpdate
  IF GlobalResponse = RequestCompleted
    DELETE(Queue:Browse:2)
    BRW2::RecordCount -= 1
  END
  BRW2::RefreshMode = RefreshOnQueue
  DO BRW2::RefreshPage
  DO BRW2::InitializeBrowse
  DO BRW2::PostNewSelection
  SELECT(?Browse:2)
  LocalRequest = OriginalRequest
  LocalResponse = RequestCancelled
  DO RefreshWindow
!----------------------------------------------------------------
BRW2::CallUpdate ROUTINE
!|
!| This routine performs the actual call to the update procedure.
!|
!| The first thing that happens is that the VIEW is closed. This is performed just in case
!| the VIEW is still open.
!|
!| Next, GlobalRequest is set the the value of LocalRequest, and the update procedure
!| (UpdateTeachers) is called.
!|
!| Upon return from the update, the routine BRW2::Reset is called to reset the VIEW
!| and reopen it.
!|
  CLOSE(BRW2::View:Browse)
  LOOP
    GlobalRequest = LocalRequest
    VCRRequest = VCRNone
    UpdateTeachers
    LocalResponse = GlobalResponse
    CASE VCRRequest
    OF VCRNone
      BREAK
    OF VCRInsert
      IF LocalRequest = ChangeRecord THEN
        LocalRequest = InsertRecord
      END
    OROF VCRForward
      IF LocalRequest = InsertRecord THEN
        GET(Teachers,0)
        CLEAR(TEA:Record,0)
      ELSE
        DO BRW2::PostVCREdit1
        BRW2::CurrentEvent = EVENT:ScrollDown
        DO BRW2::ScrollOne
        DO BRW2::PostVCREdit2
      END
    OF VCRBackward
      DO BRW2::PostVCREdit1
      BRW2::CurrentEvent = EVENT:ScrollUp
      DO BRW2::ScrollOne
      DO BRW2::PostVCREdit2
    OF VCRPageForward
      DO BRW2::PostVCREdit1
      BRW2::CurrentEvent = EVENT:PageDown
      DO BRW2::ScrollPage
      DO BRW2::PostVCREdit2
    OF VCRPageBackward
      DO BRW2::PostVCREdit1
      BRW2::CurrentEvent = EVENT:PageUp
      DO BRW2::ScrollPage
      DO BRW2::PostVCREdit2
    OF VCRFirst
      DO BRW2::PostVCREdit1
      BRW2::CurrentEvent = EVENT:ScrollTop
      DO BRW2::ScrollEnd
      DO BRW2::PostVCREdit2
    OF VCRLast
      DO BRW2::PostVCREdit1
      BRW2::CurrentEvent = EVENT:ScrollBottom
      DO BRW2::ScrollEND
      DO BRW2::PostVCREdit2
    END
  END
  DO BRW2::Reset

BRW2::PostVCREdit1 ROUTINE
  DO BRW2::Reset
  BRW2::LocateMode = LocateOnEdit
  DO BRW2::LocateRecord
  DO RefreshWindow

BRW2::PostVCREdit2 ROUTINE
  ?Browse:2{PROP:SelStart} = BRW2::CurrentChoice
  DO BRW2::NewSelection
  REGET(BRW2::View:Browse,BRW2::Position)
  CLOSE(BRW2::View:Browse)

BRW4::AssignButtons ROUTINE
  CLEAR(BrowseButtons)
  BrowseButtons.ListBox = ?Browse:4
  BrowseButtons.InsertButton = ?Insert:5
  BrowseButtons.ChangeButton = ?Change:5
  BrowseButtons.DeleteButton = ?Delete:5
  DO DisplayBrowseToolbar
!----------------------------------------------------------------------
BRW4::SelectSort ROUTINE
!|
!| This routine is called during the RefreshWindow ROUTINE present in every window procedure.
!| The purpose of this routine is to make certain that the BrowseBox is always current with your
!| user's selections. This routine...
!|
!| 1. Checks to see if any of your specified sort-order conditions are met, and if so, changes the sort order.
!| 2. If no sort order change is necessary, this routine checks to see if any of your Reset Fields has changed.
!| 3. If the sort order has changed, or if a reset field has changed, or if the ForceRefresh flag is set...
!|    a. The current record is retrieved from the disk.
!|    b. If the BrowseBox is accessed for the first time, and the Browse has been called to select a record,
!|       the page containing the current record is loaded.
!|    c. If the BrowseBox is accessed for the first time, and the Browse has not been called to select a
!|       record, the first page of information is loaded.
!|    d. If the BrowseBox is not being accessed for the first time, and the Browse sort order has changed, the
!|       new "first" page of information is loaded.
!|    e. If the BrowseBox is not being accessed for the first time, and the Browse sort order hasn't changes,
!|       the page containing the current record is reloaded.
!|    f. The record buffer is refilled from the currently highlighted BrowseBox item.
!|    f. The BrowseBox is reinitialized (BRW4::InitializeBrowse ROUTINE).
!| 4. If step 3 is not necessary, the record buffer is refilled from the currently highlighted BrowseBox item.
!|
  BRW4::LastSortOrder = BRW4::SortOrder
  BRW4::Changed = False
  IF BRW4::SortOrder = 0
    BRW4::SortOrder = 1
  END
  IF BRW4::SortOrder = BRW4::LastSortOrder
    CASE BRW4::SortOrder
    OF 1
      IF BRW4::Sort1:Reset:MAJ:Number <> MAJ:Number
        BRW4::Changed = True
      END
    END
  ELSE
  END
  IF BRW4::SortOrder <> BRW4::LastSortOrder OR BRW4::Changed OR ForceRefresh OR (BRW4::LoadPending AND ?Browse:4{PROP:VISIBLE})
    CASE BRW4::SortOrder
    OF 1
      BRW4::Sort1:Reset:MAJ:Number = MAJ:Number
    END
    DO BRW4::GetRecord
    DO BRW4::Reset
    IF BRW4::LastSortOrder = 0
      IF LocalRequest = SelectRecord
        BRW4::LocateMode = LocateOnValue
        DO BRW4::LocateRecord
      ELSE
        FREE(Queue:Browse:4)
        BRW4::RefreshMode = RefreshOnTop
        DO BRW4::RefreshPage
        DO BRW4::PostNewSelection
      END
    ELSE
      IF BRW4::Changed
        FREE(Queue:Browse:4)
        BRW4::RefreshMode = RefreshOnTop
        DO BRW4::RefreshPage
        DO BRW4::PostNewSelection
      ELSE
        BRW4::LocateMode = LocateOnValue
        DO BRW4::LocateRecord
      END
    END
    IF BRW4::RecordCount
      GET(Queue:Browse:4,BRW4::CurrentChoice)
      DO BRW4::FillBuffer
    END
    DO BRW4::InitializeBrowse
  ELSE
    IF BRW4::RecordCount
      GET(Queue:Browse:4,BRW4::CurrentChoice)
      DO BRW4::FillBuffer
    END
  END
!----------------------------------------------------------------------
BRW4::InitializeBrowse ROUTINE
!|
!| This routine initializes the BrowseBox control template. This routine is called when...
!|
!| The BrowseBox sort order has changed. This includes the first time the BrowseBox is accessed.
!| The BrowseBox returns from a record update.
!|
!| This routine performs two main functions.
!|   1. Computes all BrowseBox totals. All records that satisfy the current selection criteria
!|      are read, and totals computed. If no totals are present, this section is not generated,
!|      and may not be present in the code below.
!|   2. Calculates any runtime scrollbar positions. Again, if runtime scrollbars are not used,
!|      the code for runtime scrollbar computation will not be present.
!|
  IF NOT BRW4::ActiveInvisible THEN
     IF NOT ?Browse:4{PROP:Visible} THEN
        BRW4::LoadPending = True
        EXIT
     END
  END
  DO BRW4::Reset
  PREVIOUS(BRW4::View:Browse)
  IF ERRORCODE()
    IF ERRORCODE() = BadRecErr
      DO BRW4::RestoreResetValues
    ELSE
      StandardWarning(Warn:RecordFetchError,'Students')
      POST(Event:CloseWindow)
    END
    EXIT
  END
  CASE BRW4::SortOrder
  OF 1
    BRW4::Sort1:HighValue = STU:LastName
  END
  DO BRW4::Reset
  NEXT(BRW4::View:Browse)
  IF ERRORCODE()
    IF ERRORCODE() = BadRecErr
      DO BRW4::RestoreResetValues
    ELSE
      StandardWarning(Warn:RecordFetchError,'Students')
      POST(Event:CloseWindow)
    END
    EXIT
  END
  CASE BRW4::SortOrder
  OF 1
    BRW4::Sort1:LowValue = STU:LastName
    SetupStringStops(BRW4::Sort1:LowValue,BRW4::Sort1:HighValue,SIZE(BRW4::Sort1:LowValue),ScrollSort:AllowAlpha)
    LOOP BRW4::ScrollRecordCount = 1 TO 100
      BRW4::Sort1:KeyDistribution[BRW4::ScrollRecordCount] = NextStringStop()
    END
  END
!----------------------------------------------------------------------
BRW4::FillBuffer ROUTINE
!|
!| This routine fills the record buffer from the BrowseBox queue. This gives the appearance
!| that the record is "fresh" from the disk, without the disk access required.
!|
  STU:FirstName = BRW4::STU:FirstName
  STU:LastName = BRW4::STU:LastName
  STU:Address = BRW4::STU:Address
  STU:Address2 = BRW4::STU:Address2
  STU:City = BRW4::STU:City
  STU:State = BRW4::STU:State
  STU:Zip = BRW4::STU:Zip
  STU:Telephone = BRW4::STU:Telephone
  STU:GradYear = BRW4::STU:GradYear
  STU:Number = BRW4::STU:Number
  STU:Major = BRW4::STU:Major
!----------------------------------------------------------------------
BRW4::FillQueue ROUTINE
!|
!| This routine is used to fill the BrowseBox QUEUE from several sources.
!|
!| First, all Format Browse formulae are processed.
!|
!| Next, each field of the BrowseBox is processed. For each field...
!|
!|    The value of the field is placed in the BrowseBox queue.
!|
!| Finally, the POSITION of the current VIEW record is added to the QUEUE
!|
  BRW4::STU:FirstName = STU:FirstName
  BRW4::STU:LastName = STU:LastName
  BRW4::STU:Address = STU:Address
  BRW4::STU:Address2 = STU:Address2
  BRW4::STU:City = STU:City
  BRW4::STU:State = STU:State
  BRW4::STU:Zip = STU:Zip
  BRW4::STU:Telephone = STU:Telephone
  BRW4::STU:GradYear = STU:GradYear
  BRW4::STU:Number = STU:Number
  BRW4::STU:Major = STU:Major
  BRW4::Position = POSITION(BRW4::View:Browse)
!----------------------------------------------------------------------
BRW4::PostNewSelection ROUTINE
!|
!| This routine is used to post the NewSelection EVENT to the window. Because we only want this
!| EVENT processed once, and becuase there are several routines that need to initiate a NewSelection
!| EVENT, we keep a flag that tells us if the EVENT is already waiting to be processed. The EVENT is
!| only POSTed if this flag is false.
!|
  IF NOT BRW4::NewSelectPosted
    BRW4::NewSelectPosted = True
    POST(EVENT:NewSelection,?Browse:4)
  END
!----------------------------------------------------------------------
BRW4::NewSelection ROUTINE
!|
!| This routine performs any window bookkeeping necessary when a new record is selected in the
!| BrowseBox.
!| 1. If the new selection is made with the right mouse button, the popup menu (if applicable) is
!|    processed.
!| 2. The current record is retrieved into the buffer using the BRW4::FillBuffer ROUTINE.
!|    After this, the current vertical scrollbar position is computed, and the scrollbar positioned.
!|
  IF NOT BRW4::ActiveInvisible THEN
     IF NOT ?Browse:4{PROP:Visible} THEN
        BRW4::LoadPending = True
        EXIT
     END
  END
  BRW4::NewSelectPosted = False
  IF KEYCODE() = MouseRight OR KEYCODE() = AppsKey
     SETKEYCODE(0)
    BRW4::PopupText = ''
    IF BRW4::RecordCount
      IF BRW4::PopupText
        BRW4::PopupText = '&Insert|&Change|&Delete|-|' & BRW4::PopupText
      ELSE
        BRW4::PopupText = '&Insert|&Change|&Delete'
      END
    ELSE
      IF BRW4::PopupText
        BRW4::PopupText = '&Insert|~&Change|~&Delete|-|' & BRW4::PopupText
      ELSE
        BRW4::PopupText = '&Insert|~&Change|~&Delete'
      END
    END
    EXECUTE(POPUP(BRW4::PopupText))
      POST(EVENT:Accepted,?Insert:5)
      POST(EVENT:Accepted,?Change:5)
      POST(EVENT:Accepted,?Delete:5)
    END
  ELSIF BRW4::RecordCount
    BRW4::CurrentChoice = CHOICE(?Browse:4)
    GET(Queue:Browse:4,BRW4::CurrentChoice)
    DO BRW4::FillBuffer
    IF BRW4::RecordCount = ?Browse:4{PROP:Items}
      IF NOT ?Browse:4{PROP:VScroll}
        ?Browse:4{PROP:VScroll} = True
      END
      CASE BRW4::SortOrder
      OF 1
        LOOP BRW4::CurrentScroll = 1 TO 100
          IF BRW4::Sort1:KeyDistribution[BRW4::CurrentScroll] => UPPER(STU:LastName)
            IF BRW4::CurrentScroll <= 1
              BRW4::CurrentScroll = 0
            ELSIF BRW4::CurrentScroll = 100
              BRW4::CurrentScroll = 100
            ELSE
            END
            BREAK
          END
        END
      END
    ELSE
      IF ?Browse:4{PROP:VScroll}
        ?Browse:4{PROP:VScroll} = False
      END
    END
    DO RefreshWindow
  END
!---------------------------------------------------------------------
BRW4::ProcessScroll ROUTINE
!|
!| This routine processes any of the six scrolling EVENTs handled by the BrowseBox.
!| If one record is to be scrolled, the ROUTINE BRW4::ScrollOne is called.
!| If a page of records is to be scrolled, the ROUTINE BRW4::ScrollPage is called.
!| If the first or last page is to be displayed, the ROUTINE BRW4::ScrollEnd is called.
!|
!| If an incremental locator is in use, the value of that locator is cleared.
!| Finally, if a Fixed Thumb vertical scroll bar is used, the thumb is positioned.
!|
  IF BRW4::RecordCount
    BRW4::CurrentEvent = EVENT()
    CASE BRW4::CurrentEvent
    OF EVENT:ScrollUp OROF EVENT:ScrollDown
      DO BRW4::ScrollOne
    OF EVENT:PageUp OROF EVENT:PageDown
      DO BRW4::ScrollPage
    OF EVENT:ScrollTop OROF EVENT:ScrollBottom
      DO BRW4::ScrollEnd
    END
    ?Browse:4{PROP:SelStart} = BRW4::CurrentChoice
    DO BRW4::PostNewSelection
  END
!----------------------------------------------------------------------
BRW4::ScrollOne ROUTINE
!|
!| This routine is used to scroll a single record on the BrowseBox. Since the BrowseBox is an IMM
!| listbox, all scrolling must be handled in code. When called, this routine...
!|
!| 1. Sees if scrolling in the intended direction will cause the listbox display to shift. If not,
!|    the routine moves the list box cursor and exits.
!| 2. Calls BRW4::FillRecord to retrieve one record in the direction required.
!|
  IF BRW4::CurrentEvent = EVENT:ScrollUp AND BRW4::CurrentChoice > 1
    BRW4::CurrentChoice -= 1
    EXIT
  ELSIF BRW4::CurrentEvent = EVENT:ScrollDown AND BRW4::CurrentChoice < BRW4::RecordCount
    BRW4::CurrentChoice += 1
    EXIT
  END
  BRW4::ItemsToFill = 1
  BRW4::FillDirection = BRW4::CurrentEvent - 2
  DO BRW4::FillRecord
!----------------------------------------------------------------------
BRW4::ScrollPage ROUTINE
!|
!| This routine is used to scroll a single page of records on the BrowseBox. Since the BrowseBox is
!| an IMM listbox, all scrolling must be handled in code. When called, this routine...
!|
!| 1. Calls BRW4::FillRecord to retrieve one page of records in the direction required.
!| 2. If BRW4::FillRecord doesn't fill a page (BRW4::ItemsToFill > 0), then
!|    the list-box cursor ia shifted.
!|
  BRW4::ItemsToFill = ?Browse:4{PROP:Items}
  BRW4::FillDirection = BRW4::CurrentEvent - 4
  DO BRW4::FillRecord                           ! Fill with next read(s)
  IF BRW4::ItemsToFill
    IF BRW4::CurrentEvent = EVENT:PageUp
      BRW4::CurrentChoice -= BRW4::ItemsToFill
      IF BRW4::CurrentChoice < 1
        BRW4::CurrentChoice = 1
      END
    ELSE
      BRW4::CurrentChoice += BRW4::ItemsToFill
      IF BRW4::CurrentChoice > BRW4::RecordCount
        BRW4::CurrentChoice = BRW4::RecordCount
      END
    END
  END
!----------------------------------------------------------------------
BRW4::ScrollEnd ROUTINE
!|
!| This routine is used to load the first or last page of the displayable set of records.
!| Since the BrowseBox is an IMM listbox, all scrolling must be handled in code. When called,
!| this routine...
!|
!| 1. Resets the BrowseBox VIEW to insure that it reads from the end of the current sort order.
!| 2. Calls BRW4::FillRecord to retrieve one page of records.
!| 3. Selects the record that represents the end of the view. That is, if the first page was loaded,
!|    the first record is highlighted. If the last was loaded, the last record is highlighted.
!|
  FREE(Queue:Browse:4)
  BRW4::RecordCount = 0
  DO BRW4::Reset
  BRW4::ItemsToFill = ?Browse:4{PROP:Items}
  IF BRW4::CurrentEvent = EVENT:ScrollTop
    BRW4::FillDirection = FillForward
  ELSE
    BRW4::FillDirection = FillBackward
  END
  DO BRW4::FillRecord                           ! Fill with next read(s)
  IF BRW4::CurrentEvent = EVENT:ScrollTop
    BRW4::CurrentChoice = 1
  ELSE
    BRW4::CurrentChoice = BRW4::RecordCount
  END
!----------------------------------------------------------------------
BRW4::AlertKey ROUTINE
!|
!| This routine processes any KEYCODEs experienced by the BrowseBox.
!| NOTE: The cursor movement keys are not processed as KEYCODEs. They are processed as the
!|       appropriate BrowseBox scrolling and selection EVENTs.
!| This routine includes handling for double-click. Actually, this handling is in the form of
!| EMBEDs, which are filled by child-control templates.
!| This routine also includes the BrowseBox's locator handling.
!| After a value is entered for locating, this routine sets BRW4::LocateMode to a value
!| of 2 -- EQUATEd to LocateOnValue -- and calls the routine BRW4::LocateRecord.
!|
  IF BRW4::RecordCount
    CASE KEYCODE()                                         ! What keycode was hit
    OF AppsKey
    OROF MouseRight
       IF ?Browse:4{PROPLIST:MouseDownRow}>0
         ?Browse:4{PROP:Selected} = ?Browse:4{PROPLIST:MouseDownRow}
         BRW4::CurrentChoice = CHOICE(?Browse:4)
       END
       DO BRW4::NewSelection
    OF MouseLeft2
      POST(EVENT:Accepted,?Change:5)
      DO BRW4::FillBuffer
    OF InsertKey
      POST(EVENT:Accepted,?Insert:5)
    OF DeleteKey
      POST(EVENT:Accepted,?Delete:5)
    OF CtrlEnter
      POST(EVENT:Accepted,?Change:5)
    ELSE                                                   ! ELSE (What keycode was hit)
      CASE BRW4::SortOrder
      OF 1
        IF CHR(KEYCHAR())
          IF UPPER(SUB(STU:LastName,1,1)) = UPPER(CHR(KEYCHAR()))
            BRW4::CurrentEvent = EVENT:ScrollDown
            DO BRW4::ScrollOne
            GET(Queue:Browse:4,BRW4::CurrentChoice)
            DO BRW4::FillBuffer
          END
          IF UPPER(SUB(STU:LastName,1,1)) = UPPER(CHR(KEYCHAR()))
            ?Browse:4{PROP:SelStart} = BRW4::CurrentChoice
          ELSE
            STU:LastName = CHR(KEYCHAR())
            CLEAR(STU:FirstName)
            BRW4::LocateMode = LocateOnValue
            DO BRW4::LocateRecord
          END
        END
      END
    END                                                    ! END (What keycode was hit)
  ELSE
    CASE KEYCODE()                                         ! What keycode was hit
    OF AppsKey
    OROF MouseRight
       DO BRW4::NewSelection
    OF InsertKey
      POST(EVENT:Accepted,?Insert:5)
    ELSE                                                   ! ELSE (What keycode was hit)
      CASE BRW4::SortOrder
      OF 1
      END
    END
  END
  DO BRW4::PostNewSelection
!----------------------------------------------------------------------
BRW4::ScrollDrag ROUTINE
!|
!| This routine processes the Vertical Scroll Bar arrays to find the free key field value
!| that corresponds to the current scroll bar position.
!|
!| After the scroll position is computed, and the scroll value found, this routine sets
!| BRW4::LocateMode to that scroll value of 2 -- EQUATEd to LocateOnValue --
!| and calls the routine BRW4::LocateRecord.
!|
  IF ?Browse:4{PROP:VScrollPos} <= 1
    POST(EVENT:ScrollTop,?Browse:4)
  ELSIF ?Browse:4{PROP:VScrollPos} = 100
    POST(EVENT:ScrollBottom,?Browse:4)
  ELSE
    CASE BRW4::SortOrder
    OF 1
      STU:LastName = BRW4::Sort1:KeyDistribution[?Browse:4{PROP:VScrollPos}]
      BRW4::LocateMode = LocateOnValue
      DO BRW4::LocateRecord
    END
  END
!----------------------------------------------------------------------
BRW4::FillRecord ROUTINE
!|
!| This routine is used to retrieve a number of records from the VIEW. The number of records
!| retrieved is held in the variable BRW4::ItemsToFill. If more than one record is
!| to be retrieved, QuickScan is used to minimize reads from the disk.
!|
!| If records exist in the queue (in other words, if the browse has been used before), the record
!| at the appropriate end of the list box is retrieved, and the VIEW is reset to read starting
!| at that record.
!|
!| Next, the VIEW is accessed to retrieve BRW4::ItemsToFill records. Normally, this will
!| result in BRW4::ItemsToFill records being read from the VIEW, but if custom filtering
!| or range limiting is used (via the BRW4::ValidateRecord routine) then any number of records
!| might be read.
!|
!| For each good record, if BRW4::AddQueue is true, the queue is filled using the BRW4::FillQueue
!| routine. The record is then added to the queue. If adding this record causes the BrowseBox queue
!| to contain more records than can be displayed, the record at the opposite end of the queue is
!| deleted.
!|
!| The only time BRW4::AddQueue is false is when the BRW4::LocateRecord routine needs to
!| get the closest record to its record to be located. At this time, the record doesn't need to be
!| added to the queue, so it isn't.
!|
  IF BRW4::RecordCount
    IF BRW4::FillDirection = FillForward
      GET(Queue:Browse:4,BRW4::RecordCount)                ! Get the first queue item
    ELSE
      GET(Queue:Browse:4,1)                                ! Get the first queue item
    END
    RESET(BRW4::View:Browse,BRW4::Position)                ! Reset for sequential processing
    BRW4::SkipFirst = TRUE
  ELSE
    BRW4::SkipFirst = FALSE
  END
  LOOP WHILE BRW4::ItemsToFill
    IF BRW4::View:Browse{PROP:IPRequestCount} = 0
       BRW4::View:Browse{PROP:IPRequestCount} = BRW4::ItemsToFill
    END
    IF BRW4::FillDirection = FillForward
      NEXT(BRW4::View:Browse)
    ELSE
      PREVIOUS(BRW4::View:Browse)
    END
    IF ERRORCODE()
      IF ERRORCODE() = BadRecErr
        DO BRW4::RestoreResetValues
        BREAK
      ELSE
        StandardWarning(Warn:RecordFetchError,'Students')
        POST(EVENT:CloseWindow)
        EXIT
      END
    END
    IF BRW4::SkipFirst
       BRW4::SkipFirst = FALSE
       IF POSITION(BRW4::View:Browse) = BRW4::Position
          CYCLE
       END
    END
    IF BRW4::AddQueue
      IF BRW4::RecordCount = ?Browse:4{PROP:Items}
        IF BRW4::FillDirection = FillForward
          GET(Queue:Browse:4,1)                            ! Get the first queue item
        ELSE
          GET(Queue:Browse:4,BRW4::RecordCount)            ! Get the first queue item
        END
        DELETE(Queue:Browse:4)
        BRW4::RecordCount -= 1
      END
      DO BRW4::FillQueue
      IF BRW4::FillDirection = FillForward
        ADD(Queue:Browse:4)
      ELSE
        ADD(Queue:Browse:4,1)
      END
      BRW4::RecordCount += 1
    END
    BRW4::ItemsToFill -= 1
  END
  BRW4::AddQueue = True
  EXIT
!----------------------------------------------------------------------
BRW4::LocateRecord ROUTINE
!|
!| This routine is used to find a record in the VIEW, and to display that record
!| in the BrowseBox.
!|
!| This routine has three different modes of operation, which are invoked based on
!| the setting of BRW4::LocateMode. These modes are...
!|
!|   LocateOnPosition (1) - This mode is still supported for 1.5 compatability. This mode
!|                          is the same as LocateOnEdit.
!|   LocateOnValue    (2) - The values of the current sort order key are used. This mode
!|                          used for Locators and when the BrowseBox is called to select
!|                          a record.
!|   LocateOnEdit     (3) - The current record of the VIEW is used. This mode assumes
!|                          that there is an active VIEW record. This mode is used when
!|                          the sort order of the BrowseBox has changed
!|
!| If an appropriate record has been located, the BRW4::RefreshPage routine is
!| called to load the page containing the located record.
!|
!| If an appropriate record is not locate, the last page of the BrowseBox is loaded.
!|
  IF BRW4::LocateMode = LocateOnPosition
    BRW4::LocateMode = LocateOnEdit
  END
  CLOSE(BRW4::View:Browse)
  CASE BRW4::SortOrder
  OF 1
    IF BRW4::UsingAdditionalSortOrder THEN
       BRW4::UsingAdditionalSortOrder = False
       BRW4::View:Browse{PROP:Order} = '+STU:Major,+UPPER(STU:LastName),+UPPER(STU:FirstName)'
    END
    IF BRW4::LocateMode = LocateOnEdit
      BRW4::HighlightedPosition = POSITION(STU:MajorKey)
      RESET(STU:MajorKey,BRW4::HighlightedPosition)
    ELSE
      STU:Major = MAJ:Number
      SET(STU:MajorKey,STU:MajorKey)
    END
    BRW4::View:Browse{Prop:Filter} = 'STU:Major = BRW4::Sort1:Reset:MAJ:Number'
  END
  IF ERRORCODE()
    StandardWarning(Warn:ViewOpenError)
  END
  OPEN(BRW4::View:Browse)
  IF ERRORCODE()
    StandardWarning(Warn:ViewOpenError)
  END
  IF BRW4::UsingAdditionalSortOrder = True
    CASE BRW4::SortOrder
    OF 1
       SET(BRW4::View:Browse,2)
    END
  END
  FREE(Queue:Browse:4)
  BRW4::RecordCount = 0
  BRW4::ItemsToFill = 1
  BRW4::FillDirection = FillForward                        ! Fill with next read(s)
  BRW4::AddQueue = False
  DO BRW4::FillRecord                                      ! Fill with next read(s)
  BRW4::AddQueue = True
  IF BRW4::ItemsToFill
    BRW4::RefreshMode = RefreshOnBottom
    DO BRW4::RefreshPage
  ELSE
    BRW4::RefreshMode = RefreshOnPosition
    DO BRW4::RefreshPage
  END
  DO BRW4::PostNewSelection
  BRW4::LocateMode = 0
  EXIT
!----------------------------------------------------------------------
BRW4::RefreshPage ROUTINE
!|
!| This routine is used to load a single page of the BrowseBox.
!|
!| If this routine is called with a BRW4::RefreshMode of RefreshOnPosition,
!| the active VIEW record is loaded at the top of the page. Otherwise, if there are
!| records in the browse queue (Queue:Browse:4), then the current page is reloaded, and the
!| currently selected item remains selected.
!|
  IF NOT BRW4::ActiveInvisible THEN
     IF NOT ?Browse:4{PROP:Visible} THEN
        BRW4::LoadPending = True
        EXIT
     END
  END
  SETCURSOR(Cursor:Wait)
  IF BRW4::RefreshMode = RefreshOnPosition
    BRW4::HighlightedPosition = POSITION(BRW4::View:Browse)
    RESET(BRW4::View:Browse,BRW4::HighlightedPosition)
    BRW4::RefreshMode = RefreshOnTop
  ELSIF RECORDS(Queue:Browse:4)
    GET(Queue:Browse:4,BRW4::CurrentChoice)
    IF ERRORCODE()
      GET(Queue:Browse:4,RECORDS(Queue:Browse:4))
    END
    BRW4::HighlightedPosition = BRW4::Position
    GET(Queue:Browse:4,1)
    RESET(BRW4::View:Browse,BRW4::Position)
    BRW4::RefreshMode = RefreshOnCurrent
  ELSE
    BRW4::HighlightedPosition = ''
    DO BRW4::Reset
  END
  FREE(Queue:Browse:4)
  BRW4::RecordCount = 0
  BRW4::ItemsToFill = ?Browse:4{PROP:Items}
  IF BRW4::RefreshMode = RefreshOnBottom
    BRW4::FillDirection = FillBackward
  ELSE
    BRW4::FillDirection = FillForward
  END
  DO BRW4::FillRecord                                      ! Fill with next read(s)
  IF BRW4::HighlightedPosition
    IF BRW4::ItemsToFill
      IF NOT BRW4::RecordCount
        DO BRW4::Reset
      END
      IF BRW4::RefreshMode = RefreshOnBottom
        BRW4::FillDirection = FillForward
      ELSE
        BRW4::FillDirection = FillBackward
      END
      DO BRW4::FillRecord
    END
  END
  IF BRW4::RecordCount
    IF BRW4::HighlightedPosition
      LOOP BRW4::CurrentChoice = 1 TO BRW4::RecordCount
        GET(Queue:Browse:4,BRW4::CurrentChoice)
        IF BRW4::Position = BRW4::HighlightedPosition THEN BREAK.
      END
      IF BRW4::CurrentChoice > BRW4::RecordCount
        BRW4::CurrentChoice = BRW4::RecordCount
      END
    ELSE
      IF BRW4::RefreshMode = RefreshOnBottom
        BRW4::CurrentChoice = RECORDS(Queue:Browse:4)
      ELSE
        BRW4::CurrentChoice = 1
      END
    END
    ?Browse:4{Prop:Selected} = BRW4::CurrentChoice
    DO BRW4::FillBuffer
    ?Change:5{PROP:Disable} = 0
    ?Delete:5{PROP:Disable} = 0
  ELSE
    CLEAR(STU:Record)
    BRW4::CurrentChoice = 0
    ?Change:5{PROP:Disable} = 1
    ?Delete:5{PROP:Disable} = 1
  END
  SETCURSOR()
  BRW4::RefreshMode = 0
  EXIT
BRW4::Reset ROUTINE
!|
!| This routine is used to reset the VIEW used by the BrowseBox.
!|
  IF NOT BRW4::ActiveInvisible THEN
     IF NOT ?Browse:4{PROP:Visible} THEN
        BRW4::LoadPending = True
        EXIT
     END
  END
  BRW4::LoadPending = False
  CLOSE(BRW4::View:Browse)
  CASE BRW4::SortOrder
  OF 1
    IF BRW4::UsingAdditionalSortOrder THEN
       BRW4::UsingAdditionalSortOrder = False
       BRW4::View:Browse{PROP:Order} = '+STU:Major,+UPPER(STU:LastName),+UPPER(STU:FirstName)'
    END
    STU:Major = MAJ:Number
    SET(STU:MajorKey)
    BRW4::View:Browse{Prop:Filter} = 'STU:Major = BRW4::Sort1:Reset:MAJ:Number'
  END
  IF ERRORCODE()
    StandardWarning(Warn:ViewOpenError)
  END
  OPEN(BRW4::View:Browse)
  IF ERRORCODE()
    StandardWarning(Warn:ViewOpenError)
  END
  SET(BRW4::View:Browse)
!----------------------------------------------------------------------
BRW4::GetRecord ROUTINE
!|
!| This routine is used to retrieve the VIEW record that corresponds to a
!| chosen listbox record.
!|
  IF BRW4::RecordCount
    BRW4::CurrentChoice = CHOICE(?Browse:4)
    GET(Queue:Browse:4,BRW4::CurrentChoice)
    WATCH(BRW4::View:Browse)
    REGET(BRW4::View:Browse,BRW4::Position)
  END
!----------------------------------------------------------------------
BRW4::RestoreResetValues ROUTINE
!|
!| This routine is used to restore reset values to their saved value
!| after a bad record access from the VIEW.
!|
  CASE BRW4::SortOrder
  OF 1
    MAJ:Number = BRW4::Sort1:Reset:MAJ:Number
  END

!----------------------------------------------------------------
BRW4::ButtonInsert ROUTINE
!|
!| This routine calls the BrowseBox's update procedure (as specified in the
!| BrowseUpdateButtons control template) to insert a new record.
!|
!| First, the primary file's record  buffer is cleared, as well as any memos
!| or BLOBs. Next, any range-limit values are restored so that the inserted
!| record defaults to being added to the current display set.
!|
!| Next, LocalRequest is set to InsertRecord, and the BRW4::CallRecord routine
!| is called. This routine performs the actual call to the update procedure.
!|
!| If the insert is successful (GlobalRequest = RequestCompleted) then the newly added
!| record is displayed in the BrowseBox, at the top of the listbox.
!|
!| If the insert is not successful, the current page of the browse is refreshed.
!|
!| Finally, The BrowseBox is re-initialized, resetting scroll bars and totals.
!|
  GET(Students,0)
  CLEAR(STU:Record,0)
  STU:Photograph{PROP:Size} = 0
  CASE BRW4::SortOrder
  OF 1
    STU:Major = BRW4::Sort1:Reset:MAJ:Number
  END
  LocalRequest = InsertRecord
  DO BRW4::CallUpdate
  IF GlobalResponse = RequestCompleted
    BRW4::LocateMode = LocateOnEdit
    DO BRW4::LocateRecord
  ELSE
    BRW4::RefreshMode = RefreshOnQueue
    DO BRW4::RefreshPage
  END
  DO BRW4::InitializeBrowse
  DO BRW4::PostNewSelection
  SELECT(?Browse:4)
  LocalRequest = OriginalRequest
  LocalResponse = RequestCancelled
  DO RefreshWindow
!----------------------------------------------------------------
BRW4::ButtonChange ROUTINE
!|
!| This routine calls the BrowseBox's update procedure (as specified in the
!| BrowseUpdateButtons control template) to change a selected record.
!|
!| Whenever a button is pressed, the first thing that happens is that the
!| SyncWindow routine is called. This routine insures that the BrowseBox's
!| VIEW corresponds to the highlighted record by calling the BRW4::GetRecord routine.
!|
!| First, LocalRequest is set to ChangeRecord, and the BRW4::CallRecord routine
!| is called. This routine performs the actual call to the update procedure.
!|
!| If the change is successful (GlobalRequest = RequestCompleted) then the newly changed
!| record is displayed in the BrowseBox.
!|
!| If the change is not successful, the current page of the browse is refreshed.
!|
!| Finally, The BrowseBox is re-initialized, resetting scroll bars and totals.
!|
  LocalRequest = ChangeRecord
  DO BRW4::CallUpdate
  IF GlobalResponse = RequestCompleted
    BRW4::LocateMode = LocateOnEdit
    DO BRW4::LocateRecord
  ELSE
    BRW4::RefreshMode = RefreshOnQueue
    DO BRW4::RefreshPage
  END
  DO BRW4::InitializeBrowse
  DO BRW4::PostNewSelection
  SELECT(?Browse:4)
  LocalRequest = OriginalRequest
  LocalResponse = RequestCancelled
  DO RefreshWindow
!----------------------------------------------------------------
BRW4::ButtonDelete ROUTINE
!|
!| This routine calls the BrowseBox's update procedure (as specified in the
!| BrowseUpdateButtons control template) to delete a selected record.
!|
!| Whenever a button is pressed, the first thing that happens is that the
!| SyncWindow routine is called. This routine insures that the BrowseBox's
!| VIEW corresponds to the highlighted record by calling the BRW4::GetRecord routine.
!|
!| First, LocalRequest is set to DeleteRecord, and the BRW4::CallRecord routine
!| is called. This routine performs the actual call to the update procedure.
!|
!| If the delete is successful (GlobalRequest = RequestCompleted) then the deleted record is
!| removed from the queue.
!|
!| Next, the BrowseBox is refreshed, redisplaying the current page.
!|
!| Finally, The BrowseBox is re-initialized, resetting scroll bars and totals.
!|
  LocalRequest = DeleteRecord
  DO BRW4::CallUpdate
  IF GlobalResponse = RequestCompleted
    DELETE(Queue:Browse:4)
    BRW4::RecordCount -= 1
  END
  BRW4::RefreshMode = RefreshOnQueue
  DO BRW4::RefreshPage
  DO BRW4::InitializeBrowse
  DO BRW4::PostNewSelection
  SELECT(?Browse:4)
  LocalRequest = OriginalRequest
  LocalResponse = RequestCancelled
  DO RefreshWindow
!----------------------------------------------------------------
BRW4::CallUpdate ROUTINE
!|
!| This routine performs the actual call to the update procedure.
!|
!| The first thing that happens is that the VIEW is closed. This is performed just in case
!| the VIEW is still open.
!|
!| Next, GlobalRequest is set the the value of LocalRequest, and the update procedure
!| (UpdateStudents) is called.
!|
!| Upon return from the update, the routine BRW4::Reset is called to reset the VIEW
!| and reopen it.
!|
  CLOSE(BRW4::View:Browse)
  LOOP
    GlobalRequest = LocalRequest
    VCRRequest = VCRNone
    UpdateStudents
    LocalResponse = GlobalResponse
    CASE VCRRequest
    OF VCRNone
      BREAK
    OF VCRInsert
      IF LocalRequest = ChangeRecord THEN
        LocalRequest = InsertRecord
      END
    OROF VCRForward
      IF LocalRequest = InsertRecord THEN
        GET(Students,0)
        CLEAR(STU:Record,0)
        STU:Photograph{PROP:Size} = 0
      ELSE
        DO BRW4::PostVCREdit1
        BRW4::CurrentEvent = EVENT:ScrollDown
        DO BRW4::ScrollOne
        DO BRW4::PostVCREdit2
      END
    OF VCRBackward
      DO BRW4::PostVCREdit1
      BRW4::CurrentEvent = EVENT:ScrollUp
      DO BRW4::ScrollOne
      DO BRW4::PostVCREdit2
    OF VCRPageForward
      DO BRW4::PostVCREdit1
      BRW4::CurrentEvent = EVENT:PageDown
      DO BRW4::ScrollPage
      DO BRW4::PostVCREdit2
    OF VCRPageBackward
      DO BRW4::PostVCREdit1
      BRW4::CurrentEvent = EVENT:PageUp
      DO BRW4::ScrollPage
      DO BRW4::PostVCREdit2
    OF VCRFirst
      DO BRW4::PostVCREdit1
      BRW4::CurrentEvent = EVENT:ScrollTop
      DO BRW4::ScrollEnd
      DO BRW4::PostVCREdit2
    OF VCRLast
      DO BRW4::PostVCREdit1
      BRW4::CurrentEvent = EVENT:ScrollBottom
      DO BRW4::ScrollEND
      DO BRW4::PostVCREdit2
    END
  END
  DO BRW4::Reset

BRW4::PostVCREdit1 ROUTINE
  DO BRW4::Reset
  BRW4::LocateMode = LocateOnEdit
  DO BRW4::LocateRecord
  DO RefreshWindow

BRW4::PostVCREdit2 ROUTINE
  ?Browse:4{PROP:SelStart} = BRW4::CurrentChoice
  DO BRW4::NewSelection
  REGET(BRW4::View:Browse,BRW4::Position)
  CLOSE(BRW4::View:Browse)

!|
!| Copies a field from save buffer to actual buffer switched on current field
!|
HistoryField  ROUTINE
  CASE FOCUS()
  OF ?MAJ:Description
    MAJ:Description = History::MAJ:Record.Description
  END
  DISPLAY()
!---------------------------------------------------------------
PrimeFields ROUTINE
!|
!| This routine is called whenever the procedure is called to insert a record.
!|
!| This procedure performs three functions. These functions are..
!|
!|   1. Prime the new record with initial values specified in the dictionary
!|      and under the Field priming on Insert button.
!|   2. Generates any Auto-Increment values needed.
!|   3. Saves a copy of the new record, as primed, for use in batch-adds.
!|
!| If an auto-increment value is generated, this routine will add the new record
!| at this point, keeping its place in the file.
!|
  MAJ:Record = SAV::MAJ:Record
  SAV::MAJ:Record = MAJ:Record
ClosingWindow ROUTINE
  Update::Reloop = 0
  IF LocalResponse <> RequestCompleted
    DO CancelAutoIncrement
    IF Save:LastInsertedPosition
      LocalResponse = RequestCompleted
      REGET(Majors,Save:LastInsertedPosition)
    END
  END

CancelAutoIncrement ROUTINE
  IF LocalResponse <> RequestCompleted
  END
FORM::AssignButtons ROUTINE
  ToolBarMode=FormMode
  DISABLE(TBarBrwFirst,TBarBrwLast)
  ENABLE(TBarBrwHistory)
  CASE OriginalRequest
  OF InsertRecord
    ENABLE(TBarBrwDown)
    ENABLE(TBarBrwInsert)
    TBarBrwDown{PROP:ToolTip}='Save record and add another'
    TBarBrwInsert{PROP:ToolTip}=TBarBrwDown{PROP:ToolTip}
  OF ChangeRecord
    ENABLE(TBarBrwBottom,TBarBrwUp)
    ENABLE(TBarBrwInsert)
    TBarBrwBottom{PROP:ToolTip}='Save changes and go to last record'
    TBarBrwTop{PROP:ToolTip}='Save changes and go to first record'
    TBarBrwPageDown{PROP:ToolTip}='Save changes and page down to record'
    TBarBrwPageUp{PROP:ToolTip}='Save changes and page up to record'
    TBarBrwDown{PROP:ToolTip}='Save changes and go to next record'
    TBarBrwUP{PROP:ToolTip}='Save changes and go to previous record'
    TBarBrwInsert{PROP:ToolTip}='Save this record and add a new one'
  END
  TBarBrwHelp{PROP:Disable}=?Help{PROP:Disable}
  DISPLAY(TBarBrwFirst,TBarBrwLast)

!!! <summary>
!!! Generated from procedure template - Form
!!! Update the Courses File
!!! </summary>
UpdateCourses PROCEDURE

CurrentTab           STRING(80)                            ! 
LocalRequest         LONG                                  ! 
OriginalRequest      LONG                                  ! 
LocalResponse        LONG                                  ! 
FilesOpened          LONG                                  ! 
WindowOpened         LONG                                  ! 
WindowInitialized    LONG                                  ! 
ForceRefresh         LONG                                  ! 
RecordFiltered       LONG                                  ! 
ActionMessage        CSTRING(40)                           ! 
RecordChanged        BYTE,AUTO                             ! 

BRW2::View:Browse    VIEW(Classes)
                       PROJECT(CLA:ClassNumber)
                       PROJECT(CLA:RoomNumber)
                       PROJECT(CLA:ScheduledTime)
                       PROJECT(CLA:CourseNumber)
                     END
Queue:Browse:2       QUEUE,PRE()                           ! Browsing Queue
BRW2::CLA:ClassNumber  LIKE(CLA:ClassNumber)               ! Queue Display field
BRW2::CLA:RoomNumber   LIKE(CLA:RoomNumber)                ! Queue Display field
BRW2::CLA:ScheduledTime LIKE(CLA:ScheduledTime)            ! Queue Display field
BRW2::CLA:CourseNumber LIKE(CLA:CourseNumber)              ! Queue Display field
BRW2::Mark             BYTE                                ! Record mark flag
BRW2::Position         STRING(1024)                        ! Queue POSITION information
                     END                                   ! END (Browsing Queue)
BRW2::UsingAdditionalSortOrder BYTE                        ! When true the view is using PROP:Order
BRW2::CurrentScroll  BYTE                                  ! Queue position of scroll thumb
BRW2::ScrollRecordCount LONG                               ! Queue position of scroll thumb
BRW2::SkipFirst      BYTE                                  ! Skip first retrieved record in page fill
BRW2::Sort1:KeyDistribution LIKE(CLA:ClassNumber),DIM(100)
BRW2::Sort1:LowValue LIKE(CLA:ClassNumber)                 ! Queue position of scroll thumb
BRW2::Sort1:HighValue LIKE(CLA:ClassNumber)                ! Queue position of scroll thumb
BRW2::Sort1:Reset:COU:Number LIKE(COU:Number)
BRW2::CurrentEvent   LONG                                  !
BRW2::CurrentChoice  LONG                                  !
BRW2::RecordCount    LONG                                  !
BRW2::SortOrder      BYTE                                  !
BRW2::LocateMode     BYTE                                  !
BRW2::RefreshMode    BYTE                                  !
BRW2::LastSortOrder  BYTE                                  !
BRW2::FillDirection  BYTE                                  !
BRW2::AddQueue       BYTE                                  !
BRW2::Changed        BYTE                                  !
BRW2::RecordStatus   BYTE                                  ! Flag for Range/Filter test
BRW2::ItemsToFill    LONG                                  ! Controls records retrieved
BRW2::MaxItemsInList LONG                                  ! Retrieved after window opened
BRW2::HighlightedPosition STRING(1024)                     ! POSITION of located record
BRW2::NewSelectPosted BYTE                                 ! Queue position of located record
BRW2::PopupText      CSTRING(10000)                        !
BRW2::ActiveInvisible BYTE(1)                              !
BRW2::LoadPending    BYTE                                  !
ToolBarMode          UNSIGNED,AUTO
BrowseButtons        GROUP                      !info for current browse with focus
ListBox                SIGNED                   !Browse list control
InsertButton           SIGNED                   !Browse insert button
ChangeButton           SIGNED                   !Browse change button
DeleteButton           SIGNED                   !Browse delete button
SelectButton           SIGNED                   !Browse select button
                     END
Update::Reloop  BYTE
Update::Error   BYTE
History::COU:Record LIKE(COU:Record),STATIC
SAV::COU:Record      LIKE(COU:Record)
SAV::COU:CompleteDescription STRING(SIZE(COU:CompleteDescription))
Save:LastInsertedPosition STRING(512)
Auto::Attempts       LONG,AUTO
Auto::Save:COU:Number   LIKE(COU:Number)
WindowXPos      SIGNED,AUTO,STATIC
WindowYPos      SIGNED,AUTO,STATIC
WindowPosInit   BOOL(False),STATIC
QuickWindow          WINDOW('Update the Courses File'),AT(,,228,138),FONT('MS Sans Serif',8,COLOR:Black),GRAY,IMM, |
  MDI,HLP('~UpdateCourses'),SYSTEM
                       SHEET,AT(4,4,220,112),USE(?CurrentTab)
                         TAB('CompleteDescription')
                           PROMPT('Complete Description:'),AT(8,20),USE(?COU:CompleteDescription:Prompt)
                           TEXT,AT(79,20,141,92),USE(COU:CompleteDescription),VSCROLL
                         END
                         TAB('General')
                           PROMPT('&Description:'),AT(8,20),USE(?COU:Description:Prompt)
                           ENTRY(@S30),AT(96,20,124,10),USE(COU:Description)
                         END
                         TAB('Classes'),USE(?ClassesTab)
                           LIST,AT(8,20,212,94),USE(?Browse:2),HVSCROLL,FORMAT('52L(2)|M~Class Number~L(2)@P##-###' & |
  '##P@48R(2)|M~Room Number~C(0)@n4@80L(2)|M~Scheduled Time~L(2)@s20@'),FROM(Queue:Browse:2), |
  IMM,MSG('Browsing Records')
                           BUTTON('&Insert'),AT(77,98,45,14),USE(?Insert:3),HIDE
                           BUTTON('&Change'),AT(126,98,45,14),USE(?Change:3),HIDE
                           BUTTON('&Delete'),AT(175,98,45,14),USE(?Delete:3),HIDE
                         END
                       END
                       BUTTON('OK'),AT(81,120,45,14),USE(?OK),DEFAULT
                       BUTTON('Cancel'),AT(130,120,45,14),USE(?Cancel)
                       BUTTON('Help'),AT(179,120,45,14),USE(?Help),STD(STD:Help)
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
  IF LocalRequest = InsertRecord
    ?ClassesTab{PROP:Hide} = TRUE
  END
  CASE LocalRequest
  OF ViewRecord
    ActionMessage = 'View Record'
  OF InsertRecord
    CLEAR(Save:LastInsertedPosition)
    ActionMessage = 'Adding a Courses Record'
  OF ChangeRecord
    ActionMessage = 'Changing a Courses Record'
  OF DeleteRecord
  END
  QuickWindow{PROP:Text} = ActionMessage
  IF LocalRequest <> ViewRecord THEN
    ENABLE(TBarBrwHistory)
  END
  ACCEPT
    CASE EVENT()
    OF EVENT:AlertKey
      IF KEYCODE() = 734 AND LocalRequest <> ViewRecord THEN
        DO HistoryField
      END
    OF EVENT:CloseDown
      DO ClosingWindow
      IF Update::Reloop THEN CYCLE.
    OF EVENT:CloseWindow
      DO ClosingWindow
      IF Update::Reloop THEN CYCLE.
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
    OF EVENT:Moved
      GETPOSITION(0,WindowXPos,WindowYPos)
    OF EVENT:OpenWindow
      DO BRW2::AssignButtons
      DO FORM::AssignButtons
      IF NOT WindowInitialized
        DO InitializeWindow
        WindowInitialized = True
      END
      SELECT(?COU:CompleteDescription:Prompt)
    OF EVENT:Sized
      POST(EVENT:DoResize,0,THREAD())
    OF Event:Rejected
      BEEP
      DISPLAY(?)
      SELECT(?)
    ELSE
      IF ToolBarMode=BrowseMode THEN
        DO ListBoxDispatch
      END
      IF ToolBarMode = BrowseMode THEN
        DO UpdateDispatch
      END
      IF LocalRequest <> ViewRecord THEN
        IF ACCEPTED() = TbarBrwHistory
          DO HistoryField
        END
      END
      IF EVENT() = EVENT:Completed
        History::COU:Record = COU:Record
        CASE LocalRequest
        OF InsertRecord
          PUT(Courses)
          CASE ERRORCODE()
          OF NoError
            IF StandardWarning(Warn:NewRecordAdded) = Button:Yes
              Save:LastInsertedPosition = POSITION(Courses)
              DO PrimeFields
              DISPLAY
              SELECT(?COU:CompleteDescription:Prompt)
              CYCLE
            ELSE
              LocalResponse = RequestCompleted
              POST(EVENT:CloseWindow)
            END
          OF DupKeyErr
            IF DUPLICATE(COU:KeyNumber)
              IF StandardWarning(Warn:DuplicateKey,'COU:KeyNumber')
                SELECT(?COU:CompleteDescription:Prompt)
                VCRRequest = VCRNone
                CYCLE
              END
            END
          ELSE
            IF StandardWarning(Warn:InsertError)
              SELECT(?COU:CompleteDescription:Prompt)
              VCRRequest = VCRNone
              CYCLE
            END
          END
        OF ChangeRecord
          LOOP
            LocalResponse = RequestCancelled
            SETCURSOR(CURSOR:Wait)
            RecordChanged = FALSE
            IF (SAV::COU:Record <> COU:Record)  |
            OR SAV::COU:CompleteDescription <> COU:CompleteDescription
               RecordChanged = TRUE
            END
            IF RecordChanged THEN
              Update::Error = RIUpdate:Courses(1)
            ELSE
              Update::Error = 0
            END
            SETCURSOR()
            IF Update::Error THEN
              IF Update::Error = 1 THEN
                CASE StandardWarning(Warn:UpdateError)
                OF Button:Yes
                  CYCLE
                OF Button:No
                  POST(EVENT:CloseWindow)
                  BREAK
                END
              END
              DISPLAY
              SELECT(?COU:CompleteDescription:Prompt)
              VCRRequest = VCRNone
            ELSE
              IF RecordChanged OR VCRRequest = VCRNone THEN
                LocalResponse = RequestCompleted
              END
              POST(EVENT:CloseWindow)
            END
            BREAK
          END
        END
      END
      IF ToolbarMode = FormMode THEN
        CASE ACCEPTED()
        OF TBarBrwBottom TO TBarBrwUp
        OROF TBarBrwInsert
          VCRRequest=ACCEPTED()
          POST(EVENT:Completed)
        OF TBarBrwHelp
          PRESSKEY(F1Key)
        END
      END
    END
    CASE FIELD()
    OF ?CurrentTab
      CASE EVENT()
      OF EVENT:Accepted
        DO RefreshWindow
      OF EVENT:Selected
        DO RefreshWindow
      OF EVENT:Selecting
        DO RefreshWindow
      OF EVENT:NewSelection
          !Code to assign button control based upon current tab selection
          CASE CHOICE(?CurrentTab)
          OF 1
            DO FORM::AssignButtons
          OF 2
          OF 3
            DO BRW2::AssignButtons
          END
        DO RefreshWindow
      OF EVENT:TabChanging
        DO RefreshWindow
      END
    OF ?Browse:2
      CASE EVENT()
      OF EVENT:NewSelection
        DO BRW2::NewSelection
      OF EVENT:ScrollUp
        DO BRW2::ProcessScroll
      OF EVENT:ScrollDown
        DO BRW2::ProcessScroll
      OF EVENT:PageUp
        DO BRW2::ProcessScroll
      OF EVENT:PageDown
        DO BRW2::ProcessScroll
      OF EVENT:ScrollTop
        DO BRW2::ProcessScroll
      OF EVENT:ScrollBottom
        DO BRW2::ProcessScroll
      OF EVENT:ScrollDrag
        DO BRW2::ScrollDrag
      OF EVENT:AlertKey
        DO BRW2::AlertKey
      END
    OF ?Insert:3
      CASE EVENT()
      OF EVENT:Accepted
        DO SyncWindow
        DO BRW2::ButtonInsert
      END
    OF ?Change:3
      CASE EVENT()
      OF EVENT:Accepted
        DO SyncWindow
        DO BRW2::ButtonChange
      END
    OF ?Delete:3
      CASE EVENT()
      OF EVENT:Accepted
        DO SyncWindow
        DO BRW2::ButtonDelete
      END
    OF ?OK
      CASE EVENT()
      OF EVENT:Accepted
        DO SyncWindow
        IF LocalRequest <> ViewRecord THEN
          IF OriginalRequest = ChangeRecord OR OriginalRequest = InsertRecord
            SELECT()
          ELSE
            POST(EVENT:Completed)
          END
        END
      END
    OF ?Cancel
      CASE EVENT()
      OF EVENT:Accepted
        DO SyncWindow
        LocalResponse = RequestCancelled
        VCRRequest = VCRNone
        POST(EVENT:CloseWindow)
      END
    END
  END
  DO ProcedureReturn
!---------------------------------------------------------------------------
PrepareProcedure ROUTINE
  IF Classes::Used = 0
    CheckOpen(Classes,1)
  END
  Classes::Used += 1
  IF Courses::Used = 0
    CheckOpen(Courses,1)
  END
  Courses::Used += 1
  FilesOpened = TRUE
  DO BindFields
  RISnap:Courses
  SAV::COU:Record = COU:Record
  SAV::COU:CompleteDescription = COU:CompleteDescription
  IF LocalRequest = InsertRecord
    LocalResponse = RequestCompleted
    DO PrimeFields
    IF LocalResponse = RequestCancelled
      DO ProcedureReturn
    END
    LocalResponse = RequestCancelled
  END
  IF LocalRequest = DeleteRecord
    IF StandardWarning(Warn:StandardDelete) = Button:OK
      LOOP
        LocalResponse = RequestCancelled
        SETCURSOR(CURSOR:Wait)
        IF RIDelete:Courses()
          SETCURSOR()
          CASE StandardWarning(Warn:DeleteError)
          OF Button:Yes
            CYCLE
          OF Button:No OROF Button:Cancel
            BREAK
          END
        ELSE
          SETCURSOR()
          LocalResponse = RequestCompleted
        END
        BREAK
      END
    END
    DO ProcedureReturn
  END
  OPEN(QuickWindow)
  WindowOpened=True
  BRW2::AddQueue = True
  BRW2::RecordCount = 0
  IF WindowPosInit THEN
    SETPOSITION(0,WindowXPos,WindowYPos)
  ELSE
    GETPOSITION(0,WindowXPos,WindowYPos)
    WindowPosInit=True
  END
  Do DefineListboxStyle
  ?Browse:2{PROP:Alrt,252} = MouseLeft2
  ?Browse:2{PROP:Alrt,255} = AppsKey
  ?Browse:2{PROP:Alrt,253} = MouseRight
  ?Browse:2{PROP:Alrt,255} = InsertKey
  ?Browse:2{PROP:Alrt,254} = DeleteKey
  ?Browse:2{PROP:Alrt,253} = CtrlEnter
  ?Browse:2{PROP:Alrt,252} = MouseLeft2
  ?COU:CompleteDescription{PROP:Alrt,255} = 734
  ?COU:Description{PROP:Alrt,255} = 734

!---------------------------------------------------------------------------
BindFields ROUTINE
  BIND(CLA:RECORD)
  BIND(COU:RECORD)
  BIND('COU:CompleteDescription',COU:CompleteDescription)
  BIND('BRW2::Sort1:Reset:COU:Number',BRW2::Sort1:Reset:COU:Number) ! Added by: BrowseBox(Clarion)
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
    Classes::Used -= 1
    IF Classes::Used = 0 THEN CLOSE(Classes).
    Courses::Used -= 1
    IF Courses::Used = 0 THEN CLOSE(Courses).
  END
  IF WindowOpened
    CLOSE(QuickWindow)
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
  IF QuickWindow{Prop:AcceptAll} THEN EXIT.
  Do LookupRelated
  DO BRW2::SelectSort
  ?Browse:2{Prop:VScrollPos} = BRW2::CurrentScroll
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
  DO BRW2::GetRecord
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
BRW2::AssignButtons ROUTINE
  CLEAR(BrowseButtons)
  BrowseButtons.ListBox = ?Browse:2
  BrowseButtons.InsertButton = ?Insert:3
  BrowseButtons.ChangeButton = ?Change:3
  BrowseButtons.DeleteButton = ?Delete:3
  DO DisplayBrowseToolbar
!----------------------------------------------------------------------
BRW2::SelectSort ROUTINE
!|
!| This routine is called during the RefreshWindow ROUTINE present in every window procedure.
!| The purpose of this routine is to make certain that the BrowseBox is always current with your
!| user's selections. This routine...
!|
!| 1. Checks to see if any of your specified sort-order conditions are met, and if so, changes the sort order.
!| 2. If no sort order change is necessary, this routine checks to see if any of your Reset Fields has changed.
!| 3. If the sort order has changed, or if a reset field has changed, or if the ForceRefresh flag is set...
!|    a. The current record is retrieved from the disk.
!|    b. If the BrowseBox is accessed for the first time, and the Browse has been called to select a record,
!|       the page containing the current record is loaded.
!|    c. If the BrowseBox is accessed for the first time, and the Browse has not been called to select a
!|       record, the first page of information is loaded.
!|    d. If the BrowseBox is not being accessed for the first time, and the Browse sort order has changed, the
!|       new "first" page of information is loaded.
!|    e. If the BrowseBox is not being accessed for the first time, and the Browse sort order hasn't changes,
!|       the page containing the current record is reloaded.
!|    f. The record buffer is refilled from the currently highlighted BrowseBox item.
!|    f. The BrowseBox is reinitialized (BRW2::InitializeBrowse ROUTINE).
!| 4. If step 3 is not necessary, the record buffer is refilled from the currently highlighted BrowseBox item.
!|
  BRW2::LastSortOrder = BRW2::SortOrder
  BRW2::Changed = False
  IF BRW2::SortOrder = 0
    BRW2::SortOrder = 1
  END
  IF BRW2::SortOrder = BRW2::LastSortOrder
    CASE BRW2::SortOrder
    OF 1
      IF BRW2::Sort1:Reset:COU:Number <> COU:Number
        BRW2::Changed = True
      END
    END
  ELSE
  END
  IF BRW2::SortOrder <> BRW2::LastSortOrder OR BRW2::Changed OR ForceRefresh OR (BRW2::LoadPending AND ?Browse:2{PROP:VISIBLE})
    CASE BRW2::SortOrder
    OF 1
      BRW2::Sort1:Reset:COU:Number = COU:Number
    END
    DO BRW2::GetRecord
    DO BRW2::Reset
    IF BRW2::LastSortOrder = 0
      IF LocalRequest = SelectRecord
        BRW2::LocateMode = LocateOnValue
        DO BRW2::LocateRecord
      ELSE
        FREE(Queue:Browse:2)
        BRW2::RefreshMode = RefreshOnTop
        DO BRW2::RefreshPage
        DO BRW2::PostNewSelection
      END
    ELSE
      IF BRW2::Changed
        FREE(Queue:Browse:2)
        BRW2::RefreshMode = RefreshOnTop
        DO BRW2::RefreshPage
        DO BRW2::PostNewSelection
      ELSE
        BRW2::LocateMode = LocateOnValue
        DO BRW2::LocateRecord
      END
    END
    IF BRW2::RecordCount
      GET(Queue:Browse:2,BRW2::CurrentChoice)
      DO BRW2::FillBuffer
    END
    DO BRW2::InitializeBrowse
  ELSE
    IF BRW2::RecordCount
      GET(Queue:Browse:2,BRW2::CurrentChoice)
      DO BRW2::FillBuffer
    END
  END
!----------------------------------------------------------------------
BRW2::InitializeBrowse ROUTINE
!|
!| This routine initializes the BrowseBox control template. This routine is called when...
!|
!| The BrowseBox sort order has changed. This includes the first time the BrowseBox is accessed.
!| The BrowseBox returns from a record update.
!|
!| This routine performs two main functions.
!|   1. Computes all BrowseBox totals. All records that satisfy the current selection criteria
!|      are read, and totals computed. If no totals are present, this section is not generated,
!|      and may not be present in the code below.
!|   2. Calculates any runtime scrollbar positions. Again, if runtime scrollbars are not used,
!|      the code for runtime scrollbar computation will not be present.
!|
  IF NOT BRW2::ActiveInvisible THEN
     IF NOT ?Browse:2{PROP:Visible} THEN
        BRW2::LoadPending = True
        EXIT
     END
  END
  DO BRW2::Reset
  PREVIOUS(BRW2::View:Browse)
  IF ERRORCODE()
    IF ERRORCODE() = BadRecErr
      DO BRW2::RestoreResetValues
    ELSE
      StandardWarning(Warn:RecordFetchError,'Classes')
      POST(Event:CloseWindow)
    END
    EXIT
  END
  CASE BRW2::SortOrder
  OF 1
    BRW2::Sort1:HighValue = CLA:ClassNumber
  END
  DO BRW2::Reset
  NEXT(BRW2::View:Browse)
  IF ERRORCODE()
    IF ERRORCODE() = BadRecErr
      DO BRW2::RestoreResetValues
    ELSE
      StandardWarning(Warn:RecordFetchError,'Classes')
      POST(Event:CloseWindow)
    END
    EXIT
  END
  CASE BRW2::SortOrder
  OF 1
    BRW2::Sort1:LowValue = CLA:ClassNumber
    SetupRealStops(BRW2::Sort1:LowValue,BRW2::Sort1:HighValue)
    LOOP BRW2::ScrollRecordCount = 1 TO 100
      BRW2::Sort1:KeyDistribution[BRW2::ScrollRecordCount] = NextRealStop()
    END
  END
!----------------------------------------------------------------------
BRW2::FillBuffer ROUTINE
!|
!| This routine fills the record buffer from the BrowseBox queue. This gives the appearance
!| that the record is "fresh" from the disk, without the disk access required.
!|
  CLA:ClassNumber = BRW2::CLA:ClassNumber
  CLA:RoomNumber = BRW2::CLA:RoomNumber
  CLA:ScheduledTime = BRW2::CLA:ScheduledTime
  CLA:CourseNumber = BRW2::CLA:CourseNumber
!----------------------------------------------------------------------
BRW2::FillQueue ROUTINE
!|
!| This routine is used to fill the BrowseBox QUEUE from several sources.
!|
!| First, all Format Browse formulae are processed.
!|
!| Next, each field of the BrowseBox is processed. For each field...
!|
!|    The value of the field is placed in the BrowseBox queue.
!|
!| Finally, the POSITION of the current VIEW record is added to the QUEUE
!|
  BRW2::CLA:ClassNumber = CLA:ClassNumber
  BRW2::CLA:RoomNumber = CLA:RoomNumber
  BRW2::CLA:ScheduledTime = CLA:ScheduledTime
  BRW2::CLA:CourseNumber = CLA:CourseNumber
  BRW2::Position = POSITION(BRW2::View:Browse)
!----------------------------------------------------------------------
BRW2::PostNewSelection ROUTINE
!|
!| This routine is used to post the NewSelection EVENT to the window. Because we only want this
!| EVENT processed once, and becuase there are several routines that need to initiate a NewSelection
!| EVENT, we keep a flag that tells us if the EVENT is already waiting to be processed. The EVENT is
!| only POSTed if this flag is false.
!|
  IF NOT BRW2::NewSelectPosted
    BRW2::NewSelectPosted = True
    POST(EVENT:NewSelection,?Browse:2)
  END
!----------------------------------------------------------------------
BRW2::NewSelection ROUTINE
!|
!| This routine performs any window bookkeeping necessary when a new record is selected in the
!| BrowseBox.
!| 1. If the new selection is made with the right mouse button, the popup menu (if applicable) is
!|    processed.
!| 2. The current record is retrieved into the buffer using the BRW2::FillBuffer ROUTINE.
!|    After this, the current vertical scrollbar position is computed, and the scrollbar positioned.
!|
  IF NOT BRW2::ActiveInvisible THEN
     IF NOT ?Browse:2{PROP:Visible} THEN
        BRW2::LoadPending = True
        EXIT
     END
  END
  BRW2::NewSelectPosted = False
  IF KEYCODE() = MouseRight OR KEYCODE() = AppsKey
     SETKEYCODE(0)
    BRW2::PopupText = ''
    IF BRW2::RecordCount
      IF BRW2::PopupText
        BRW2::PopupText = '&Insert|&Change|&Delete|-|' & BRW2::PopupText
      ELSE
        BRW2::PopupText = '&Insert|&Change|&Delete'
      END
    ELSE
      IF BRW2::PopupText
        BRW2::PopupText = '&Insert|~&Change|~&Delete|-|' & BRW2::PopupText
      ELSE
        BRW2::PopupText = '&Insert|~&Change|~&Delete'
      END
    END
    EXECUTE(POPUP(BRW2::PopupText))
      POST(EVENT:Accepted,?Insert:3)
      POST(EVENT:Accepted,?Change:3)
      POST(EVENT:Accepted,?Delete:3)
    END
  ELSIF BRW2::RecordCount
    BRW2::CurrentChoice = CHOICE(?Browse:2)
    GET(Queue:Browse:2,BRW2::CurrentChoice)
    DO BRW2::FillBuffer
    IF BRW2::RecordCount = ?Browse:2{PROP:Items}
      IF NOT ?Browse:2{PROP:VScroll}
        ?Browse:2{PROP:VScroll} = True
      END
      CASE BRW2::SortOrder
      OF 1
        LOOP BRW2::CurrentScroll = 1 TO 100
          IF BRW2::Sort1:KeyDistribution[BRW2::CurrentScroll] => CLA:ClassNumber
            IF BRW2::CurrentScroll <= 1
              BRW2::CurrentScroll = 0
            ELSIF BRW2::CurrentScroll = 100
              BRW2::CurrentScroll = 100
            ELSE
            END
            BREAK
          END
        END
      END
    ELSE
      IF ?Browse:2{PROP:VScroll}
        ?Browse:2{PROP:VScroll} = False
      END
    END
    DO RefreshWindow
  END
!---------------------------------------------------------------------
BRW2::ProcessScroll ROUTINE
!|
!| This routine processes any of the six scrolling EVENTs handled by the BrowseBox.
!| If one record is to be scrolled, the ROUTINE BRW2::ScrollOne is called.
!| If a page of records is to be scrolled, the ROUTINE BRW2::ScrollPage is called.
!| If the first or last page is to be displayed, the ROUTINE BRW2::ScrollEnd is called.
!|
!| If an incremental locator is in use, the value of that locator is cleared.
!| Finally, if a Fixed Thumb vertical scroll bar is used, the thumb is positioned.
!|
  IF BRW2::RecordCount
    BRW2::CurrentEvent = EVENT()
    CASE BRW2::CurrentEvent
    OF EVENT:ScrollUp OROF EVENT:ScrollDown
      DO BRW2::ScrollOne
    OF EVENT:PageUp OROF EVENT:PageDown
      DO BRW2::ScrollPage
    OF EVENT:ScrollTop OROF EVENT:ScrollBottom
      DO BRW2::ScrollEnd
    END
    ?Browse:2{PROP:SelStart} = BRW2::CurrentChoice
    DO BRW2::PostNewSelection
  END
!----------------------------------------------------------------------
BRW2::ScrollOne ROUTINE
!|
!| This routine is used to scroll a single record on the BrowseBox. Since the BrowseBox is an IMM
!| listbox, all scrolling must be handled in code. When called, this routine...
!|
!| 1. Sees if scrolling in the intended direction will cause the listbox display to shift. If not,
!|    the routine moves the list box cursor and exits.
!| 2. Calls BRW2::FillRecord to retrieve one record in the direction required.
!|
  IF BRW2::CurrentEvent = EVENT:ScrollUp AND BRW2::CurrentChoice > 1
    BRW2::CurrentChoice -= 1
    EXIT
  ELSIF BRW2::CurrentEvent = EVENT:ScrollDown AND BRW2::CurrentChoice < BRW2::RecordCount
    BRW2::CurrentChoice += 1
    EXIT
  END
  BRW2::ItemsToFill = 1
  BRW2::FillDirection = BRW2::CurrentEvent - 2
  DO BRW2::FillRecord
!----------------------------------------------------------------------
BRW2::ScrollPage ROUTINE
!|
!| This routine is used to scroll a single page of records on the BrowseBox. Since the BrowseBox is
!| an IMM listbox, all scrolling must be handled in code. When called, this routine...
!|
!| 1. Calls BRW2::FillRecord to retrieve one page of records in the direction required.
!| 2. If BRW2::FillRecord doesn't fill a page (BRW2::ItemsToFill > 0), then
!|    the list-box cursor ia shifted.
!|
  BRW2::ItemsToFill = ?Browse:2{PROP:Items}
  BRW2::FillDirection = BRW2::CurrentEvent - 4
  DO BRW2::FillRecord                           ! Fill with next read(s)
  IF BRW2::ItemsToFill
    IF BRW2::CurrentEvent = EVENT:PageUp
      BRW2::CurrentChoice -= BRW2::ItemsToFill
      IF BRW2::CurrentChoice < 1
        BRW2::CurrentChoice = 1
      END
    ELSE
      BRW2::CurrentChoice += BRW2::ItemsToFill
      IF BRW2::CurrentChoice > BRW2::RecordCount
        BRW2::CurrentChoice = BRW2::RecordCount
      END
    END
  END
!----------------------------------------------------------------------
BRW2::ScrollEnd ROUTINE
!|
!| This routine is used to load the first or last page of the displayable set of records.
!| Since the BrowseBox is an IMM listbox, all scrolling must be handled in code. When called,
!| this routine...
!|
!| 1. Resets the BrowseBox VIEW to insure that it reads from the end of the current sort order.
!| 2. Calls BRW2::FillRecord to retrieve one page of records.
!| 3. Selects the record that represents the end of the view. That is, if the first page was loaded,
!|    the first record is highlighted. If the last was loaded, the last record is highlighted.
!|
  FREE(Queue:Browse:2)
  BRW2::RecordCount = 0
  DO BRW2::Reset
  BRW2::ItemsToFill = ?Browse:2{PROP:Items}
  IF BRW2::CurrentEvent = EVENT:ScrollTop
    BRW2::FillDirection = FillForward
  ELSE
    BRW2::FillDirection = FillBackward
  END
  DO BRW2::FillRecord                           ! Fill with next read(s)
  IF BRW2::CurrentEvent = EVENT:ScrollTop
    BRW2::CurrentChoice = 1
  ELSE
    BRW2::CurrentChoice = BRW2::RecordCount
  END
!----------------------------------------------------------------------
BRW2::AlertKey ROUTINE
!|
!| This routine processes any KEYCODEs experienced by the BrowseBox.
!| NOTE: The cursor movement keys are not processed as KEYCODEs. They are processed as the
!|       appropriate BrowseBox scrolling and selection EVENTs.
!| This routine includes handling for double-click. Actually, this handling is in the form of
!| EMBEDs, which are filled by child-control templates.
!| This routine also includes the BrowseBox's locator handling.
!| After a value is entered for locating, this routine sets BRW2::LocateMode to a value
!| of 2 -- EQUATEd to LocateOnValue -- and calls the routine BRW2::LocateRecord.
!|
  IF BRW2::RecordCount
    CASE KEYCODE()                                         ! What keycode was hit
    OF AppsKey
    OROF MouseRight
       IF ?Browse:2{PROPLIST:MouseDownRow}>0
         ?Browse:2{PROP:Selected} = ?Browse:2{PROPLIST:MouseDownRow}
         BRW2::CurrentChoice = CHOICE(?Browse:2)
       END
       DO BRW2::NewSelection
    OF MouseLeft2
      POST(EVENT:Accepted,?Change:3)
      DO BRW2::FillBuffer
    OF InsertKey
      POST(EVENT:Accepted,?Insert:3)
    OF DeleteKey
      POST(EVENT:Accepted,?Delete:3)
    OF CtrlEnter
      POST(EVENT:Accepted,?Change:3)
    ELSE                                                   ! ELSE (What keycode was hit)
      CASE BRW2::SortOrder
      OF 1
        IF CHR(KEYCHAR())
          IF UPPER(SUB(CLA:ClassNumber,1,1)) = UPPER(CHR(KEYCHAR()))
            BRW2::CurrentEvent = EVENT:ScrollDown
            DO BRW2::ScrollOne
            GET(Queue:Browse:2,BRW2::CurrentChoice)
            DO BRW2::FillBuffer
          END
          IF UPPER(SUB(CLA:ClassNumber,1,1)) = UPPER(CHR(KEYCHAR()))
            ?Browse:2{PROP:SelStart} = BRW2::CurrentChoice
          ELSE
            CLA:ClassNumber = CHR(KEYCHAR())
            BRW2::LocateMode = LocateOnValue
            DO BRW2::LocateRecord
          END
        END
      END
    END                                                    ! END (What keycode was hit)
  ELSE
    CASE KEYCODE()                                         ! What keycode was hit
    OF AppsKey
    OROF MouseRight
       DO BRW2::NewSelection
    OF InsertKey
      POST(EVENT:Accepted,?Insert:3)
    ELSE                                                   ! ELSE (What keycode was hit)
      CASE BRW2::SortOrder
      OF 1
      END
    END
  END
  DO BRW2::PostNewSelection
!----------------------------------------------------------------------
BRW2::ScrollDrag ROUTINE
!|
!| This routine processes the Vertical Scroll Bar arrays to find the free key field value
!| that corresponds to the current scroll bar position.
!|
!| After the scroll position is computed, and the scroll value found, this routine sets
!| BRW2::LocateMode to that scroll value of 2 -- EQUATEd to LocateOnValue --
!| and calls the routine BRW2::LocateRecord.
!|
  IF ?Browse:2{PROP:VScrollPos} <= 1
    POST(EVENT:ScrollTop,?Browse:2)
  ELSIF ?Browse:2{PROP:VScrollPos} = 100
    POST(EVENT:ScrollBottom,?Browse:2)
  ELSE
    CASE BRW2::SortOrder
    OF 1
      CLA:ClassNumber = BRW2::Sort1:KeyDistribution[?Browse:2{PROP:VScrollPos}]
      BRW2::LocateMode = LocateOnValue
      DO BRW2::LocateRecord
    END
  END
!----------------------------------------------------------------------
BRW2::FillRecord ROUTINE
!|
!| This routine is used to retrieve a number of records from the VIEW. The number of records
!| retrieved is held in the variable BRW2::ItemsToFill. If more than one record is
!| to be retrieved, QuickScan is used to minimize reads from the disk.
!|
!| If records exist in the queue (in other words, if the browse has been used before), the record
!| at the appropriate end of the list box is retrieved, and the VIEW is reset to read starting
!| at that record.
!|
!| Next, the VIEW is accessed to retrieve BRW2::ItemsToFill records. Normally, this will
!| result in BRW2::ItemsToFill records being read from the VIEW, but if custom filtering
!| or range limiting is used (via the BRW2::ValidateRecord routine) then any number of records
!| might be read.
!|
!| For each good record, if BRW2::AddQueue is true, the queue is filled using the BRW2::FillQueue
!| routine. The record is then added to the queue. If adding this record causes the BrowseBox queue
!| to contain more records than can be displayed, the record at the opposite end of the queue is
!| deleted.
!|
!| The only time BRW2::AddQueue is false is when the BRW2::LocateRecord routine needs to
!| get the closest record to its record to be located. At this time, the record doesn't need to be
!| added to the queue, so it isn't.
!|
  IF BRW2::RecordCount
    IF BRW2::FillDirection = FillForward
      GET(Queue:Browse:2,BRW2::RecordCount)                ! Get the first queue item
    ELSE
      GET(Queue:Browse:2,1)                                ! Get the first queue item
    END
    RESET(BRW2::View:Browse,BRW2::Position)                ! Reset for sequential processing
    BRW2::SkipFirst = TRUE
  ELSE
    BRW2::SkipFirst = FALSE
  END
  LOOP WHILE BRW2::ItemsToFill
    IF BRW2::View:Browse{PROP:IPRequestCount} = 0
       BRW2::View:Browse{PROP:IPRequestCount} = BRW2::ItemsToFill
    END
    IF BRW2::FillDirection = FillForward
      NEXT(BRW2::View:Browse)
    ELSE
      PREVIOUS(BRW2::View:Browse)
    END
    IF ERRORCODE()
      IF ERRORCODE() = BadRecErr
        DO BRW2::RestoreResetValues
        BREAK
      ELSE
        StandardWarning(Warn:RecordFetchError,'Classes')
        POST(EVENT:CloseWindow)
        EXIT
      END
    END
    IF BRW2::SkipFirst
       BRW2::SkipFirst = FALSE
       IF POSITION(BRW2::View:Browse) = BRW2::Position
          CYCLE
       END
    END
    IF BRW2::AddQueue
      IF BRW2::RecordCount = ?Browse:2{PROP:Items}
        IF BRW2::FillDirection = FillForward
          GET(Queue:Browse:2,1)                            ! Get the first queue item
        ELSE
          GET(Queue:Browse:2,BRW2::RecordCount)            ! Get the first queue item
        END
        DELETE(Queue:Browse:2)
        BRW2::RecordCount -= 1
      END
      DO BRW2::FillQueue
      IF BRW2::FillDirection = FillForward
        ADD(Queue:Browse:2)
      ELSE
        ADD(Queue:Browse:2,1)
      END
      BRW2::RecordCount += 1
    END
    BRW2::ItemsToFill -= 1
  END
  BRW2::AddQueue = True
  EXIT
!----------------------------------------------------------------------
BRW2::LocateRecord ROUTINE
!|
!| This routine is used to find a record in the VIEW, and to display that record
!| in the BrowseBox.
!|
!| This routine has three different modes of operation, which are invoked based on
!| the setting of BRW2::LocateMode. These modes are...
!|
!|   LocateOnPosition (1) - This mode is still supported for 1.5 compatability. This mode
!|                          is the same as LocateOnEdit.
!|   LocateOnValue    (2) - The values of the current sort order key are used. This mode
!|                          used for Locators and when the BrowseBox is called to select
!|                          a record.
!|   LocateOnEdit     (3) - The current record of the VIEW is used. This mode assumes
!|                          that there is an active VIEW record. This mode is used when
!|                          the sort order of the BrowseBox has changed
!|
!| If an appropriate record has been located, the BRW2::RefreshPage routine is
!| called to load the page containing the located record.
!|
!| If an appropriate record is not locate, the last page of the BrowseBox is loaded.
!|
  IF BRW2::LocateMode = LocateOnPosition
    BRW2::LocateMode = LocateOnEdit
  END
  CLOSE(BRW2::View:Browse)
  CASE BRW2::SortOrder
  OF 1
    IF BRW2::UsingAdditionalSortOrder THEN
       BRW2::UsingAdditionalSortOrder = False
       BRW2::View:Browse{PROP:Order} = '+CLA:CourseNumber,+CLA:ClassNumber'
    END
    IF BRW2::LocateMode = LocateOnEdit
      BRW2::HighlightedPosition = POSITION(CLA:KeyCourseNumber)
      RESET(CLA:KeyCourseNumber,BRW2::HighlightedPosition)
    ELSE
      CLA:CourseNumber = COU:Number
      SET(CLA:KeyCourseNumber,CLA:KeyCourseNumber)
    END
    BRW2::View:Browse{Prop:Filter} = 'CLA:CourseNumber = BRW2::Sort1:Reset:COU:Number'
  END
  IF ERRORCODE()
    StandardWarning(Warn:ViewOpenError)
  END
  OPEN(BRW2::View:Browse)
  IF ERRORCODE()
    StandardWarning(Warn:ViewOpenError)
  END
  IF BRW2::UsingAdditionalSortOrder = True
    CASE BRW2::SortOrder
    OF 1
       SET(BRW2::View:Browse,2)
    END
  END
  FREE(Queue:Browse:2)
  BRW2::RecordCount = 0
  BRW2::ItemsToFill = 1
  BRW2::FillDirection = FillForward                        ! Fill with next read(s)
  BRW2::AddQueue = False
  DO BRW2::FillRecord                                      ! Fill with next read(s)
  BRW2::AddQueue = True
  IF BRW2::ItemsToFill
    BRW2::RefreshMode = RefreshOnBottom
    DO BRW2::RefreshPage
  ELSE
    BRW2::RefreshMode = RefreshOnPosition
    DO BRW2::RefreshPage
  END
  DO BRW2::PostNewSelection
  BRW2::LocateMode = 0
  EXIT
!----------------------------------------------------------------------
BRW2::RefreshPage ROUTINE
!|
!| This routine is used to load a single page of the BrowseBox.
!|
!| If this routine is called with a BRW2::RefreshMode of RefreshOnPosition,
!| the active VIEW record is loaded at the top of the page. Otherwise, if there are
!| records in the browse queue (Queue:Browse:2), then the current page is reloaded, and the
!| currently selected item remains selected.
!|
  IF NOT BRW2::ActiveInvisible THEN
     IF NOT ?Browse:2{PROP:Visible} THEN
        BRW2::LoadPending = True
        EXIT
     END
  END
  SETCURSOR(Cursor:Wait)
  IF BRW2::RefreshMode = RefreshOnPosition
    BRW2::HighlightedPosition = POSITION(BRW2::View:Browse)
    RESET(BRW2::View:Browse,BRW2::HighlightedPosition)
    BRW2::RefreshMode = RefreshOnTop
  ELSIF RECORDS(Queue:Browse:2)
    GET(Queue:Browse:2,BRW2::CurrentChoice)
    IF ERRORCODE()
      GET(Queue:Browse:2,RECORDS(Queue:Browse:2))
    END
    BRW2::HighlightedPosition = BRW2::Position
    GET(Queue:Browse:2,1)
    RESET(BRW2::View:Browse,BRW2::Position)
    BRW2::RefreshMode = RefreshOnCurrent
  ELSE
    BRW2::HighlightedPosition = ''
    DO BRW2::Reset
  END
  FREE(Queue:Browse:2)
  BRW2::RecordCount = 0
  BRW2::ItemsToFill = ?Browse:2{PROP:Items}
  IF BRW2::RefreshMode = RefreshOnBottom
    BRW2::FillDirection = FillBackward
  ELSE
    BRW2::FillDirection = FillForward
  END
  DO BRW2::FillRecord                                      ! Fill with next read(s)
  IF BRW2::HighlightedPosition
    IF BRW2::ItemsToFill
      IF NOT BRW2::RecordCount
        DO BRW2::Reset
      END
      IF BRW2::RefreshMode = RefreshOnBottom
        BRW2::FillDirection = FillForward
      ELSE
        BRW2::FillDirection = FillBackward
      END
      DO BRW2::FillRecord
    END
  END
  IF BRW2::RecordCount
    IF BRW2::HighlightedPosition
      LOOP BRW2::CurrentChoice = 1 TO BRW2::RecordCount
        GET(Queue:Browse:2,BRW2::CurrentChoice)
        IF BRW2::Position = BRW2::HighlightedPosition THEN BREAK.
      END
      IF BRW2::CurrentChoice > BRW2::RecordCount
        BRW2::CurrentChoice = BRW2::RecordCount
      END
    ELSE
      IF BRW2::RefreshMode = RefreshOnBottom
        BRW2::CurrentChoice = RECORDS(Queue:Browse:2)
      ELSE
        BRW2::CurrentChoice = 1
      END
    END
    ?Browse:2{Prop:Selected} = BRW2::CurrentChoice
    DO BRW2::FillBuffer
    ?Change:3{PROP:Disable} = 0
    ?Delete:3{PROP:Disable} = 0
  ELSE
    CLEAR(CLA:Record)
    BRW2::CurrentChoice = 0
    ?Change:3{PROP:Disable} = 1
    ?Delete:3{PROP:Disable} = 1
  END
  SETCURSOR()
  BRW2::RefreshMode = 0
  EXIT
BRW2::Reset ROUTINE
!|
!| This routine is used to reset the VIEW used by the BrowseBox.
!|
  IF NOT BRW2::ActiveInvisible THEN
     IF NOT ?Browse:2{PROP:Visible} THEN
        BRW2::LoadPending = True
        EXIT
     END
  END
  BRW2::LoadPending = False
  CLOSE(BRW2::View:Browse)
  CASE BRW2::SortOrder
  OF 1
    IF BRW2::UsingAdditionalSortOrder THEN
       BRW2::UsingAdditionalSortOrder = False
       BRW2::View:Browse{PROP:Order} = '+CLA:CourseNumber,+CLA:ClassNumber'
    END
    CLA:CourseNumber = COU:Number
    SET(CLA:KeyCourseNumber)
    BRW2::View:Browse{Prop:Filter} = 'CLA:CourseNumber = BRW2::Sort1:Reset:COU:Number'
  END
  IF ERRORCODE()
    StandardWarning(Warn:ViewOpenError)
  END
  OPEN(BRW2::View:Browse)
  IF ERRORCODE()
    StandardWarning(Warn:ViewOpenError)
  END
  SET(BRW2::View:Browse)
!----------------------------------------------------------------------
BRW2::GetRecord ROUTINE
!|
!| This routine is used to retrieve the VIEW record that corresponds to a
!| chosen listbox record.
!|
  IF BRW2::RecordCount
    BRW2::CurrentChoice = CHOICE(?Browse:2)
    GET(Queue:Browse:2,BRW2::CurrentChoice)
    WATCH(BRW2::View:Browse)
    REGET(BRW2::View:Browse,BRW2::Position)
  END
!----------------------------------------------------------------------
BRW2::RestoreResetValues ROUTINE
!|
!| This routine is used to restore reset values to their saved value
!| after a bad record access from the VIEW.
!|
  CASE BRW2::SortOrder
  OF 1
    COU:Number = BRW2::Sort1:Reset:COU:Number
  END
!--------------------------------------------------------------------------
DisplayBrowseToolbar      ROUTINE
  ENABLE(TBarBrwBottom,TBarBrwLocate)
  TBarBrwHelp{PROP:DISABLE}=?Help{PROP:DISABLE}
  IF BrowseButtons.InsertButton THEN
    TBarBrwInsert{PROP:Disable} = BrowseButtons.InsertButton{PROP:Disable}
  END
  IF BrowseButtons.ChangeButton THEN
    TBarBrwChange{PROP:Disable} = BrowseButtons.ChangeButton{PROP:Disable}
  END
  IF BrowseButtons.DeleteButton THEN
    TBarBrwDelete{PROP:Disable} = BrowseButtons.DeleteButton{PROP:Disable}
  END
  DISABLE(TBarBrwHistory)
  ToolBarMode = BrowseMode
  TBarBrwDown{PROP:ToolTip} = 'Go to the Next Record'
  TBarBrwBottom{PROP:ToolTip} = 'Go to the Last Page'
  TBarBrwTop{PROP:ToolTip} = 'Go to the First Page'
  TBarBrwPageDown{PROP:ToolTip} = 'Go to the Next Page'
  TBarBrwPageUp{PROP:ToolTip} = 'Go to the Prior Page'
  TBarBrwDown{PROP:ToolTip} = 'Go to the Next Record'
  TBarBrwUP{PROP:ToolTip} = 'Go to the Prior Record'
  TBarBrwInsert{PROP:ToolTip} = 'Insert a new Record'
  DISPLAY(TBarBrwFirst,TBarBrwLast)
!--------------------------------------------------------------------------
ListBoxDispatch ROUTINE
  DO DisplayBrowseToolbar
  IF FOCUS() <> BrowseButtons.ListBox THEN  ! List box must have focus when on update form
    EXIT
  END
  IF ACCEPTED() THEN            !trap remote browse box control calls
    EXECUTE(ACCEPTED()-TBarBrwBottom+1)
      POST(EVENT:ScrollBottom,BrowseButtons.ListBox)
      POST(EVENT:ScrollTop,BrowseButtons.ListBox)
      POST(EVENT:PageDown,BrowseButtons.ListBox)
      POST(EVENT:PageUp,BrowseButtons.ListBox)
      POST(EVENT:ScrollDown,BrowseButtons.ListBox)
      POST(EVENT:ScrollUp,BrowseButtons.ListBox)
      POST(EVENT:Locate,BrowseButtons.ListBox)
      BEGIN                     !EXECUTE Place Holder - Ditto has no effect on a browse
      END
      PRESSKEY(F1Key)
    END
  END

UpdateDispatch ROUTINE
  DISABLE(TBarBrwDelete)
  DISABLE(TBarBrwChange)
  IF BrowseButtons.DeleteButton AND BrowseButtons.DeleteButton{PROP:Disable} = 0 THEN
    ENABLE(TBarBrwDelete)
  END
  IF BrowseButtons.ChangeButton AND BrowseButtons.ChangeButton{PROP:Disable} = 0 THEN
    ENABLE(TBarBrwChange)
  END
  IF FOCUS() <> BrowseButtons.ListBox THEN  ! List box must have focus when on update form
    EXIT
  END
  IF INRANGE(ACCEPTED(),TBarBrwInsert,TBarBrwDelete) THEN         !trap remote browse update control calls
    EXECUTE(ACCEPTED()-TBarBrwInsert+1)
      POST(EVENT:Accepted,BrowseButtons.InsertButton)
      POST(EVENT:Accepted,BrowseButtons.ChangeButton)
      POST(EVENT:Accepted,BrowseButtons.DeleteButton)
    END
  END
!----------------------------------------------------------------
BRW2::ButtonInsert ROUTINE
!|
!| This routine calls the BrowseBox's update procedure (as specified in the
!| BrowseUpdateButtons control template) to insert a new record.
!|
!| First, the primary file's record  buffer is cleared, as well as any memos
!| or BLOBs. Next, any range-limit values are restored so that the inserted
!| record defaults to being added to the current display set.
!|
!| Next, LocalRequest is set to InsertRecord, and the BRW2::CallRecord routine
!| is called. This routine performs the actual call to the update procedure.
!|
!| If the insert is successful (GlobalRequest = RequestCompleted) then the newly added
!| record is displayed in the BrowseBox, at the top of the listbox.
!|
!| If the insert is not successful, the current page of the browse is refreshed.
!|
!| Finally, The BrowseBox is re-initialized, resetting scroll bars and totals.
!|
  GET(Classes,0)
  CLEAR(CLA:Record,0)
  CASE BRW2::SortOrder
  OF 1
    CLA:CourseNumber = BRW2::Sort1:Reset:COU:Number
  END
  LocalRequest = InsertRecord
  DO BRW2::CallUpdate
  IF GlobalResponse = RequestCompleted
    BRW2::LocateMode = LocateOnEdit
    DO BRW2::LocateRecord
  ELSE
    BRW2::RefreshMode = RefreshOnQueue
    DO BRW2::RefreshPage
  END
  DO BRW2::InitializeBrowse
  DO BRW2::PostNewSelection
  SELECT(?Browse:2)
  LocalRequest = OriginalRequest
  LocalResponse = RequestCancelled
  DO RefreshWindow
!----------------------------------------------------------------
BRW2::ButtonChange ROUTINE
!|
!| This routine calls the BrowseBox's update procedure (as specified in the
!| BrowseUpdateButtons control template) to change a selected record.
!|
!| Whenever a button is pressed, the first thing that happens is that the
!| SyncWindow routine is called. This routine insures that the BrowseBox's
!| VIEW corresponds to the highlighted record by calling the BRW2::GetRecord routine.
!|
!| First, LocalRequest is set to ChangeRecord, and the BRW2::CallRecord routine
!| is called. This routine performs the actual call to the update procedure.
!|
!| If the change is successful (GlobalRequest = RequestCompleted) then the newly changed
!| record is displayed in the BrowseBox.
!|
!| If the change is not successful, the current page of the browse is refreshed.
!|
!| Finally, The BrowseBox is re-initialized, resetting scroll bars and totals.
!|
  LocalRequest = ChangeRecord
  DO BRW2::CallUpdate
  IF GlobalResponse = RequestCompleted
    BRW2::LocateMode = LocateOnEdit
    DO BRW2::LocateRecord
  ELSE
    BRW2::RefreshMode = RefreshOnQueue
    DO BRW2::RefreshPage
  END
  DO BRW2::InitializeBrowse
  DO BRW2::PostNewSelection
  SELECT(?Browse:2)
  LocalRequest = OriginalRequest
  LocalResponse = RequestCancelled
  DO RefreshWindow
!----------------------------------------------------------------
BRW2::ButtonDelete ROUTINE
!|
!| This routine calls the BrowseBox's update procedure (as specified in the
!| BrowseUpdateButtons control template) to delete a selected record.
!|
!| Whenever a button is pressed, the first thing that happens is that the
!| SyncWindow routine is called. This routine insures that the BrowseBox's
!| VIEW corresponds to the highlighted record by calling the BRW2::GetRecord routine.
!|
!| First, LocalRequest is set to DeleteRecord, and the BRW2::CallRecord routine
!| is called. This routine performs the actual call to the update procedure.
!|
!| If the delete is successful (GlobalRequest = RequestCompleted) then the deleted record is
!| removed from the queue.
!|
!| Next, the BrowseBox is refreshed, redisplaying the current page.
!|
!| Finally, The BrowseBox is re-initialized, resetting scroll bars and totals.
!|
  LocalRequest = DeleteRecord
  DO BRW2::CallUpdate
  IF GlobalResponse = RequestCompleted
    DELETE(Queue:Browse:2)
    BRW2::RecordCount -= 1
  END
  BRW2::RefreshMode = RefreshOnQueue
  DO BRW2::RefreshPage
  DO BRW2::InitializeBrowse
  DO BRW2::PostNewSelection
  SELECT(?Browse:2)
  LocalRequest = OriginalRequest
  LocalResponse = RequestCancelled
  DO RefreshWindow
!----------------------------------------------------------------
BRW2::CallUpdate ROUTINE
!|
!| This routine performs the actual call to the update procedure.
!|
!| The first thing that happens is that the VIEW is closed. This is performed just in case
!| the VIEW is still open.
!|
!| Next, GlobalRequest is set the the value of LocalRequest, and the update procedure
!| (UpdateClasses) is called.
!|
!| Upon return from the update, the routine BRW2::Reset is called to reset the VIEW
!| and reopen it.
!|
  CLOSE(BRW2::View:Browse)
  LOOP
    GlobalRequest = LocalRequest
    VCRRequest = VCRNone
    UpdateClasses
    LocalResponse = GlobalResponse
    CASE VCRRequest
    OF VCRNone
      BREAK
    OF VCRInsert
      IF LocalRequest = ChangeRecord THEN
        LocalRequest = InsertRecord
      END
    OROF VCRForward
      IF LocalRequest = InsertRecord THEN
        GET(Classes,0)
        CLEAR(CLA:Record,0)
      ELSE
        DO BRW2::PostVCREdit1
        BRW2::CurrentEvent = EVENT:ScrollDown
        DO BRW2::ScrollOne
        DO BRW2::PostVCREdit2
      END
    OF VCRBackward
      DO BRW2::PostVCREdit1
      BRW2::CurrentEvent = EVENT:ScrollUp
      DO BRW2::ScrollOne
      DO BRW2::PostVCREdit2
    OF VCRPageForward
      DO BRW2::PostVCREdit1
      BRW2::CurrentEvent = EVENT:PageDown
      DO BRW2::ScrollPage
      DO BRW2::PostVCREdit2
    OF VCRPageBackward
      DO BRW2::PostVCREdit1
      BRW2::CurrentEvent = EVENT:PageUp
      DO BRW2::ScrollPage
      DO BRW2::PostVCREdit2
    OF VCRFirst
      DO BRW2::PostVCREdit1
      BRW2::CurrentEvent = EVENT:ScrollTop
      DO BRW2::ScrollEnd
      DO BRW2::PostVCREdit2
    OF VCRLast
      DO BRW2::PostVCREdit1
      BRW2::CurrentEvent = EVENT:ScrollBottom
      DO BRW2::ScrollEND
      DO BRW2::PostVCREdit2
    END
  END
  DO BRW2::Reset

BRW2::PostVCREdit1 ROUTINE
  DO BRW2::Reset
  BRW2::LocateMode = LocateOnEdit
  DO BRW2::LocateRecord
  DO RefreshWindow

BRW2::PostVCREdit2 ROUTINE
  ?Browse:2{PROP:SelStart} = BRW2::CurrentChoice
  DO BRW2::NewSelection
  REGET(BRW2::View:Browse,BRW2::Position)
  CLOSE(BRW2::View:Browse)

!|
!| Copies a field from save buffer to actual buffer switched on current field
!|
HistoryField  ROUTINE
  CASE FOCUS()
  OF ?COU:Description
    COU:Description = History::COU:Record.Description
  END
  DISPLAY()
!---------------------------------------------------------------
PrimeFields ROUTINE
!|
!| This routine is called whenever the procedure is called to insert a record.
!|
!| This procedure performs three functions. These functions are..
!|
!|   1. Prime the new record with initial values specified in the dictionary
!|      and under the Field priming on Insert button.
!|   2. Generates any Auto-Increment values needed.
!|   3. Saves a copy of the new record, as primed, for use in batch-adds.
!|
!| If an auto-increment value is generated, this routine will add the new record
!| at this point, keeping its place in the file.
!|
  COU:Record = SAV::COU:Record
  COU:CompleteDescription = SAV::COU:CompleteDescription
  SAV::COU:Record = COU:Record
  SAV::COU:CompleteDescription = COU:CompleteDescription
  Auto::Attempts = 0
  LOOP
    SET(COU:KeyNumber)
    PREVIOUS(Courses)
    IF ERRORCODE() AND ERRORCODE() <> BadRecErr
      StandardWarning(Warn:RecordFetchError,'Courses')
      POST(Event:CloseWindow)
      EXIT
    END
    IF ERRORCODE()
      Auto::Save:COU:Number = 1
    ELSE
      Auto::Save:COU:Number = COU:Number + 1
    END
    COU:Record = SAV::COU:Record
    COU:CompleteDescription = SAV::COU:CompleteDescription
    COU:Number = Auto::Save:COU:Number
    SAV::COU:Record = COU:Record
    SAV::COU:CompleteDescription = COU:CompleteDescription
    ADD(Courses)
    IF ERRORCODE()
      Auto::Attempts += 1
      IF Auto::Attempts = 3
        IF StandardWarning(Warn:AutoIncError) = Button:Retry
          Auto::Attempts = 0
        ELSE
          LocalResponse = RequestCancelled
          EXIT
        END
      END
      CYCLE
    END
    BREAK
  END
ClosingWindow ROUTINE
  Update::Reloop = 0
  IF LocalResponse <> RequestCompleted
    DO CancelAutoIncrement
    IF Save:LastInsertedPosition
      LocalResponse = RequestCompleted
      REGET(Courses,Save:LastInsertedPosition)
    END
  END

CancelAutoIncrement ROUTINE
  IF LocalResponse <> RequestCompleted
    IF OriginalRequest = InsertRecord
      IF LocalResponse = RequestCancelled
        DELETE(Courses)
      END
    END
  END
FORM::AssignButtons ROUTINE
  ToolBarMode=FormMode
  DISABLE(TBarBrwFirst,TBarBrwLast)
  ENABLE(TBarBrwHistory)
  CASE OriginalRequest
  OF InsertRecord
    ENABLE(TBarBrwDown)
    ENABLE(TBarBrwInsert)
    TBarBrwDown{PROP:ToolTip}='Save record and add another'
    TBarBrwInsert{PROP:ToolTip}=TBarBrwDown{PROP:ToolTip}
  OF ChangeRecord
    ENABLE(TBarBrwBottom,TBarBrwUp)
    ENABLE(TBarBrwInsert)
    TBarBrwBottom{PROP:ToolTip}='Save changes and go to last record'
    TBarBrwTop{PROP:ToolTip}='Save changes and go to first record'
    TBarBrwPageDown{PROP:ToolTip}='Save changes and page down to record'
    TBarBrwPageUp{PROP:ToolTip}='Save changes and page up to record'
    TBarBrwDown{PROP:ToolTip}='Save changes and go to next record'
    TBarBrwUP{PROP:ToolTip}='Save changes and go to previous record'
    TBarBrwInsert{PROP:ToolTip}='Save this record and add a new one'
  END
  TBarBrwHelp{PROP:Disable}=?Help{PROP:Disable}
  DISPLAY(TBarBrwFirst,TBarBrwLast)

!!! <summary>
!!! Generated from procedure template - Window
!!! </summary>
ClassTree PROCEDURE

LocalRequest         LONG                                  ! 
OriginalRequest      LONG                                  ! 
LocalResponse        LONG                                  ! 
FilesOpened          LONG                                  ! 
WindowOpened         LONG                                  ! 
WindowInitialized    LONG                                  ! 
ForceRefresh         LONG                                  ! 
DisplayString        STRING(255)                           ! 
ToolbarMode          UNSIGNED,AUTO
ActiveReltree        SIGNED
REL1::SaveRecords    LONG,AUTO
REL1::SavePointer    LONG,AUTO
REL1::SaveLevel      BYTE,AUTO
REL1::Action         LONG,AUTO
Queue:RelTree        QUEUE,PRE()                           ! Browsing Queue
REL1::Display          STRING(200)                         ! Queue position of scroll thumb
REL1::NormalFG         LONG
REL1::NormalBG         LONG
REL1::SelectedFG       LONG
REL1::SelectedBG       LONG
REL1::Icon             SHORT
REL1::Level            LONG                                ! Queue position of scroll thumb
REL1::Loaded           SHORT                               ! Queue position of scroll thumb
REL1::Position         STRING(512)                         ! Queue position of scroll thumb
                     END
REL1::LoadedQueue    QUEUE,PRE()                           ! Status Queue
REL1::LoadedLevel      LONG                                ! Queue position of scroll thumb
REL1::LoadedPosition   STRING(512)                         ! Queue position of scroll thumb
                     END
REL1::CurrentLevel   LONG                                  ! Queue position of scroll thumb
REL1::CurrentChoice  LONG                                  ! Queue position of scroll thumb
REL1::NewItemLevel   LONG                                  ! Queue position of scroll thumb
REL1::NewItemPosition STRING(512)                          ! Queue position of scroll thumb
REL1::LoadAll        LONG
WinResize            WindowResizeType
window               WINDOW('Tree List of Classes and Enrollments'),AT(,,162,174),RESIZE,GRAY,IMM,MDI,HLP('~ClassTree'), |
  SYSTEM
                       LIST,AT(4,5,150,147),USE(?RelTree),VSCROLL,FORMAT('800L|M*ITS(99)@s200@'),FROM(Queue:RelTree)
                       BUTTON('&Expand All'),AT(8,155,45,14),USE(?Expand)
                       BUTTON('Co&ntract All'),AT(58,155,45,14),USE(?Contract)
                       BUTTON('&Insert'),AT(7,103,45,14),USE(?Insert),HIDE
                       BUTTON('&Change'),AT(57,103,45,14),USE(?Change),HIDE
                       BUTTON('&Delete'),AT(107,103,45,14),USE(?Delete),HIDE
                       BUTTON('Close'),AT(108,155,45,14),USE(?Close)
                       BUTTON('Help'),AT(0,0,45,14),USE(?Help),HIDE,STD(STD:Help)
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
    OF EVENT:CloseDown
      WinResize.Kill()
    OF EVENT:CloseWindow
      WinResize.Kill()
    OF EVENT:DoResize
      ForceRefresh = True
      DO RefreshWindow
    OF EVENT:GainFocus
      REL1::CurrentChoice = CHOICE(?RelTree)
      GET(Queue:RelTree,REL1::CurrentChoice)
      REL1::NewItemLevel = REL1::Level
      REL1::NewItemPosition = REL1::Position
      DO REL1::RefreshTree
      ForceRefresh = True
      IF NOT WindowInitialized
        DO InitializeWindow
        WindowInitialized = True
      ELSE
        DO RefreshWindow
      END
    OF EVENT:OpenWindow
      DO REL1::AssignButtons
      IF NOT WindowInitialized
        DO InitializeWindow
        WindowInitialized = True
      END
      SELECT(?RelTree)
    OF EVENT:Sized
      POST(EVENT:DoResize,0,THREAD())
    OF Event:Rejected
      BEEP
      DISPLAY(?)
      SELECT(?)
    ELSE
      DO REL1::ReltreeDispatch
    END
    CASE FIELD()
    OF ?RelTree
      CASE EVENT()
      OF EVENT:NewSelection
        IF KEYCODE() = MouseRight
          EXECUTE(POPUP('&Insert|&Change|&Delete|-|&Expand All|Co&ntract All'))
            DO REL1::AddEntry
            DO REL1::EditEntry
            DO REL1::RemoveEntry
            DO REL1::ExpandAll
            DO REL1::ContractAll
          END
        END
      OF EVENT:Expanded
        DO REL1::LoadLevel
      OF EVENT:Contracted
        DO REL1::UnloadLevel
      ELSE
        CASE EVENT()
        OF Event:AlertKey
          CASE KEYCODE()
          OF CtrlRight
            ?RelTree{PropList:MouseDownRow} = CHOICE(?RelTree)
            POST(Event:Expanded,?RelTree)
          OF CtrlLeft
            ?RelTree{PropList:MouseDownRow} = CHOICE(?RelTree)
            POST(Event:Contracted,?RelTree)
          OF MouseLeft2
            DO REL1::EditEntry
          END
        END
      END
    OF ?Expand
      CASE EVENT()
      OF EVENT:Accepted
        SETCURSOR(CURSOR:Wait)
        DO SyncWindow
        ?RelTree{PropList:MouseDownRow} = CHOICE(?RelTree)
        DO REL1::ExpandAll
        SETCURSOR
      END
    OF ?Contract
      CASE EVENT()
      OF EVENT:Accepted
        SETCURSOR(CURSOR:Wait)
        DO SyncWindow
        ?RelTree{PropList:MouseDownRow} = CHOICE(?RelTree)
        DO REL1::ContractAll
        SETCURSOR
      END
    OF ?Insert
      CASE EVENT()
      OF EVENT:Accepted
        DO SyncWindow
        ?RelTree{PropList:MouseDownRow} = CHOICE(?RelTree)
        DO REL1::AddEntry
      END
    OF ?Change
      CASE EVENT()
      OF EVENT:Accepted
        DO SyncWindow
        ?RelTree{PropList:MouseDownRow} = CHOICE(?RelTree)
        DO REL1::EditEntry
      END
    OF ?Delete
      CASE EVENT()
      OF EVENT:Accepted
        DO SyncWindow
        ?RelTree{PropList:MouseDownRow} = CHOICE(?RelTree)
        DO REL1::RemoveEntry
      END
    OF ?Close
      CASE EVENT()
      OF EVENT:Accepted
        DO SyncWindow
        IF LocalRequest = SelectRecord
           LocalResponse = RequestCancelled
        ELSE
           LocalResponse = RequestCompleted
        END
        POST(EVENT:CloseWindow)
      END
    END
  END
  DO ProcedureReturn
!---------------------------------------------------------------------------
PrepareProcedure ROUTINE
  IF Classes::Used = 0
    CheckOpen(Classes,1)
  END
  Classes::Used += 1
  IF Courses::Used = 0
    CheckOpen(Courses,1)
  END
  Courses::Used += 1
  IF Enrollment::Used = 0
    CheckOpen(Enrollment,1)
  END
  Enrollment::Used += 1
  IF Students::Used = 0
    CheckOpen(Students,1)
  END
  Students::Used += 1
  FilesOpened = TRUE
  DO BindFields
  DO REL1::ContractAll
  OPEN(window)
  WindowOpened=True
  ?RelTree{Prop:IconList,1} = '~Closed.ico'
  ?RelTree{Prop:IconList,2} = '~Open.ico'
  ?RelTree{Prop:IconList,3} = '~~open.ico'
  ?RelTree{Prop:Selected} = 1
  window{PROP:MinWidth}=162                                ! Restrict the minimum window width
  window{PROP:MinHeight}=174                               ! Restrict the minimum window height
  WinResize.Init(AppStrategy:Spread)
  Do DefineListboxStyle
  ?RelTree{Prop:Alrt,255} = CtrlRight
  ?RelTree{Prop:Alrt,254} = CtrlLeft
  ?RelTree{Prop:Alrt,253} = MouseLeft2

!---------------------------------------------------------------------------
BindFields ROUTINE
  BIND(CLA:RECORD)
  BIND(COU:RECORD)
  BIND('COU:CompleteDescription',COU:CompleteDescription)
  BIND(ENR:RECORD)
  BIND(STU:RECORD)
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
    Classes::Used -= 1
    IF Classes::Used = 0 THEN CLOSE(Classes).
    Courses::Used -= 1
    IF Courses::Used = 0 THEN CLOSE(Courses).
    Enrollment::Used -= 1
    IF Enrollment::Used = 0 THEN CLOSE(Enrollment).
    Students::Used -= 1
    IF Students::Used = 0 THEN CLOSE(Students).
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
REL1::ReltreeDispatch ROUTINE
  IF ToolbarMode = TreeMode AND ActiveReltree = ?RelTree THEN
    CASE ACCEPTED()
    OF TBarBrwHelp
      PRESSKEY(F1Key)
    OF TBarBrwBottom TO TBarBrwUp
      ?RelTree{PROPLIST:MouseDownRow} = CHOICE(?RelTree) !! Server routines assume this
      EXECUTE(ACCEPTED()-TBarBrwBottom+1)
        DO REL1::NextParent
        DO REL1::PreviousParent
        DO REL1::NextLevel
        DO REL1::PreviousLevel
        DO REL1::NextRecord
        DO REL1::PreviousRecord
      END
    OF TBarBrwInsert TO TBarBrwDelete
      ?RelTree{PROPLIST:MouseDownRow} = CHOICE(?RelTree) !! Server routines assume this
      EXECUTE(ACCEPTED()-TBarBrwInsert+1)
        DO REL1::AddEntry
        DO REL1::EditEntry
        DO REL1::RemoveEntry
      END
    END
  END

REL1::NextParent ROUTINE
  GET(Queue:RelTree,CHOICE(?RelTree))
  IF ABS(REL1::Level) > 1 THEN
    REL1::SaveLevel = ABS(REL1::Level)-1
    DO REL1::NextSavedLevel
  END

REL1::PreviousParent ROUTINE
  GET(Queue:RelTree,CHOICE(?RelTree))
  IF ABS(REL1::Level) > 1 THEN
    REL1::SaveLevel = ABS(REL1::Level)-1
    DO REL1::PreviousSavedLevel
  END

REL1::NextLevel ROUTINE
  GET(Queue:RelTree,CHOICE(?RelTree))
  REL1::SaveLevel = ABS(REL1::Level)
  DO REL1::NextSavedLevel

REL1::NextSavedLevel ROUTINE
  LOOP
    LOOP
      GET(Queue:RelTree,POINTER(Queue:RelTree)+1)
      IF ERRORCODE() THEN
        EXIT                ! Unable to find another record on similar level
      END
    WHILE ABS(REL1::Level) > REL1::SaveLevel
    IF ABS(REL1::Level) = REL1::SaveLevel THEN
      SELECT(?RelTree,POINTER(Queue:RelTree))
      EXIT
    END
    REL1::SavePointer = POINTER(Queue:RelTree)
    ?RelTree{PROPLIST:MouseDownRow} = REL1::SavePointer
    DO REL1::LoadLevel
    GET(Queue:RelTree,REL1::SavePointer)
  END

REL1::PreviousSavedLevel ROUTINE
  LOOP
    LOOP
      GET(Queue:RelTree,POINTER(Queue:RelTree)-1)
      IF ERRORCODE() THEN
        EXIT                ! Unable to find another record on similar level
      END
    WHILE ABS(REL1::Level) > REL1::SaveLevel
    IF ABS(REL1::Level) = REL1::SaveLevel THEN
      SELECT(?RelTree,POINTER(Queue:RelTree))
      EXIT
    END
    REL1::SavePointer = POINTER(Queue:RelTree)
    REL1::SaveRecords = RECORDS(Queue:RelTree)
    ?RelTree{PROPLIST:MouseDownRow} = REL1::SavePointer
    DO REL1::LoadLevel
    IF RECORDS(Queue:RelTree) <> REL1::SaveRecords THEN
      REL1::SavePointer += 1 + RECORDS(Queue:RelTree) - REL1::SaveRecords
    END
    GET(Queue:RelTree,REL1::SavePointer)
  END

REL1::PreviousLevel ROUTINE
  GET(Queue:RelTree,CHOICE(?RelTree))
  REL1::SaveLevel = ABS(REL1::Level)
  DO REL1::PreviousSavedLevel

REL1::NextRecord ROUTINE
  DO REL1::LoadLevel
  IF CHOICE(?RelTree) < RECORDS(Queue:RelTree)
    SELECT(?RelTree,CHOICE(?RelTree)+1)
  END

REL1::PreviousRecord ROUTINE
  REL1::SavePointer = CHOICE(?RelTree)-1
  LOOP
    REL1::SaveRecords = RECORDS(Queue:RelTree)
    ?RelTree{PROPLIST:MouseDownRow} = REL1::SavePointer
    DO REL1::LoadLevel
    IF RECORDS(Queue:RelTree) = REL1::SaveRecords THEN
      BREAK
    END
    REL1::SavePointer += RECORDS(Queue:RelTree) - REL1::SaveRecords
  END
  SELECT(?RelTree,REL1::SavePointer)

REL1::AssignButtons ROUTINE
  Toolbarmode = TreeMode
  ActiveReltree = ?RelTree
  ENABLE(TBarBrwBottom,TBarBrwUp)
  TBarBrwBottom{PROP:ToolTip}='Go to next parent record'
  TBarBrwTop{PROP:ToolTip}='Go to parent record'
  TBarBrwPageDown{PROP:ToolTip}='Go to next similar record'
  TBarBrwPageUp{PROP:ToolTip}='Go to previous similar record'
  TBarBrwDown{PROP:ToolTip}='Go to next record'
  TBarBrwUP{PROP:ToolTip}='Go to previous record'
  IF ?Delete{PROP:Disable} THEN
    DISABLE(TBarBrwDelete)
  ELSE
    ENABLE(TBarBrwDelete)
  END
  IF ?Insert{PROP:Disable} THEN
    DISABLE(TBarBrwInsert)
  ELSE
    ENABLE(TBarBrwInsert)
  END
  IF ?Change{PROP:Disable} THEN
    DISABLE(TBarBrwChange)
  ELSE
    ENABLE(TBarBrwChange)
  END
  IF ?Help{PROP:Disable} THEN
    DISABLE(TBarBrwHelp)
  ELSE
    ENABLE(TBarBrwHelp)
  END

!---------------------------------------------------------------------------
REL1::Load:Courses ROUTINE
!|
!| This routine is used to load the base level of the RelationTree.
!|
!| First, the Title line is added.
!|
!| Next, each record of the file Courses is read. If the record is not filtered,
!| then the following happens:
!|
!|   First, the queue REL1::LoadedQueue is searched, to see if the tree branch
!|   corresponding to the record is "loaded", that is, if the branch is currently opened.
!|
!|   If the branch is open, then the records for that branch are read from the file
!|   Classes. This is done in the routine REL1::Load:Classes.
!|
!|   If the branch is not open, then the RelationTree looks for a single record from
!|   Classes, to see if any child records are available. If they are, the
!|   branch can be expanded, so REL1::Level gets a -1. This
!|   value is used by the list box to display a "closed" box next to the entry.
!|
!|   Finally, the queue record that corresponds to the Courses record read is
!|   formatted and added to the queue Queue:RelTree. This is done in the routine
!|   REL1::Format:Courses.
!|
  REL1::Display = 'Classes and Enrollments'
  REL1::Loaded = 0
  REL1::Position = ''
  REL1::Level = 0
  REL1::Icon = 3
  REL1::NormalFG = -1
  REL1::NormalBG = -1
  REL1::SelectedFG = -1
  REL1::SelectedBG = -1
  ADD(Queue:RelTree)
  SET(Courses)
  LOOP
    NEXT(Courses)
    IF ERRORCODE()
      IF ERRORCODE() = BadRecErr
        BREAK
      ELSE
        StandardWarning(Warn:RecordFetchError,'Students')
        POST(Event:CloseWindow)
        EXIT
      END
    END
    REL1::Loaded = 0
    REL1::Position = POSITION(Courses)
    REL1::Level = 1
    REL1::LoadedLevel = ABS(REL1::Level)
    REL1::LoadedPosition = REL1::Position
    GET(REL1::LoadedQueue,REL1::LoadedLevel,REL1::LoadedPosition)
    IF ERRORCODE() AND NOT REL1::LoadAll
      CLA:CourseNumber = COU:Number
      CLEAR(CLA:ClassNumber,0)
      SET(CLA:KeyCourseNumber,CLA:KeyCourseNumber)
      LOOP
        NEXT(Classes)
        IF ERRORCODE()
          IF ERRORCODE() = BadRecErr
            BREAK
          ELSE
            StandardWarning(Warn:RecordFetchError,'Classes')
            POST(Event:CloseWindow)
            EXIT
          END
        END
        IF UPPER(CLA:CourseNumber) <> UPPER(COU:Number) THEN BREAK.
        REL1::Level = -1
        BREAK
      END
      DO REL1::Format:Courses
      ADD(Queue:RelTree,POINTER(Queue:RelTree)+1)
    ELSE
      IF REL1::LoadAll
        ADD(REL1::LoadedQueue,REL1::LoadedLevel,REL1::LoadedPosition)
      END
      REL1::Level = 1
      REL1::Loaded = True
      DO REL1::Format:Courses
      ADD(Queue:RelTree,POINTER(Queue:RelTree)+1)
      DO REL1::Load:Classes
    END
  END

!---------------------------------------------------------------------------
REL1::Format:Courses ROUTINE
!|
!| This routine formats a line of the display queue Queue:RelTree to display the
!| contents of a record of Courses.
!|
!| First, the variable DisplayString is assigned the formatted value.
!|
!| Next, the queue variable REL1::Display is assigned the value in
!| DisplayString. It is possible for the display string to be reformatted in
!| the EMBED point "Relation Tree, Before Setting Display on Primary File".
!|
!| Next, any coloring done to the line is performed.
!|
!| Next, any icon assigments are made.
!|
  DisplayString = COU:Description
  REL1::Display = DisplayString
  REL1::NormalFG = 255
  REL1::NormalBG = -1
  REL1::SelectedFG = 255
  REL1::SelectedBG = -1
  IF REL1::Level > 0
    REL1::Icon = 2
  ELSIF REL1::Level < 0
    REL1::Icon = 1
  ELSE
    REL1::Icon = 0
  END

!---------------------------------------------------------------------------
REL1::LoadLevel ROUTINE
!|
!| This routine is used to load a single level of the RelationTree.
!|
!| First, we see where the load comes from. Since the alert-key handling sets
!| ?RelTree{PropList:MouseDownRow} to CHOICE, we can rely on this property
!| containing the correct selection.
!|
!| Next, we retrieve the Queue:RelTree record that corresponds to the requested
!| load row. If the requested load row is already loaded, we don't have to do
!| anything. If the requested row is not loaded...
!|
!|   First, we set REL1::Level to a positive value for the selected
!|   row and put that record back into the queue Queue:RelTree. The presence of
!|   records with a greater Level below this record in the queue tells the
!|   listbox that the level is opened.
!|
!|   Next, we add a record the the queue REL1::LoadedQueue. This queue
!|   is used to rebuild the display when necessary.
!|
!|   Next, we retrieve the file record that corresponds to the requested load row.
!|
!|   Finally, we reformat the Queue:RelTree entry. This allows for any changes in icon
!|   and colors based on conditional usage.
!|
  REL1::CurrentChoice = ?RelTree{PropList:MouseDownRow}
  GET(Queue:RelTree,REL1::CurrentChoice)
  IF NOT REL1::Loaded
    REL1::Level = ABS(REL1::Level)
    PUT(Queue:RelTree)
    REL1::Loaded = True
    REL1::LoadedLevel = ABS(REL1::Level)
    REL1::LoadedPosition = REL1::Position
    ADD(REL1::LoadedQueue,REL1::LoadedLevel,REL1::LoadedPosition)
    EXECUTE(ABS(REL1::Level))
      BEGIN
        REGET(Courses,REL1::Position)
        DO REL1::Format:Courses
      END
      BEGIN
        REGET(Classes,REL1::Position)
        DO REL1::Format:Classes
      END
      BEGIN
        REGET(Enrollment,REL1::Position)
        DO REL1::Format:Enrollment
      END
    END
    PUT(Queue:RelTree)
    EXECUTE(ABS(REL1::Level))
      DO REL1::Load:Classes
      DO REL1::Load:Enrollment
    END
  END
!---------------------------------------------------------------------------
REL1::UnloadLevel ROUTINE
!|
!| This routine is used to unload a level of the RelationTree.
!|
!| First, we see where the unload comes from. Since the alert-key handling sets
!| ?RelTree{PropList:MouseDownRow} to CHOICE, we can rely on this property
!| containing the correct selection.
!|
!| Next, we retrieve the Queue:RelTree record that corresponds to the requested
!| unload row. If the requested load row isn't loaded, we don't have to do
!| anything. If the requested row is loaded...
!|
!|   First, we set REL1::Level to a negative value for the selected
!|   row and put that record back into the queue Queue:RelTree. Since there
!|   won't be any records at lower levels, we use the negative value to signal
!|   the listbox that the level is closed, but children exist.
!|
!|   Next, we retrieve the record the the queue REL1::LoadedQueue that
!|   corresponds to the unloaded level. This queue record is then deleted.
!|
!|   Next, we retrieve the file record that corresponds to the requested load row.
!|
!|   Next, we reformat the Queue:RelTree entry. This allows for any changes in icon
!|   and colors based on conditional usage.
!|
!|   Finally, we run through all of the Queue:RelTree entries for branches below the
!|   unloaded level, and delete these entries.
!|
  REL1::CurrentChoice = ?RelTree{PropList:MouseDownRow}
  GET(Queue:RelTree,REL1::CurrentChoice)
  IF REL1::Loaded
    REL1::Level = -ABS(REL1::Level)
    PUT(Queue:RelTree)
    REL1::Loaded = False
    REL1::LoadedLevel = ABS(REL1::Level)
    REL1::LoadedPosition = REL1::Position
    GET(REL1::LoadedQueue,REL1::LoadedLevel,REL1::LoadedPosition)
    IF NOT ERRORCODE()
      DELETE(REL1::LoadedQueue)
    END
    EXECUTE(ABS(REL1::Level))
      BEGIN
        REGET(Courses,REL1::Position)
        DO REL1::Format:Courses
      END
      BEGIN
        REGET(Classes,REL1::Position)
        DO REL1::Format:Classes
      END
      BEGIN
        REGET(Enrollment,REL1::Position)
        DO REL1::Format:Enrollment
      END
    END
    PUT(Queue:RelTree)
    REL1::CurrentLevel = ABS(REL1::Level)
    REL1::CurrentChoice += 1
    LOOP
      GET(Queue:RelTree,REL1::CurrentChoice)
      IF ERRORCODE() THEN BREAK.
      IF ABS(REL1::Level) <= REL1::CurrentLevel THEN BREAK.
      DELETE(Queue:RelTree)
    END
  END
!---------------------------------------------------------------------------
REL1::Load:Classes ROUTINE
!|
!| This routine is used to load the base level of the RelationTree.
!|
!| For each record of the file Classes is read. If the record is not filtered,
!| then the following happens:
!|
!|   First, the queue REL1::LoadedQueue is searched, to see if the tree branch
!|   corresponding to the record is "loaded", that is, if the branch is currently opened.
!|
!|   If the branch is open, then the records for that branch are read from the file
!|   Enrollment. This is done in the routine REL1::Load:Enrollment.
!|
!|   If the branch is not open, then the RelationTree looks for a single record from
!|   Enrollment, to see if any child records are available. If they are, the
!|   branch can be expanded, so REL1::Level gets a -2. This
!|   value is used by the list box to display a "closed" box next to the entry.
!|
!|   Finally, the queue record that corresponds to the Classes record read is
!|   formatted and added to the queue Queue:RelTree. This is done in the routine
!|   REL1::Format:Classes.
!|
  CLA:CourseNumber = COU:Number
  CLEAR(CLA:ClassNumber)
  SET(CLA:KeyCourseNumber,CLA:KeyCourseNumber)
  LOOP
    NEXT(Classes)
    IF ERRORCODE()
      IF ERRORCODE() = BadRecErr
        BREAK
      ELSE
        StandardWarning(Warn:RecordFetchError,'Classes')
        POST(Event:CloseWindow)
        EXIT
      END
    END
    IF CLA:CourseNumber <> COU:Number THEN BREAK.
    REL1::Loaded = 0
    REL1::Position = POSITION(Classes)
    REL1::Level = 2
    REL1::LoadedLevel = ABS(REL1::Level)
    REL1::LoadedPosition = REL1::Position
    GET(REL1::LoadedQueue,REL1::LoadedLevel,REL1::LoadedPosition)
    IF ERRORCODE() AND NOT REL1::LoadAll
      ENR:ClassNumber = CLA:ClassNumber
      CLEAR(ENR:StudentNumber,0)
      SET(ENR:SeqStu,ENR:SeqStu)
      LOOP
        NEXT(Enrollment)
        IF ERRORCODE()
          IF ERRORCODE() = BadRecErr
            BREAK
          ELSE
            StandardWarning(Warn:RecordFetchError,'Enrollment')
            POST(Event:CloseWindow)
            EXIT
          END
        END
        IF UPPER(ENR:ClassNumber) <> UPPER(CLA:ClassNumber) THEN BREAK.
        REL1::Level = -2
        BREAK
      END
      DO REL1::Format:Classes
      ADD(Queue:RelTree,POINTER(Queue:RelTree)+1)
    ELSE
      IF REL1::LoadAll
        ADD(REL1::LoadedQueue,REL1::LoadedLevel,REL1::LoadedPosition)
      END
      REL1::Level = 2
      REL1::Loaded = True
      DO REL1::Format:Classes
      ADD(Queue:RelTree,POINTER(Queue:RelTree)+1)
      DO REL1::Load:Enrollment
    END
  END

!-------------------------------------------------------
REL1::Format:Classes ROUTINE
!|
!| This routine formats a line of the display queue Queue:RelTree to display the
!| contents of a record of Classes.
!|
!| First, the variable DisplayString is assigned the formatted value.
!|
!| Next, the queue variable REL1::Display is assigned the value in
!| DisplayString. It is possible for the display string to be reformatted in
!| the EMBED point "Relation Tree, Before Setting Display on Primary File".
!|
!| Next, any coloring done to the line is performed.
!|
!| Next, any icon assigments are made.
!|
  DisplayString = CLA:ScheduledTime
  REL1::Display = DisplayString
  REL1::NormalFG = 16711680
  REL1::NormalBG = -1
  REL1::SelectedFG = 16711680
  REL1::SelectedBG = -1
  IF REL1::Level > 0
    REL1::Icon = 2
  ELSIF REL1::Level < 0
    REL1::Icon = 1
  ELSE
    REL1::Icon = 0
  END
!---------------------------------------------------------------------------
REL1::Load:Enrollment ROUTINE
!|
!| This routine is used to load the base level of the RelationTree.
!|
!| Next, each record of the file Enrollment is read. If the record is not filtered,
!| the queue record that corresponds to this record is formatted and added to the queue
!| Queue:RelTree. This is done in the routine REL1::Format:Enrollment.
!|
  ENR:ClassNumber = CLA:ClassNumber
  CLEAR(ENR:StudentNumber)
  SET(ENR:SeqStu,ENR:SeqStu)
  LOOP
    NEXT(Enrollment)
    IF ERRORCODE()
      IF ERRORCODE() = BadRecErr
        BREAK
      ELSE
        StandardWarning(Warn:RecordFetchError,'Enrollment')
        POST(Event:CloseWindow)
        EXIT
      END
    END
    IF ENR:ClassNumber <> CLA:ClassNumber THEN BREAK.
    REL1::Loaded = 0
    REL1::Position = POSITION(Enrollment)
    REL1::Level = 3
    DO REL1::Format:Enrollment
    ADD(Queue:RelTree,POINTER(Queue:RelTree)+1)
  END

!-------------------------------------------------------
REL1::Format:Enrollment ROUTINE
!|
!| This routine formats a line of the display queue Queue:RelTree to display the
!| contents of a record of Enrollment.
!|
!| First, the variable DisplayString is assigned the formatted value.
!|
!| Next, the queue variable REL1::Display is assigned the value in
!| DisplayString. It is possible for the display string to be reformatted in
!| the EMBED point "Relation Tree, Before Setting Display on Primary File".
!|
!| Next, any coloring done to the line is performed.
!|
!| Next, any icon assigments are made.
!|
   STU:Number = ENR:StudentNumber                          ! Move value for lookup
   GET(Students,STU:KeyStudentNumber)                      ! Get value from file
   IF ERRORCODE()                                          ! IF record not found
     CLEAR(STU:Record)                                     ! Clear the record buffer
   END                                                     ! END (IF record not found)
  DisplayString = CLIP(STU:LastName) & ', ' & STU:FirstName
  REL1::Display = DisplayString
  REL1::NormalFG = -1
  REL1::NormalBG = -1
  REL1::SelectedFG = -1
  REL1::SelectedBG = -1
  REL1::Icon = 1

REL1::AddEntry ROUTINE
  REL1::Action = InsertRecord
  DO REL1::UpdateLoop

REL1::EditEntry ROUTINE
  REL1::Action = ChangeRecord
  DO REL1::UpdateLoop

REL1::RemoveEntry ROUTINE
  REL1::Action = DeleteRecord
  DO REL1::UpdateLoop

REL1::UpdateLoop ROUTINE
  LOOP
    VCRRequest = VCRNone
    ?RelTree{PROPLIST:MouseDownRow} = CHOICE(?RelTree)
    CASE REL1::Action
      OF InsertRecord
        DO REL1::AddEntryServer
      OF DeleteRecord
        DO REL1::RemoveEntryServer
      OF ChangeRecord
        DO REL1::EditEntryServer
    END
    CASE VCRRequest
      OF VCRForward
        DO REL1::NextRecord
      OF VCRBackward
        DO REL1::PreviousRecord
      OF VCRPageForward
        DO REL1::NextLevel
      OF VCRPageBackward
        DO REL1::PreviousLevel
      OF VCRFirst
        DO REL1::PreviousParent
      OF VCRLast
        DO REL1::NextParent
      OF VCRInsert
        DO REL1::PreviousParent
        REL1::Action = InsertRecord
      OF VCRNone
        BREAK
    END
  END
!---------------------------------------------------------------------------
REL1::AddEntryServer ROUTINE
!|
!| This routine calls the RelationTree's update procedure to insert a new record.
!|
!| First, we see where the insert request comes from. Since no alert-key handling
!| is present for editing, ?RelTree{PropList:MouseDownRow} is all that is
!| necessary for editing, and we can rely on this property containing the
!| correct selection.
!|
!| Next, we retrieve the Queue:RelTree record that corresponds to the requested
!| insert row. The new record will be added to the RelationTree level BELOW
!| the requested insert row. To add a first-level record, the RelationTree
!| header must be selected for the insert.
!|
!| Next, the record is cleared, and any related values are primed.
!|
!| Next, GlobalRequest is set to InsertRecord, and the appropriate update procedure
!| is called.
!|
!| Finally, if the insert is successful (GlobalRequest = RequestCompleted) then the
!| RelationTree is refreshed, and the newly inserted record highlighted.
!|
  IF ?Insert{PROP:Disable}
    EXIT
  END
  REL1::CurrentChoice = ?RelTree{PropList:MouseDownRow}
  GET(Queue:RelTree,REL1::CurrentChoice)
  CASE ABS(REL1::Level)
  OF 0
    GET(Courses,0)
    CLEAR(COU:CompleteDescription)
    CLEAR(COU:Record)
    GlobalRequest = InsertRecord
    UpdateCourses
    IF GlobalResponse = RequestCompleted
      REL1::NewItemLevel = 1
      REL1::NewItemPosition = POSITION(Courses)
      DO REL1::RefreshTree
    END
  OF 1
    REGET(Courses,REL1::Position)
    GET(Classes,0)
    CLEAR(CLA:Record)
    CLA:CourseNumber = COU:Number
    GlobalRequest = InsertRecord
    UpdateClasses
    IF GlobalResponse = RequestCompleted
      REL1::NewItemLevel = 2
      REL1::NewItemPosition = POSITION(Classes)
      DO REL1::RefreshTree
    END
  OF 2
  OROF 3
    LOOP WHILE ABS(REL1::Level) = 3
      REL1::CurrentChoice -= 1
      GET(Queue:RelTree,REL1::CurrentChoice)
    UNTIL ERRORCODE()
    REGET(Classes,REL1::Position)
    GET(Enrollment,0)
    CLEAR(ENR:Record)
    ENR:ClassNumber = CLA:ClassNumber
    GlobalRequest = InsertRecord
    UpdateEnrollment
    IF GlobalResponse = RequestCompleted
      REL1::NewItemLevel = 3
      REL1::NewItemPosition = POSITION(Enrollment)
      DO REL1::RefreshTree
    END
  END
!---------------------------------------------------------------------------
REL1::EditEntryServer ROUTINE
!|
!| This routine calls the RelationTree's update procedure to change a record.
!|
!| First, we see where the change request comes from. Since no alert-key handling
!| is present for editing, ?RelTree{PropList:MouseDownRow} is all that is
!| necessary for editing, and we can rely on this property containing the
!| correct selection.
!|
!| Next, we retrieve the Queue:RelTree record that corresponds to the requested
!| change row. and retrieve the appropriate record from disk.
!|
!| Next, GlobalRequest is set to ChangeRecord, and the appropriate update procedure
!| is called.
!|
!| Finally, if the change is successful (GlobalRequest = RequestCompleted) then the
!| RelationTree is refreshed, and the newly changed record highlighted.
!|
  REL1::CurrentChoice = ?RelTree{PropList:MouseDownRow}
  GET(Queue:RelTree,REL1::CurrentChoice)
  CASE ABS(REL1::Level)
  OF 1
    REGET(Courses,REL1::Position)
    GlobalRequest = ChangeRecord
    UpdateCourses
    IF GlobalResponse = RequestCompleted
      REL1::NewItemLevel = 1
      REL1::NewItemPosition = POSITION(Courses)
      DO REL1::RefreshTree
    END
  OF 2
    REGET(Classes,REL1::Position)
    GlobalRequest = ChangeRecord
    UpdateClasses
    IF GlobalResponse = RequestCompleted
      REL1::NewItemLevel = 1
      REL1::NewItemPosition = POSITION(Classes)
      DO REL1::RefreshTree
    END
  OF 3
    REGET(Enrollment,REL1::Position)
    GlobalRequest = ChangeRecord
    UpdateEnrollment
    IF GlobalResponse = RequestCompleted
      REL1::NewItemLevel = 1
      REL1::NewItemPosition = POSITION(Enrollment)
      DO REL1::RefreshTree
    END
  END
!---------------------------------------------------------------------------
REL1::RemoveEntryServer ROUTINE
!|
!| This routine calls the RelationTree's update procedure to delete a record.
!|
!| First, we see where the delete request comes from. Since no alert-key handling
!| is present for editing, ?RelTree{PropList:MouseDownRow} is all that is
!| necessary for editing, and we can rely on this property containing the
!| correct selection.
!|
!| Next, we retrieve the Queue:RelTree record that corresponds to the requested
!| delete row. and retrieve the appropriate record from disk.
!|
!| Next, GlobalRequest is set to DeleteRecord, and the appropriate update procedure
!| is called.
!|
!| Finally, if the change is successful (GlobalRequest = RequestCompleted) then the
!| RelationTree is refreshed, and the record below the deleted record is highlighted.
!|
  IF ?Change{PROP:Disable}
    EXIT
  END
  IF ?Delete{PROP:Disable}
    EXIT
  END
  REL1::CurrentChoice = ?RelTree{PropList:MouseDownRow}
  GET(Queue:RelTree,REL1::CurrentChoice)
  CASE ABS(REL1::Level)
  OF 1
    REGET(Courses,REL1::Position)
    GlobalRequest = DeleteRecord
    UpdateCourses
    IF GlobalResponse = RequestCompleted
      DO REL1::RefreshTree
    END
  OF 2
    REGET(Classes,REL1::Position)
    GlobalRequest = DeleteRecord
    UpdateClasses
    IF GlobalResponse = RequestCompleted
      DO REL1::RefreshTree
    END
  OF 3
    REGET(Enrollment,REL1::Position)
    GlobalRequest = DeleteRecord
    UpdateEnrollment
    IF GlobalResponse = RequestCompleted
      DO REL1::RefreshTree
    END
  END
!---------------------------------------------------------------------------
REL1::RefreshTree ROUTINE
!|
!| This routine is used to refresh the RelationTree.
!|
!| First, the queue Queue:RelTree is FREEd. The display is always completely rebuilt.
!|
!| Next, the routine REL1::Load:Courses is called. This routine will
!| call any other routines necessary to rebuild the display.
!|
!| Finally, if a new item has been added (via REL1::AddEntry), then the
!| queue is searched for that entry, and the record is highlighted.
!|
  FREE(Queue:RelTree)
  DO REL1::Load:Courses
  IF REL1::NewItemLevel
    REL1::CurrentChoice = 0
    LOOP
      REL1::CurrentChoice += 1
      GET(Queue:RelTree,REL1::CurrentChoice)
      IF ERRORCODE() THEN BREAK.
      IF ABS(REL1::Level) <> ABS(REL1::NewItemLevel) THEN CYCLE.
      IF REL1::Position <> REL1::NewItemPosition THEN CYCLE.
      SELECT(?RelTree,REL1::CurrentChoice)
      BREAK
    END
  END
!---------------------------------------------------------------------------
REL1::ContractAll ROUTINE
!|
!| This routine re-initializes the RelationTree.
!|
!| The two queues used by the RelationTree (Queue:RelTree and REL1::LoadedQueue)
!| are FREEd, and the routine REL1::Load:Courses is called, which loads
!| the first level of the RelationTree.
!|
  FREE(Queue:RelTree)
  FREE(REL1::LoadedQueue)
  DO REL1::Load:Courses
!---------------------------------------------------------------------------
REL1::ExpandAll ROUTINE
!|
!| This routine expands every branch of the RelationTree.
!|
!| First, The two queues used by the RelationTree (Queue:RelTree and REL1::LoadedQueue)
!| are FREEd.
!|
!| Next, the variable REL1::LoadAll is set to true, and the routine REL1::Load:Courses
!| is called. Since REL1::LoadAll is True, all branches are completely loaded.
!|
  FREE(Queue:RelTree)
  FREE(REL1::LoadedQueue)
  REL1::LoadAll = True
  DO REL1::Load:Courses
  REL1::LoadAll = False
!!! <summary>
!!! Generated from procedure template - Browse
!!! Select a Classes Record
!!! </summary>
SelectClasses PROCEDURE

CurrentTab           STRING(80)                            ! 
LocalRequest         LONG                                  ! 
OriginalRequest      LONG                                  ! 
LocalResponse        LONG                                  ! 
FilesOpened          LONG                                  ! 
WindowOpened         LONG                                  ! 
WindowInitialized    LONG                                  ! 
ForceRefresh         LONG                                  ! 
RecordFiltered       LONG                                  ! 
LOC:DropThread       LONG                                  ! 
LOC:DropControl      LONG                                  ! 
LOC:ThreadRef        &LONG                                 ! 

BRW1::View:Browse    VIEW(Classes)
                       PROJECT(CLA:ClassNumber)
                       PROJECT(CLA:RoomNumber)
                       PROJECT(CLA:ScheduledTime)
                       PROJECT(CLA:CourseNumber)
                       PROJECT(CLA:TeacherNumber)
                       JOIN(TEA:KeyTeacherNumber,CLA:TeacherNumber)
                         PROJECT(TEA:LastName)
                         PROJECT(TEA:Number)
                       END
                       JOIN(COU:KeyNumber,CLA:CourseNumber)
                         PROJECT(COU:Description)
                         PROJECT(COU:Number)
                       END
                     END
Queue:Browse:1       QUEUE,PRE()                           ! Browsing Queue
BRW1::CLA:ClassNumber  LIKE(CLA:ClassNumber)               ! Queue Display field
BRW1::COU:Description  LIKE(COU:Description)               ! Queue Display field
BRW1::CLA:RoomNumber   LIKE(CLA:RoomNumber)                ! Queue Display field
BRW1::CLA:ScheduledTime LIKE(CLA:ScheduledTime)            ! Queue Display field
BRW1::TEA:LastName     LIKE(TEA:LastName)                  ! Queue Display field
BRW1::CLA:CourseNumber LIKE(CLA:CourseNumber)              ! Queue Display field
BRW1::CLA:TeacherNumber LIKE(CLA:TeacherNumber)            ! Queue Display field
BRW1::TEA:Number       LIKE(TEA:Number)                    ! Queue Display field
BRW1::COU:Number       LIKE(COU:Number)                    ! Queue Display field
BRW1::Mark             BYTE                                ! Record mark flag
BRW1::Position         STRING(1024)                        ! Queue POSITION information
                     END                                   ! END (Browsing Queue)
BRW1::UsingAdditionalSortOrder BYTE                        ! When true the view is using PROP:Order
BRW1::CurrentScroll  BYTE                                  ! Queue position of scroll thumb
BRW1::ScrollRecordCount LONG                               ! Queue position of scroll thumb
BRW1::SkipFirst      BYTE                                  ! Skip first retrieved record in page fill
BRW1::Sort1:KeyDistribution LIKE(CLA:CourseNumber),DIM(100)
BRW1::Sort1:LowValue LIKE(CLA:CourseNumber)                ! Queue position of scroll thumb
BRW1::Sort1:HighValue LIKE(CLA:CourseNumber)               ! Queue position of scroll thumb
BRW1::Sort2:KeyDistribution LIKE(CLA:TeacherNumber),DIM(100)
BRW1::Sort2:LowValue LIKE(CLA:TeacherNumber)               ! Queue position of scroll thumb
BRW1::Sort2:HighValue LIKE(CLA:TeacherNumber)              ! Queue position of scroll thumb
BRW1::Sort3:KeyDistribution LIKE(CLA:ClassNumber),DIM(100)
BRW1::Sort3:LowValue LIKE(CLA:ClassNumber)                 ! Queue position of scroll thumb
BRW1::Sort3:HighValue LIKE(CLA:ClassNumber)                ! Queue position of scroll thumb
BRW1::CurrentEvent   LONG                                  !
BRW1::CurrentChoice  LONG                                  !
BRW1::RecordCount    LONG                                  !
BRW1::SortOrder      BYTE                                  !
BRW1::LocateMode     BYTE                                  !
BRW1::RefreshMode    BYTE                                  !
BRW1::LastSortOrder  BYTE                                  !
BRW1::FillDirection  BYTE                                  !
BRW1::AddQueue       BYTE                                  !
BRW1::Changed        BYTE                                  !
BRW1::RecordStatus   BYTE                                  ! Flag for Range/Filter test
BRW1::ItemsToFill    LONG                                  ! Controls records retrieved
BRW1::MaxItemsInList LONG                                  ! Retrieved after window opened
BRW1::HighlightedPosition STRING(1024)                     ! POSITION of located record
BRW1::NewSelectPosted BYTE                                 ! Queue position of located record
BRW1::PopupText      CSTRING(10000)                        !
BRW1::ActiveInvisible BYTE(1)                              !
BRW1::LoadPending    BYTE                                  !
ToolBarMode          UNSIGNED,AUTO
BrowseButtons        GROUP                      !info for current browse with focus
ListBox                SIGNED                   !Browse list control
InsertButton           SIGNED                   !Browse insert button
ChangeButton           SIGNED                   !Browse change button
DeleteButton           SIGNED                   !Browse delete button
SelectButton           SIGNED                   !Browse select button
                     END
QuickWindow          WINDOW('Drag Class to Enroll in'),AT(,,189,191),FONT('MS Sans Serif',8,COLOR:Black),CENTER, |
  GRAY,IMM,MDI,HLP('~SelectClasses'),SYSTEM
                       LIST,AT(8,30,172,142),USE(?Browse:1),HVSCROLL,DRAGID('Classes'),FORMAT('[52L(2)|M~Class' & |
  ' Number~@P##-#####P@120L(2)|M~Description~@S30@/52C(2)|M~Room~C(0)@n4@80L(2)|M~Sched' & |
  'uled Time~@s20@80L(2)|M~Instructor~@S20@]|M'),FROM(Queue:Browse:1),IMM,MSG('Browsing Records')
                       SHEET,AT(4,4,180,172),USE(?CurrentTab)
                         TAB('by Class Number')
                         END
                         TAB('by Course Number')
                         END
                         TAB('by Teacher Number')
                         END
                       END
                       BUTTON('Help'),AT(0,0,45,14),USE(?Help),HIDE,STD(STD:Help)
                     END
  CODE
  PUSHBIND
  !Setup inter-thread processes
  LOC:DropThread  = GLO:DropThread
  LOC:DropControl = GLO:DropControl
  LOC:ThreadRef &= GLO:ThreadRef
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
    OF EVENT:CloseWindow
      !Flag thread closed
      LOC:ThreadRef = 0
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
      DO BRW1::AssignButtons
      IF NOT WindowInitialized
        DO InitializeWindow
        WindowInitialized = True
      END
      SELECT(?Browse:1)
    OF EVENT:Sized
      POST(EVENT:DoResize,0,THREAD())
    OF Event:Rejected
      BEEP
      DISPLAY(?)
      SELECT(?)
    ELSE
      IF ToolBarMode=BrowseMode THEN
        DO ListBoxDispatch
      END
    END
    CASE FIELD()
    OF ?Browse:1
      CASE EVENT()
      OF EVENT:NewSelection
        DO BRW1::NewSelection
      OF EVENT:ScrollUp
        DO BRW1::ProcessScroll
      OF EVENT:ScrollDown
        DO BRW1::ProcessScroll
      OF EVENT:PageUp
        DO BRW1::ProcessScroll
      OF EVENT:PageDown
        DO BRW1::ProcessScroll
      OF EVENT:ScrollTop
        DO BRW1::ProcessScroll
      OF EVENT:ScrollBottom
        DO BRW1::ProcessScroll
      OF EVENT:ScrollDrag
        DO BRW1::ScrollDrag
      OF EVENT:AlertKey
        DO BRW1::AlertKey
      OF EVENT:Drag
        !Pass dragged data
        DO BRW1::NewSelection
        SETDROPID(FORMAT(CLA:ClassNumber,@P##-#####P))
      END
    OF ?CurrentTab
      CASE EVENT()
      OF EVENT:Accepted
        DO RefreshWindow
      OF EVENT:Selected
        DO RefreshWindow
      OF EVENT:Selecting
        DO RefreshWindow
      OF EVENT:NewSelection
        DO RefreshWindow
      OF EVENT:TabChanging
        DO RefreshWindow
      END
    END
  END
  DO ProcedureReturn
!---------------------------------------------------------------------------
PrepareProcedure ROUTINE
  IF Classes::Used = 0
    CheckOpen(Classes,1)
  END
  Classes::Used += 1
  IF Courses::Used = 0
    CheckOpen(Courses,1)
  END
  Courses::Used += 1
  IF Teachers::Used = 0
    CheckOpen(Teachers,1)
  END
  Teachers::Used += 1
  FilesOpened = TRUE
  DO BindFields
  OPEN(QuickWindow)
  WindowOpened=True
  BRW1::AddQueue = True
  BRW1::RecordCount = 0
  Do DefineListboxStyle
  ?Browse:1{PROP:Alrt,252} = MouseLeft2
  ?Browse:1{PROP:Alrt,255} = AppsKey
  ?Browse:1{PROP:Alrt,253} = MouseRight

!---------------------------------------------------------------------------
BindFields ROUTINE
  BIND(CLA:RECORD)
  BIND(COU:RECORD)
  BIND('COU:CompleteDescription',COU:CompleteDescription)
  BIND(TEA:RECORD)
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
    Classes::Used -= 1
    IF Classes::Used = 0 THEN CLOSE(Classes).
    Courses::Used -= 1
    IF Courses::Used = 0 THEN CLOSE(Courses).
    Teachers::Used -= 1
    IF Teachers::Used = 0 THEN CLOSE(Teachers).
  END
  IF WindowOpened
    CLOSE(QuickWindow)
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
  IF QuickWindow{Prop:AcceptAll} THEN EXIT.
  Do LookupRelated
  DO BRW1::SelectSort
  ?Browse:1{Prop:VScrollPos} = BRW1::CurrentScroll
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
  DO BRW1::GetRecord
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
BRW1::AssignButtons ROUTINE
  CLEAR(BrowseButtons)
  BrowseButtons.ListBox = ?Browse:1
  DO DisplayBrowseToolbar
!----------------------------------------------------------------------
BRW1::SelectSort ROUTINE
!|
!| This routine is called during the RefreshWindow ROUTINE present in every window procedure.
!| The purpose of this routine is to make certain that the BrowseBox is always current with your
!| user's selections. This routine...
!|
!| 1. Checks to see if any of your specified sort-order conditions are met, and if so, changes the sort order.
!| 2. If no sort order change is necessary, this routine checks to see if any of your Reset Fields has changed.
!| 3. If the sort order has changed, or if a reset field has changed, or if the ForceRefresh flag is set...
!|    a. The current record is retrieved from the disk.
!|    b. If the BrowseBox is accessed for the first time, and the Browse has been called to select a record,
!|       the page containing the current record is loaded.
!|    c. If the BrowseBox is accessed for the first time, and the Browse has not been called to select a
!|       record, the first page of information is loaded.
!|    d. If the BrowseBox is not being accessed for the first time, and the Browse sort order has changed, the
!|       new "first" page of information is loaded.
!|    e. If the BrowseBox is not being accessed for the first time, and the Browse sort order hasn't changes,
!|       the page containing the current record is reloaded.
!|    f. The record buffer is refilled from the currently highlighted BrowseBox item.
!|    f. The BrowseBox is reinitialized (BRW1::InitializeBrowse ROUTINE).
!| 4. If step 3 is not necessary, the record buffer is refilled from the currently highlighted BrowseBox item.
!|
  BRW1::LastSortOrder = BRW1::SortOrder
  BRW1::Changed = False
  IF CHOICE(?CurrentTab) = 2
    BRW1::SortOrder = 1
  ELSIF CHOICE(?CurrentTab) = 3
    BRW1::SortOrder = 2
  ELSE
    BRW1::SortOrder = 3
  END
  IF BRW1::SortOrder <> BRW1::LastSortOrder OR BRW1::Changed OR ForceRefresh OR (BRW1::LoadPending AND ?Browse:1{PROP:VISIBLE})
    DO BRW1::GetRecord
    DO BRW1::Reset
    IF BRW1::LastSortOrder = 0
      IF LocalRequest = SelectRecord
        BRW1::LocateMode = LocateOnValue
        DO BRW1::LocateRecord
      ELSE
        FREE(Queue:Browse:1)
        BRW1::RefreshMode = RefreshOnTop
        DO BRW1::RefreshPage
        DO BRW1::PostNewSelection
      END
    ELSE
      IF BRW1::Changed
        FREE(Queue:Browse:1)
        BRW1::RefreshMode = RefreshOnTop
        DO BRW1::RefreshPage
        DO BRW1::PostNewSelection
      ELSE
        BRW1::LocateMode = LocateOnValue
        DO BRW1::LocateRecord
      END
    END
    IF BRW1::RecordCount
      GET(Queue:Browse:1,BRW1::CurrentChoice)
      DO BRW1::FillBuffer
    END
    DO BRW1::InitializeBrowse
  ELSE
    IF BRW1::RecordCount
      GET(Queue:Browse:1,BRW1::CurrentChoice)
      DO BRW1::FillBuffer
    END
  END
!----------------------------------------------------------------------
BRW1::InitializeBrowse ROUTINE
!|
!| This routine initializes the BrowseBox control template. This routine is called when...
!|
!| The BrowseBox sort order has changed. This includes the first time the BrowseBox is accessed.
!| The BrowseBox returns from a record update.
!|
!| This routine performs two main functions.
!|   1. Computes all BrowseBox totals. All records that satisfy the current selection criteria
!|      are read, and totals computed. If no totals are present, this section is not generated,
!|      and may not be present in the code below.
!|   2. Calculates any runtime scrollbar positions. Again, if runtime scrollbars are not used,
!|      the code for runtime scrollbar computation will not be present.
!|
  IF NOT BRW1::ActiveInvisible THEN
     IF NOT ?Browse:1{PROP:Visible} THEN
        BRW1::LoadPending = True
        EXIT
     END
  END
  DO BRW1::Reset
  PREVIOUS(BRW1::View:Browse)
  IF ERRORCODE()
    IF ERRORCODE() = BadRecErr
      DO BRW1::RestoreResetValues
    ELSE
      StandardWarning(Warn:RecordFetchError,'Classes')
      POST(Event:CloseWindow)
    END
    EXIT
  END
  CASE BRW1::SortOrder
  OF 1
    BRW1::Sort1:HighValue = CLA:CourseNumber
  OF 2
    BRW1::Sort2:HighValue = CLA:TeacherNumber
  OF 3
    BRW1::Sort3:HighValue = CLA:ClassNumber
  END
  DO BRW1::Reset
  NEXT(BRW1::View:Browse)
  IF ERRORCODE()
    IF ERRORCODE() = BadRecErr
      DO BRW1::RestoreResetValues
    ELSE
      StandardWarning(Warn:RecordFetchError,'Classes')
      POST(Event:CloseWindow)
    END
    EXIT
  END
  CASE BRW1::SortOrder
  OF 1
    BRW1::Sort1:LowValue = CLA:CourseNumber
    SetupRealStops(BRW1::Sort1:LowValue,BRW1::Sort1:HighValue)
    LOOP BRW1::ScrollRecordCount = 1 TO 100
      BRW1::Sort1:KeyDistribution[BRW1::ScrollRecordCount] = NextRealStop()
    END
  OF 2
    BRW1::Sort2:LowValue = CLA:TeacherNumber
    SetupRealStops(BRW1::Sort2:LowValue,BRW1::Sort2:HighValue)
    LOOP BRW1::ScrollRecordCount = 1 TO 100
      BRW1::Sort2:KeyDistribution[BRW1::ScrollRecordCount] = NextRealStop()
    END
  OF 3
    BRW1::Sort3:LowValue = CLA:ClassNumber
    SetupRealStops(BRW1::Sort3:LowValue,BRW1::Sort3:HighValue)
    LOOP BRW1::ScrollRecordCount = 1 TO 100
      BRW1::Sort3:KeyDistribution[BRW1::ScrollRecordCount] = NextRealStop()
    END
  END
!----------------------------------------------------------------------
BRW1::FillBuffer ROUTINE
!|
!| This routine fills the record buffer from the BrowseBox queue. This gives the appearance
!| that the record is "fresh" from the disk, without the disk access required.
!|
  CLA:ClassNumber = BRW1::CLA:ClassNumber
  COU:Description = BRW1::COU:Description
  CLA:RoomNumber = BRW1::CLA:RoomNumber
  CLA:ScheduledTime = BRW1::CLA:ScheduledTime
  TEA:LastName = BRW1::TEA:LastName
  CLA:CourseNumber = BRW1::CLA:CourseNumber
  CLA:TeacherNumber = BRW1::CLA:TeacherNumber
  TEA:Number = BRW1::TEA:Number
  COU:Number = BRW1::COU:Number
!----------------------------------------------------------------------
BRW1::FillQueue ROUTINE
!|
!| This routine is used to fill the BrowseBox QUEUE from several sources.
!|
!| First, all Format Browse formulae are processed.
!|
!| Next, each field of the BrowseBox is processed. For each field...
!|
!|    The value of the field is placed in the BrowseBox queue.
!|
!| Finally, the POSITION of the current VIEW record is added to the QUEUE
!|
  BRW1::CLA:ClassNumber = CLA:ClassNumber
  BRW1::COU:Description = COU:Description
  BRW1::CLA:RoomNumber = CLA:RoomNumber
  BRW1::CLA:ScheduledTime = CLA:ScheduledTime
  BRW1::TEA:LastName = TEA:LastName
  BRW1::CLA:CourseNumber = CLA:CourseNumber
  BRW1::CLA:TeacherNumber = CLA:TeacherNumber
  BRW1::TEA:Number = TEA:Number
  BRW1::COU:Number = COU:Number
  BRW1::Position = POSITION(BRW1::View:Browse)
!----------------------------------------------------------------------
BRW1::PostNewSelection ROUTINE
!|
!| This routine is used to post the NewSelection EVENT to the window. Because we only want this
!| EVENT processed once, and becuase there are several routines that need to initiate a NewSelection
!| EVENT, we keep a flag that tells us if the EVENT is already waiting to be processed. The EVENT is
!| only POSTed if this flag is false.
!|
  IF NOT BRW1::NewSelectPosted
    BRW1::NewSelectPosted = True
    POST(EVENT:NewSelection,?Browse:1)
  END
!----------------------------------------------------------------------
BRW1::NewSelection ROUTINE
!|
!| This routine performs any window bookkeeping necessary when a new record is selected in the
!| BrowseBox.
!| 1. If the new selection is made with the right mouse button, the popup menu (if applicable) is
!|    processed.
!| 2. The current record is retrieved into the buffer using the BRW1::FillBuffer ROUTINE.
!|    After this, the current vertical scrollbar position is computed, and the scrollbar positioned.
!|
  IF NOT BRW1::ActiveInvisible THEN
     IF NOT ?Browse:1{PROP:Visible} THEN
        BRW1::LoadPending = True
        EXIT
     END
  END
  BRW1::NewSelectPosted = False
  IF KEYCODE() = MouseRight OR KEYCODE() = AppsKey
     SETKEYCODE(0)
    BRW1::PopupText = ''
    IF BRW1::RecordCount
    ELSE
    END
    EXECUTE(POPUP(BRW1::PopupText))
    END
  ELSIF BRW1::RecordCount
    BRW1::CurrentChoice = CHOICE(?Browse:1)
    GET(Queue:Browse:1,BRW1::CurrentChoice)
    DO BRW1::FillBuffer
    IF BRW1::RecordCount = ?Browse:1{PROP:Items}
      IF NOT ?Browse:1{PROP:VScroll}
        ?Browse:1{PROP:VScroll} = True
      END
      CASE BRW1::SortOrder
      OF 1
        LOOP BRW1::CurrentScroll = 1 TO 100
          IF BRW1::Sort1:KeyDistribution[BRW1::CurrentScroll] => CLA:CourseNumber
            IF BRW1::CurrentScroll <= 1
              BRW1::CurrentScroll = 0
            ELSIF BRW1::CurrentScroll = 100
              BRW1::CurrentScroll = 100
            ELSE
            END
            BREAK
          END
        END
      OF 2
        LOOP BRW1::CurrentScroll = 1 TO 100
          IF BRW1::Sort2:KeyDistribution[BRW1::CurrentScroll] => CLA:TeacherNumber
            IF BRW1::CurrentScroll <= 1
              BRW1::CurrentScroll = 0
            ELSIF BRW1::CurrentScroll = 100
              BRW1::CurrentScroll = 100
            ELSE
            END
            BREAK
          END
        END
      OF 3
        LOOP BRW1::CurrentScroll = 1 TO 100
          IF BRW1::Sort3:KeyDistribution[BRW1::CurrentScroll] => CLA:ClassNumber
            IF BRW1::CurrentScroll <= 1
              BRW1::CurrentScroll = 0
            ELSIF BRW1::CurrentScroll = 100
              BRW1::CurrentScroll = 100
            ELSE
            END
            BREAK
          END
        END
      END
    ELSE
      IF ?Browse:1{PROP:VScroll}
        ?Browse:1{PROP:VScroll} = False
      END
    END
    DO RefreshWindow
  END
!---------------------------------------------------------------------
BRW1::ProcessScroll ROUTINE
!|
!| This routine processes any of the six scrolling EVENTs handled by the BrowseBox.
!| If one record is to be scrolled, the ROUTINE BRW1::ScrollOne is called.
!| If a page of records is to be scrolled, the ROUTINE BRW1::ScrollPage is called.
!| If the first or last page is to be displayed, the ROUTINE BRW1::ScrollEnd is called.
!|
!| If an incremental locator is in use, the value of that locator is cleared.
!| Finally, if a Fixed Thumb vertical scroll bar is used, the thumb is positioned.
!|
  IF BRW1::RecordCount
    BRW1::CurrentEvent = EVENT()
    CASE BRW1::CurrentEvent
    OF EVENT:ScrollUp OROF EVENT:ScrollDown
      DO BRW1::ScrollOne
    OF EVENT:PageUp OROF EVENT:PageDown
      DO BRW1::ScrollPage
    OF EVENT:ScrollTop OROF EVENT:ScrollBottom
      DO BRW1::ScrollEnd
    END
    ?Browse:1{PROP:SelStart} = BRW1::CurrentChoice
    DO BRW1::PostNewSelection
  END
!----------------------------------------------------------------------
BRW1::ScrollOne ROUTINE
!|
!| This routine is used to scroll a single record on the BrowseBox. Since the BrowseBox is an IMM
!| listbox, all scrolling must be handled in code. When called, this routine...
!|
!| 1. Sees if scrolling in the intended direction will cause the listbox display to shift. If not,
!|    the routine moves the list box cursor and exits.
!| 2. Calls BRW1::FillRecord to retrieve one record in the direction required.
!|
  IF BRW1::CurrentEvent = EVENT:ScrollUp AND BRW1::CurrentChoice > 1
    BRW1::CurrentChoice -= 1
    EXIT
  ELSIF BRW1::CurrentEvent = EVENT:ScrollDown AND BRW1::CurrentChoice < BRW1::RecordCount
    BRW1::CurrentChoice += 1
    EXIT
  END
  BRW1::ItemsToFill = 1
  BRW1::FillDirection = BRW1::CurrentEvent - 2
  DO BRW1::FillRecord
!----------------------------------------------------------------------
BRW1::ScrollPage ROUTINE
!|
!| This routine is used to scroll a single page of records on the BrowseBox. Since the BrowseBox is
!| an IMM listbox, all scrolling must be handled in code. When called, this routine...
!|
!| 1. Calls BRW1::FillRecord to retrieve one page of records in the direction required.
!| 2. If BRW1::FillRecord doesn't fill a page (BRW1::ItemsToFill > 0), then
!|    the list-box cursor ia shifted.
!|
  BRW1::ItemsToFill = ?Browse:1{PROP:Items}
  BRW1::FillDirection = BRW1::CurrentEvent - 4
  DO BRW1::FillRecord                           ! Fill with next read(s)
  IF BRW1::ItemsToFill
    IF BRW1::CurrentEvent = EVENT:PageUp
      BRW1::CurrentChoice -= BRW1::ItemsToFill
      IF BRW1::CurrentChoice < 1
        BRW1::CurrentChoice = 1
      END
    ELSE
      BRW1::CurrentChoice += BRW1::ItemsToFill
      IF BRW1::CurrentChoice > BRW1::RecordCount
        BRW1::CurrentChoice = BRW1::RecordCount
      END
    END
  END
!----------------------------------------------------------------------
BRW1::ScrollEnd ROUTINE
!|
!| This routine is used to load the first or last page of the displayable set of records.
!| Since the BrowseBox is an IMM listbox, all scrolling must be handled in code. When called,
!| this routine...
!|
!| 1. Resets the BrowseBox VIEW to insure that it reads from the end of the current sort order.
!| 2. Calls BRW1::FillRecord to retrieve one page of records.
!| 3. Selects the record that represents the end of the view. That is, if the first page was loaded,
!|    the first record is highlighted. If the last was loaded, the last record is highlighted.
!|
  FREE(Queue:Browse:1)
  BRW1::RecordCount = 0
  DO BRW1::Reset
  BRW1::ItemsToFill = ?Browse:1{PROP:Items}
  IF BRW1::CurrentEvent = EVENT:ScrollTop
    BRW1::FillDirection = FillForward
  ELSE
    BRW1::FillDirection = FillBackward
  END
  DO BRW1::FillRecord                           ! Fill with next read(s)
  IF BRW1::CurrentEvent = EVENT:ScrollTop
    BRW1::CurrentChoice = 1
  ELSE
    BRW1::CurrentChoice = BRW1::RecordCount
  END
!----------------------------------------------------------------------
BRW1::AlertKey ROUTINE
!|
!| This routine processes any KEYCODEs experienced by the BrowseBox.
!| NOTE: The cursor movement keys are not processed as KEYCODEs. They are processed as the
!|       appropriate BrowseBox scrolling and selection EVENTs.
!| This routine includes handling for double-click. Actually, this handling is in the form of
!| EMBEDs, which are filled by child-control templates.
!| This routine also includes the BrowseBox's locator handling.
!| After a value is entered for locating, this routine sets BRW1::LocateMode to a value
!| of 2 -- EQUATEd to LocateOnValue -- and calls the routine BRW1::LocateRecord.
!|
  IF BRW1::RecordCount
    CASE KEYCODE()                                         ! What keycode was hit
    OF AppsKey
    OROF MouseRight
       IF ?Browse:1{PROPLIST:MouseDownRow}>0
         ?Browse:1{PROP:Selected} = ?Browse:1{PROPLIST:MouseDownRow}
         BRW1::CurrentChoice = CHOICE(?Browse:1)
       END
       DO BRW1::NewSelection
    OF MouseLeft2
      !Pass dragged data
      DO BRW1::NewSelection
      SETDROPID(FORMAT(CLA:ClassNumber,@P##-#####P))
      POST(EVENT:Drop,LOC:DropControl,LOC:DropThread)
    ELSE                                                   ! ELSE (What keycode was hit)
      CASE BRW1::SortOrder
      OF 1
        IF CHR(KEYCHAR())
          IF UPPER(SUB(CLA:CourseNumber,1,1)) = UPPER(CHR(KEYCHAR()))
            BRW1::CurrentEvent = EVENT:ScrollDown
            DO BRW1::ScrollOne
            GET(Queue:Browse:1,BRW1::CurrentChoice)
            DO BRW1::FillBuffer
          END
          IF UPPER(SUB(CLA:CourseNumber,1,1)) = UPPER(CHR(KEYCHAR()))
            ?Browse:1{PROP:SelStart} = BRW1::CurrentChoice
          ELSE
            CLA:CourseNumber = CHR(KEYCHAR())
            CLEAR(CLA:ClassNumber)
            BRW1::LocateMode = LocateOnValue
            DO BRW1::LocateRecord
          END
        END
      OF 2
        IF CHR(KEYCHAR())
          IF UPPER(SUB(CLA:TeacherNumber,1,1)) = UPPER(CHR(KEYCHAR()))
            BRW1::CurrentEvent = EVENT:ScrollDown
            DO BRW1::ScrollOne
            GET(Queue:Browse:1,BRW1::CurrentChoice)
            DO BRW1::FillBuffer
          END
          IF UPPER(SUB(CLA:TeacherNumber,1,1)) = UPPER(CHR(KEYCHAR()))
            ?Browse:1{PROP:SelStart} = BRW1::CurrentChoice
          ELSE
            CLA:TeacherNumber = CHR(KEYCHAR())
            BRW1::LocateMode = LocateOnValue
            DO BRW1::LocateRecord
          END
        END
      OF 3
        IF CHR(KEYCHAR())
          IF UPPER(SUB(CLA:ClassNumber,1,1)) = UPPER(CHR(KEYCHAR()))
            BRW1::CurrentEvent = EVENT:ScrollDown
            DO BRW1::ScrollOne
            GET(Queue:Browse:1,BRW1::CurrentChoice)
            DO BRW1::FillBuffer
          END
          IF UPPER(SUB(CLA:ClassNumber,1,1)) = UPPER(CHR(KEYCHAR()))
            ?Browse:1{PROP:SelStart} = BRW1::CurrentChoice
          ELSE
            CLA:ClassNumber = CHR(KEYCHAR())
            BRW1::LocateMode = LocateOnValue
            DO BRW1::LocateRecord
          END
        END
      END
    END                                                    ! END (What keycode was hit)
  ELSE
    CASE KEYCODE()                                         ! What keycode was hit
    OF AppsKey
    OROF MouseRight
       DO BRW1::NewSelection
    ELSE                                                   ! ELSE (What keycode was hit)
      CASE BRW1::SortOrder
      OF 1
      OF 2
      OF 3
      END
    END
  END
  DO BRW1::PostNewSelection
!----------------------------------------------------------------------
BRW1::ScrollDrag ROUTINE
!|
!| This routine processes the Vertical Scroll Bar arrays to find the free key field value
!| that corresponds to the current scroll bar position.
!|
!| After the scroll position is computed, and the scroll value found, this routine sets
!| BRW1::LocateMode to that scroll value of 2 -- EQUATEd to LocateOnValue --
!| and calls the routine BRW1::LocateRecord.
!|
  IF ?Browse:1{PROP:VScrollPos} <= 1
    POST(EVENT:ScrollTop,?Browse:1)
  ELSIF ?Browse:1{PROP:VScrollPos} = 100
    POST(EVENT:ScrollBottom,?Browse:1)
  ELSE
    CASE BRW1::SortOrder
    OF 1
      CLA:CourseNumber = BRW1::Sort1:KeyDistribution[?Browse:1{PROP:VScrollPos}]
      BRW1::LocateMode = LocateOnValue
      DO BRW1::LocateRecord
    OF 2
      CLA:TeacherNumber = BRW1::Sort2:KeyDistribution[?Browse:1{PROP:VScrollPos}]
      BRW1::LocateMode = LocateOnValue
      DO BRW1::LocateRecord
    OF 3
      CLA:ClassNumber = BRW1::Sort3:KeyDistribution[?Browse:1{PROP:VScrollPos}]
      BRW1::LocateMode = LocateOnValue
      DO BRW1::LocateRecord
    END
  END
!----------------------------------------------------------------------
BRW1::FillRecord ROUTINE
!|
!| This routine is used to retrieve a number of records from the VIEW. The number of records
!| retrieved is held in the variable BRW1::ItemsToFill. If more than one record is
!| to be retrieved, QuickScan is used to minimize reads from the disk.
!|
!| If records exist in the queue (in other words, if the browse has been used before), the record
!| at the appropriate end of the list box is retrieved, and the VIEW is reset to read starting
!| at that record.
!|
!| Next, the VIEW is accessed to retrieve BRW1::ItemsToFill records. Normally, this will
!| result in BRW1::ItemsToFill records being read from the VIEW, but if custom filtering
!| or range limiting is used (via the BRW1::ValidateRecord routine) then any number of records
!| might be read.
!|
!| For each good record, if BRW1::AddQueue is true, the queue is filled using the BRW1::FillQueue
!| routine. The record is then added to the queue. If adding this record causes the BrowseBox queue
!| to contain more records than can be displayed, the record at the opposite end of the queue is
!| deleted.
!|
!| The only time BRW1::AddQueue is false is when the BRW1::LocateRecord routine needs to
!| get the closest record to its record to be located. At this time, the record doesn't need to be
!| added to the queue, so it isn't.
!|
  IF BRW1::RecordCount
    IF BRW1::FillDirection = FillForward
      GET(Queue:Browse:1,BRW1::RecordCount)                ! Get the first queue item
    ELSE
      GET(Queue:Browse:1,1)                                ! Get the first queue item
    END
    RESET(BRW1::View:Browse,BRW1::Position)                ! Reset for sequential processing
    BRW1::SkipFirst = TRUE
  ELSE
    BRW1::SkipFirst = FALSE
  END
  LOOP WHILE BRW1::ItemsToFill
    IF BRW1::View:Browse{PROP:IPRequestCount} = 0
       BRW1::View:Browse{PROP:IPRequestCount} = BRW1::ItemsToFill
    END
    IF BRW1::FillDirection = FillForward
      NEXT(BRW1::View:Browse)
    ELSE
      PREVIOUS(BRW1::View:Browse)
    END
    IF ERRORCODE()
      IF ERRORCODE() = BadRecErr
        DO BRW1::RestoreResetValues
        BREAK
      ELSE
        StandardWarning(Warn:RecordFetchError,'Classes')
        POST(EVENT:CloseWindow)
        EXIT
      END
    END
    IF BRW1::SkipFirst
       BRW1::SkipFirst = FALSE
       IF POSITION(BRW1::View:Browse) = BRW1::Position
          CYCLE
       END
    END
    IF BRW1::AddQueue
      IF BRW1::RecordCount = ?Browse:1{PROP:Items}
        IF BRW1::FillDirection = FillForward
          GET(Queue:Browse:1,1)                            ! Get the first queue item
        ELSE
          GET(Queue:Browse:1,BRW1::RecordCount)            ! Get the first queue item
        END
        DELETE(Queue:Browse:1)
        BRW1::RecordCount -= 1
      END
      DO BRW1::FillQueue
      IF BRW1::FillDirection = FillForward
        ADD(Queue:Browse:1)
      ELSE
        ADD(Queue:Browse:1,1)
      END
      BRW1::RecordCount += 1
    END
    BRW1::ItemsToFill -= 1
  END
  BRW1::AddQueue = True
  EXIT
!----------------------------------------------------------------------
BRW1::LocateRecord ROUTINE
!|
!| This routine is used to find a record in the VIEW, and to display that record
!| in the BrowseBox.
!|
!| This routine has three different modes of operation, which are invoked based on
!| the setting of BRW1::LocateMode. These modes are...
!|
!|   LocateOnPosition (1) - This mode is still supported for 1.5 compatability. This mode
!|                          is the same as LocateOnEdit.
!|   LocateOnValue    (2) - The values of the current sort order key are used. This mode
!|                          used for Locators and when the BrowseBox is called to select
!|                          a record.
!|   LocateOnEdit     (3) - The current record of the VIEW is used. This mode assumes
!|                          that there is an active VIEW record. This mode is used when
!|                          the sort order of the BrowseBox has changed
!|
!| If an appropriate record has been located, the BRW1::RefreshPage routine is
!| called to load the page containing the located record.
!|
!| If an appropriate record is not locate, the last page of the BrowseBox is loaded.
!|
  IF BRW1::LocateMode = LocateOnPosition
    BRW1::LocateMode = LocateOnEdit
  END
  CLOSE(BRW1::View:Browse)
  CASE BRW1::SortOrder
  OF 1
    IF BRW1::UsingAdditionalSortOrder THEN
       BRW1::UsingAdditionalSortOrder = False
       BRW1::View:Browse{PROP:Order} = '+CLA:CourseNumber,+CLA:ClassNumber'
    END
    IF BRW1::LocateMode = LocateOnEdit
      BRW1::HighlightedPosition = POSITION(CLA:KeyCourseNumber)
      RESET(CLA:KeyCourseNumber,BRW1::HighlightedPosition)
    ELSE
      SET(CLA:KeyCourseNumber,CLA:KeyCourseNumber)
    END
  OF 2
    IF BRW1::UsingAdditionalSortOrder THEN
       BRW1::UsingAdditionalSortOrder = False
       BRW1::View:Browse{PROP:Order} = '+CLA:TeacherNumber'
    END
    IF BRW1::LocateMode = LocateOnEdit
      BRW1::HighlightedPosition = POSITION(CLA:KeyTeacherNumber)
      RESET(CLA:KeyTeacherNumber,BRW1::HighlightedPosition)
    ELSE
      SET(CLA:KeyTeacherNumber,CLA:KeyTeacherNumber)
    END
  OF 3
    IF BRW1::UsingAdditionalSortOrder THEN
       BRW1::UsingAdditionalSortOrder = False
       BRW1::View:Browse{PROP:Order} = '+CLA:ClassNumber'
    END
    IF BRW1::LocateMode = LocateOnEdit
      BRW1::HighlightedPosition = POSITION(CLA:KeyClassNumber)
      RESET(CLA:KeyClassNumber,BRW1::HighlightedPosition)
    ELSE
      SET(CLA:KeyClassNumber,CLA:KeyClassNumber)
    END
  END
  IF ERRORCODE()
    StandardWarning(Warn:ViewOpenError)
  END
  OPEN(BRW1::View:Browse)
  IF ERRORCODE()
    StandardWarning(Warn:ViewOpenError)
  END
  IF BRW1::UsingAdditionalSortOrder = True
    CASE BRW1::SortOrder
    OF 1
       SET(BRW1::View:Browse,1)
    OF 2
       SET(BRW1::View:Browse,1)
    OF 3
       SET(BRW1::View:Browse,1)
    END
  END
  FREE(Queue:Browse:1)
  BRW1::RecordCount = 0
  BRW1::ItemsToFill = 1
  BRW1::FillDirection = FillForward                        ! Fill with next read(s)
  BRW1::AddQueue = False
  DO BRW1::FillRecord                                      ! Fill with next read(s)
  BRW1::AddQueue = True
  IF BRW1::ItemsToFill
    BRW1::RefreshMode = RefreshOnBottom
    DO BRW1::RefreshPage
  ELSE
    BRW1::RefreshMode = RefreshOnPosition
    DO BRW1::RefreshPage
  END
  DO BRW1::PostNewSelection
  BRW1::LocateMode = 0
  EXIT
!----------------------------------------------------------------------
BRW1::RefreshPage ROUTINE
!|
!| This routine is used to load a single page of the BrowseBox.
!|
!| If this routine is called with a BRW1::RefreshMode of RefreshOnPosition,
!| the active VIEW record is loaded at the top of the page. Otherwise, if there are
!| records in the browse queue (Queue:Browse:1), then the current page is reloaded, and the
!| currently selected item remains selected.
!|
  IF NOT BRW1::ActiveInvisible THEN
     IF NOT ?Browse:1{PROP:Visible} THEN
        BRW1::LoadPending = True
        EXIT
     END
  END
  SETCURSOR(Cursor:Wait)
  IF BRW1::RefreshMode = RefreshOnPosition
    BRW1::HighlightedPosition = POSITION(BRW1::View:Browse)
    RESET(BRW1::View:Browse,BRW1::HighlightedPosition)
    BRW1::RefreshMode = RefreshOnTop
  ELSIF RECORDS(Queue:Browse:1)
    GET(Queue:Browse:1,BRW1::CurrentChoice)
    IF ERRORCODE()
      GET(Queue:Browse:1,RECORDS(Queue:Browse:1))
    END
    BRW1::HighlightedPosition = BRW1::Position
    GET(Queue:Browse:1,1)
    RESET(BRW1::View:Browse,BRW1::Position)
    BRW1::RefreshMode = RefreshOnCurrent
  ELSE
    BRW1::HighlightedPosition = ''
    DO BRW1::Reset
  END
  FREE(Queue:Browse:1)
  BRW1::RecordCount = 0
  BRW1::ItemsToFill = ?Browse:1{PROP:Items}
  IF BRW1::RefreshMode = RefreshOnBottom
    BRW1::FillDirection = FillBackward
  ELSE
    BRW1::FillDirection = FillForward
  END
  DO BRW1::FillRecord                                      ! Fill with next read(s)
  IF BRW1::HighlightedPosition
    IF BRW1::ItemsToFill
      IF NOT BRW1::RecordCount
        DO BRW1::Reset
      END
      IF BRW1::RefreshMode = RefreshOnBottom
        BRW1::FillDirection = FillForward
      ELSE
        BRW1::FillDirection = FillBackward
      END
      DO BRW1::FillRecord
    END
  END
  IF BRW1::RecordCount
    IF BRW1::HighlightedPosition
      LOOP BRW1::CurrentChoice = 1 TO BRW1::RecordCount
        GET(Queue:Browse:1,BRW1::CurrentChoice)
        IF BRW1::Position = BRW1::HighlightedPosition THEN BREAK.
      END
      IF BRW1::CurrentChoice > BRW1::RecordCount
        BRW1::CurrentChoice = BRW1::RecordCount
      END
    ELSE
      IF BRW1::RefreshMode = RefreshOnBottom
        BRW1::CurrentChoice = RECORDS(Queue:Browse:1)
      ELSE
        BRW1::CurrentChoice = 1
      END
    END
    ?Browse:1{Prop:Selected} = BRW1::CurrentChoice
    DO BRW1::FillBuffer
  ELSE
    CLEAR(CLA:Record)
    BRW1::CurrentChoice = 0
  END
  SETCURSOR()
  BRW1::RefreshMode = 0
  EXIT
BRW1::Reset ROUTINE
!|
!| This routine is used to reset the VIEW used by the BrowseBox.
!|
  IF NOT BRW1::ActiveInvisible THEN
     IF NOT ?Browse:1{PROP:Visible} THEN
        BRW1::LoadPending = True
        EXIT
     END
  END
  BRW1::LoadPending = False
  CLOSE(BRW1::View:Browse)
  CASE BRW1::SortOrder
  OF 1
    IF BRW1::UsingAdditionalSortOrder THEN
       BRW1::UsingAdditionalSortOrder = False
       BRW1::View:Browse{PROP:Order} = '+CLA:CourseNumber,+CLA:ClassNumber'
    END
    SET(CLA:KeyCourseNumber)
  OF 2
    IF BRW1::UsingAdditionalSortOrder THEN
       BRW1::UsingAdditionalSortOrder = False
       BRW1::View:Browse{PROP:Order} = '+CLA:TeacherNumber'
    END
    SET(CLA:KeyTeacherNumber)
  OF 3
    IF BRW1::UsingAdditionalSortOrder THEN
       BRW1::UsingAdditionalSortOrder = False
       BRW1::View:Browse{PROP:Order} = '+CLA:ClassNumber'
    END
    SET(CLA:KeyClassNumber)
  END
  IF ERRORCODE()
    StandardWarning(Warn:ViewOpenError)
  END
  OPEN(BRW1::View:Browse)
  IF ERRORCODE()
    StandardWarning(Warn:ViewOpenError)
  END
  SET(BRW1::View:Browse)
!----------------------------------------------------------------------
BRW1::GetRecord ROUTINE
!|
!| This routine is used to retrieve the VIEW record that corresponds to a
!| chosen listbox record.
!|
  IF BRW1::RecordCount
    BRW1::CurrentChoice = CHOICE(?Browse:1)
    GET(Queue:Browse:1,BRW1::CurrentChoice)
    WATCH(BRW1::View:Browse)
    REGET(BRW1::View:Browse,BRW1::Position)
  END
!----------------------------------------------------------------------
BRW1::RestoreResetValues ROUTINE
!|
!| This routine is used to restore reset values to their saved value
!| after a bad record access from the VIEW.
!|
!--------------------------------------------------------------------------
DisplayBrowseToolbar      ROUTINE
  ENABLE(TBarBrwBottom,TBarBrwLocate)
  TBarBrwHelp{PROP:DISABLE}=?Help{PROP:DISABLE}
  DISABLE(TBarBrwHistory)
  ToolBarMode = BrowseMode
  TBarBrwDown{PROP:ToolTip} = 'Go to the Next Record'
  TBarBrwBottom{PROP:ToolTip} = 'Go to the Last Page'
  TBarBrwTop{PROP:ToolTip} = 'Go to the First Page'
  TBarBrwPageDown{PROP:ToolTip} = 'Go to the Next Page'
  TBarBrwPageUp{PROP:ToolTip} = 'Go to the Prior Page'
  TBarBrwDown{PROP:ToolTip} = 'Go to the Next Record'
  TBarBrwUP{PROP:ToolTip} = 'Go to the Prior Record'
  TBarBrwInsert{PROP:ToolTip} = 'Insert a new Record'
  DISPLAY(TBarBrwFirst,TBarBrwLast)
!--------------------------------------------------------------------------
ListBoxDispatch ROUTINE
  DO DisplayBrowseToolbar
  IF ACCEPTED() THEN            !trap remote browse box control calls
    EXECUTE(ACCEPTED()-TBarBrwBottom+1)
      POST(EVENT:ScrollBottom,BrowseButtons.ListBox)
      POST(EVENT:ScrollTop,BrowseButtons.ListBox)
      POST(EVENT:PageDown,BrowseButtons.ListBox)
      POST(EVENT:PageUp,BrowseButtons.ListBox)
      POST(EVENT:ScrollDown,BrowseButtons.ListBox)
      POST(EVENT:ScrollUp,BrowseButtons.ListBox)
      POST(EVENT:Locate,BrowseButtons.ListBox)
      BEGIN                     !EXECUTE Place Holder - Ditto has no effect on a browse
      END
      PRESSKEY(F1Key)
    END
  END
!!! <summary>
!!! Generated from procedure template - Browse
!!! Select a Courses Record
!!! </summary>
SelectCourses PROCEDURE

CurrentTab           STRING(80)                            ! 
LocalRequest         LONG                                  ! 
OriginalRequest      LONG                                  ! 
LocalResponse        LONG                                  ! 
FilesOpened          LONG                                  ! 
WindowOpened         LONG                                  ! 
WindowInitialized    LONG                                  ! 
ForceRefresh         LONG                                  ! 
RecordFiltered       LONG                                  ! 
LOC:DropThread       LONG                                  ! 
LOC:DropControl      LONG                                  ! 
LOC:ThreadRef        &LONG                                 ! 

BRW1::View:Browse    VIEW(Courses)
                       PROJECT(COU:Description)
                       PROJECT(COU:Number)
                     END
Queue:Browse:1       QUEUE,PRE()                           ! Browsing Queue
BRW1::COU:Description  LIKE(COU:Description)               ! Queue Display field
BRW1::COU:Number       LIKE(COU:Number)                    ! Queue Display field
BRW1::Mark             BYTE                                ! Record mark flag
BRW1::Position         STRING(1024)                        ! Queue POSITION information
                     END                                   ! END (Browsing Queue)
BRW1::UsingAdditionalSortOrder BYTE                        ! When true the view is using PROP:Order
BRW1::CurrentScroll  BYTE                                  ! Queue position of scroll thumb
BRW1::ScrollRecordCount LONG                               ! Queue position of scroll thumb
BRW1::SkipFirst      BYTE                                  ! Skip first retrieved record in page fill
BRW1::Sort1:KeyDistribution LIKE(COU:Description),DIM(100)
BRW1::Sort1:LowValue LIKE(COU:Description)                 ! Queue position of scroll thumb
BRW1::Sort1:HighValue LIKE(COU:Description)                ! Queue position of scroll thumb
BRW1::CurrentEvent   LONG                                  !
BRW1::CurrentChoice  LONG                                  !
BRW1::RecordCount    LONG                                  !
BRW1::SortOrder      BYTE                                  !
BRW1::LocateMode     BYTE                                  !
BRW1::RefreshMode    BYTE                                  !
BRW1::LastSortOrder  BYTE                                  !
BRW1::FillDirection  BYTE                                  !
BRW1::AddQueue       BYTE                                  !
BRW1::Changed        BYTE                                  !
BRW1::RecordStatus   BYTE                                  ! Flag for Range/Filter test
BRW1::ItemsToFill    LONG                                  ! Controls records retrieved
BRW1::MaxItemsInList LONG                                  ! Retrieved after window opened
BRW1::HighlightedPosition STRING(1024)                     ! POSITION of located record
BRW1::NewSelectPosted BYTE                                 ! Queue position of located record
BRW1::PopupText      CSTRING(10000)                        !
BRW1::ActiveInvisible BYTE(1)                              !
BRW1::LoadPending    BYTE                                  !
ToolBarMode          UNSIGNED,AUTO
BrowseButtons        GROUP                      !info for current browse with focus
ListBox                SIGNED                   !Browse list control
InsertButton           SIGNED                   !Browse insert button
ChangeButton           SIGNED                   !Browse change button
DeleteButton           SIGNED                   !Browse delete button
SelectButton           SIGNED                   !Browse select button
                     END
QuickWindow          WINDOW('Drag Course'),AT(,,158,195),FONT('MS Sans Serif',8,COLOR:Black),CENTER,GRAY,IMM,MDI, |
  HLP('~SelectCourses'),SYSTEM
                       LIST,AT(8,20,142,154),USE(?Browse:1),HVSCROLL,DRAGID('Courses'),FORMAT('80L(2)|M~Descri' & |
  'ption~L(2)@S30@'),FROM(Queue:Browse:1),IMM,MSG('Browsing Records')
                       SHEET,AT(4,4,150,177),USE(?CurrentTab)
                         TAB('by Course Description')
                         END
                       END
                       BUTTON('Help'),AT(0,0,45,14),USE(?Help),HIDE,STD(STD:Help)
                     END
  CODE
  PUSHBIND
  !Setup inter-thread processing
  LOC:DropThread  = GLO:DropThread
  LOC:DropControl = GLO:DropControl
  LOC:ThreadRef &= GLO:ThreadRef
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
    OF EVENT:CloseWindow
      !Flag thread closed
      LOC:ThreadRef = 0
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
      DO BRW1::AssignButtons
      IF NOT WindowInitialized
        DO InitializeWindow
        WindowInitialized = True
      END
      SELECT(?Browse:1)
    OF EVENT:Sized
      POST(EVENT:DoResize,0,THREAD())
    OF Event:Rejected
      BEEP
      DISPLAY(?)
      SELECT(?)
    ELSE
      IF ToolBarMode=BrowseMode THEN
        DO ListBoxDispatch
      END
    END
    CASE FIELD()
    OF ?Browse:1
      CASE EVENT()
      OF EVENT:NewSelection
        DO BRW1::NewSelection
      OF EVENT:ScrollUp
        DO BRW1::ProcessScroll
      OF EVENT:ScrollDown
        DO BRW1::ProcessScroll
      OF EVENT:PageUp
        DO BRW1::ProcessScroll
      OF EVENT:PageDown
        DO BRW1::ProcessScroll
      OF EVENT:ScrollTop
        DO BRW1::ProcessScroll
      OF EVENT:ScrollBottom
        DO BRW1::ProcessScroll
      OF EVENT:ScrollDrag
        DO BRW1::ScrollDrag
      OF EVENT:AlertKey
        DO BRW1::AlertKey
      OF EVENT:Drag
        !Pass dragged data
        DO BRW1::NewSelection
        SETDROPID(COU:Number)
      END
    OF ?CurrentTab
      CASE EVENT()
      OF EVENT:Accepted
        DO RefreshWindow
      OF EVENT:Selected
        DO RefreshWindow
      OF EVENT:Selecting
        DO RefreshWindow
      OF EVENT:NewSelection
        DO RefreshWindow
      OF EVENT:TabChanging
        DO RefreshWindow
      END
    END
  END
  DO ProcedureReturn
!---------------------------------------------------------------------------
PrepareProcedure ROUTINE
  IF Courses::Used = 0
    CheckOpen(Courses,1)
  END
  Courses::Used += 1
  FilesOpened = TRUE
  DO BindFields
  OPEN(QuickWindow)
  WindowOpened=True
  BRW1::AddQueue = True
  BRW1::RecordCount = 0
  Do DefineListboxStyle
  ?Browse:1{PROP:Alrt,252} = MouseLeft2
  ?Browse:1{PROP:Alrt,255} = AppsKey
  ?Browse:1{PROP:Alrt,253} = MouseRight

!---------------------------------------------------------------------------
BindFields ROUTINE
  BIND(COU:RECORD)
  BIND('COU:CompleteDescription',COU:CompleteDescription)
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
    Courses::Used -= 1
    IF Courses::Used = 0 THEN CLOSE(Courses).
  END
  IF WindowOpened
    CLOSE(QuickWindow)
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
  IF QuickWindow{Prop:AcceptAll} THEN EXIT.
  Do LookupRelated
  DO BRW1::SelectSort
  ?Browse:1{Prop:VScrollPos} = BRW1::CurrentScroll
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
  DO BRW1::GetRecord
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
BRW1::AssignButtons ROUTINE
  CLEAR(BrowseButtons)
  BrowseButtons.ListBox = ?Browse:1
  DO DisplayBrowseToolbar
!----------------------------------------------------------------------
BRW1::SelectSort ROUTINE
!|
!| This routine is called during the RefreshWindow ROUTINE present in every window procedure.
!| The purpose of this routine is to make certain that the BrowseBox is always current with your
!| user's selections. This routine...
!|
!| 1. Checks to see if any of your specified sort-order conditions are met, and if so, changes the sort order.
!| 2. If no sort order change is necessary, this routine checks to see if any of your Reset Fields has changed.
!| 3. If the sort order has changed, or if a reset field has changed, or if the ForceRefresh flag is set...
!|    a. The current record is retrieved from the disk.
!|    b. If the BrowseBox is accessed for the first time, and the Browse has been called to select a record,
!|       the page containing the current record is loaded.
!|    c. If the BrowseBox is accessed for the first time, and the Browse has not been called to select a
!|       record, the first page of information is loaded.
!|    d. If the BrowseBox is not being accessed for the first time, and the Browse sort order has changed, the
!|       new "first" page of information is loaded.
!|    e. If the BrowseBox is not being accessed for the first time, and the Browse sort order hasn't changes,
!|       the page containing the current record is reloaded.
!|    f. The record buffer is refilled from the currently highlighted BrowseBox item.
!|    f. The BrowseBox is reinitialized (BRW1::InitializeBrowse ROUTINE).
!| 4. If step 3 is not necessary, the record buffer is refilled from the currently highlighted BrowseBox item.
!|
  BRW1::LastSortOrder = BRW1::SortOrder
  BRW1::Changed = False
  IF BRW1::SortOrder = 0
    BRW1::SortOrder = 1
  END
  IF BRW1::SortOrder <> BRW1::LastSortOrder OR BRW1::Changed OR ForceRefresh OR (BRW1::LoadPending AND ?Browse:1{PROP:VISIBLE})
    DO BRW1::GetRecord
    DO BRW1::Reset
    IF BRW1::LastSortOrder = 0
      IF LocalRequest = SelectRecord
        BRW1::LocateMode = LocateOnValue
        DO BRW1::LocateRecord
      ELSE
        FREE(Queue:Browse:1)
        BRW1::RefreshMode = RefreshOnTop
        DO BRW1::RefreshPage
        DO BRW1::PostNewSelection
      END
    ELSE
      IF BRW1::Changed
        FREE(Queue:Browse:1)
        BRW1::RefreshMode = RefreshOnTop
        DO BRW1::RefreshPage
        DO BRW1::PostNewSelection
      ELSE
        BRW1::LocateMode = LocateOnValue
        DO BRW1::LocateRecord
      END
    END
    IF BRW1::RecordCount
      GET(Queue:Browse:1,BRW1::CurrentChoice)
      DO BRW1::FillBuffer
    END
    DO BRW1::InitializeBrowse
  ELSE
    IF BRW1::RecordCount
      GET(Queue:Browse:1,BRW1::CurrentChoice)
      DO BRW1::FillBuffer
    END
  END
!----------------------------------------------------------------------
BRW1::InitializeBrowse ROUTINE
!|
!| This routine initializes the BrowseBox control template. This routine is called when...
!|
!| The BrowseBox sort order has changed. This includes the first time the BrowseBox is accessed.
!| The BrowseBox returns from a record update.
!|
!| This routine performs two main functions.
!|   1. Computes all BrowseBox totals. All records that satisfy the current selection criteria
!|      are read, and totals computed. If no totals are present, this section is not generated,
!|      and may not be present in the code below.
!|   2. Calculates any runtime scrollbar positions. Again, if runtime scrollbars are not used,
!|      the code for runtime scrollbar computation will not be present.
!|
  IF NOT BRW1::ActiveInvisible THEN
     IF NOT ?Browse:1{PROP:Visible} THEN
        BRW1::LoadPending = True
        EXIT
     END
  END
  DO BRW1::Reset
  PREVIOUS(BRW1::View:Browse)
  IF ERRORCODE()
    IF ERRORCODE() = BadRecErr
      DO BRW1::RestoreResetValues
    ELSE
      StandardWarning(Warn:RecordFetchError,'Courses')
      POST(Event:CloseWindow)
    END
    EXIT
  END
  CASE BRW1::SortOrder
  OF 1
    BRW1::Sort1:HighValue = COU:Description
  END
  DO BRW1::Reset
  NEXT(BRW1::View:Browse)
  IF ERRORCODE()
    IF ERRORCODE() = BadRecErr
      DO BRW1::RestoreResetValues
    ELSE
      StandardWarning(Warn:RecordFetchError,'Courses')
      POST(Event:CloseWindow)
    END
    EXIT
  END
  CASE BRW1::SortOrder
  OF 1
    BRW1::Sort1:LowValue = COU:Description
    SetupStringStops(BRW1::Sort1:LowValue,BRW1::Sort1:HighValue,SIZE(BRW1::Sort1:LowValue),ScrollSort:AllowAlpha)
    LOOP BRW1::ScrollRecordCount = 1 TO 100
      BRW1::Sort1:KeyDistribution[BRW1::ScrollRecordCount] = NextStringStop()
    END
  END
!----------------------------------------------------------------------
BRW1::FillBuffer ROUTINE
!|
!| This routine fills the record buffer from the BrowseBox queue. This gives the appearance
!| that the record is "fresh" from the disk, without the disk access required.
!|
  COU:Description = BRW1::COU:Description
  COU:Number = BRW1::COU:Number
!----------------------------------------------------------------------
BRW1::FillQueue ROUTINE
!|
!| This routine is used to fill the BrowseBox QUEUE from several sources.
!|
!| First, all Format Browse formulae are processed.
!|
!| Next, each field of the BrowseBox is processed. For each field...
!|
!|    The value of the field is placed in the BrowseBox queue.
!|
!| Finally, the POSITION of the current VIEW record is added to the QUEUE
!|
  BRW1::COU:Description = COU:Description
  BRW1::COU:Number = COU:Number
  BRW1::Position = POSITION(BRW1::View:Browse)
!----------------------------------------------------------------------
BRW1::PostNewSelection ROUTINE
!|
!| This routine is used to post the NewSelection EVENT to the window. Because we only want this
!| EVENT processed once, and becuase there are several routines that need to initiate a NewSelection
!| EVENT, we keep a flag that tells us if the EVENT is already waiting to be processed. The EVENT is
!| only POSTed if this flag is false.
!|
  IF NOT BRW1::NewSelectPosted
    BRW1::NewSelectPosted = True
    POST(EVENT:NewSelection,?Browse:1)
  END
!----------------------------------------------------------------------
BRW1::NewSelection ROUTINE
!|
!| This routine performs any window bookkeeping necessary when a new record is selected in the
!| BrowseBox.
!| 1. If the new selection is made with the right mouse button, the popup menu (if applicable) is
!|    processed.
!| 2. The current record is retrieved into the buffer using the BRW1::FillBuffer ROUTINE.
!|    After this, the current vertical scrollbar position is computed, and the scrollbar positioned.
!|
  IF NOT BRW1::ActiveInvisible THEN
     IF NOT ?Browse:1{PROP:Visible} THEN
        BRW1::LoadPending = True
        EXIT
     END
  END
  BRW1::NewSelectPosted = False
  IF KEYCODE() = MouseRight OR KEYCODE() = AppsKey
     SETKEYCODE(0)
    BRW1::PopupText = ''
    IF BRW1::RecordCount
    ELSE
    END
    EXECUTE(POPUP(BRW1::PopupText))
    END
  ELSIF BRW1::RecordCount
    BRW1::CurrentChoice = CHOICE(?Browse:1)
    GET(Queue:Browse:1,BRW1::CurrentChoice)
    DO BRW1::FillBuffer
    IF BRW1::RecordCount = ?Browse:1{PROP:Items}
      IF NOT ?Browse:1{PROP:VScroll}
        ?Browse:1{PROP:VScroll} = True
      END
      CASE BRW1::SortOrder
      OF 1
        LOOP BRW1::CurrentScroll = 1 TO 100
          IF BRW1::Sort1:KeyDistribution[BRW1::CurrentScroll] => UPPER(COU:Description)
            IF BRW1::CurrentScroll <= 1
              BRW1::CurrentScroll = 0
            ELSIF BRW1::CurrentScroll = 100
              BRW1::CurrentScroll = 100
            ELSE
            END
            BREAK
          END
        END
      END
    ELSE
      IF ?Browse:1{PROP:VScroll}
        ?Browse:1{PROP:VScroll} = False
      END
    END
    DO RefreshWindow
  END
!---------------------------------------------------------------------
BRW1::ProcessScroll ROUTINE
!|
!| This routine processes any of the six scrolling EVENTs handled by the BrowseBox.
!| If one record is to be scrolled, the ROUTINE BRW1::ScrollOne is called.
!| If a page of records is to be scrolled, the ROUTINE BRW1::ScrollPage is called.
!| If the first or last page is to be displayed, the ROUTINE BRW1::ScrollEnd is called.
!|
!| If an incremental locator is in use, the value of that locator is cleared.
!| Finally, if a Fixed Thumb vertical scroll bar is used, the thumb is positioned.
!|
  IF BRW1::RecordCount
    BRW1::CurrentEvent = EVENT()
    CASE BRW1::CurrentEvent
    OF EVENT:ScrollUp OROF EVENT:ScrollDown
      DO BRW1::ScrollOne
    OF EVENT:PageUp OROF EVENT:PageDown
      DO BRW1::ScrollPage
    OF EVENT:ScrollTop OROF EVENT:ScrollBottom
      DO BRW1::ScrollEnd
    END
    ?Browse:1{PROP:SelStart} = BRW1::CurrentChoice
    DO BRW1::PostNewSelection
  END
!----------------------------------------------------------------------
BRW1::ScrollOne ROUTINE
!|
!| This routine is used to scroll a single record on the BrowseBox. Since the BrowseBox is an IMM
!| listbox, all scrolling must be handled in code. When called, this routine...
!|
!| 1. Sees if scrolling in the intended direction will cause the listbox display to shift. If not,
!|    the routine moves the list box cursor and exits.
!| 2. Calls BRW1::FillRecord to retrieve one record in the direction required.
!|
  IF BRW1::CurrentEvent = EVENT:ScrollUp AND BRW1::CurrentChoice > 1
    BRW1::CurrentChoice -= 1
    EXIT
  ELSIF BRW1::CurrentEvent = EVENT:ScrollDown AND BRW1::CurrentChoice < BRW1::RecordCount
    BRW1::CurrentChoice += 1
    EXIT
  END
  BRW1::ItemsToFill = 1
  BRW1::FillDirection = BRW1::CurrentEvent - 2
  DO BRW1::FillRecord
!----------------------------------------------------------------------
BRW1::ScrollPage ROUTINE
!|
!| This routine is used to scroll a single page of records on the BrowseBox. Since the BrowseBox is
!| an IMM listbox, all scrolling must be handled in code. When called, this routine...
!|
!| 1. Calls BRW1::FillRecord to retrieve one page of records in the direction required.
!| 2. If BRW1::FillRecord doesn't fill a page (BRW1::ItemsToFill > 0), then
!|    the list-box cursor ia shifted.
!|
  BRW1::ItemsToFill = ?Browse:1{PROP:Items}
  BRW1::FillDirection = BRW1::CurrentEvent - 4
  DO BRW1::FillRecord                           ! Fill with next read(s)
  IF BRW1::ItemsToFill
    IF BRW1::CurrentEvent = EVENT:PageUp
      BRW1::CurrentChoice -= BRW1::ItemsToFill
      IF BRW1::CurrentChoice < 1
        BRW1::CurrentChoice = 1
      END
    ELSE
      BRW1::CurrentChoice += BRW1::ItemsToFill
      IF BRW1::CurrentChoice > BRW1::RecordCount
        BRW1::CurrentChoice = BRW1::RecordCount
      END
    END
  END
!----------------------------------------------------------------------
BRW1::ScrollEnd ROUTINE
!|
!| This routine is used to load the first or last page of the displayable set of records.
!| Since the BrowseBox is an IMM listbox, all scrolling must be handled in code. When called,
!| this routine...
!|
!| 1. Resets the BrowseBox VIEW to insure that it reads from the end of the current sort order.
!| 2. Calls BRW1::FillRecord to retrieve one page of records.
!| 3. Selects the record that represents the end of the view. That is, if the first page was loaded,
!|    the first record is highlighted. If the last was loaded, the last record is highlighted.
!|
  FREE(Queue:Browse:1)
  BRW1::RecordCount = 0
  DO BRW1::Reset
  BRW1::ItemsToFill = ?Browse:1{PROP:Items}
  IF BRW1::CurrentEvent = EVENT:ScrollTop
    BRW1::FillDirection = FillForward
  ELSE
    BRW1::FillDirection = FillBackward
  END
  DO BRW1::FillRecord                           ! Fill with next read(s)
  IF BRW1::CurrentEvent = EVENT:ScrollTop
    BRW1::CurrentChoice = 1
  ELSE
    BRW1::CurrentChoice = BRW1::RecordCount
  END
!----------------------------------------------------------------------
BRW1::AlertKey ROUTINE
!|
!| This routine processes any KEYCODEs experienced by the BrowseBox.
!| NOTE: The cursor movement keys are not processed as KEYCODEs. They are processed as the
!|       appropriate BrowseBox scrolling and selection EVENTs.
!| This routine includes handling for double-click. Actually, this handling is in the form of
!| EMBEDs, which are filled by child-control templates.
!| This routine also includes the BrowseBox's locator handling.
!| After a value is entered for locating, this routine sets BRW1::LocateMode to a value
!| of 2 -- EQUATEd to LocateOnValue -- and calls the routine BRW1::LocateRecord.
!|
  IF BRW1::RecordCount
    CASE KEYCODE()                                         ! What keycode was hit
    OF AppsKey
    OROF MouseRight
       IF ?Browse:1{PROPLIST:MouseDownRow}>0
         ?Browse:1{PROP:Selected} = ?Browse:1{PROPLIST:MouseDownRow}
         BRW1::CurrentChoice = CHOICE(?Browse:1)
       END
       DO BRW1::NewSelection
    OF MouseLeft2
      !Pass dragged data
      DO BRW1::NewSelection
      SETDROPID(COU:Number)
      POST(EVENT:Drop,LOC:DropControl,LOC:DropThread)
    ELSE                                                   ! ELSE (What keycode was hit)
      CASE BRW1::SortOrder
      OF 1
        IF CHR(KEYCHAR())
          IF UPPER(SUB(COU:Description,1,1)) = UPPER(CHR(KEYCHAR()))
            BRW1::CurrentEvent = EVENT:ScrollDown
            DO BRW1::ScrollOne
            GET(Queue:Browse:1,BRW1::CurrentChoice)
            DO BRW1::FillBuffer
          END
          IF UPPER(SUB(COU:Description,1,1)) = UPPER(CHR(KEYCHAR()))
            ?Browse:1{PROP:SelStart} = BRW1::CurrentChoice
          ELSE
            COU:Description = CHR(KEYCHAR())
            BRW1::LocateMode = LocateOnValue
            DO BRW1::LocateRecord
          END
        END
      END
    END                                                    ! END (What keycode was hit)
  ELSE
    CASE KEYCODE()                                         ! What keycode was hit
    OF AppsKey
    OROF MouseRight
       DO BRW1::NewSelection
    ELSE                                                   ! ELSE (What keycode was hit)
      CASE BRW1::SortOrder
      OF 1
      END
    END
  END
  DO BRW1::PostNewSelection
!----------------------------------------------------------------------
BRW1::ScrollDrag ROUTINE
!|
!| This routine processes the Vertical Scroll Bar arrays to find the free key field value
!| that corresponds to the current scroll bar position.
!|
!| After the scroll position is computed, and the scroll value found, this routine sets
!| BRW1::LocateMode to that scroll value of 2 -- EQUATEd to LocateOnValue --
!| and calls the routine BRW1::LocateRecord.
!|
  IF ?Browse:1{PROP:VScrollPos} <= 1
    POST(EVENT:ScrollTop,?Browse:1)
  ELSIF ?Browse:1{PROP:VScrollPos} = 100
    POST(EVENT:ScrollBottom,?Browse:1)
  ELSE
    CASE BRW1::SortOrder
    OF 1
      COU:Description = BRW1::Sort1:KeyDistribution[?Browse:1{PROP:VScrollPos}]
      BRW1::LocateMode = LocateOnValue
      DO BRW1::LocateRecord
    END
  END
!----------------------------------------------------------------------
BRW1::FillRecord ROUTINE
!|
!| This routine is used to retrieve a number of records from the VIEW. The number of records
!| retrieved is held in the variable BRW1::ItemsToFill. If more than one record is
!| to be retrieved, QuickScan is used to minimize reads from the disk.
!|
!| If records exist in the queue (in other words, if the browse has been used before), the record
!| at the appropriate end of the list box is retrieved, and the VIEW is reset to read starting
!| at that record.
!|
!| Next, the VIEW is accessed to retrieve BRW1::ItemsToFill records. Normally, this will
!| result in BRW1::ItemsToFill records being read from the VIEW, but if custom filtering
!| or range limiting is used (via the BRW1::ValidateRecord routine) then any number of records
!| might be read.
!|
!| For each good record, if BRW1::AddQueue is true, the queue is filled using the BRW1::FillQueue
!| routine. The record is then added to the queue. If adding this record causes the BrowseBox queue
!| to contain more records than can be displayed, the record at the opposite end of the queue is
!| deleted.
!|
!| The only time BRW1::AddQueue is false is when the BRW1::LocateRecord routine needs to
!| get the closest record to its record to be located. At this time, the record doesn't need to be
!| added to the queue, so it isn't.
!|
  IF BRW1::RecordCount
    IF BRW1::FillDirection = FillForward
      GET(Queue:Browse:1,BRW1::RecordCount)                ! Get the first queue item
    ELSE
      GET(Queue:Browse:1,1)                                ! Get the first queue item
    END
    RESET(BRW1::View:Browse,BRW1::Position)                ! Reset for sequential processing
    BRW1::SkipFirst = TRUE
  ELSE
    BRW1::SkipFirst = FALSE
  END
  LOOP WHILE BRW1::ItemsToFill
    IF BRW1::View:Browse{PROP:IPRequestCount} = 0
       BRW1::View:Browse{PROP:IPRequestCount} = BRW1::ItemsToFill
    END
    IF BRW1::FillDirection = FillForward
      NEXT(BRW1::View:Browse)
    ELSE
      PREVIOUS(BRW1::View:Browse)
    END
    IF ERRORCODE()
      IF ERRORCODE() = BadRecErr
        DO BRW1::RestoreResetValues
        BREAK
      ELSE
        StandardWarning(Warn:RecordFetchError,'Courses')
        POST(EVENT:CloseWindow)
        EXIT
      END
    END
    IF BRW1::SkipFirst
       BRW1::SkipFirst = FALSE
       IF POSITION(BRW1::View:Browse) = BRW1::Position
          CYCLE
       END
    END
    IF BRW1::AddQueue
      IF BRW1::RecordCount = ?Browse:1{PROP:Items}
        IF BRW1::FillDirection = FillForward
          GET(Queue:Browse:1,1)                            ! Get the first queue item
        ELSE
          GET(Queue:Browse:1,BRW1::RecordCount)            ! Get the first queue item
        END
        DELETE(Queue:Browse:1)
        BRW1::RecordCount -= 1
      END
      DO BRW1::FillQueue
      IF BRW1::FillDirection = FillForward
        ADD(Queue:Browse:1)
      ELSE
        ADD(Queue:Browse:1,1)
      END
      BRW1::RecordCount += 1
    END
    BRW1::ItemsToFill -= 1
  END
  BRW1::AddQueue = True
  EXIT
!----------------------------------------------------------------------
BRW1::LocateRecord ROUTINE
!|
!| This routine is used to find a record in the VIEW, and to display that record
!| in the BrowseBox.
!|
!| This routine has three different modes of operation, which are invoked based on
!| the setting of BRW1::LocateMode. These modes are...
!|
!|   LocateOnPosition (1) - This mode is still supported for 1.5 compatability. This mode
!|                          is the same as LocateOnEdit.
!|   LocateOnValue    (2) - The values of the current sort order key are used. This mode
!|                          used for Locators and when the BrowseBox is called to select
!|                          a record.
!|   LocateOnEdit     (3) - The current record of the VIEW is used. This mode assumes
!|                          that there is an active VIEW record. This mode is used when
!|                          the sort order of the BrowseBox has changed
!|
!| If an appropriate record has been located, the BRW1::RefreshPage routine is
!| called to load the page containing the located record.
!|
!| If an appropriate record is not locate, the last page of the BrowseBox is loaded.
!|
  IF BRW1::LocateMode = LocateOnPosition
    BRW1::LocateMode = LocateOnEdit
  END
  CLOSE(BRW1::View:Browse)
  CASE BRW1::SortOrder
  OF 1
    IF BRW1::UsingAdditionalSortOrder THEN
       BRW1::UsingAdditionalSortOrder = False
       BRW1::View:Browse{PROP:Order} = '+UPPER(COU:Description)'
    END
    IF BRW1::LocateMode = LocateOnEdit
      BRW1::HighlightedPosition = POSITION(COU:KeyDescription)
      RESET(COU:KeyDescription,BRW1::HighlightedPosition)
    ELSE
      SET(COU:KeyDescription,COU:KeyDescription)
    END
  END
  IF ERRORCODE()
    StandardWarning(Warn:ViewOpenError)
  END
  OPEN(BRW1::View:Browse)
  IF ERRORCODE()
    StandardWarning(Warn:ViewOpenError)
  END
  IF BRW1::UsingAdditionalSortOrder = True
    CASE BRW1::SortOrder
    OF 1
       SET(BRW1::View:Browse,1)
    END
  END
  FREE(Queue:Browse:1)
  BRW1::RecordCount = 0
  BRW1::ItemsToFill = 1
  BRW1::FillDirection = FillForward                        ! Fill with next read(s)
  BRW1::AddQueue = False
  DO BRW1::FillRecord                                      ! Fill with next read(s)
  BRW1::AddQueue = True
  IF BRW1::ItemsToFill
    BRW1::RefreshMode = RefreshOnBottom
    DO BRW1::RefreshPage
  ELSE
    BRW1::RefreshMode = RefreshOnPosition
    DO BRW1::RefreshPage
  END
  DO BRW1::PostNewSelection
  BRW1::LocateMode = 0
  EXIT
!----------------------------------------------------------------------
BRW1::RefreshPage ROUTINE
!|
!| This routine is used to load a single page of the BrowseBox.
!|
!| If this routine is called with a BRW1::RefreshMode of RefreshOnPosition,
!| the active VIEW record is loaded at the top of the page. Otherwise, if there are
!| records in the browse queue (Queue:Browse:1), then the current page is reloaded, and the
!| currently selected item remains selected.
!|
  IF NOT BRW1::ActiveInvisible THEN
     IF NOT ?Browse:1{PROP:Visible} THEN
        BRW1::LoadPending = True
        EXIT
     END
  END
  SETCURSOR(Cursor:Wait)
  IF BRW1::RefreshMode = RefreshOnPosition
    BRW1::HighlightedPosition = POSITION(BRW1::View:Browse)
    RESET(BRW1::View:Browse,BRW1::HighlightedPosition)
    BRW1::RefreshMode = RefreshOnTop
  ELSIF RECORDS(Queue:Browse:1)
    GET(Queue:Browse:1,BRW1::CurrentChoice)
    IF ERRORCODE()
      GET(Queue:Browse:1,RECORDS(Queue:Browse:1))
    END
    BRW1::HighlightedPosition = BRW1::Position
    GET(Queue:Browse:1,1)
    RESET(BRW1::View:Browse,BRW1::Position)
    BRW1::RefreshMode = RefreshOnCurrent
  ELSE
    BRW1::HighlightedPosition = ''
    DO BRW1::Reset
  END
  FREE(Queue:Browse:1)
  BRW1::RecordCount = 0
  BRW1::ItemsToFill = ?Browse:1{PROP:Items}
  IF BRW1::RefreshMode = RefreshOnBottom
    BRW1::FillDirection = FillBackward
  ELSE
    BRW1::FillDirection = FillForward
  END
  DO BRW1::FillRecord                                      ! Fill with next read(s)
  IF BRW1::HighlightedPosition
    IF BRW1::ItemsToFill
      IF NOT BRW1::RecordCount
        DO BRW1::Reset
      END
      IF BRW1::RefreshMode = RefreshOnBottom
        BRW1::FillDirection = FillForward
      ELSE
        BRW1::FillDirection = FillBackward
      END
      DO BRW1::FillRecord
    END
  END
  IF BRW1::RecordCount
    IF BRW1::HighlightedPosition
      LOOP BRW1::CurrentChoice = 1 TO BRW1::RecordCount
        GET(Queue:Browse:1,BRW1::CurrentChoice)
        IF BRW1::Position = BRW1::HighlightedPosition THEN BREAK.
      END
      IF BRW1::CurrentChoice > BRW1::RecordCount
        BRW1::CurrentChoice = BRW1::RecordCount
      END
    ELSE
      IF BRW1::RefreshMode = RefreshOnBottom
        BRW1::CurrentChoice = RECORDS(Queue:Browse:1)
      ELSE
        BRW1::CurrentChoice = 1
      END
    END
    ?Browse:1{Prop:Selected} = BRW1::CurrentChoice
    DO BRW1::FillBuffer
  ELSE
    CLEAR(COU:Record)
    CLEAR(COU:CompleteDescription)
    BRW1::CurrentChoice = 0
  END
  SETCURSOR()
  BRW1::RefreshMode = 0
  EXIT
BRW1::Reset ROUTINE
!|
!| This routine is used to reset the VIEW used by the BrowseBox.
!|
  IF NOT BRW1::ActiveInvisible THEN
     IF NOT ?Browse:1{PROP:Visible} THEN
        BRW1::LoadPending = True
        EXIT
     END
  END
  BRW1::LoadPending = False
  CLOSE(BRW1::View:Browse)
  CASE BRW1::SortOrder
  OF 1
    IF BRW1::UsingAdditionalSortOrder THEN
       BRW1::UsingAdditionalSortOrder = False
       BRW1::View:Browse{PROP:Order} = '+UPPER(COU:Description)'
    END
    SET(COU:KeyDescription)
  END
  IF ERRORCODE()
    StandardWarning(Warn:ViewOpenError)
  END
  OPEN(BRW1::View:Browse)
  IF ERRORCODE()
    StandardWarning(Warn:ViewOpenError)
  END
  SET(BRW1::View:Browse)
!----------------------------------------------------------------------
BRW1::GetRecord ROUTINE
!|
!| This routine is used to retrieve the VIEW record that corresponds to a
!| chosen listbox record.
!|
  IF BRW1::RecordCount
    BRW1::CurrentChoice = CHOICE(?Browse:1)
    GET(Queue:Browse:1,BRW1::CurrentChoice)
    WATCH(BRW1::View:Browse)
    REGET(BRW1::View:Browse,BRW1::Position)
  END
!----------------------------------------------------------------------
BRW1::RestoreResetValues ROUTINE
!|
!| This routine is used to restore reset values to their saved value
!| after a bad record access from the VIEW.
!|
!--------------------------------------------------------------------------
DisplayBrowseToolbar      ROUTINE
  ENABLE(TBarBrwBottom,TBarBrwLocate)
  TBarBrwHelp{PROP:DISABLE}=?Help{PROP:DISABLE}
  DISABLE(TBarBrwHistory)
  ToolBarMode = BrowseMode
  TBarBrwDown{PROP:ToolTip} = 'Go to the Next Record'
  TBarBrwBottom{PROP:ToolTip} = 'Go to the Last Page'
  TBarBrwTop{PROP:ToolTip} = 'Go to the First Page'
  TBarBrwPageDown{PROP:ToolTip} = 'Go to the Next Page'
  TBarBrwPageUp{PROP:ToolTip} = 'Go to the Prior Page'
  TBarBrwDown{PROP:ToolTip} = 'Go to the Next Record'
  TBarBrwUP{PROP:ToolTip} = 'Go to the Prior Record'
  TBarBrwInsert{PROP:ToolTip} = 'Insert a new Record'
  DISPLAY(TBarBrwFirst,TBarBrwLast)
!--------------------------------------------------------------------------
ListBoxDispatch ROUTINE
  DO DisplayBrowseToolbar
  IF ACCEPTED() THEN            !trap remote browse box control calls
    EXECUTE(ACCEPTED()-TBarBrwBottom+1)
      POST(EVENT:ScrollBottom,BrowseButtons.ListBox)
      POST(EVENT:ScrollTop,BrowseButtons.ListBox)
      POST(EVENT:PageDown,BrowseButtons.ListBox)
      POST(EVENT:PageUp,BrowseButtons.ListBox)
      POST(EVENT:ScrollDown,BrowseButtons.ListBox)
      POST(EVENT:ScrollUp,BrowseButtons.ListBox)
      POST(EVENT:Locate,BrowseButtons.ListBox)
      BEGIN                     !EXECUTE Place Holder - Ditto has no effect on a browse
      END
      PRESSKEY(F1Key)
    END
  END
!!! <summary>
!!! Generated from procedure template - Browse
!!! Select a Majors Record
!!! </summary>
SelectMajors PROCEDURE

CurrentTab           STRING(80)                            ! 
LocalRequest         LONG                                  ! 
OriginalRequest      LONG                                  ! 
LocalResponse        LONG                                  ! 
FilesOpened          LONG                                  ! 
WindowOpened         LONG                                  ! 
WindowInitialized    LONG                                  ! 
ForceRefresh         LONG                                  ! 
RecordFiltered       LONG                                  ! 
LOC:ThreadRef        &LONG                                 ! 
LOC:DropThread       LONG                                  ! 
LOC:DropControl      LONG                                  ! 

BRW1::View:Browse    VIEW(Majors)
                       PROJECT(MAJ:Description)
                       PROJECT(MAJ:Number)
                     END
Queue:Browse:1       QUEUE,PRE()                           ! Browsing Queue
BRW1::MAJ:Description  LIKE(MAJ:Description)               ! Queue Display field
BRW1::MAJ:Number       LIKE(MAJ:Number)                    ! Queue Display field
BRW1::Mark             BYTE                                ! Record mark flag
BRW1::Position         STRING(1024)                        ! Queue POSITION information
                     END                                   ! END (Browsing Queue)
BRW1::UsingAdditionalSortOrder BYTE                        ! When true the view is using PROP:Order
BRW1::CurrentScroll  BYTE                                  ! Queue position of scroll thumb
BRW1::ScrollRecordCount LONG                               ! Queue position of scroll thumb
BRW1::SkipFirst      BYTE                                  ! Skip first retrieved record in page fill
BRW1::Sort1:KeyDistribution LIKE(MAJ:Description),DIM(100)
BRW1::Sort1:LowValue LIKE(MAJ:Description)                 ! Queue position of scroll thumb
BRW1::Sort1:HighValue LIKE(MAJ:Description)                ! Queue position of scroll thumb
BRW1::Sort2:KeyDistribution LIKE(MAJ:Number),DIM(100)
BRW1::Sort2:LowValue LIKE(MAJ:Number)                      ! Queue position of scroll thumb
BRW1::Sort2:HighValue LIKE(MAJ:Number)                     ! Queue position of scroll thumb
BRW1::CurrentEvent   LONG                                  !
BRW1::CurrentChoice  LONG                                  !
BRW1::RecordCount    LONG                                  !
BRW1::SortOrder      BYTE                                  !
BRW1::LocateMode     BYTE                                  !
BRW1::RefreshMode    BYTE                                  !
BRW1::LastSortOrder  BYTE                                  !
BRW1::FillDirection  BYTE                                  !
BRW1::AddQueue       BYTE                                  !
BRW1::Changed        BYTE                                  !
BRW1::RecordStatus   BYTE                                  ! Flag for Range/Filter test
BRW1::ItemsToFill    LONG                                  ! Controls records retrieved
BRW1::MaxItemsInList LONG                                  ! Retrieved after window opened
BRW1::HighlightedPosition STRING(1024)                     ! POSITION of located record
BRW1::NewSelectPosted BYTE                                 ! Queue position of located record
BRW1::PopupText      CSTRING(10000)                        !
BRW1::ActiveInvisible BYTE(1)                              !
BRW1::LoadPending    BYTE                                  !
ToolBarMode          UNSIGNED,AUTO
BrowseButtons        GROUP                      !info for current browse with focus
ListBox                SIGNED                   !Browse list control
InsertButton           SIGNED                   !Browse insert button
ChangeButton           SIGNED                   !Browse change button
DeleteButton           SIGNED                   !Browse delete button
SelectButton           SIGNED                   !Browse select button
                     END
QuickWindow          WINDOW('Drag Major'),AT(,,159,195),FONT('MS Sans Serif',8,COLOR:Black),CENTER,GRAY,IMM,MDI, |
  HLP('~SelectMajors'),SYSTEM
                       LIST,AT(8,23,142,148),USE(?Browse:1),HVSCROLL,DRAGID('Majors'),FORMAT('80L(2)|M~Descrip' & |
  'tion~L(2)@S20@'),FROM(Queue:Browse:1),IMM,MSG('Browsing Records')
                       SHEET,AT(4,4,150,172),USE(?CurrentTab)
                         TAB('by Major Number')
                         END
                         TAB('by Major Description')
                         END
                       END
                       BUTTON('Help'),AT(0,0,45,14),USE(?Help),HIDE,STD(STD:Help)
                     END
  CODE
  PUSHBIND
  !Setup inter-thread processing
  LOC:DropThread  = GLO:DropThread
  LOC:DropControl = GLO:DropControl
  LOC:ThreadRef &= GLO:ThreadRef
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
    OF EVENT:CloseWindow
      !Flag thread closed
      LOC:ThreadRef = 0
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
      DO BRW1::AssignButtons
      IF NOT WindowInitialized
        DO InitializeWindow
        WindowInitialized = True
      END
      SELECT(?Browse:1)
    OF EVENT:Sized
      POST(EVENT:DoResize,0,THREAD())
    OF Event:Rejected
      BEEP
      DISPLAY(?)
      SELECT(?)
    ELSE
      IF ToolBarMode=BrowseMode THEN
        DO ListBoxDispatch
      END
    END
    CASE FIELD()
    OF ?Browse:1
      CASE EVENT()
      OF EVENT:NewSelection
        DO BRW1::NewSelection
      OF EVENT:ScrollUp
        DO BRW1::ProcessScroll
      OF EVENT:ScrollDown
        DO BRW1::ProcessScroll
      OF EVENT:PageUp
        DO BRW1::ProcessScroll
      OF EVENT:PageDown
        DO BRW1::ProcessScroll
      OF EVENT:ScrollTop
        DO BRW1::ProcessScroll
      OF EVENT:ScrollBottom
        DO BRW1::ProcessScroll
      OF EVENT:ScrollDrag
        DO BRW1::ScrollDrag
      OF EVENT:AlertKey
        DO BRW1::AlertKey
      OF EVENT:Drag
        !Pass dragged data
        DO BRW1::NewSelection
        SETDROPID(MAJ:Number)
      END
    OF ?CurrentTab
      CASE EVENT()
      OF EVENT:Accepted
        DO RefreshWindow
      OF EVENT:Selected
        DO RefreshWindow
      OF EVENT:Selecting
        DO RefreshWindow
      OF EVENT:NewSelection
        DO RefreshWindow
      OF EVENT:TabChanging
        DO RefreshWindow
      END
    END
  END
  DO ProcedureReturn
!---------------------------------------------------------------------------
PrepareProcedure ROUTINE
  IF Majors::Used = 0
    CheckOpen(Majors,1)
  END
  Majors::Used += 1
  FilesOpened = TRUE
  DO BindFields
  OPEN(QuickWindow)
  WindowOpened=True
  BRW1::AddQueue = True
  BRW1::RecordCount = 0
  Do DefineListboxStyle
  ?Browse:1{PROP:Alrt,252} = MouseLeft2
  ?Browse:1{PROP:Alrt,255} = AppsKey
  ?Browse:1{PROP:Alrt,253} = MouseRight

!---------------------------------------------------------------------------
BindFields ROUTINE
  BIND(MAJ:RECORD)
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
    Majors::Used -= 1
    IF Majors::Used = 0 THEN CLOSE(Majors).
  END
  IF WindowOpened
    CLOSE(QuickWindow)
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
  IF QuickWindow{Prop:AcceptAll} THEN EXIT.
  Do LookupRelated
  DO BRW1::SelectSort
  ?Browse:1{Prop:VScrollPos} = BRW1::CurrentScroll
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
  DO BRW1::GetRecord
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
BRW1::AssignButtons ROUTINE
  CLEAR(BrowseButtons)
  BrowseButtons.ListBox = ?Browse:1
  DO DisplayBrowseToolbar
!----------------------------------------------------------------------
BRW1::SelectSort ROUTINE
!|
!| This routine is called during the RefreshWindow ROUTINE present in every window procedure.
!| The purpose of this routine is to make certain that the BrowseBox is always current with your
!| user's selections. This routine...
!|
!| 1. Checks to see if any of your specified sort-order conditions are met, and if so, changes the sort order.
!| 2. If no sort order change is necessary, this routine checks to see if any of your Reset Fields has changed.
!| 3. If the sort order has changed, or if a reset field has changed, or if the ForceRefresh flag is set...
!|    a. The current record is retrieved from the disk.
!|    b. If the BrowseBox is accessed for the first time, and the Browse has been called to select a record,
!|       the page containing the current record is loaded.
!|    c. If the BrowseBox is accessed for the first time, and the Browse has not been called to select a
!|       record, the first page of information is loaded.
!|    d. If the BrowseBox is not being accessed for the first time, and the Browse sort order has changed, the
!|       new "first" page of information is loaded.
!|    e. If the BrowseBox is not being accessed for the first time, and the Browse sort order hasn't changes,
!|       the page containing the current record is reloaded.
!|    f. The record buffer is refilled from the currently highlighted BrowseBox item.
!|    f. The BrowseBox is reinitialized (BRW1::InitializeBrowse ROUTINE).
!| 4. If step 3 is not necessary, the record buffer is refilled from the currently highlighted BrowseBox item.
!|
  BRW1::LastSortOrder = BRW1::SortOrder
  BRW1::Changed = False
  IF CHOICE(?CurrentTab) = 2
    BRW1::SortOrder = 1
  ELSE
    BRW1::SortOrder = 2
  END
  IF BRW1::SortOrder <> BRW1::LastSortOrder OR BRW1::Changed OR ForceRefresh OR (BRW1::LoadPending AND ?Browse:1{PROP:VISIBLE})
    DO BRW1::GetRecord
    DO BRW1::Reset
    IF BRW1::LastSortOrder = 0
      IF LocalRequest = SelectRecord
        BRW1::LocateMode = LocateOnValue
        DO BRW1::LocateRecord
      ELSE
        FREE(Queue:Browse:1)
        BRW1::RefreshMode = RefreshOnTop
        DO BRW1::RefreshPage
        DO BRW1::PostNewSelection
      END
    ELSE
      IF BRW1::Changed
        FREE(Queue:Browse:1)
        BRW1::RefreshMode = RefreshOnTop
        DO BRW1::RefreshPage
        DO BRW1::PostNewSelection
      ELSE
        BRW1::LocateMode = LocateOnValue
        DO BRW1::LocateRecord
      END
    END
    IF BRW1::RecordCount
      GET(Queue:Browse:1,BRW1::CurrentChoice)
      DO BRW1::FillBuffer
    END
    DO BRW1::InitializeBrowse
  ELSE
    IF BRW1::RecordCount
      GET(Queue:Browse:1,BRW1::CurrentChoice)
      DO BRW1::FillBuffer
    END
  END
!----------------------------------------------------------------------
BRW1::InitializeBrowse ROUTINE
!|
!| This routine initializes the BrowseBox control template. This routine is called when...
!|
!| The BrowseBox sort order has changed. This includes the first time the BrowseBox is accessed.
!| The BrowseBox returns from a record update.
!|
!| This routine performs two main functions.
!|   1. Computes all BrowseBox totals. All records that satisfy the current selection criteria
!|      are read, and totals computed. If no totals are present, this section is not generated,
!|      and may not be present in the code below.
!|   2. Calculates any runtime scrollbar positions. Again, if runtime scrollbars are not used,
!|      the code for runtime scrollbar computation will not be present.
!|
  IF NOT BRW1::ActiveInvisible THEN
     IF NOT ?Browse:1{PROP:Visible} THEN
        BRW1::LoadPending = True
        EXIT
     END
  END
  DO BRW1::Reset
  PREVIOUS(BRW1::View:Browse)
  IF ERRORCODE()
    IF ERRORCODE() = BadRecErr
      DO BRW1::RestoreResetValues
    ELSE
      StandardWarning(Warn:RecordFetchError,'Majors')
      POST(Event:CloseWindow)
    END
    EXIT
  END
  CASE BRW1::SortOrder
  OF 1
    BRW1::Sort1:HighValue = MAJ:Description
  OF 2
    BRW1::Sort2:HighValue = MAJ:Number
  END
  DO BRW1::Reset
  NEXT(BRW1::View:Browse)
  IF ERRORCODE()
    IF ERRORCODE() = BadRecErr
      DO BRW1::RestoreResetValues
    ELSE
      StandardWarning(Warn:RecordFetchError,'Majors')
      POST(Event:CloseWindow)
    END
    EXIT
  END
  CASE BRW1::SortOrder
  OF 1
    BRW1::Sort1:LowValue = MAJ:Description
    SetupStringStops(BRW1::Sort1:LowValue,BRW1::Sort1:HighValue,SIZE(BRW1::Sort1:LowValue),ScrollSort:AllowAlpha)
    LOOP BRW1::ScrollRecordCount = 1 TO 100
      BRW1::Sort1:KeyDistribution[BRW1::ScrollRecordCount] = NextStringStop()
    END
  OF 2
    BRW1::Sort2:LowValue = MAJ:Number
    SetupRealStops(BRW1::Sort2:LowValue,BRW1::Sort2:HighValue)
    LOOP BRW1::ScrollRecordCount = 1 TO 100
      BRW1::Sort2:KeyDistribution[BRW1::ScrollRecordCount] = NextRealStop()
    END
  END
!----------------------------------------------------------------------
BRW1::FillBuffer ROUTINE
!|
!| This routine fills the record buffer from the BrowseBox queue. This gives the appearance
!| that the record is "fresh" from the disk, without the disk access required.
!|
  MAJ:Description = BRW1::MAJ:Description
  MAJ:Number = BRW1::MAJ:Number
!----------------------------------------------------------------------
BRW1::FillQueue ROUTINE
!|
!| This routine is used to fill the BrowseBox QUEUE from several sources.
!|
!| First, all Format Browse formulae are processed.
!|
!| Next, each field of the BrowseBox is processed. For each field...
!|
!|    The value of the field is placed in the BrowseBox queue.
!|
!| Finally, the POSITION of the current VIEW record is added to the QUEUE
!|
  BRW1::MAJ:Description = MAJ:Description
  BRW1::MAJ:Number = MAJ:Number
  BRW1::Position = POSITION(BRW1::View:Browse)
!----------------------------------------------------------------------
BRW1::PostNewSelection ROUTINE
!|
!| This routine is used to post the NewSelection EVENT to the window. Because we only want this
!| EVENT processed once, and becuase there are several routines that need to initiate a NewSelection
!| EVENT, we keep a flag that tells us if the EVENT is already waiting to be processed. The EVENT is
!| only POSTed if this flag is false.
!|
  IF NOT BRW1::NewSelectPosted
    BRW1::NewSelectPosted = True
    POST(EVENT:NewSelection,?Browse:1)
  END
!----------------------------------------------------------------------
BRW1::NewSelection ROUTINE
!|
!| This routine performs any window bookkeeping necessary when a new record is selected in the
!| BrowseBox.
!| 1. If the new selection is made with the right mouse button, the popup menu (if applicable) is
!|    processed.
!| 2. The current record is retrieved into the buffer using the BRW1::FillBuffer ROUTINE.
!|    After this, the current vertical scrollbar position is computed, and the scrollbar positioned.
!|
  IF NOT BRW1::ActiveInvisible THEN
     IF NOT ?Browse:1{PROP:Visible} THEN
        BRW1::LoadPending = True
        EXIT
     END
  END
  BRW1::NewSelectPosted = False
  IF KEYCODE() = MouseRight OR KEYCODE() = AppsKey
     SETKEYCODE(0)
    BRW1::PopupText = ''
    IF BRW1::RecordCount
    ELSE
    END
    EXECUTE(POPUP(BRW1::PopupText))
    END
  ELSIF BRW1::RecordCount
    BRW1::CurrentChoice = CHOICE(?Browse:1)
    GET(Queue:Browse:1,BRW1::CurrentChoice)
    DO BRW1::FillBuffer
    IF BRW1::RecordCount = ?Browse:1{PROP:Items}
      IF NOT ?Browse:1{PROP:VScroll}
        ?Browse:1{PROP:VScroll} = True
      END
      CASE BRW1::SortOrder
      OF 1
        LOOP BRW1::CurrentScroll = 1 TO 100
          IF BRW1::Sort1:KeyDistribution[BRW1::CurrentScroll] => UPPER(MAJ:Description)
            IF BRW1::CurrentScroll <= 1
              BRW1::CurrentScroll = 0
            ELSIF BRW1::CurrentScroll = 100
              BRW1::CurrentScroll = 100
            ELSE
            END
            BREAK
          END
        END
      OF 2
        LOOP BRW1::CurrentScroll = 1 TO 100
          IF BRW1::Sort2:KeyDistribution[BRW1::CurrentScroll] => MAJ:Number
            IF BRW1::CurrentScroll <= 1
              BRW1::CurrentScroll = 0
            ELSIF BRW1::CurrentScroll = 100
              BRW1::CurrentScroll = 100
            ELSE
            END
            BREAK
          END
        END
      END
    ELSE
      IF ?Browse:1{PROP:VScroll}
        ?Browse:1{PROP:VScroll} = False
      END
    END
    DO RefreshWindow
  END
!---------------------------------------------------------------------
BRW1::ProcessScroll ROUTINE
!|
!| This routine processes any of the six scrolling EVENTs handled by the BrowseBox.
!| If one record is to be scrolled, the ROUTINE BRW1::ScrollOne is called.
!| If a page of records is to be scrolled, the ROUTINE BRW1::ScrollPage is called.
!| If the first or last page is to be displayed, the ROUTINE BRW1::ScrollEnd is called.
!|
!| If an incremental locator is in use, the value of that locator is cleared.
!| Finally, if a Fixed Thumb vertical scroll bar is used, the thumb is positioned.
!|
  IF BRW1::RecordCount
    BRW1::CurrentEvent = EVENT()
    CASE BRW1::CurrentEvent
    OF EVENT:ScrollUp OROF EVENT:ScrollDown
      DO BRW1::ScrollOne
    OF EVENT:PageUp OROF EVENT:PageDown
      DO BRW1::ScrollPage
    OF EVENT:ScrollTop OROF EVENT:ScrollBottom
      DO BRW1::ScrollEnd
    END
    ?Browse:1{PROP:SelStart} = BRW1::CurrentChoice
    DO BRW1::PostNewSelection
  END
!----------------------------------------------------------------------
BRW1::ScrollOne ROUTINE
!|
!| This routine is used to scroll a single record on the BrowseBox. Since the BrowseBox is an IMM
!| listbox, all scrolling must be handled in code. When called, this routine...
!|
!| 1. Sees if scrolling in the intended direction will cause the listbox display to shift. If not,
!|    the routine moves the list box cursor and exits.
!| 2. Calls BRW1::FillRecord to retrieve one record in the direction required.
!|
  IF BRW1::CurrentEvent = EVENT:ScrollUp AND BRW1::CurrentChoice > 1
    BRW1::CurrentChoice -= 1
    EXIT
  ELSIF BRW1::CurrentEvent = EVENT:ScrollDown AND BRW1::CurrentChoice < BRW1::RecordCount
    BRW1::CurrentChoice += 1
    EXIT
  END
  BRW1::ItemsToFill = 1
  BRW1::FillDirection = BRW1::CurrentEvent - 2
  DO BRW1::FillRecord
!----------------------------------------------------------------------
BRW1::ScrollPage ROUTINE
!|
!| This routine is used to scroll a single page of records on the BrowseBox. Since the BrowseBox is
!| an IMM listbox, all scrolling must be handled in code. When called, this routine...
!|
!| 1. Calls BRW1::FillRecord to retrieve one page of records in the direction required.
!| 2. If BRW1::FillRecord doesn't fill a page (BRW1::ItemsToFill > 0), then
!|    the list-box cursor ia shifted.
!|
  BRW1::ItemsToFill = ?Browse:1{PROP:Items}
  BRW1::FillDirection = BRW1::CurrentEvent - 4
  DO BRW1::FillRecord                           ! Fill with next read(s)
  IF BRW1::ItemsToFill
    IF BRW1::CurrentEvent = EVENT:PageUp
      BRW1::CurrentChoice -= BRW1::ItemsToFill
      IF BRW1::CurrentChoice < 1
        BRW1::CurrentChoice = 1
      END
    ELSE
      BRW1::CurrentChoice += BRW1::ItemsToFill
      IF BRW1::CurrentChoice > BRW1::RecordCount
        BRW1::CurrentChoice = BRW1::RecordCount
      END
    END
  END
!----------------------------------------------------------------------
BRW1::ScrollEnd ROUTINE
!|
!| This routine is used to load the first or last page of the displayable set of records.
!| Since the BrowseBox is an IMM listbox, all scrolling must be handled in code. When called,
!| this routine...
!|
!| 1. Resets the BrowseBox VIEW to insure that it reads from the end of the current sort order.
!| 2. Calls BRW1::FillRecord to retrieve one page of records.
!| 3. Selects the record that represents the end of the view. That is, if the first page was loaded,
!|    the first record is highlighted. If the last was loaded, the last record is highlighted.
!|
  FREE(Queue:Browse:1)
  BRW1::RecordCount = 0
  DO BRW1::Reset
  BRW1::ItemsToFill = ?Browse:1{PROP:Items}
  IF BRW1::CurrentEvent = EVENT:ScrollTop
    BRW1::FillDirection = FillForward
  ELSE
    BRW1::FillDirection = FillBackward
  END
  DO BRW1::FillRecord                           ! Fill with next read(s)
  IF BRW1::CurrentEvent = EVENT:ScrollTop
    BRW1::CurrentChoice = 1
  ELSE
    BRW1::CurrentChoice = BRW1::RecordCount
  END
!----------------------------------------------------------------------
BRW1::AlertKey ROUTINE
!|
!| This routine processes any KEYCODEs experienced by the BrowseBox.
!| NOTE: The cursor movement keys are not processed as KEYCODEs. They are processed as the
!|       appropriate BrowseBox scrolling and selection EVENTs.
!| This routine includes handling for double-click. Actually, this handling is in the form of
!| EMBEDs, which are filled by child-control templates.
!| This routine also includes the BrowseBox's locator handling.
!| After a value is entered for locating, this routine sets BRW1::LocateMode to a value
!| of 2 -- EQUATEd to LocateOnValue -- and calls the routine BRW1::LocateRecord.
!|
  IF BRW1::RecordCount
    CASE KEYCODE()                                         ! What keycode was hit
    OF AppsKey
    OROF MouseRight
       IF ?Browse:1{PROPLIST:MouseDownRow}>0
         ?Browse:1{PROP:Selected} = ?Browse:1{PROPLIST:MouseDownRow}
         BRW1::CurrentChoice = CHOICE(?Browse:1)
       END
       DO BRW1::NewSelection
    OF MouseLeft2
      !Pass dragged data
      DO BRW1::NewSelection
      SETDROPID(MAJ:Number)
      POST(EVENT:Drop,GLO:DropControl,GLO:DropThread)
    ELSE                                                   ! ELSE (What keycode was hit)
      CASE BRW1::SortOrder
      OF 1
        IF CHR(KEYCHAR())
          IF UPPER(SUB(MAJ:Description,1,1)) = UPPER(CHR(KEYCHAR()))
            BRW1::CurrentEvent = EVENT:ScrollDown
            DO BRW1::ScrollOne
            GET(Queue:Browse:1,BRW1::CurrentChoice)
            DO BRW1::FillBuffer
          END
          IF UPPER(SUB(MAJ:Description,1,1)) = UPPER(CHR(KEYCHAR()))
            ?Browse:1{PROP:SelStart} = BRW1::CurrentChoice
          ELSE
            MAJ:Description = CHR(KEYCHAR())
            BRW1::LocateMode = LocateOnValue
            DO BRW1::LocateRecord
          END
        END
      OF 2
        IF CHR(KEYCHAR())
          IF UPPER(SUB(MAJ:Number,1,1)) = UPPER(CHR(KEYCHAR()))
            BRW1::CurrentEvent = EVENT:ScrollDown
            DO BRW1::ScrollOne
            GET(Queue:Browse:1,BRW1::CurrentChoice)
            DO BRW1::FillBuffer
          END
          IF UPPER(SUB(MAJ:Number,1,1)) = UPPER(CHR(KEYCHAR()))
            ?Browse:1{PROP:SelStart} = BRW1::CurrentChoice
          ELSE
            MAJ:Number = CHR(KEYCHAR())
            BRW1::LocateMode = LocateOnValue
            DO BRW1::LocateRecord
          END
        END
      END
    END                                                    ! END (What keycode was hit)
  ELSE
    CASE KEYCODE()                                         ! What keycode was hit
    OF AppsKey
    OROF MouseRight
       DO BRW1::NewSelection
    ELSE                                                   ! ELSE (What keycode was hit)
      CASE BRW1::SortOrder
      OF 1
      OF 2
      END
    END
  END
  DO BRW1::PostNewSelection
!----------------------------------------------------------------------
BRW1::ScrollDrag ROUTINE
!|
!| This routine processes the Vertical Scroll Bar arrays to find the free key field value
!| that corresponds to the current scroll bar position.
!|
!| After the scroll position is computed, and the scroll value found, this routine sets
!| BRW1::LocateMode to that scroll value of 2 -- EQUATEd to LocateOnValue --
!| and calls the routine BRW1::LocateRecord.
!|
  IF ?Browse:1{PROP:VScrollPos} <= 1
    POST(EVENT:ScrollTop,?Browse:1)
  ELSIF ?Browse:1{PROP:VScrollPos} = 100
    POST(EVENT:ScrollBottom,?Browse:1)
  ELSE
    CASE BRW1::SortOrder
    OF 1
      MAJ:Description = BRW1::Sort1:KeyDistribution[?Browse:1{PROP:VScrollPos}]
      BRW1::LocateMode = LocateOnValue
      DO BRW1::LocateRecord
    OF 2
      MAJ:Number = BRW1::Sort2:KeyDistribution[?Browse:1{PROP:VScrollPos}]
      BRW1::LocateMode = LocateOnValue
      DO BRW1::LocateRecord
    END
  END
!----------------------------------------------------------------------
BRW1::FillRecord ROUTINE
!|
!| This routine is used to retrieve a number of records from the VIEW. The number of records
!| retrieved is held in the variable BRW1::ItemsToFill. If more than one record is
!| to be retrieved, QuickScan is used to minimize reads from the disk.
!|
!| If records exist in the queue (in other words, if the browse has been used before), the record
!| at the appropriate end of the list box is retrieved, and the VIEW is reset to read starting
!| at that record.
!|
!| Next, the VIEW is accessed to retrieve BRW1::ItemsToFill records. Normally, this will
!| result in BRW1::ItemsToFill records being read from the VIEW, but if custom filtering
!| or range limiting is used (via the BRW1::ValidateRecord routine) then any number of records
!| might be read.
!|
!| For each good record, if BRW1::AddQueue is true, the queue is filled using the BRW1::FillQueue
!| routine. The record is then added to the queue. If adding this record causes the BrowseBox queue
!| to contain more records than can be displayed, the record at the opposite end of the queue is
!| deleted.
!|
!| The only time BRW1::AddQueue is false is when the BRW1::LocateRecord routine needs to
!| get the closest record to its record to be located. At this time, the record doesn't need to be
!| added to the queue, so it isn't.
!|
  IF BRW1::RecordCount
    IF BRW1::FillDirection = FillForward
      GET(Queue:Browse:1,BRW1::RecordCount)                ! Get the first queue item
    ELSE
      GET(Queue:Browse:1,1)                                ! Get the first queue item
    END
    RESET(BRW1::View:Browse,BRW1::Position)                ! Reset for sequential processing
    BRW1::SkipFirst = TRUE
  ELSE
    BRW1::SkipFirst = FALSE
  END
  LOOP WHILE BRW1::ItemsToFill
    IF BRW1::View:Browse{PROP:IPRequestCount} = 0
       BRW1::View:Browse{PROP:IPRequestCount} = BRW1::ItemsToFill
    END
    IF BRW1::FillDirection = FillForward
      NEXT(BRW1::View:Browse)
    ELSE
      PREVIOUS(BRW1::View:Browse)
    END
    IF ERRORCODE()
      IF ERRORCODE() = BadRecErr
        DO BRW1::RestoreResetValues
        BREAK
      ELSE
        StandardWarning(Warn:RecordFetchError,'Majors')
        POST(EVENT:CloseWindow)
        EXIT
      END
    END
    IF BRW1::SkipFirst
       BRW1::SkipFirst = FALSE
       IF POSITION(BRW1::View:Browse) = BRW1::Position
          CYCLE
       END
    END
    IF BRW1::AddQueue
      IF BRW1::RecordCount = ?Browse:1{PROP:Items}
        IF BRW1::FillDirection = FillForward
          GET(Queue:Browse:1,1)                            ! Get the first queue item
        ELSE
          GET(Queue:Browse:1,BRW1::RecordCount)            ! Get the first queue item
        END
        DELETE(Queue:Browse:1)
        BRW1::RecordCount -= 1
      END
      DO BRW1::FillQueue
      IF BRW1::FillDirection = FillForward
        ADD(Queue:Browse:1)
      ELSE
        ADD(Queue:Browse:1,1)
      END
      BRW1::RecordCount += 1
    END
    BRW1::ItemsToFill -= 1
  END
  BRW1::AddQueue = True
  EXIT
!----------------------------------------------------------------------
BRW1::LocateRecord ROUTINE
!|
!| This routine is used to find a record in the VIEW, and to display that record
!| in the BrowseBox.
!|
!| This routine has three different modes of operation, which are invoked based on
!| the setting of BRW1::LocateMode. These modes are...
!|
!|   LocateOnPosition (1) - This mode is still supported for 1.5 compatability. This mode
!|                          is the same as LocateOnEdit.
!|   LocateOnValue    (2) - The values of the current sort order key are used. This mode
!|                          used for Locators and when the BrowseBox is called to select
!|                          a record.
!|   LocateOnEdit     (3) - The current record of the VIEW is used. This mode assumes
!|                          that there is an active VIEW record. This mode is used when
!|                          the sort order of the BrowseBox has changed
!|
!| If an appropriate record has been located, the BRW1::RefreshPage routine is
!| called to load the page containing the located record.
!|
!| If an appropriate record is not locate, the last page of the BrowseBox is loaded.
!|
  IF BRW1::LocateMode = LocateOnPosition
    BRW1::LocateMode = LocateOnEdit
  END
  CLOSE(BRW1::View:Browse)
  CASE BRW1::SortOrder
  OF 1
    IF BRW1::UsingAdditionalSortOrder THEN
       BRW1::UsingAdditionalSortOrder = False
       BRW1::View:Browse{PROP:Order} = '+UPPER(MAJ:Description)'
    END
    IF BRW1::LocateMode = LocateOnEdit
      BRW1::HighlightedPosition = POSITION(MAJ:KeyDescription)
      RESET(MAJ:KeyDescription,BRW1::HighlightedPosition)
    ELSE
      SET(MAJ:KeyDescription,MAJ:KeyDescription)
    END
  OF 2
    IF BRW1::UsingAdditionalSortOrder THEN
       BRW1::UsingAdditionalSortOrder = False
       BRW1::View:Browse{PROP:Order} = '+MAJ:Number'
    END
    IF BRW1::LocateMode = LocateOnEdit
      BRW1::HighlightedPosition = POSITION(MAJ:KeyNumber)
      RESET(MAJ:KeyNumber,BRW1::HighlightedPosition)
    ELSE
      SET(MAJ:KeyNumber,MAJ:KeyNumber)
    END
  END
  IF ERRORCODE()
    StandardWarning(Warn:ViewOpenError)
  END
  OPEN(BRW1::View:Browse)
  IF ERRORCODE()
    StandardWarning(Warn:ViewOpenError)
  END
  IF BRW1::UsingAdditionalSortOrder = True
    CASE BRW1::SortOrder
    OF 1
       SET(BRW1::View:Browse,1)
    OF 2
       SET(BRW1::View:Browse,1)
    END
  END
  FREE(Queue:Browse:1)
  BRW1::RecordCount = 0
  BRW1::ItemsToFill = 1
  BRW1::FillDirection = FillForward                        ! Fill with next read(s)
  BRW1::AddQueue = False
  DO BRW1::FillRecord                                      ! Fill with next read(s)
  BRW1::AddQueue = True
  IF BRW1::ItemsToFill
    BRW1::RefreshMode = RefreshOnBottom
    DO BRW1::RefreshPage
  ELSE
    BRW1::RefreshMode = RefreshOnPosition
    DO BRW1::RefreshPage
  END
  DO BRW1::PostNewSelection
  BRW1::LocateMode = 0
  EXIT
!----------------------------------------------------------------------
BRW1::RefreshPage ROUTINE
!|
!| This routine is used to load a single page of the BrowseBox.
!|
!| If this routine is called with a BRW1::RefreshMode of RefreshOnPosition,
!| the active VIEW record is loaded at the top of the page. Otherwise, if there are
!| records in the browse queue (Queue:Browse:1), then the current page is reloaded, and the
!| currently selected item remains selected.
!|
  IF NOT BRW1::ActiveInvisible THEN
     IF NOT ?Browse:1{PROP:Visible} THEN
        BRW1::LoadPending = True
        EXIT
     END
  END
  SETCURSOR(Cursor:Wait)
  IF BRW1::RefreshMode = RefreshOnPosition
    BRW1::HighlightedPosition = POSITION(BRW1::View:Browse)
    RESET(BRW1::View:Browse,BRW1::HighlightedPosition)
    BRW1::RefreshMode = RefreshOnTop
  ELSIF RECORDS(Queue:Browse:1)
    GET(Queue:Browse:1,BRW1::CurrentChoice)
    IF ERRORCODE()
      GET(Queue:Browse:1,RECORDS(Queue:Browse:1))
    END
    BRW1::HighlightedPosition = BRW1::Position
    GET(Queue:Browse:1,1)
    RESET(BRW1::View:Browse,BRW1::Position)
    BRW1::RefreshMode = RefreshOnCurrent
  ELSE
    BRW1::HighlightedPosition = ''
    DO BRW1::Reset
  END
  FREE(Queue:Browse:1)
  BRW1::RecordCount = 0
  BRW1::ItemsToFill = ?Browse:1{PROP:Items}
  IF BRW1::RefreshMode = RefreshOnBottom
    BRW1::FillDirection = FillBackward
  ELSE
    BRW1::FillDirection = FillForward
  END
  DO BRW1::FillRecord                                      ! Fill with next read(s)
  IF BRW1::HighlightedPosition
    IF BRW1::ItemsToFill
      IF NOT BRW1::RecordCount
        DO BRW1::Reset
      END
      IF BRW1::RefreshMode = RefreshOnBottom
        BRW1::FillDirection = FillForward
      ELSE
        BRW1::FillDirection = FillBackward
      END
      DO BRW1::FillRecord
    END
  END
  IF BRW1::RecordCount
    IF BRW1::HighlightedPosition
      LOOP BRW1::CurrentChoice = 1 TO BRW1::RecordCount
        GET(Queue:Browse:1,BRW1::CurrentChoice)
        IF BRW1::Position = BRW1::HighlightedPosition THEN BREAK.
      END
      IF BRW1::CurrentChoice > BRW1::RecordCount
        BRW1::CurrentChoice = BRW1::RecordCount
      END
    ELSE
      IF BRW1::RefreshMode = RefreshOnBottom
        BRW1::CurrentChoice = RECORDS(Queue:Browse:1)
      ELSE
        BRW1::CurrentChoice = 1
      END
    END
    ?Browse:1{Prop:Selected} = BRW1::CurrentChoice
    DO BRW1::FillBuffer
  ELSE
    CLEAR(MAJ:Record)
    BRW1::CurrentChoice = 0
  END
  SETCURSOR()
  BRW1::RefreshMode = 0
  EXIT
BRW1::Reset ROUTINE
!|
!| This routine is used to reset the VIEW used by the BrowseBox.
!|
  IF NOT BRW1::ActiveInvisible THEN
     IF NOT ?Browse:1{PROP:Visible} THEN
        BRW1::LoadPending = True
        EXIT
     END
  END
  BRW1::LoadPending = False
  CLOSE(BRW1::View:Browse)
  CASE BRW1::SortOrder
  OF 1
    IF BRW1::UsingAdditionalSortOrder THEN
       BRW1::UsingAdditionalSortOrder = False
       BRW1::View:Browse{PROP:Order} = '+UPPER(MAJ:Description)'
    END
    SET(MAJ:KeyDescription)
  OF 2
    IF BRW1::UsingAdditionalSortOrder THEN
       BRW1::UsingAdditionalSortOrder = False
       BRW1::View:Browse{PROP:Order} = '+MAJ:Number'
    END
    SET(MAJ:KeyNumber)
  END
  IF ERRORCODE()
    StandardWarning(Warn:ViewOpenError)
  END
  OPEN(BRW1::View:Browse)
  IF ERRORCODE()
    StandardWarning(Warn:ViewOpenError)
  END
  SET(BRW1::View:Browse)
!----------------------------------------------------------------------
BRW1::GetRecord ROUTINE
!|
!| This routine is used to retrieve the VIEW record that corresponds to a
!| chosen listbox record.
!|
  IF BRW1::RecordCount
    BRW1::CurrentChoice = CHOICE(?Browse:1)
    GET(Queue:Browse:1,BRW1::CurrentChoice)
    WATCH(BRW1::View:Browse)
    REGET(BRW1::View:Browse,BRW1::Position)
  END
!----------------------------------------------------------------------
BRW1::RestoreResetValues ROUTINE
!|
!| This routine is used to restore reset values to their saved value
!| after a bad record access from the VIEW.
!|
!--------------------------------------------------------------------------
DisplayBrowseToolbar      ROUTINE
  ENABLE(TBarBrwBottom,TBarBrwLocate)
  TBarBrwHelp{PROP:DISABLE}=?Help{PROP:DISABLE}
  DISABLE(TBarBrwHistory)
  ToolBarMode = BrowseMode
  TBarBrwDown{PROP:ToolTip} = 'Go to the Next Record'
  TBarBrwBottom{PROP:ToolTip} = 'Go to the Last Page'
  TBarBrwTop{PROP:ToolTip} = 'Go to the First Page'
  TBarBrwPageDown{PROP:ToolTip} = 'Go to the Next Page'
  TBarBrwPageUp{PROP:ToolTip} = 'Go to the Prior Page'
  TBarBrwDown{PROP:ToolTip} = 'Go to the Next Record'
  TBarBrwUP{PROP:ToolTip} = 'Go to the Prior Record'
  TBarBrwInsert{PROP:ToolTip} = 'Insert a new Record'
  DISPLAY(TBarBrwFirst,TBarBrwLast)
!--------------------------------------------------------------------------
ListBoxDispatch ROUTINE
  DO DisplayBrowseToolbar
  IF ACCEPTED() THEN            !trap remote browse box control calls
    EXECUTE(ACCEPTED()-TBarBrwBottom+1)
      POST(EVENT:ScrollBottom,BrowseButtons.ListBox)
      POST(EVENT:ScrollTop,BrowseButtons.ListBox)
      POST(EVENT:PageDown,BrowseButtons.ListBox)
      POST(EVENT:PageUp,BrowseButtons.ListBox)
      POST(EVENT:ScrollDown,BrowseButtons.ListBox)
      POST(EVENT:ScrollUp,BrowseButtons.ListBox)
      POST(EVENT:Locate,BrowseButtons.ListBox)
      BEGIN                     !EXECUTE Place Holder - Ditto has no effect on a browse
      END
      PRESSKEY(F1Key)
    END
  END
!!! <summary>
!!! Generated from procedure template - Browse
!!! Select a Students Record
!!! </summary>
SelectStudents PROCEDURE

CurrentTab           STRING(80)                            ! 
LocalRequest         LONG                                  ! 
OriginalRequest      LONG                                  ! 
LocalResponse        LONG                                  ! 
FilesOpened          LONG                                  ! 
WindowOpened         LONG                                  ! 
WindowInitialized    LONG                                  ! 
ForceRefresh         LONG                                  ! 
RecordFiltered       LONG                                  ! 
LOC:DropThread       LONG                                  ! 
LOC:DropControl      LONG                                  ! 
LOC:ThreadRef        &LONG                                 ! 

BRW1::View:Browse    VIEW(Students)
                       PROJECT(STU:LastName)
                       PROJECT(STU:FirstName)
                       PROJECT(STU:Number)
                       PROJECT(STU:GradYear)
                       PROJECT(STU:Major)
                       JOIN(MAJ:KeyNumber,STU:Major)
                         PROJECT(MAJ:Description)
                         PROJECT(MAJ:Number)
                       END
                     END
Queue:Browse:1       QUEUE,PRE()                           ! Browsing Queue
BRW1::STU:LastName     LIKE(STU:LastName)                  ! Queue Display field
BRW1::STU:FirstName    LIKE(STU:FirstName)                 ! Queue Display field
BRW1::STU:Number       LIKE(STU:Number)                    ! Queue Display field
BRW1::MAJ:Description  LIKE(MAJ:Description)               ! Queue Display field
BRW1::STU:GradYear     LIKE(STU:GradYear)                  ! Queue Display field
BRW1::STU:Major        LIKE(STU:Major)                     ! Queue Display field
BRW1::MAJ:Number       LIKE(MAJ:Number)                    ! Queue Display field
BRW1::Mark             BYTE                                ! Record mark flag
BRW1::Position         STRING(1024)                        ! Queue POSITION information
                     END                                   ! END (Browsing Queue)
BRW1::UsingAdditionalSortOrder BYTE                        ! When true the view is using PROP:Order
BRW1::CurrentScroll  BYTE                                  ! Queue position of scroll thumb
BRW1::ScrollRecordCount LONG                               ! Queue position of scroll thumb
BRW1::SkipFirst      BYTE                                  ! Skip first retrieved record in page fill
BRW1::Sort1:KeyDistribution LIKE(STU:LastName),DIM(100)
BRW1::Sort1:LowValue LIKE(STU:LastName)                    ! Queue position of scroll thumb
BRW1::Sort1:HighValue LIKE(STU:LastName)                   ! Queue position of scroll thumb
BRW1::Sort2:KeyDistribution LIKE(STU:GradYear),DIM(100)
BRW1::Sort2:LowValue LIKE(STU:GradYear)                    ! Queue position of scroll thumb
BRW1::Sort2:HighValue LIKE(STU:GradYear)                   ! Queue position of scroll thumb
BRW1::Sort3:KeyDistribution LIKE(STU:Major),DIM(100)
BRW1::Sort3:LowValue LIKE(STU:Major)                       ! Queue position of scroll thumb
BRW1::Sort3:HighValue LIKE(STU:Major)                      ! Queue position of scroll thumb
BRW1::CurrentEvent   LONG                                  !
BRW1::CurrentChoice  LONG                                  !
BRW1::RecordCount    LONG                                  !
BRW1::SortOrder      BYTE                                  !
BRW1::LocateMode     BYTE                                  !
BRW1::RefreshMode    BYTE                                  !
BRW1::LastSortOrder  BYTE                                  !
BRW1::FillDirection  BYTE                                  !
BRW1::AddQueue       BYTE                                  !
BRW1::Changed        BYTE                                  !
BRW1::RecordStatus   BYTE                                  ! Flag for Range/Filter test
BRW1::ItemsToFill    LONG                                  ! Controls records retrieved
BRW1::MaxItemsInList LONG                                  ! Retrieved after window opened
BRW1::HighlightedPosition STRING(1024)                     ! POSITION of located record
BRW1::NewSelectPosted BYTE                                 ! Queue position of located record
BRW1::PopupText      CSTRING(10000)                        !
BRW1::ActiveInvisible BYTE(1)                              !
BRW1::LoadPending    BYTE                                  !
ToolBarMode          UNSIGNED,AUTO
BrowseButtons        GROUP                      !info for current browse with focus
ListBox                SIGNED                   !Browse list control
InsertButton           SIGNED                   !Browse insert button
ChangeButton           SIGNED                   !Browse change button
DeleteButton           SIGNED                   !Browse delete button
SelectButton           SIGNED                   !Browse select button
                     END
QuickWindow          WINDOW('Drag Student to Enroll'),AT(,,257,187),FONT('MS Sans Serif',8,COLOR:Black),CENTER, |
  GRAY,IMM,MDI,HLP('~SelectStudents'),SYSTEM
                       LIST,AT(8,20,237,141),USE(?Browse:1),HVSCROLL,DRAGID('Students'),FORMAT('[80L(2)|M~Last' & |
  ' Name~@S20@80L(2)|M~First Name~@S20@/80C(1)|M~Number~C(0)@P###-##-####P@93C(3)|M~Maj' & |
  'or~C(0)@S20@40C(2)|M~Grad Year~@n4@]|M'),FROM(Queue:Browse:1),IMM,MSG('Browsing Records')
                       SHEET,AT(4,4,247,162),USE(?CurrentTab)
                         TAB('by Major')
                         END
                         TAB('by Last Name')
                         END
                         TAB('by Grad Year')
                         END
                       END
                       BUTTON('Help'),AT(0,0,45,14),USE(?Help),HIDE,STD(STD:Help)
                     END
  CODE
  PUSHBIND
  !Setup inter-thread processing
  LOC:DropThread  = GLO:DropThread
  LOC:DropControl = GLO:DropControl
  LOC:ThreadRef &= GLO:ThreadRef
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
    OF EVENT:CloseWindow
      !Flag thread closed
      LOC:ThreadRef = 0
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
      DO BRW1::AssignButtons
      IF NOT WindowInitialized
        DO InitializeWindow
        WindowInitialized = True
      END
      SELECT(?Browse:1)
    OF EVENT:Sized
      POST(EVENT:DoResize,0,THREAD())
    OF Event:Rejected
      BEEP
      DISPLAY(?)
      SELECT(?)
    ELSE
      IF ToolBarMode=BrowseMode THEN
        DO ListBoxDispatch
      END
    END
    CASE FIELD()
    OF ?Browse:1
      CASE EVENT()
      OF EVENT:NewSelection
        DO BRW1::NewSelection
      OF EVENT:ScrollUp
        DO BRW1::ProcessScroll
      OF EVENT:ScrollDown
        DO BRW1::ProcessScroll
      OF EVENT:PageUp
        DO BRW1::ProcessScroll
      OF EVENT:PageDown
        DO BRW1::ProcessScroll
      OF EVENT:ScrollTop
        DO BRW1::ProcessScroll
      OF EVENT:ScrollBottom
        DO BRW1::ProcessScroll
      OF EVENT:ScrollDrag
        DO BRW1::ScrollDrag
      OF EVENT:AlertKey
        DO BRW1::AlertKey
      OF EVENT:Drag
        !Pass dragged data
        DO BRW1::NewSelection
        SETDROPID(FORMAT(STU:Number,@P###-##-####P))
      END
    OF ?CurrentTab
      CASE EVENT()
      OF EVENT:Accepted
        DO RefreshWindow
      OF EVENT:Selected
        DO RefreshWindow
      OF EVENT:Selecting
        DO RefreshWindow
      OF EVENT:NewSelection
        DO RefreshWindow
      OF EVENT:TabChanging
        DO RefreshWindow
      END
    END
  END
  DO ProcedureReturn
!---------------------------------------------------------------------------
PrepareProcedure ROUTINE
  IF Majors::Used = 0
    CheckOpen(Majors,1)
  END
  Majors::Used += 1
  IF Students::Used = 0
    CheckOpen(Students,1)
  END
  Students::Used += 1
  FilesOpened = TRUE
  DO BindFields
  OPEN(QuickWindow)
  WindowOpened=True
  BRW1::AddQueue = True
  BRW1::RecordCount = 0
  Do DefineListboxStyle
  ?Browse:1{PROP:Alrt,252} = MouseLeft2
  ?Browse:1{PROP:Alrt,255} = AppsKey
  ?Browse:1{PROP:Alrt,253} = MouseRight

!---------------------------------------------------------------------------
BindFields ROUTINE
  BIND(MAJ:RECORD)
  BIND(STU:RECORD)
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
    Majors::Used -= 1
    IF Majors::Used = 0 THEN CLOSE(Majors).
    Students::Used -= 1
    IF Students::Used = 0 THEN CLOSE(Students).
  END
  IF WindowOpened
    CLOSE(QuickWindow)
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
  IF QuickWindow{Prop:AcceptAll} THEN EXIT.
  Do LookupRelated
  DO BRW1::SelectSort
  ?Browse:1{Prop:VScrollPos} = BRW1::CurrentScroll
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
  DO BRW1::GetRecord
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
BRW1::AssignButtons ROUTINE
  CLEAR(BrowseButtons)
  BrowseButtons.ListBox = ?Browse:1
  DO DisplayBrowseToolbar
!----------------------------------------------------------------------
BRW1::SelectSort ROUTINE
!|
!| This routine is called during the RefreshWindow ROUTINE present in every window procedure.
!| The purpose of this routine is to make certain that the BrowseBox is always current with your
!| user's selections. This routine...
!|
!| 1. Checks to see if any of your specified sort-order conditions are met, and if so, changes the sort order.
!| 2. If no sort order change is necessary, this routine checks to see if any of your Reset Fields has changed.
!| 3. If the sort order has changed, or if a reset field has changed, or if the ForceRefresh flag is set...
!|    a. The current record is retrieved from the disk.
!|    b. If the BrowseBox is accessed for the first time, and the Browse has been called to select a record,
!|       the page containing the current record is loaded.
!|    c. If the BrowseBox is accessed for the first time, and the Browse has not been called to select a
!|       record, the first page of information is loaded.
!|    d. If the BrowseBox is not being accessed for the first time, and the Browse sort order has changed, the
!|       new "first" page of information is loaded.
!|    e. If the BrowseBox is not being accessed for the first time, and the Browse sort order hasn't changes,
!|       the page containing the current record is reloaded.
!|    f. The record buffer is refilled from the currently highlighted BrowseBox item.
!|    f. The BrowseBox is reinitialized (BRW1::InitializeBrowse ROUTINE).
!| 4. If step 3 is not necessary, the record buffer is refilled from the currently highlighted BrowseBox item.
!|
  BRW1::LastSortOrder = BRW1::SortOrder
  BRW1::Changed = False
  IF CHOICE(?CurrentTab) = 2
    BRW1::SortOrder = 1
  ELSIF CHOICE(?CurrentTab) = 3
    BRW1::SortOrder = 2
  ELSE
    BRW1::SortOrder = 3
  END
  IF BRW1::SortOrder <> BRW1::LastSortOrder OR BRW1::Changed OR ForceRefresh OR (BRW1::LoadPending AND ?Browse:1{PROP:VISIBLE})
    DO BRW1::GetRecord
    DO BRW1::Reset
    IF BRW1::LastSortOrder = 0
      IF LocalRequest = SelectRecord
        BRW1::LocateMode = LocateOnValue
        DO BRW1::LocateRecord
      ELSE
        FREE(Queue:Browse:1)
        BRW1::RefreshMode = RefreshOnTop
        DO BRW1::RefreshPage
        DO BRW1::PostNewSelection
      END
    ELSE
      IF BRW1::Changed
        FREE(Queue:Browse:1)
        BRW1::RefreshMode = RefreshOnTop
        DO BRW1::RefreshPage
        DO BRW1::PostNewSelection
      ELSE
        BRW1::LocateMode = LocateOnValue
        DO BRW1::LocateRecord
      END
    END
    IF BRW1::RecordCount
      GET(Queue:Browse:1,BRW1::CurrentChoice)
      DO BRW1::FillBuffer
    END
    DO BRW1::InitializeBrowse
  ELSE
    IF BRW1::RecordCount
      GET(Queue:Browse:1,BRW1::CurrentChoice)
      DO BRW1::FillBuffer
    END
  END
!----------------------------------------------------------------------
BRW1::InitializeBrowse ROUTINE
!|
!| This routine initializes the BrowseBox control template. This routine is called when...
!|
!| The BrowseBox sort order has changed. This includes the first time the BrowseBox is accessed.
!| The BrowseBox returns from a record update.
!|
!| This routine performs two main functions.
!|   1. Computes all BrowseBox totals. All records that satisfy the current selection criteria
!|      are read, and totals computed. If no totals are present, this section is not generated,
!|      and may not be present in the code below.
!|   2. Calculates any runtime scrollbar positions. Again, if runtime scrollbars are not used,
!|      the code for runtime scrollbar computation will not be present.
!|
  IF NOT BRW1::ActiveInvisible THEN
     IF NOT ?Browse:1{PROP:Visible} THEN
        BRW1::LoadPending = True
        EXIT
     END
  END
  DO BRW1::Reset
  PREVIOUS(BRW1::View:Browse)
  IF ERRORCODE()
    IF ERRORCODE() = BadRecErr
      DO BRW1::RestoreResetValues
    ELSE
      StandardWarning(Warn:RecordFetchError,'Students')
      POST(Event:CloseWindow)
    END
    EXIT
  END
  CASE BRW1::SortOrder
  OF 1
    BRW1::Sort1:HighValue = STU:LastName
  OF 2
    BRW1::Sort2:HighValue = STU:GradYear
  OF 3
    BRW1::Sort3:HighValue = STU:Major
  END
  DO BRW1::Reset
  NEXT(BRW1::View:Browse)
  IF ERRORCODE()
    IF ERRORCODE() = BadRecErr
      DO BRW1::RestoreResetValues
    ELSE
      StandardWarning(Warn:RecordFetchError,'Students')
      POST(Event:CloseWindow)
    END
    EXIT
  END
  CASE BRW1::SortOrder
  OF 1
    BRW1::Sort1:LowValue = STU:LastName
    SetupStringStops(BRW1::Sort1:LowValue,BRW1::Sort1:HighValue,SIZE(BRW1::Sort1:LowValue),ScrollSort:AllowAlpha)
    LOOP BRW1::ScrollRecordCount = 1 TO 100
      BRW1::Sort1:KeyDistribution[BRW1::ScrollRecordCount] = NextStringStop()
    END
  OF 2
    BRW1::Sort2:LowValue = STU:GradYear
    SetupRealStops(BRW1::Sort2:LowValue,BRW1::Sort2:HighValue)
    LOOP BRW1::ScrollRecordCount = 1 TO 100
      BRW1::Sort2:KeyDistribution[BRW1::ScrollRecordCount] = NextRealStop()
    END
  OF 3
    BRW1::Sort3:LowValue = STU:Major
    SetupRealStops(BRW1::Sort3:LowValue,BRW1::Sort3:HighValue)
    LOOP BRW1::ScrollRecordCount = 1 TO 100
      BRW1::Sort3:KeyDistribution[BRW1::ScrollRecordCount] = NextRealStop()
    END
  END
!----------------------------------------------------------------------
BRW1::FillBuffer ROUTINE
!|
!| This routine fills the record buffer from the BrowseBox queue. This gives the appearance
!| that the record is "fresh" from the disk, without the disk access required.
!|
  STU:LastName = BRW1::STU:LastName
  STU:FirstName = BRW1::STU:FirstName
  STU:Number = BRW1::STU:Number
  MAJ:Description = BRW1::MAJ:Description
  STU:GradYear = BRW1::STU:GradYear
  STU:Major = BRW1::STU:Major
  MAJ:Number = BRW1::MAJ:Number
!----------------------------------------------------------------------
BRW1::FillQueue ROUTINE
!|
!| This routine is used to fill the BrowseBox QUEUE from several sources.
!|
!| First, all Format Browse formulae are processed.
!|
!| Next, each field of the BrowseBox is processed. For each field...
!|
!|    The value of the field is placed in the BrowseBox queue.
!|
!| Finally, the POSITION of the current VIEW record is added to the QUEUE
!|
  BRW1::STU:LastName = STU:LastName
  BRW1::STU:FirstName = STU:FirstName
  BRW1::STU:Number = STU:Number
  BRW1::MAJ:Description = MAJ:Description
  BRW1::STU:GradYear = STU:GradYear
  BRW1::STU:Major = STU:Major
  BRW1::MAJ:Number = MAJ:Number
  BRW1::Position = POSITION(BRW1::View:Browse)
!----------------------------------------------------------------------
BRW1::PostNewSelection ROUTINE
!|
!| This routine is used to post the NewSelection EVENT to the window. Because we only want this
!| EVENT processed once, and becuase there are several routines that need to initiate a NewSelection
!| EVENT, we keep a flag that tells us if the EVENT is already waiting to be processed. The EVENT is
!| only POSTed if this flag is false.
!|
  IF NOT BRW1::NewSelectPosted
    BRW1::NewSelectPosted = True
    POST(EVENT:NewSelection,?Browse:1)
  END
!----------------------------------------------------------------------
BRW1::NewSelection ROUTINE
!|
!| This routine performs any window bookkeeping necessary when a new record is selected in the
!| BrowseBox.
!| 1. If the new selection is made with the right mouse button, the popup menu (if applicable) is
!|    processed.
!| 2. The current record is retrieved into the buffer using the BRW1::FillBuffer ROUTINE.
!|    After this, the current vertical scrollbar position is computed, and the scrollbar positioned.
!|
  IF NOT BRW1::ActiveInvisible THEN
     IF NOT ?Browse:1{PROP:Visible} THEN
        BRW1::LoadPending = True
        EXIT
     END
  END
  BRW1::NewSelectPosted = False
  IF KEYCODE() = MouseRight OR KEYCODE() = AppsKey
     SETKEYCODE(0)
    BRW1::PopupText = ''
    IF BRW1::RecordCount
    ELSE
    END
    EXECUTE(POPUP(BRW1::PopupText))
    END
  ELSIF BRW1::RecordCount
    BRW1::CurrentChoice = CHOICE(?Browse:1)
    GET(Queue:Browse:1,BRW1::CurrentChoice)
    DO BRW1::FillBuffer
    IF BRW1::RecordCount = ?Browse:1{PROP:Items}
      IF NOT ?Browse:1{PROP:VScroll}
        ?Browse:1{PROP:VScroll} = True
      END
      CASE BRW1::SortOrder
      OF 1
        LOOP BRW1::CurrentScroll = 1 TO 100
          IF BRW1::Sort1:KeyDistribution[BRW1::CurrentScroll] => UPPER(STU:LastName)
            IF BRW1::CurrentScroll <= 1
              BRW1::CurrentScroll = 0
            ELSIF BRW1::CurrentScroll = 100
              BRW1::CurrentScroll = 100
            ELSE
            END
            BREAK
          END
        END
      OF 2
        LOOP BRW1::CurrentScroll = 100 TO 1 BY -1
          IF BRW1::Sort2:KeyDistribution[BRW1::CurrentScroll] => STU:GradYear
            IF BRW1::CurrentScroll <= 1
              BRW1::CurrentScroll = 0
            ELSIF BRW1::CurrentScroll = 100
              BRW1::CurrentScroll = 100
            ELSE
            END
            BREAK
          END
        END
      OF 3
        LOOP BRW1::CurrentScroll = 1 TO 100
          IF BRW1::Sort3:KeyDistribution[BRW1::CurrentScroll] => STU:Major
            IF BRW1::CurrentScroll <= 1
              BRW1::CurrentScroll = 0
            ELSIF BRW1::CurrentScroll = 100
              BRW1::CurrentScroll = 100
            ELSE
            END
            BREAK
          END
        END
      END
    ELSE
      IF ?Browse:1{PROP:VScroll}
        ?Browse:1{PROP:VScroll} = False
      END
    END
    DO RefreshWindow
  END
!---------------------------------------------------------------------
BRW1::ProcessScroll ROUTINE
!|
!| This routine processes any of the six scrolling EVENTs handled by the BrowseBox.
!| If one record is to be scrolled, the ROUTINE BRW1::ScrollOne is called.
!| If a page of records is to be scrolled, the ROUTINE BRW1::ScrollPage is called.
!| If the first or last page is to be displayed, the ROUTINE BRW1::ScrollEnd is called.
!|
!| If an incremental locator is in use, the value of that locator is cleared.
!| Finally, if a Fixed Thumb vertical scroll bar is used, the thumb is positioned.
!|
  IF BRW1::RecordCount
    BRW1::CurrentEvent = EVENT()
    CASE BRW1::CurrentEvent
    OF EVENT:ScrollUp OROF EVENT:ScrollDown
      DO BRW1::ScrollOne
    OF EVENT:PageUp OROF EVENT:PageDown
      DO BRW1::ScrollPage
    OF EVENT:ScrollTop OROF EVENT:ScrollBottom
      DO BRW1::ScrollEnd
    END
    ?Browse:1{PROP:SelStart} = BRW1::CurrentChoice
    DO BRW1::PostNewSelection
  END
!----------------------------------------------------------------------
BRW1::ScrollOne ROUTINE
!|
!| This routine is used to scroll a single record on the BrowseBox. Since the BrowseBox is an IMM
!| listbox, all scrolling must be handled in code. When called, this routine...
!|
!| 1. Sees if scrolling in the intended direction will cause the listbox display to shift. If not,
!|    the routine moves the list box cursor and exits.
!| 2. Calls BRW1::FillRecord to retrieve one record in the direction required.
!|
  IF BRW1::CurrentEvent = EVENT:ScrollUp AND BRW1::CurrentChoice > 1
    BRW1::CurrentChoice -= 1
    EXIT
  ELSIF BRW1::CurrentEvent = EVENT:ScrollDown AND BRW1::CurrentChoice < BRW1::RecordCount
    BRW1::CurrentChoice += 1
    EXIT
  END
  BRW1::ItemsToFill = 1
  BRW1::FillDirection = BRW1::CurrentEvent - 2
  DO BRW1::FillRecord
!----------------------------------------------------------------------
BRW1::ScrollPage ROUTINE
!|
!| This routine is used to scroll a single page of records on the BrowseBox. Since the BrowseBox is
!| an IMM listbox, all scrolling must be handled in code. When called, this routine...
!|
!| 1. Calls BRW1::FillRecord to retrieve one page of records in the direction required.
!| 2. If BRW1::FillRecord doesn't fill a page (BRW1::ItemsToFill > 0), then
!|    the list-box cursor ia shifted.
!|
  BRW1::ItemsToFill = ?Browse:1{PROP:Items}
  BRW1::FillDirection = BRW1::CurrentEvent - 4
  DO BRW1::FillRecord                           ! Fill with next read(s)
  IF BRW1::ItemsToFill
    IF BRW1::CurrentEvent = EVENT:PageUp
      BRW1::CurrentChoice -= BRW1::ItemsToFill
      IF BRW1::CurrentChoice < 1
        BRW1::CurrentChoice = 1
      END
    ELSE
      BRW1::CurrentChoice += BRW1::ItemsToFill
      IF BRW1::CurrentChoice > BRW1::RecordCount
        BRW1::CurrentChoice = BRW1::RecordCount
      END
    END
  END
!----------------------------------------------------------------------
BRW1::ScrollEnd ROUTINE
!|
!| This routine is used to load the first or last page of the displayable set of records.
!| Since the BrowseBox is an IMM listbox, all scrolling must be handled in code. When called,
!| this routine...
!|
!| 1. Resets the BrowseBox VIEW to insure that it reads from the end of the current sort order.
!| 2. Calls BRW1::FillRecord to retrieve one page of records.
!| 3. Selects the record that represents the end of the view. That is, if the first page was loaded,
!|    the first record is highlighted. If the last was loaded, the last record is highlighted.
!|
  FREE(Queue:Browse:1)
  BRW1::RecordCount = 0
  DO BRW1::Reset
  BRW1::ItemsToFill = ?Browse:1{PROP:Items}
  IF BRW1::CurrentEvent = EVENT:ScrollTop
    BRW1::FillDirection = FillForward
  ELSE
    BRW1::FillDirection = FillBackward
  END
  DO BRW1::FillRecord                           ! Fill with next read(s)
  IF BRW1::CurrentEvent = EVENT:ScrollTop
    BRW1::CurrentChoice = 1
  ELSE
    BRW1::CurrentChoice = BRW1::RecordCount
  END
!----------------------------------------------------------------------
BRW1::AlertKey ROUTINE
!|
!| This routine processes any KEYCODEs experienced by the BrowseBox.
!| NOTE: The cursor movement keys are not processed as KEYCODEs. They are processed as the
!|       appropriate BrowseBox scrolling and selection EVENTs.
!| This routine includes handling for double-click. Actually, this handling is in the form of
!| EMBEDs, which are filled by child-control templates.
!| This routine also includes the BrowseBox's locator handling.
!| After a value is entered for locating, this routine sets BRW1::LocateMode to a value
!| of 2 -- EQUATEd to LocateOnValue -- and calls the routine BRW1::LocateRecord.
!|
  IF BRW1::RecordCount
    CASE KEYCODE()                                         ! What keycode was hit
    OF AppsKey
    OROF MouseRight
       IF ?Browse:1{PROPLIST:MouseDownRow}>0
         ?Browse:1{PROP:Selected} = ?Browse:1{PROPLIST:MouseDownRow}
         BRW1::CurrentChoice = CHOICE(?Browse:1)
       END
       DO BRW1::NewSelection
    OF MouseLeft2
      !Pass dragged data
      DO BRW1::NewSelection
      SETDROPID(FORMAT(STU:Number,@P###-##-####P))
      POST(EVENT:Drop,LOC:DropControl,LOC:DropThread)
    ELSE                                                   ! ELSE (What keycode was hit)
      CASE BRW1::SortOrder
      OF 1
        IF CHR(KEYCHAR())
          IF UPPER(SUB(STU:LastName,1,1)) = UPPER(CHR(KEYCHAR()))
            BRW1::CurrentEvent = EVENT:ScrollDown
            DO BRW1::ScrollOne
            GET(Queue:Browse:1,BRW1::CurrentChoice)
            DO BRW1::FillBuffer
          END
          IF UPPER(SUB(STU:LastName,1,1)) = UPPER(CHR(KEYCHAR()))
            ?Browse:1{PROP:SelStart} = BRW1::CurrentChoice
          ELSE
            STU:LastName = CHR(KEYCHAR())
            BRW1::LocateMode = LocateOnValue
            DO BRW1::LocateRecord
          END
        END
      OF 2
        IF CHR(KEYCHAR())
          IF UPPER(SUB(STU:GradYear,1,1)) = UPPER(CHR(KEYCHAR()))
            BRW1::CurrentEvent = EVENT:ScrollDown
            DO BRW1::ScrollOne
            GET(Queue:Browse:1,BRW1::CurrentChoice)
            DO BRW1::FillBuffer
          END
          IF UPPER(SUB(STU:GradYear,1,1)) = UPPER(CHR(KEYCHAR()))
            ?Browse:1{PROP:SelStart} = BRW1::CurrentChoice
          ELSE
            STU:GradYear = CHR(KEYCHAR())
            CLEAR(STU:LastName)
            CLEAR(STU:FirstName)
            BRW1::LocateMode = LocateOnValue
            DO BRW1::LocateRecord
          END
        END
      OF 3
        IF CHR(KEYCHAR())
          IF UPPER(SUB(STU:Major,1,1)) = UPPER(CHR(KEYCHAR()))
            BRW1::CurrentEvent = EVENT:ScrollDown
            DO BRW1::ScrollOne
            GET(Queue:Browse:1,BRW1::CurrentChoice)
            DO BRW1::FillBuffer
          END
          IF UPPER(SUB(STU:Major,1,1)) = UPPER(CHR(KEYCHAR()))
            ?Browse:1{PROP:SelStart} = BRW1::CurrentChoice
          ELSE
            STU:Major = CHR(KEYCHAR())
            CLEAR(STU:LastName)
            CLEAR(STU:FirstName)
            BRW1::LocateMode = LocateOnValue
            DO BRW1::LocateRecord
          END
        END
      END
    END                                                    ! END (What keycode was hit)
  ELSE
    CASE KEYCODE()                                         ! What keycode was hit
    OF AppsKey
    OROF MouseRight
       DO BRW1::NewSelection
    ELSE                                                   ! ELSE (What keycode was hit)
      CASE BRW1::SortOrder
      OF 1
      OF 2
      OF 3
      END
    END
  END
  DO BRW1::PostNewSelection
!----------------------------------------------------------------------
BRW1::ScrollDrag ROUTINE
!|
!| This routine processes the Vertical Scroll Bar arrays to find the free key field value
!| that corresponds to the current scroll bar position.
!|
!| After the scroll position is computed, and the scroll value found, this routine sets
!| BRW1::LocateMode to that scroll value of 2 -- EQUATEd to LocateOnValue --
!| and calls the routine BRW1::LocateRecord.
!|
  IF ?Browse:1{PROP:VScrollPos} <= 1
    POST(EVENT:ScrollTop,?Browse:1)
  ELSIF ?Browse:1{PROP:VScrollPos} = 100
    POST(EVENT:ScrollBottom,?Browse:1)
  ELSE
    CASE BRW1::SortOrder
    OF 1
      STU:LastName = BRW1::Sort1:KeyDistribution[?Browse:1{PROP:VScrollPos}]
      BRW1::LocateMode = LocateOnValue
      DO BRW1::LocateRecord
    OF 2
      STU:GradYear = BRW1::Sort2:KeyDistribution[?Browse:1{PROP:VScrollPos}]
      BRW1::LocateMode = LocateOnValue
      DO BRW1::LocateRecord
    OF 3
      STU:Major = BRW1::Sort3:KeyDistribution[?Browse:1{PROP:VScrollPos}]
      BRW1::LocateMode = LocateOnValue
      DO BRW1::LocateRecord
    END
  END
!----------------------------------------------------------------------
BRW1::FillRecord ROUTINE
!|
!| This routine is used to retrieve a number of records from the VIEW. The number of records
!| retrieved is held in the variable BRW1::ItemsToFill. If more than one record is
!| to be retrieved, QuickScan is used to minimize reads from the disk.
!|
!| If records exist in the queue (in other words, if the browse has been used before), the record
!| at the appropriate end of the list box is retrieved, and the VIEW is reset to read starting
!| at that record.
!|
!| Next, the VIEW is accessed to retrieve BRW1::ItemsToFill records. Normally, this will
!| result in BRW1::ItemsToFill records being read from the VIEW, but if custom filtering
!| or range limiting is used (via the BRW1::ValidateRecord routine) then any number of records
!| might be read.
!|
!| For each good record, if BRW1::AddQueue is true, the queue is filled using the BRW1::FillQueue
!| routine. The record is then added to the queue. If adding this record causes the BrowseBox queue
!| to contain more records than can be displayed, the record at the opposite end of the queue is
!| deleted.
!|
!| The only time BRW1::AddQueue is false is when the BRW1::LocateRecord routine needs to
!| get the closest record to its record to be located. At this time, the record doesn't need to be
!| added to the queue, so it isn't.
!|
  IF BRW1::RecordCount
    IF BRW1::FillDirection = FillForward
      GET(Queue:Browse:1,BRW1::RecordCount)                ! Get the first queue item
    ELSE
      GET(Queue:Browse:1,1)                                ! Get the first queue item
    END
    RESET(BRW1::View:Browse,BRW1::Position)                ! Reset for sequential processing
    BRW1::SkipFirst = TRUE
  ELSE
    BRW1::SkipFirst = FALSE
  END
  LOOP WHILE BRW1::ItemsToFill
    IF BRW1::View:Browse{PROP:IPRequestCount} = 0
       BRW1::View:Browse{PROP:IPRequestCount} = BRW1::ItemsToFill
    END
    IF BRW1::FillDirection = FillForward
      NEXT(BRW1::View:Browse)
    ELSE
      PREVIOUS(BRW1::View:Browse)
    END
    IF ERRORCODE()
      IF ERRORCODE() = BadRecErr
        DO BRW1::RestoreResetValues
        BREAK
      ELSE
        StandardWarning(Warn:RecordFetchError,'Students')
        POST(EVENT:CloseWindow)
        EXIT
      END
    END
    IF BRW1::SkipFirst
       BRW1::SkipFirst = FALSE
       IF POSITION(BRW1::View:Browse) = BRW1::Position
          CYCLE
       END
    END
    IF BRW1::AddQueue
      IF BRW1::RecordCount = ?Browse:1{PROP:Items}
        IF BRW1::FillDirection = FillForward
          GET(Queue:Browse:1,1)                            ! Get the first queue item
        ELSE
          GET(Queue:Browse:1,BRW1::RecordCount)            ! Get the first queue item
        END
        DELETE(Queue:Browse:1)
        BRW1::RecordCount -= 1
      END
      DO BRW1::FillQueue
      IF BRW1::FillDirection = FillForward
        ADD(Queue:Browse:1)
      ELSE
        ADD(Queue:Browse:1,1)
      END
      BRW1::RecordCount += 1
    END
    BRW1::ItemsToFill -= 1
  END
  BRW1::AddQueue = True
  EXIT
!----------------------------------------------------------------------
BRW1::LocateRecord ROUTINE
!|
!| This routine is used to find a record in the VIEW, and to display that record
!| in the BrowseBox.
!|
!| This routine has three different modes of operation, which are invoked based on
!| the setting of BRW1::LocateMode. These modes are...
!|
!|   LocateOnPosition (1) - This mode is still supported for 1.5 compatability. This mode
!|                          is the same as LocateOnEdit.
!|   LocateOnValue    (2) - The values of the current sort order key are used. This mode
!|                          used for Locators and when the BrowseBox is called to select
!|                          a record.
!|   LocateOnEdit     (3) - The current record of the VIEW is used. This mode assumes
!|                          that there is an active VIEW record. This mode is used when
!|                          the sort order of the BrowseBox has changed
!|
!| If an appropriate record has been located, the BRW1::RefreshPage routine is
!| called to load the page containing the located record.
!|
!| If an appropriate record is not locate, the last page of the BrowseBox is loaded.
!|
  IF BRW1::LocateMode = LocateOnPosition
    BRW1::LocateMode = LocateOnEdit
  END
  CLOSE(BRW1::View:Browse)
  CASE BRW1::SortOrder
  OF 1
    IF BRW1::UsingAdditionalSortOrder THEN
       BRW1::UsingAdditionalSortOrder = False
       BRW1::View:Browse{PROP:Order} = '+UPPER(STU:LastName)'
    END
    IF BRW1::LocateMode = LocateOnEdit
      BRW1::HighlightedPosition = POSITION(STU:KeyLastName)
      RESET(STU:KeyLastName,BRW1::HighlightedPosition)
    ELSE
      SET(STU:KeyLastName,STU:KeyLastName)
    END
  OF 2
    IF BRW1::UsingAdditionalSortOrder THEN
       BRW1::UsingAdditionalSortOrder = False
       BRW1::View:Browse{PROP:Order} = '-STU:GradYear,+UPPER(STU:LastName),+UPPER(STU:FirstName)'
    END
    IF BRW1::LocateMode = LocateOnEdit
      BRW1::HighlightedPosition = POSITION(STU:KeyGradYear)
      RESET(STU:KeyGradYear,BRW1::HighlightedPosition)
    ELSE
      SET(STU:KeyGradYear,STU:KeyGradYear)
    END
  OF 3
    IF BRW1::UsingAdditionalSortOrder THEN
       BRW1::UsingAdditionalSortOrder = False
       BRW1::View:Browse{PROP:Order} = '+STU:Major,+UPPER(STU:LastName),+UPPER(STU:FirstName)'
    END
    IF BRW1::LocateMode = LocateOnEdit
      BRW1::HighlightedPosition = POSITION(STU:MajorKey)
      RESET(STU:MajorKey,BRW1::HighlightedPosition)
    ELSE
      SET(STU:MajorKey,STU:MajorKey)
    END
  END
  IF ERRORCODE()
    StandardWarning(Warn:ViewOpenError)
  END
  OPEN(BRW1::View:Browse)
  IF ERRORCODE()
    StandardWarning(Warn:ViewOpenError)
  END
  IF BRW1::UsingAdditionalSortOrder = True
    CASE BRW1::SortOrder
    OF 1
       SET(BRW1::View:Browse,1)
    OF 2
       SET(BRW1::View:Browse,1)
    OF 3
       SET(BRW1::View:Browse,1)
    END
  END
  FREE(Queue:Browse:1)
  BRW1::RecordCount = 0
  BRW1::ItemsToFill = 1
  BRW1::FillDirection = FillForward                        ! Fill with next read(s)
  BRW1::AddQueue = False
  DO BRW1::FillRecord                                      ! Fill with next read(s)
  BRW1::AddQueue = True
  IF BRW1::ItemsToFill
    BRW1::RefreshMode = RefreshOnBottom
    DO BRW1::RefreshPage
  ELSE
    BRW1::RefreshMode = RefreshOnPosition
    DO BRW1::RefreshPage
  END
  DO BRW1::PostNewSelection
  BRW1::LocateMode = 0
  EXIT
!----------------------------------------------------------------------
BRW1::RefreshPage ROUTINE
!|
!| This routine is used to load a single page of the BrowseBox.
!|
!| If this routine is called with a BRW1::RefreshMode of RefreshOnPosition,
!| the active VIEW record is loaded at the top of the page. Otherwise, if there are
!| records in the browse queue (Queue:Browse:1), then the current page is reloaded, and the
!| currently selected item remains selected.
!|
  IF NOT BRW1::ActiveInvisible THEN
     IF NOT ?Browse:1{PROP:Visible} THEN
        BRW1::LoadPending = True
        EXIT
     END
  END
  SETCURSOR(Cursor:Wait)
  IF BRW1::RefreshMode = RefreshOnPosition
    BRW1::HighlightedPosition = POSITION(BRW1::View:Browse)
    RESET(BRW1::View:Browse,BRW1::HighlightedPosition)
    BRW1::RefreshMode = RefreshOnTop
  ELSIF RECORDS(Queue:Browse:1)
    GET(Queue:Browse:1,BRW1::CurrentChoice)
    IF ERRORCODE()
      GET(Queue:Browse:1,RECORDS(Queue:Browse:1))
    END
    BRW1::HighlightedPosition = BRW1::Position
    GET(Queue:Browse:1,1)
    RESET(BRW1::View:Browse,BRW1::Position)
    BRW1::RefreshMode = RefreshOnCurrent
  ELSE
    BRW1::HighlightedPosition = ''
    DO BRW1::Reset
  END
  FREE(Queue:Browse:1)
  BRW1::RecordCount = 0
  BRW1::ItemsToFill = ?Browse:1{PROP:Items}
  IF BRW1::RefreshMode = RefreshOnBottom
    BRW1::FillDirection = FillBackward
  ELSE
    BRW1::FillDirection = FillForward
  END
  DO BRW1::FillRecord                                      ! Fill with next read(s)
  IF BRW1::HighlightedPosition
    IF BRW1::ItemsToFill
      IF NOT BRW1::RecordCount
        DO BRW1::Reset
      END
      IF BRW1::RefreshMode = RefreshOnBottom
        BRW1::FillDirection = FillForward
      ELSE
        BRW1::FillDirection = FillBackward
      END
      DO BRW1::FillRecord
    END
  END
  IF BRW1::RecordCount
    IF BRW1::HighlightedPosition
      LOOP BRW1::CurrentChoice = 1 TO BRW1::RecordCount
        GET(Queue:Browse:1,BRW1::CurrentChoice)
        IF BRW1::Position = BRW1::HighlightedPosition THEN BREAK.
      END
      IF BRW1::CurrentChoice > BRW1::RecordCount
        BRW1::CurrentChoice = BRW1::RecordCount
      END
    ELSE
      IF BRW1::RefreshMode = RefreshOnBottom
        BRW1::CurrentChoice = RECORDS(Queue:Browse:1)
      ELSE
        BRW1::CurrentChoice = 1
      END
    END
    ?Browse:1{Prop:Selected} = BRW1::CurrentChoice
    DO BRW1::FillBuffer
  ELSE
    CLEAR(STU:Record)
    BRW1::CurrentChoice = 0
  END
  SETCURSOR()
  BRW1::RefreshMode = 0
  EXIT
BRW1::Reset ROUTINE
!|
!| This routine is used to reset the VIEW used by the BrowseBox.
!|
  IF NOT BRW1::ActiveInvisible THEN
     IF NOT ?Browse:1{PROP:Visible} THEN
        BRW1::LoadPending = True
        EXIT
     END
  END
  BRW1::LoadPending = False
  CLOSE(BRW1::View:Browse)
  CASE BRW1::SortOrder
  OF 1
    IF BRW1::UsingAdditionalSortOrder THEN
       BRW1::UsingAdditionalSortOrder = False
       BRW1::View:Browse{PROP:Order} = '+UPPER(STU:LastName)'
    END
    SET(STU:KeyLastName)
  OF 2
    IF BRW1::UsingAdditionalSortOrder THEN
       BRW1::UsingAdditionalSortOrder = False
       BRW1::View:Browse{PROP:Order} = '-STU:GradYear,+UPPER(STU:LastName),+UPPER(STU:FirstName)'
    END
    SET(STU:KeyGradYear)
  OF 3
    IF BRW1::UsingAdditionalSortOrder THEN
       BRW1::UsingAdditionalSortOrder = False
       BRW1::View:Browse{PROP:Order} = '+STU:Major,+UPPER(STU:LastName),+UPPER(STU:FirstName)'
    END
    SET(STU:MajorKey)
  END
  IF ERRORCODE()
    StandardWarning(Warn:ViewOpenError)
  END
  OPEN(BRW1::View:Browse)
  IF ERRORCODE()
    StandardWarning(Warn:ViewOpenError)
  END
  SET(BRW1::View:Browse)
!----------------------------------------------------------------------
BRW1::GetRecord ROUTINE
!|
!| This routine is used to retrieve the VIEW record that corresponds to a
!| chosen listbox record.
!|
  IF BRW1::RecordCount
    BRW1::CurrentChoice = CHOICE(?Browse:1)
    GET(Queue:Browse:1,BRW1::CurrentChoice)
    WATCH(BRW1::View:Browse)
    REGET(BRW1::View:Browse,BRW1::Position)
  END
!----------------------------------------------------------------------
BRW1::RestoreResetValues ROUTINE
!|
!| This routine is used to restore reset values to their saved value
!| after a bad record access from the VIEW.
!|
!--------------------------------------------------------------------------
DisplayBrowseToolbar      ROUTINE
  ENABLE(TBarBrwBottom,TBarBrwLocate)
  TBarBrwHelp{PROP:DISABLE}=?Help{PROP:DISABLE}
  DISABLE(TBarBrwHistory)
  ToolBarMode = BrowseMode
  TBarBrwDown{PROP:ToolTip} = 'Go to the Next Record'
  TBarBrwBottom{PROP:ToolTip} = 'Go to the Last Page'
  TBarBrwTop{PROP:ToolTip} = 'Go to the First Page'
  TBarBrwPageDown{PROP:ToolTip} = 'Go to the Next Page'
  TBarBrwPageUp{PROP:ToolTip} = 'Go to the Prior Page'
  TBarBrwDown{PROP:ToolTip} = 'Go to the Next Record'
  TBarBrwUP{PROP:ToolTip} = 'Go to the Prior Record'
  TBarBrwInsert{PROP:ToolTip} = 'Insert a new Record'
  DISPLAY(TBarBrwFirst,TBarBrwLast)
!--------------------------------------------------------------------------
ListBoxDispatch ROUTINE
  DO DisplayBrowseToolbar
  IF ACCEPTED() THEN            !trap remote browse box control calls
    EXECUTE(ACCEPTED()-TBarBrwBottom+1)
      POST(EVENT:ScrollBottom,BrowseButtons.ListBox)
      POST(EVENT:ScrollTop,BrowseButtons.ListBox)
      POST(EVENT:PageDown,BrowseButtons.ListBox)
      POST(EVENT:PageUp,BrowseButtons.ListBox)
      POST(EVENT:ScrollDown,BrowseButtons.ListBox)
      POST(EVENT:ScrollUp,BrowseButtons.ListBox)
      POST(EVENT:Locate,BrowseButtons.ListBox)
      BEGIN                     !EXECUTE Place Holder - Ditto has no effect on a browse
      END
      PRESSKEY(F1Key)
    END
  END
!!! <summary>
!!! Generated from procedure template - Browse
!!! Select a Teachers Record
!!! </summary>
SelectTeachers PROCEDURE

CurrentTab           STRING(80)                            ! 
LocalRequest         LONG                                  ! 
OriginalRequest      LONG                                  ! 
LocalResponse        LONG                                  ! 
FilesOpened          LONG                                  ! 
WindowOpened         LONG                                  ! 
WindowInitialized    LONG                                  ! 
ForceRefresh         LONG                                  ! 
RecordFiltered       LONG                                  ! 
LOC:DropThread       LONG                                  ! 
LOC:DropControl      LONG                                  ! 
LOC:ThreadRef        &LONG                                 ! 

BRW1::View:Browse    VIEW(Teachers)
                       PROJECT(TEA:LastName)
                       PROJECT(TEA:FirstName)
                       PROJECT(TEA:Number)
                       PROJECT(TEA:Department)
                       JOIN(MAJ:KeyNumber,TEA:Department)
                         PROJECT(MAJ:Description)
                         PROJECT(MAJ:Number)
                       END
                     END
Queue:Browse:1       QUEUE,PRE()                           ! Browsing Queue
BRW1::TEA:LastName     LIKE(TEA:LastName)                  ! Queue Display field
BRW1::TEA:FirstName    LIKE(TEA:FirstName)                 ! Queue Display field
BRW1::MAJ:Description  LIKE(MAJ:Description)               ! Queue Display field
BRW1::TEA:Number       LIKE(TEA:Number)                    ! Queue Display field
BRW1::TEA:Department   LIKE(TEA:Department)                ! Queue Display field
BRW1::MAJ:Number       LIKE(MAJ:Number)                    ! Queue Display field
BRW1::Mark             BYTE                                ! Record mark flag
BRW1::Position         STRING(1024)                        ! Queue POSITION information
                     END                                   ! END (Browsing Queue)
BRW1::UsingAdditionalSortOrder BYTE                        ! When true the view is using PROP:Order
BRW1::CurrentScroll  BYTE                                  ! Queue position of scroll thumb
BRW1::ScrollRecordCount LONG                               ! Queue position of scroll thumb
BRW1::SkipFirst      BYTE                                  ! Skip first retrieved record in page fill
BRW1::Sort1:KeyDistribution LIKE(TEA:Department),DIM(100)
BRW1::Sort1:LowValue LIKE(TEA:Department)                  ! Queue position of scroll thumb
BRW1::Sort1:HighValue LIKE(TEA:Department)                 ! Queue position of scroll thumb
BRW1::Sort2:KeyDistribution LIKE(TEA:LastName),DIM(100)
BRW1::Sort2:LowValue LIKE(TEA:LastName)                    ! Queue position of scroll thumb
BRW1::Sort2:HighValue LIKE(TEA:LastName)                   ! Queue position of scroll thumb
BRW1::CurrentEvent   LONG                                  !
BRW1::CurrentChoice  LONG                                  !
BRW1::RecordCount    LONG                                  !
BRW1::SortOrder      BYTE                                  !
BRW1::LocateMode     BYTE                                  !
BRW1::RefreshMode    BYTE                                  !
BRW1::LastSortOrder  BYTE                                  !
BRW1::FillDirection  BYTE                                  !
BRW1::AddQueue       BYTE                                  !
BRW1::Changed        BYTE                                  !
BRW1::RecordStatus   BYTE                                  ! Flag for Range/Filter test
BRW1::ItemsToFill    LONG                                  ! Controls records retrieved
BRW1::MaxItemsInList LONG                                  ! Retrieved after window opened
BRW1::HighlightedPosition STRING(1024)                     ! POSITION of located record
BRW1::NewSelectPosted BYTE                                 ! Queue position of located record
BRW1::PopupText      CSTRING(10000)                        !
BRW1::ActiveInvisible BYTE(1)                              !
BRW1::LoadPending    BYTE                                  !
ToolBarMode          UNSIGNED,AUTO
BrowseButtons        GROUP                      !info for current browse with focus
ListBox                SIGNED                   !Browse list control
InsertButton           SIGNED                   !Browse insert button
ChangeButton           SIGNED                   !Browse change button
DeleteButton           SIGNED                   !Browse delete button
SelectButton           SIGNED                   !Browse select button
                     END
QuickWindow          WINDOW('Drag a Teacher'),AT(,,251,171),FONT('MS Sans Serif',8,COLOR:Black),CENTER,GRAY,IMM, |
  MDI,HLP('~SelectTeachers'),SYSTEM
                       LIST,AT(8,20,230,124),USE(?Browse:1),HVSCROLL,DRAGID('Teachers'),FORMAT('80L(2)|M~Last ' & |
  'Name~@S20@80L(2)|M~First Name~@S20@80L(2)|M~Department~@S20@'),FROM(Queue:Browse:1),IMM, |
  MSG('Browsing Records')
                       SHEET,AT(4,4,242,162),USE(?CurrentTab)
                         TAB('by Last Name')
                         END
                         TAB('by Department')
                         END
                       END
                       BUTTON('Help'),AT(0,0,45,14),USE(?Help),HIDE,STD(STD:Help)
                     END
  CODE
  PUSHBIND
  !Setup inter-thread processing
  LOC:DropThread  = GLO:DropThread
  LOC:DropControl = GLO:DropControl
  LOC:ThreadRef &= GLO:ThreadRef
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
    OF EVENT:CloseWindow
      !Flag thread closed
      LOC:ThreadRef = 0
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
      DO BRW1::AssignButtons
      IF NOT WindowInitialized
        DO InitializeWindow
        WindowInitialized = True
      END
      SELECT(?Browse:1)
    OF EVENT:Sized
      POST(EVENT:DoResize,0,THREAD())
    OF Event:Rejected
      BEEP
      DISPLAY(?)
      SELECT(?)
    ELSE
      IF ToolBarMode=BrowseMode THEN
        DO ListBoxDispatch
      END
    END
    CASE FIELD()
    OF ?Browse:1
      CASE EVENT()
      OF EVENT:NewSelection
        DO BRW1::NewSelection
      OF EVENT:ScrollUp
        DO BRW1::ProcessScroll
      OF EVENT:ScrollDown
        DO BRW1::ProcessScroll
      OF EVENT:PageUp
        DO BRW1::ProcessScroll
      OF EVENT:PageDown
        DO BRW1::ProcessScroll
      OF EVENT:ScrollTop
        DO BRW1::ProcessScroll
      OF EVENT:ScrollBottom
        DO BRW1::ProcessScroll
      OF EVENT:ScrollDrag
        DO BRW1::ScrollDrag
      OF EVENT:AlertKey
        DO BRW1::AlertKey
      OF EVENT:Drag
        !Pass dragged data
        DO BRW1::NewSelection
        SETDROPID(Tea:Number)
      END
    OF ?CurrentTab
      CASE EVENT()
      OF EVENT:Accepted
        DO RefreshWindow
      OF EVENT:Selected
        DO RefreshWindow
      OF EVENT:Selecting
        DO RefreshWindow
      OF EVENT:NewSelection
        DO RefreshWindow
      OF EVENT:TabChanging
        DO RefreshWindow
      END
    END
  END
  DO ProcedureReturn
!---------------------------------------------------------------------------
PrepareProcedure ROUTINE
  IF Majors::Used = 0
    CheckOpen(Majors,1)
  END
  Majors::Used += 1
  IF Teachers::Used = 0
    CheckOpen(Teachers,1)
  END
  Teachers::Used += 1
  FilesOpened = TRUE
  DO BindFields
  OPEN(QuickWindow)
  WindowOpened=True
  BRW1::AddQueue = True
  BRW1::RecordCount = 0
  Do DefineListboxStyle
  ?Browse:1{PROP:Alrt,252} = MouseLeft2
  ?Browse:1{PROP:Alrt,255} = AppsKey
  ?Browse:1{PROP:Alrt,253} = MouseRight

!---------------------------------------------------------------------------
BindFields ROUTINE
  BIND(MAJ:RECORD)
  BIND(TEA:RECORD)
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
    Majors::Used -= 1
    IF Majors::Used = 0 THEN CLOSE(Majors).
    Teachers::Used -= 1
    IF Teachers::Used = 0 THEN CLOSE(Teachers).
  END
  IF WindowOpened
    CLOSE(QuickWindow)
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
  IF QuickWindow{Prop:AcceptAll} THEN EXIT.
  Do LookupRelated
  DO BRW1::SelectSort
  ?Browse:1{Prop:VScrollPos} = BRW1::CurrentScroll
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
  DO BRW1::GetRecord
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
BRW1::AssignButtons ROUTINE
  CLEAR(BrowseButtons)
  BrowseButtons.ListBox = ?Browse:1
  DO DisplayBrowseToolbar
!----------------------------------------------------------------------
BRW1::SelectSort ROUTINE
!|
!| This routine is called during the RefreshWindow ROUTINE present in every window procedure.
!| The purpose of this routine is to make certain that the BrowseBox is always current with your
!| user's selections. This routine...
!|
!| 1. Checks to see if any of your specified sort-order conditions are met, and if so, changes the sort order.
!| 2. If no sort order change is necessary, this routine checks to see if any of your Reset Fields has changed.
!| 3. If the sort order has changed, or if a reset field has changed, or if the ForceRefresh flag is set...
!|    a. The current record is retrieved from the disk.
!|    b. If the BrowseBox is accessed for the first time, and the Browse has been called to select a record,
!|       the page containing the current record is loaded.
!|    c. If the BrowseBox is accessed for the first time, and the Browse has not been called to select a
!|       record, the first page of information is loaded.
!|    d. If the BrowseBox is not being accessed for the first time, and the Browse sort order has changed, the
!|       new "first" page of information is loaded.
!|    e. If the BrowseBox is not being accessed for the first time, and the Browse sort order hasn't changes,
!|       the page containing the current record is reloaded.
!|    f. The record buffer is refilled from the currently highlighted BrowseBox item.
!|    f. The BrowseBox is reinitialized (BRW1::InitializeBrowse ROUTINE).
!| 4. If step 3 is not necessary, the record buffer is refilled from the currently highlighted BrowseBox item.
!|
  BRW1::LastSortOrder = BRW1::SortOrder
  BRW1::Changed = False
  IF CHOICE(?CurrentTab) = 2
    BRW1::SortOrder = 1
  ELSE
    BRW1::SortOrder = 2
  END
  IF BRW1::SortOrder <> BRW1::LastSortOrder OR BRW1::Changed OR ForceRefresh OR (BRW1::LoadPending AND ?Browse:1{PROP:VISIBLE})
    DO BRW1::GetRecord
    DO BRW1::Reset
    IF BRW1::LastSortOrder = 0
      IF LocalRequest = SelectRecord
        BRW1::LocateMode = LocateOnValue
        DO BRW1::LocateRecord
      ELSE
        FREE(Queue:Browse:1)
        BRW1::RefreshMode = RefreshOnTop
        DO BRW1::RefreshPage
        DO BRW1::PostNewSelection
      END
    ELSE
      IF BRW1::Changed
        FREE(Queue:Browse:1)
        BRW1::RefreshMode = RefreshOnTop
        DO BRW1::RefreshPage
        DO BRW1::PostNewSelection
      ELSE
        BRW1::LocateMode = LocateOnValue
        DO BRW1::LocateRecord
      END
    END
    IF BRW1::RecordCount
      GET(Queue:Browse:1,BRW1::CurrentChoice)
      DO BRW1::FillBuffer
    END
    DO BRW1::InitializeBrowse
  ELSE
    IF BRW1::RecordCount
      GET(Queue:Browse:1,BRW1::CurrentChoice)
      DO BRW1::FillBuffer
    END
  END
!----------------------------------------------------------------------
BRW1::InitializeBrowse ROUTINE
!|
!| This routine initializes the BrowseBox control template. This routine is called when...
!|
!| The BrowseBox sort order has changed. This includes the first time the BrowseBox is accessed.
!| The BrowseBox returns from a record update.
!|
!| This routine performs two main functions.
!|   1. Computes all BrowseBox totals. All records that satisfy the current selection criteria
!|      are read, and totals computed. If no totals are present, this section is not generated,
!|      and may not be present in the code below.
!|   2. Calculates any runtime scrollbar positions. Again, if runtime scrollbars are not used,
!|      the code for runtime scrollbar computation will not be present.
!|
  IF NOT BRW1::ActiveInvisible THEN
     IF NOT ?Browse:1{PROP:Visible} THEN
        BRW1::LoadPending = True
        EXIT
     END
  END
  DO BRW1::Reset
  PREVIOUS(BRW1::View:Browse)
  IF ERRORCODE()
    IF ERRORCODE() = BadRecErr
      DO BRW1::RestoreResetValues
    ELSE
      StandardWarning(Warn:RecordFetchError,'Teachers')
      POST(Event:CloseWindow)
    END
    EXIT
  END
  CASE BRW1::SortOrder
  OF 1
    BRW1::Sort1:HighValue = TEA:Department
  OF 2
    BRW1::Sort2:HighValue = TEA:LastName
  END
  DO BRW1::Reset
  NEXT(BRW1::View:Browse)
  IF ERRORCODE()
    IF ERRORCODE() = BadRecErr
      DO BRW1::RestoreResetValues
    ELSE
      StandardWarning(Warn:RecordFetchError,'Teachers')
      POST(Event:CloseWindow)
    END
    EXIT
  END
  CASE BRW1::SortOrder
  OF 1
    BRW1::Sort1:LowValue = TEA:Department
    SetupRealStops(BRW1::Sort1:LowValue,BRW1::Sort1:HighValue)
    LOOP BRW1::ScrollRecordCount = 1 TO 100
      BRW1::Sort1:KeyDistribution[BRW1::ScrollRecordCount] = NextRealStop()
    END
  OF 2
    BRW1::Sort2:LowValue = TEA:LastName
    SetupStringStops(BRW1::Sort2:LowValue,BRW1::Sort2:HighValue,SIZE(BRW1::Sort2:LowValue),ScrollSort:AllowAlpha)
    LOOP BRW1::ScrollRecordCount = 1 TO 100
      BRW1::Sort2:KeyDistribution[BRW1::ScrollRecordCount] = NextStringStop()
    END
  END
!----------------------------------------------------------------------
BRW1::FillBuffer ROUTINE
!|
!| This routine fills the record buffer from the BrowseBox queue. This gives the appearance
!| that the record is "fresh" from the disk, without the disk access required.
!|
  TEA:LastName = BRW1::TEA:LastName
  TEA:FirstName = BRW1::TEA:FirstName
  MAJ:Description = BRW1::MAJ:Description
  TEA:Number = BRW1::TEA:Number
  TEA:Department = BRW1::TEA:Department
  MAJ:Number = BRW1::MAJ:Number
!----------------------------------------------------------------------
BRW1::FillQueue ROUTINE
!|
!| This routine is used to fill the BrowseBox QUEUE from several sources.
!|
!| First, all Format Browse formulae are processed.
!|
!| Next, each field of the BrowseBox is processed. For each field...
!|
!|    The value of the field is placed in the BrowseBox queue.
!|
!| Finally, the POSITION of the current VIEW record is added to the QUEUE
!|
  BRW1::TEA:LastName = TEA:LastName
  BRW1::TEA:FirstName = TEA:FirstName
  BRW1::MAJ:Description = MAJ:Description
  BRW1::TEA:Number = TEA:Number
  BRW1::TEA:Department = TEA:Department
  BRW1::MAJ:Number = MAJ:Number
  BRW1::Position = POSITION(BRW1::View:Browse)
!----------------------------------------------------------------------
BRW1::PostNewSelection ROUTINE
!|
!| This routine is used to post the NewSelection EVENT to the window. Because we only want this
!| EVENT processed once, and becuase there are several routines that need to initiate a NewSelection
!| EVENT, we keep a flag that tells us if the EVENT is already waiting to be processed. The EVENT is
!| only POSTed if this flag is false.
!|
  IF NOT BRW1::NewSelectPosted
    BRW1::NewSelectPosted = True
    POST(EVENT:NewSelection,?Browse:1)
  END
!----------------------------------------------------------------------
BRW1::NewSelection ROUTINE
!|
!| This routine performs any window bookkeeping necessary when a new record is selected in the
!| BrowseBox.
!| 1. If the new selection is made with the right mouse button, the popup menu (if applicable) is
!|    processed.
!| 2. The current record is retrieved into the buffer using the BRW1::FillBuffer ROUTINE.
!|    After this, the current vertical scrollbar position is computed, and the scrollbar positioned.
!|
  IF NOT BRW1::ActiveInvisible THEN
     IF NOT ?Browse:1{PROP:Visible} THEN
        BRW1::LoadPending = True
        EXIT
     END
  END
  BRW1::NewSelectPosted = False
  IF KEYCODE() = MouseRight OR KEYCODE() = AppsKey
     SETKEYCODE(0)
    BRW1::PopupText = ''
    IF BRW1::RecordCount
    ELSE
    END
    EXECUTE(POPUP(BRW1::PopupText))
    END
  ELSIF BRW1::RecordCount
    BRW1::CurrentChoice = CHOICE(?Browse:1)
    GET(Queue:Browse:1,BRW1::CurrentChoice)
    DO BRW1::FillBuffer
    IF BRW1::RecordCount = ?Browse:1{PROP:Items}
      IF NOT ?Browse:1{PROP:VScroll}
        ?Browse:1{PROP:VScroll} = True
      END
      CASE BRW1::SortOrder
      OF 1
        LOOP BRW1::CurrentScroll = 1 TO 100
          IF BRW1::Sort1:KeyDistribution[BRW1::CurrentScroll] => TEA:Department
            IF BRW1::CurrentScroll <= 1
              BRW1::CurrentScroll = 0
            ELSIF BRW1::CurrentScroll = 100
              BRW1::CurrentScroll = 100
            ELSE
            END
            BREAK
          END
        END
      OF 2
        LOOP BRW1::CurrentScroll = 1 TO 100
          IF BRW1::Sort2:KeyDistribution[BRW1::CurrentScroll] => UPPER(TEA:LastName)
            IF BRW1::CurrentScroll <= 1
              BRW1::CurrentScroll = 0
            ELSIF BRW1::CurrentScroll = 100
              BRW1::CurrentScroll = 100
            ELSE
            END
            BREAK
          END
        END
      END
    ELSE
      IF ?Browse:1{PROP:VScroll}
        ?Browse:1{PROP:VScroll} = False
      END
    END
    DO RefreshWindow
  END
!---------------------------------------------------------------------
BRW1::ProcessScroll ROUTINE
!|
!| This routine processes any of the six scrolling EVENTs handled by the BrowseBox.
!| If one record is to be scrolled, the ROUTINE BRW1::ScrollOne is called.
!| If a page of records is to be scrolled, the ROUTINE BRW1::ScrollPage is called.
!| If the first or last page is to be displayed, the ROUTINE BRW1::ScrollEnd is called.
!|
!| If an incremental locator is in use, the value of that locator is cleared.
!| Finally, if a Fixed Thumb vertical scroll bar is used, the thumb is positioned.
!|
  IF BRW1::RecordCount
    BRW1::CurrentEvent = EVENT()
    CASE BRW1::CurrentEvent
    OF EVENT:ScrollUp OROF EVENT:ScrollDown
      DO BRW1::ScrollOne
    OF EVENT:PageUp OROF EVENT:PageDown
      DO BRW1::ScrollPage
    OF EVENT:ScrollTop OROF EVENT:ScrollBottom
      DO BRW1::ScrollEnd
    END
    ?Browse:1{PROP:SelStart} = BRW1::CurrentChoice
    DO BRW1::PostNewSelection
  END
!----------------------------------------------------------------------
BRW1::ScrollOne ROUTINE
!|
!| This routine is used to scroll a single record on the BrowseBox. Since the BrowseBox is an IMM
!| listbox, all scrolling must be handled in code. When called, this routine...
!|
!| 1. Sees if scrolling in the intended direction will cause the listbox display to shift. If not,
!|    the routine moves the list box cursor and exits.
!| 2. Calls BRW1::FillRecord to retrieve one record in the direction required.
!|
  IF BRW1::CurrentEvent = EVENT:ScrollUp AND BRW1::CurrentChoice > 1
    BRW1::CurrentChoice -= 1
    EXIT
  ELSIF BRW1::CurrentEvent = EVENT:ScrollDown AND BRW1::CurrentChoice < BRW1::RecordCount
    BRW1::CurrentChoice += 1
    EXIT
  END
  BRW1::ItemsToFill = 1
  BRW1::FillDirection = BRW1::CurrentEvent - 2
  DO BRW1::FillRecord
!----------------------------------------------------------------------
BRW1::ScrollPage ROUTINE
!|
!| This routine is used to scroll a single page of records on the BrowseBox. Since the BrowseBox is
!| an IMM listbox, all scrolling must be handled in code. When called, this routine...
!|
!| 1. Calls BRW1::FillRecord to retrieve one page of records in the direction required.
!| 2. If BRW1::FillRecord doesn't fill a page (BRW1::ItemsToFill > 0), then
!|    the list-box cursor ia shifted.
!|
  BRW1::ItemsToFill = ?Browse:1{PROP:Items}
  BRW1::FillDirection = BRW1::CurrentEvent - 4
  DO BRW1::FillRecord                           ! Fill with next read(s)
  IF BRW1::ItemsToFill
    IF BRW1::CurrentEvent = EVENT:PageUp
      BRW1::CurrentChoice -= BRW1::ItemsToFill
      IF BRW1::CurrentChoice < 1
        BRW1::CurrentChoice = 1
      END
    ELSE
      BRW1::CurrentChoice += BRW1::ItemsToFill
      IF BRW1::CurrentChoice > BRW1::RecordCount
        BRW1::CurrentChoice = BRW1::RecordCount
      END
    END
  END
!----------------------------------------------------------------------
BRW1::ScrollEnd ROUTINE
!|
!| This routine is used to load the first or last page of the displayable set of records.
!| Since the BrowseBox is an IMM listbox, all scrolling must be handled in code. When called,
!| this routine...
!|
!| 1. Resets the BrowseBox VIEW to insure that it reads from the end of the current sort order.
!| 2. Calls BRW1::FillRecord to retrieve one page of records.
!| 3. Selects the record that represents the end of the view. That is, if the first page was loaded,
!|    the first record is highlighted. If the last was loaded, the last record is highlighted.
!|
  FREE(Queue:Browse:1)
  BRW1::RecordCount = 0
  DO BRW1::Reset
  BRW1::ItemsToFill = ?Browse:1{PROP:Items}
  IF BRW1::CurrentEvent = EVENT:ScrollTop
    BRW1::FillDirection = FillForward
  ELSE
    BRW1::FillDirection = FillBackward
  END
  DO BRW1::FillRecord                           ! Fill with next read(s)
  IF BRW1::CurrentEvent = EVENT:ScrollTop
    BRW1::CurrentChoice = 1
  ELSE
    BRW1::CurrentChoice = BRW1::RecordCount
  END
!----------------------------------------------------------------------
BRW1::AlertKey ROUTINE
!|
!| This routine processes any KEYCODEs experienced by the BrowseBox.
!| NOTE: The cursor movement keys are not processed as KEYCODEs. They are processed as the
!|       appropriate BrowseBox scrolling and selection EVENTs.
!| This routine includes handling for double-click. Actually, this handling is in the form of
!| EMBEDs, which are filled by child-control templates.
!| This routine also includes the BrowseBox's locator handling.
!| After a value is entered for locating, this routine sets BRW1::LocateMode to a value
!| of 2 -- EQUATEd to LocateOnValue -- and calls the routine BRW1::LocateRecord.
!|
  IF BRW1::RecordCount
    CASE KEYCODE()                                         ! What keycode was hit
    OF AppsKey
    OROF MouseRight
       IF ?Browse:1{PROPLIST:MouseDownRow}>0
         ?Browse:1{PROP:Selected} = ?Browse:1{PROPLIST:MouseDownRow}
         BRW1::CurrentChoice = CHOICE(?Browse:1)
       END
       DO BRW1::NewSelection
    OF MouseLeft2
      !Pass dragged data
      DO BRW1::NewSelection
      SETDROPID(Tea:Number)
      POST(EVENT:Drop,LOC:DropControl,LOC:DropThread)
    ELSE                                                   ! ELSE (What keycode was hit)
      CASE BRW1::SortOrder
      OF 1
        IF CHR(KEYCHAR())
          IF UPPER(SUB(TEA:Department,1,1)) = UPPER(CHR(KEYCHAR()))
            BRW1::CurrentEvent = EVENT:ScrollDown
            DO BRW1::ScrollOne
            GET(Queue:Browse:1,BRW1::CurrentChoice)
            DO BRW1::FillBuffer
          END
          IF UPPER(SUB(TEA:Department,1,1)) = UPPER(CHR(KEYCHAR()))
            ?Browse:1{PROP:SelStart} = BRW1::CurrentChoice
          ELSE
            TEA:Department = CHR(KEYCHAR())
            BRW1::LocateMode = LocateOnValue
            DO BRW1::LocateRecord
          END
        END
      OF 2
        IF CHR(KEYCHAR())
          IF UPPER(SUB(TEA:LastName,1,1)) = UPPER(CHR(KEYCHAR()))
            BRW1::CurrentEvent = EVENT:ScrollDown
            DO BRW1::ScrollOne
            GET(Queue:Browse:1,BRW1::CurrentChoice)
            DO BRW1::FillBuffer
          END
          IF UPPER(SUB(TEA:LastName,1,1)) = UPPER(CHR(KEYCHAR()))
            ?Browse:1{PROP:SelStart} = BRW1::CurrentChoice
          ELSE
            TEA:LastName = CHR(KEYCHAR())
            BRW1::LocateMode = LocateOnValue
            DO BRW1::LocateRecord
          END
        END
      END
    END                                                    ! END (What keycode was hit)
  ELSE
    CASE KEYCODE()                                         ! What keycode was hit
    OF AppsKey
    OROF MouseRight
       DO BRW1::NewSelection
    ELSE                                                   ! ELSE (What keycode was hit)
      CASE BRW1::SortOrder
      OF 1
      OF 2
      END
    END
  END
  DO BRW1::PostNewSelection
!----------------------------------------------------------------------
BRW1::ScrollDrag ROUTINE
!|
!| This routine processes the Vertical Scroll Bar arrays to find the free key field value
!| that corresponds to the current scroll bar position.
!|
!| After the scroll position is computed, and the scroll value found, this routine sets
!| BRW1::LocateMode to that scroll value of 2 -- EQUATEd to LocateOnValue --
!| and calls the routine BRW1::LocateRecord.
!|
  IF ?Browse:1{PROP:VScrollPos} <= 1
    POST(EVENT:ScrollTop,?Browse:1)
  ELSIF ?Browse:1{PROP:VScrollPos} = 100
    POST(EVENT:ScrollBottom,?Browse:1)
  ELSE
    CASE BRW1::SortOrder
    OF 1
      TEA:Department = BRW1::Sort1:KeyDistribution[?Browse:1{PROP:VScrollPos}]
      BRW1::LocateMode = LocateOnValue
      DO BRW1::LocateRecord
    OF 2
      TEA:LastName = BRW1::Sort2:KeyDistribution[?Browse:1{PROP:VScrollPos}]
      BRW1::LocateMode = LocateOnValue
      DO BRW1::LocateRecord
    END
  END
!----------------------------------------------------------------------
BRW1::FillRecord ROUTINE
!|
!| This routine is used to retrieve a number of records from the VIEW. The number of records
!| retrieved is held in the variable BRW1::ItemsToFill. If more than one record is
!| to be retrieved, QuickScan is used to minimize reads from the disk.
!|
!| If records exist in the queue (in other words, if the browse has been used before), the record
!| at the appropriate end of the list box is retrieved, and the VIEW is reset to read starting
!| at that record.
!|
!| Next, the VIEW is accessed to retrieve BRW1::ItemsToFill records. Normally, this will
!| result in BRW1::ItemsToFill records being read from the VIEW, but if custom filtering
!| or range limiting is used (via the BRW1::ValidateRecord routine) then any number of records
!| might be read.
!|
!| For each good record, if BRW1::AddQueue is true, the queue is filled using the BRW1::FillQueue
!| routine. The record is then added to the queue. If adding this record causes the BrowseBox queue
!| to contain more records than can be displayed, the record at the opposite end of the queue is
!| deleted.
!|
!| The only time BRW1::AddQueue is false is when the BRW1::LocateRecord routine needs to
!| get the closest record to its record to be located. At this time, the record doesn't need to be
!| added to the queue, so it isn't.
!|
  IF BRW1::RecordCount
    IF BRW1::FillDirection = FillForward
      GET(Queue:Browse:1,BRW1::RecordCount)                ! Get the first queue item
    ELSE
      GET(Queue:Browse:1,1)                                ! Get the first queue item
    END
    RESET(BRW1::View:Browse,BRW1::Position)                ! Reset for sequential processing
    BRW1::SkipFirst = TRUE
  ELSE
    BRW1::SkipFirst = FALSE
  END
  LOOP WHILE BRW1::ItemsToFill
    IF BRW1::View:Browse{PROP:IPRequestCount} = 0
       BRW1::View:Browse{PROP:IPRequestCount} = BRW1::ItemsToFill
    END
    IF BRW1::FillDirection = FillForward
      NEXT(BRW1::View:Browse)
    ELSE
      PREVIOUS(BRW1::View:Browse)
    END
    IF ERRORCODE()
      IF ERRORCODE() = BadRecErr
        DO BRW1::RestoreResetValues
        BREAK
      ELSE
        StandardWarning(Warn:RecordFetchError,'Teachers')
        POST(EVENT:CloseWindow)
        EXIT
      END
    END
    IF BRW1::SkipFirst
       BRW1::SkipFirst = FALSE
       IF POSITION(BRW1::View:Browse) = BRW1::Position
          CYCLE
       END
    END
    IF BRW1::AddQueue
      IF BRW1::RecordCount = ?Browse:1{PROP:Items}
        IF BRW1::FillDirection = FillForward
          GET(Queue:Browse:1,1)                            ! Get the first queue item
        ELSE
          GET(Queue:Browse:1,BRW1::RecordCount)            ! Get the first queue item
        END
        DELETE(Queue:Browse:1)
        BRW1::RecordCount -= 1
      END
      DO BRW1::FillQueue
      IF BRW1::FillDirection = FillForward
        ADD(Queue:Browse:1)
      ELSE
        ADD(Queue:Browse:1,1)
      END
      BRW1::RecordCount += 1
    END
    BRW1::ItemsToFill -= 1
  END
  BRW1::AddQueue = True
  EXIT
!----------------------------------------------------------------------
BRW1::LocateRecord ROUTINE
!|
!| This routine is used to find a record in the VIEW, and to display that record
!| in the BrowseBox.
!|
!| This routine has three different modes of operation, which are invoked based on
!| the setting of BRW1::LocateMode. These modes are...
!|
!|   LocateOnPosition (1) - This mode is still supported for 1.5 compatability. This mode
!|                          is the same as LocateOnEdit.
!|   LocateOnValue    (2) - The values of the current sort order key are used. This mode
!|                          used for Locators and when the BrowseBox is called to select
!|                          a record.
!|   LocateOnEdit     (3) - The current record of the VIEW is used. This mode assumes
!|                          that there is an active VIEW record. This mode is used when
!|                          the sort order of the BrowseBox has changed
!|
!| If an appropriate record has been located, the BRW1::RefreshPage routine is
!| called to load the page containing the located record.
!|
!| If an appropriate record is not locate, the last page of the BrowseBox is loaded.
!|
  IF BRW1::LocateMode = LocateOnPosition
    BRW1::LocateMode = LocateOnEdit
  END
  CLOSE(BRW1::View:Browse)
  CASE BRW1::SortOrder
  OF 1
    IF BRW1::UsingAdditionalSortOrder THEN
       BRW1::UsingAdditionalSortOrder = False
       BRW1::View:Browse{PROP:Order} = '+TEA:Department'
    END
    IF BRW1::LocateMode = LocateOnEdit
      BRW1::HighlightedPosition = POSITION(TEA:KeyDepartment)
      RESET(TEA:KeyDepartment,BRW1::HighlightedPosition)
    ELSE
      SET(TEA:KeyDepartment,TEA:KeyDepartment)
    END
  OF 2
    IF BRW1::UsingAdditionalSortOrder THEN
       BRW1::UsingAdditionalSortOrder = False
       BRW1::View:Browse{PROP:Order} = '+UPPER(TEA:LastName)'
    END
    IF BRW1::LocateMode = LocateOnEdit
      BRW1::HighlightedPosition = POSITION(TEA:KeyLastName)
      RESET(TEA:KeyLastName,BRW1::HighlightedPosition)
    ELSE
      SET(TEA:KeyLastName,TEA:KeyLastName)
    END
  END
  IF ERRORCODE()
    StandardWarning(Warn:ViewOpenError)
  END
  OPEN(BRW1::View:Browse)
  IF ERRORCODE()
    StandardWarning(Warn:ViewOpenError)
  END
  IF BRW1::UsingAdditionalSortOrder = True
    CASE BRW1::SortOrder
    OF 1
       SET(BRW1::View:Browse,1)
    OF 2
       SET(BRW1::View:Browse,1)
    END
  END
  FREE(Queue:Browse:1)
  BRW1::RecordCount = 0
  BRW1::ItemsToFill = 1
  BRW1::FillDirection = FillForward                        ! Fill with next read(s)
  BRW1::AddQueue = False
  DO BRW1::FillRecord                                      ! Fill with next read(s)
  BRW1::AddQueue = True
  IF BRW1::ItemsToFill
    BRW1::RefreshMode = RefreshOnBottom
    DO BRW1::RefreshPage
  ELSE
    BRW1::RefreshMode = RefreshOnPosition
    DO BRW1::RefreshPage
  END
  DO BRW1::PostNewSelection
  BRW1::LocateMode = 0
  EXIT
!----------------------------------------------------------------------
BRW1::RefreshPage ROUTINE
!|
!| This routine is used to load a single page of the BrowseBox.
!|
!| If this routine is called with a BRW1::RefreshMode of RefreshOnPosition,
!| the active VIEW record is loaded at the top of the page. Otherwise, if there are
!| records in the browse queue (Queue:Browse:1), then the current page is reloaded, and the
!| currently selected item remains selected.
!|
  IF NOT BRW1::ActiveInvisible THEN
     IF NOT ?Browse:1{PROP:Visible} THEN
        BRW1::LoadPending = True
        EXIT
     END
  END
  SETCURSOR(Cursor:Wait)
  IF BRW1::RefreshMode = RefreshOnPosition
    BRW1::HighlightedPosition = POSITION(BRW1::View:Browse)
    RESET(BRW1::View:Browse,BRW1::HighlightedPosition)
    BRW1::RefreshMode = RefreshOnTop
  ELSIF RECORDS(Queue:Browse:1)
    GET(Queue:Browse:1,BRW1::CurrentChoice)
    IF ERRORCODE()
      GET(Queue:Browse:1,RECORDS(Queue:Browse:1))
    END
    BRW1::HighlightedPosition = BRW1::Position
    GET(Queue:Browse:1,1)
    RESET(BRW1::View:Browse,BRW1::Position)
    BRW1::RefreshMode = RefreshOnCurrent
  ELSE
    BRW1::HighlightedPosition = ''
    DO BRW1::Reset
  END
  FREE(Queue:Browse:1)
  BRW1::RecordCount = 0
  BRW1::ItemsToFill = ?Browse:1{PROP:Items}
  IF BRW1::RefreshMode = RefreshOnBottom
    BRW1::FillDirection = FillBackward
  ELSE
    BRW1::FillDirection = FillForward
  END
  DO BRW1::FillRecord                                      ! Fill with next read(s)
  IF BRW1::HighlightedPosition
    IF BRW1::ItemsToFill
      IF NOT BRW1::RecordCount
        DO BRW1::Reset
      END
      IF BRW1::RefreshMode = RefreshOnBottom
        BRW1::FillDirection = FillForward
      ELSE
        BRW1::FillDirection = FillBackward
      END
      DO BRW1::FillRecord
    END
  END
  IF BRW1::RecordCount
    IF BRW1::HighlightedPosition
      LOOP BRW1::CurrentChoice = 1 TO BRW1::RecordCount
        GET(Queue:Browse:1,BRW1::CurrentChoice)
        IF BRW1::Position = BRW1::HighlightedPosition THEN BREAK.
      END
      IF BRW1::CurrentChoice > BRW1::RecordCount
        BRW1::CurrentChoice = BRW1::RecordCount
      END
    ELSE
      IF BRW1::RefreshMode = RefreshOnBottom
        BRW1::CurrentChoice = RECORDS(Queue:Browse:1)
      ELSE
        BRW1::CurrentChoice = 1
      END
    END
    ?Browse:1{Prop:Selected} = BRW1::CurrentChoice
    DO BRW1::FillBuffer
  ELSE
    CLEAR(TEA:Record)
    BRW1::CurrentChoice = 0
  END
  SETCURSOR()
  BRW1::RefreshMode = 0
  EXIT
BRW1::Reset ROUTINE
!|
!| This routine is used to reset the VIEW used by the BrowseBox.
!|
  IF NOT BRW1::ActiveInvisible THEN
     IF NOT ?Browse:1{PROP:Visible} THEN
        BRW1::LoadPending = True
        EXIT
     END
  END
  BRW1::LoadPending = False
  CLOSE(BRW1::View:Browse)
  CASE BRW1::SortOrder
  OF 1
    IF BRW1::UsingAdditionalSortOrder THEN
       BRW1::UsingAdditionalSortOrder = False
       BRW1::View:Browse{PROP:Order} = '+TEA:Department'
    END
    SET(TEA:KeyDepartment)
  OF 2
    IF BRW1::UsingAdditionalSortOrder THEN
       BRW1::UsingAdditionalSortOrder = False
       BRW1::View:Browse{PROP:Order} = '+UPPER(TEA:LastName)'
    END
    SET(TEA:KeyLastName)
  END
  IF ERRORCODE()
    StandardWarning(Warn:ViewOpenError)
  END
  OPEN(BRW1::View:Browse)
  IF ERRORCODE()
    StandardWarning(Warn:ViewOpenError)
  END
  SET(BRW1::View:Browse)
!----------------------------------------------------------------------
BRW1::GetRecord ROUTINE
!|
!| This routine is used to retrieve the VIEW record that corresponds to a
!| chosen listbox record.
!|
  IF BRW1::RecordCount
    BRW1::CurrentChoice = CHOICE(?Browse:1)
    GET(Queue:Browse:1,BRW1::CurrentChoice)
    WATCH(BRW1::View:Browse)
    REGET(BRW1::View:Browse,BRW1::Position)
  END
!----------------------------------------------------------------------
BRW1::RestoreResetValues ROUTINE
!|
!| This routine is used to restore reset values to their saved value
!| after a bad record access from the VIEW.
!|
!--------------------------------------------------------------------------
DisplayBrowseToolbar      ROUTINE
  ENABLE(TBarBrwBottom,TBarBrwLocate)
  TBarBrwHelp{PROP:DISABLE}=?Help{PROP:DISABLE}
  DISABLE(TBarBrwHistory)
  ToolBarMode = BrowseMode
  TBarBrwDown{PROP:ToolTip} = 'Go to the Next Record'
  TBarBrwBottom{PROP:ToolTip} = 'Go to the Last Page'
  TBarBrwTop{PROP:ToolTip} = 'Go to the First Page'
  TBarBrwPageDown{PROP:ToolTip} = 'Go to the Next Page'
  TBarBrwPageUp{PROP:ToolTip} = 'Go to the Prior Page'
  TBarBrwDown{PROP:ToolTip} = 'Go to the Next Record'
  TBarBrwUP{PROP:ToolTip} = 'Go to the Prior Record'
  TBarBrwInsert{PROP:ToolTip} = 'Insert a new Record'
  DISPLAY(TBarBrwFirst,TBarBrwLast)
!--------------------------------------------------------------------------
ListBoxDispatch ROUTINE
  DO DisplayBrowseToolbar
  IF ACCEPTED() THEN            !trap remote browse box control calls
    EXECUTE(ACCEPTED()-TBarBrwBottom+1)
      POST(EVENT:ScrollBottom,BrowseButtons.ListBox)
      POST(EVENT:ScrollTop,BrowseButtons.ListBox)
      POST(EVENT:PageDown,BrowseButtons.ListBox)
      POST(EVENT:PageUp,BrowseButtons.ListBox)
      POST(EVENT:ScrollDown,BrowseButtons.ListBox)
      POST(EVENT:ScrollUp,BrowseButtons.ListBox)
      POST(EVENT:Locate,BrowseButtons.ListBox)
      BEGIN                     !EXECUTE Place Holder - Ditto has no effect on a browse
      END
      PRESSKEY(F1Key)
    END
  END
!!! <summary>
!!! Generated from procedure template - Form
!!! Update the Classes File
!!! </summary>
UpdateClasses PROCEDURE

CurrentTab           STRING(80)                            ! 
LocalRequest         LONG                                  ! 
OriginalRequest      LONG                                  ! 
LocalResponse        LONG                                  ! 
FilesOpened          LONG                                  ! 
WindowOpened         LONG                                  ! 
WindowInitialized    LONG                                  ! 
ForceRefresh         LONG                                  ! 
RecordFiltered       LONG                                  ! 
ActionMessage        CSTRING(40)                           ! 
RecordChanged        BYTE,AUTO                             ! 
SelectCoursesThread  LONG                                  ! 
SelectTeachersThread LONG                                  ! 

BRW2::View:Browse    VIEW(Enrollment)
                       PROJECT(ENR:StudentNumber)
                       PROJECT(ENR:MidtermExam)
                       PROJECT(ENR:FinalExam)
                       PROJECT(ENR:TermPaper)
                       PROJECT(ENR:ClassNumber)
                       JOIN(STU:KeyStudentNumber,ENR:StudentNumber)
                         PROJECT(STU:LastName)
                         PROJECT(STU:FirstName)
                         PROJECT(STU:Number)
                       END
                     END
Queue:Browse:2       QUEUE,PRE()                           ! Browsing Queue
BRW2::ENR:StudentNumber LIKE(ENR:StudentNumber)            ! Queue Display field
BRW2::STU:LastName     LIKE(STU:LastName)                  ! Queue Display field
BRW2::STU:FirstName    LIKE(STU:FirstName)                 ! Queue Display field
BRW2::ENR:MidtermExam  LIKE(ENR:MidtermExam)               ! Queue Display field
BRW2::ENR:FinalExam    LIKE(ENR:FinalExam)                 ! Queue Display field
BRW2::ENR:TermPaper    LIKE(ENR:TermPaper)                 ! Queue Display field
BRW2::ENR:ClassNumber  LIKE(ENR:ClassNumber)               ! Queue Display field
BRW2::STU:Number       LIKE(STU:Number)                    ! Queue Display field
BRW2::Mark             BYTE                                ! Record mark flag
BRW2::Position         STRING(1024)                        ! Queue POSITION information
                     END                                   ! END (Browsing Queue)
BRW2::UsingAdditionalSortOrder BYTE                        ! When true the view is using PROP:Order
BRW2::CurrentScroll  BYTE                                  ! Queue position of scroll thumb
BRW2::ScrollRecordCount LONG                               ! Queue position of scroll thumb
BRW2::SkipFirst      BYTE                                  ! Skip first retrieved record in page fill
BRW2::Sort1:KeyDistribution LIKE(ENR:StudentNumber),DIM(100)
BRW2::Sort1:LowValue LIKE(ENR:StudentNumber)               ! Queue position of scroll thumb
BRW2::Sort1:HighValue LIKE(ENR:StudentNumber)              ! Queue position of scroll thumb
BRW2::Sort1:Reset:CLA:ClassNumber LIKE(CLA:ClassNumber)
BRW2::CurrentEvent   LONG                                  !
BRW2::CurrentChoice  LONG                                  !
BRW2::RecordCount    LONG                                  !
BRW2::SortOrder      BYTE                                  !
BRW2::LocateMode     BYTE                                  !
BRW2::RefreshMode    BYTE                                  !
BRW2::LastSortOrder  BYTE                                  !
BRW2::FillDirection  BYTE                                  !
BRW2::AddQueue       BYTE                                  !
BRW2::Changed        BYTE                                  !
BRW2::RecordStatus   BYTE                                  ! Flag for Range/Filter test
BRW2::ItemsToFill    LONG                                  ! Controls records retrieved
BRW2::MaxItemsInList LONG                                  ! Retrieved after window opened
BRW2::HighlightedPosition STRING(1024)                     ! POSITION of located record
BRW2::NewSelectPosted BYTE                                 ! Queue position of located record
BRW2::PopupText      CSTRING(10000)                        !
BRW2::ActiveInvisible BYTE(1)                              !
BRW2::LoadPending    BYTE                                  !
ToolBarMode          UNSIGNED,AUTO
BrowseButtons        GROUP                      !info for current browse with focus
ListBox                SIGNED                   !Browse list control
InsertButton           SIGNED                   !Browse insert button
ChangeButton           SIGNED                   !Browse change button
DeleteButton           SIGNED                   !Browse delete button
SelectButton           SIGNED                   !Browse select button
                     END
Update::Reloop  BYTE
Update::Error   BYTE
History::CLA:Record LIKE(CLA:Record),STATIC
SAV::CLA:Record      LIKE(CLA:Record)
Save:LastInsertedPosition STRING(512)
WindowXPos      SIGNED,AUTO,STATIC
WindowYPos      SIGNED,AUTO,STATIC
WindowPosInit   BOOL(False),STATIC
QuickWindow          WINDOW('Update the Classes File'),AT(,,164,138),FONT('MS Sans Serif',8,COLOR:Black),GRAY,IMM, |
  MDI,HLP('~UpdateClasses'),SYSTEM
                       SHEET,AT(2,3,158,114),USE(?CurrentTab)
                         TAB('General')
                           PROMPT('&Class Number:'),AT(7,21),USE(?CLA:ClassNumber:Prompt)
                           ENTRY(@P##-#####P),AT(71,21,40,10),USE(CLA:ClassNumber),RIGHT(1)
                           PROMPT('&Course Number:'),AT(7,34),USE(?CLA:CourseNumber:Prompt)
                           ENTRY(@n4),AT(71,34,24,10),USE(CLA:CourseNumber),RIGHT(1),ALRT(F10Key),DROPID('Courses')
                           BUTTON('...'),AT(98,33,12,12),USE(?Button7)
                           STRING(@S20),AT(71,47),USE(COU:Description)
                           ENTRY(@p###-##-####p),AT(71,60,,10),USE(CLA:TeacherNumber),RIGHT(1),ALRT(F10Key),DROPID('Teachers')
                           BUTTON('...'),AT(128,60,12,12),USE(?Button7:2)
                           PROMPT('&Teacher Number:'),AT(7,60),USE(?CLA:TeacherNumber:Prompt)
                           STRING(@S20),AT(71,73),USE(TEA:LastName)
                           PROMPT('Room Number:'),AT(7,90),USE(?CLA:RoomNumber:Prompt)
                           ENTRY(@n4),AT(71,90,40,10),USE(CLA:RoomNumber)
                           PROMPT('Scheduled Time:'),AT(7,103),USE(?CLA:ScheduledTime:Prompt)
                           ENTRY(@s20),AT(71,103,84,10),USE(CLA:ScheduledTime)
                         END
                         TAB('Enrollment'),USE(?EnrollmentTab)
                           LIST,AT(7,21,148,94),USE(?Browse:2),HVSCROLL,FORMAT('60L(2)|M~Student Number~@p###-##-#' & |
  '###p@80L(2)|M~Last Name~@S20@80L(2)|M~First Name~@S20@52R(2)|M~Midterm Exam~C(0)@n3@' & |
  '44R(2)|M~Final Exam~C(0)@n3@44R(2)|M~Term Paper~C(0)@n3@'),FROM(Queue:Browse:2),IMM,MSG('Browsing Records')
                           BUTTON('&Insert'),AT(12,98,45,14),USE(?Insert:3),HIDE
                           BUTTON('&Change'),AT(61,98,45,14),USE(?Change:3),HIDE
                           BUTTON('&Delete'),AT(110,98,45,14),USE(?Delete:3),HIDE
                         END
                       END
                       BUTTON('OK'),AT(17,120,45,14),USE(?OK),DEFAULT
                       BUTTON('Cancel'),AT(66,120,45,14),USE(?Cancel)
                       BUTTON('Help'),AT(115,120,45,14),USE(?Help),STD(STD:Help)
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
  IF LocalRequest = InsertRecord
    ?EnrollmentTab{PROP:Hide} = TRUE
  END
  CASE LocalRequest
  OF ViewRecord
    ActionMessage = 'View Record'
  OF InsertRecord
    CLEAR(Save:LastInsertedPosition)
    ActionMessage = 'Adding a Classes Record'
  OF ChangeRecord
    ActionMessage = 'Changing a Classes Record'
  OF DeleteRecord
  END
  QuickWindow{PROP:Text} = ActionMessage
  IF LocalRequest <> ViewRecord THEN
    ENABLE(TBarBrwHistory)
  END
  ACCEPT
    CASE EVENT()
    OF EVENT:AlertKey
      IF KEYCODE() = 734 AND LocalRequest <> ViewRecord THEN
        DO HistoryField
      END
    OF EVENT:CloseDown
      DO ClosingWindow
      IF Update::Reloop THEN CYCLE.
    OF EVENT:CloseWindow
      !Close drag thread
      IF SelectCoursesThread
        POST(EVENT:CloseWindow,,SelectCoursesThread)
      END
      !Close drag thread
      IF SelectTeachersThread
        POST(EVENT:CloseWindow,,SelectTeachersThread)
        SelectTeachersThread = 0
      END
      DO ClosingWindow
      IF Update::Reloop THEN CYCLE.
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
    OF EVENT:Moved
      GETPOSITION(0,WindowXPos,WindowYPos)
    OF EVENT:OpenWindow
      DO BRW2::AssignButtons
      DO FORM::AssignButtons
      IF NOT WindowInitialized
        DO InitializeWindow
        WindowInitialized = True
      END
      SELECT(?CLA:ClassNumber:Prompt)
    OF EVENT:Sized
      POST(EVENT:DoResize,0,THREAD())
    OF Event:Rejected
      BEEP
      DISPLAY(?)
      SELECT(?)
    ELSE
      IF ToolBarMode=BrowseMode THEN
        DO ListBoxDispatch
      END
      IF ToolBarMode = BrowseMode THEN
        DO UpdateDispatch
      END
      IF LocalRequest <> ViewRecord THEN
        IF ACCEPTED() = TbarBrwHistory
          DO HistoryField
        END
      END
      IF EVENT() = EVENT:Completed
        History::CLA:Record = CLA:Record
        CASE LocalRequest
        OF InsertRecord
          ADD(Classes)
          CASE ERRORCODE()
          OF NoError
            IF StandardWarning(Warn:NewRecordAdded) = Button:Yes
              Save:LastInsertedPosition = POSITION(Classes)
              DO PrimeFields
              DISPLAY
              SELECT(?CLA:ClassNumber:Prompt)
              CYCLE
            ELSE
              LocalResponse = RequestCompleted
              POST(EVENT:CloseWindow)
            END
          OF DupKeyErr
            IF DUPLICATE(CLA:KeyClassNumber)
              IF StandardWarning(Warn:DuplicateKey,'CLA:KeyClassNumber')
                SELECT(?CLA:ClassNumber:Prompt)
                VCRRequest = VCRNone
                CYCLE
              END
            END
          ELSE
            IF StandardWarning(Warn:InsertError)
              SELECT(?CLA:ClassNumber:Prompt)
              VCRRequest = VCRNone
              CYCLE
            END
          END
        OF ChangeRecord
          LOOP
            LocalResponse = RequestCancelled
            SETCURSOR(CURSOR:Wait)
            RecordChanged = FALSE
            IF (SAV::CLA:Record <> CLA:Record) 
               RecordChanged = TRUE
            END
            IF RecordChanged THEN
              Update::Error = RIUpdate:Classes(1)
            ELSE
              Update::Error = 0
            END
            SETCURSOR()
            IF Update::Error THEN
              IF Update::Error = 1 THEN
                CASE StandardWarning(Warn:UpdateError)
                OF Button:Yes
                  CYCLE
                OF Button:No
                  POST(EVENT:CloseWindow)
                  BREAK
                END
              END
              DISPLAY
              SELECT(?CLA:ClassNumber:Prompt)
              VCRRequest = VCRNone
            ELSE
              IF RecordChanged OR VCRRequest = VCRNone THEN
                LocalResponse = RequestCompleted
              END
              POST(EVENT:CloseWindow)
            END
            BREAK
          END
        END
      END
      IF ToolbarMode = FormMode THEN
        CASE ACCEPTED()
        OF TBarBrwBottom TO TBarBrwUp
        OROF TBarBrwInsert
          VCRRequest=ACCEPTED()
          POST(EVENT:Completed)
        OF TBarBrwHelp
          PRESSKEY(F1Key)
        END
      END
    END
    CASE FIELD()
    OF ?CurrentTab
      CASE EVENT()
      OF EVENT:Accepted
        DO RefreshWindow
      OF EVENT:Selected
        DO RefreshWindow
      OF EVENT:Selecting
        DO RefreshWindow
      OF EVENT:NewSelection
          !Code to assign button control based upon current tab selection
          CASE CHOICE(?CurrentTab)
          OF 1
            DO FORM::AssignButtons
          OF 2
            DO BRW2::AssignButtons
          END
        DO RefreshWindow
      OF EVENT:TabChanging
        DO RefreshWindow
      END
    OF ?CLA:CourseNumber
      CASE EVENT()
      OF EVENT:Accepted
        !Close drag toolbox
        IF SelectCoursesThread
          POST(EVENT:CloseWindow,,SelectCoursesThread)
        END
        COU:Number = CLA:CourseNumber
        GET(Courses,COU:KeyNumber)
        IF ERRORCODE()
          IF StandardWarning(Warn:NotInFile,'CLA:CourseNumber','Courses')
            SELECT(?CLA:CourseNumber)
            QuickWindow{PROP:AcceptAll} = False
            CYCLE
          END
        END
      OF EVENT:Selected
        !Call drag toolbox
        IF OriginalRequest = InsertRecord AND NOT CLA:CourseNumber
          GLO:DropThread = THREAD()
          GLO:DropControl = ?CLA:CourseNumber
          SelectCoursesThread = START(SelectCourses)
          GLO:ThreadRef &= SelectCoursesThread
        END
      OF EVENT:AlertKey
        !Call drag toolbox
        IF NOT SelectCoursesThread
          GLO:DropThread = THREAD()
          GLO:DropControl = ?CLA:CourseNumber
          SelectCoursesThread = START(SelectCourses)
          GLO:ThreadRef &= SelectCoursesThread
        END
      OF EVENT:Drop
        !Receive dropped data and close drag toolbox
        CLA:CourseNumber = DROPID()
        DISPLAY
        POST(EVENT:CloseWindow,,SelectCoursesThread)
        SelectCoursesThread = 0
        DO RefreshWindow
        PRESSKEY(TabKey)
        PRESSKEY(TabKey)
      END
    OF ?Button7
      CASE EVENT()
      OF EVENT:Accepted
        !Call drag toolbox
        POST(EVENT:AlertKey,?CLA:CourseNumber)
        DO SyncWindow
      END
    OF ?CLA:TeacherNumber
      CASE EVENT()
      OF EVENT:Accepted
        !Close drag toolbox
        IF SelectTeachersThread
          POST(EVENT:CloseWindow,,SelectTeachersThread)
          SelectTeachersThread = 0
        END
        TEA:Number = CLA:TeacherNumber
        GET(Teachers,TEA:KeyTeacherNumber)
        IF ERRORCODE()
          IF StandardWarning(Warn:NotInFile,'CLA:TeacherNumber','Teachers')
            SELECT(?CLA:TeacherNumber)
            QuickWindow{PROP:AcceptAll} = False
            CYCLE
          END
        END
      OF EVENT:Selected
        !Call drag toolbox
        IF OriginalRequest = InsertRecord AND NOT CLA:TeacherNumber
          GLO:DropThread = THREAD()
          GLO:DropControl = ?CLA:TeacherNumber
          SelectTeachersThread = START(SelectTeachers)
          GLO:ThreadRef &= SelectTeachersThread
        END
      OF EVENT:AlertKey
        !Call drag toolbox
        IF NOT SelectTeachersThread
          GLO:DropThread = THREAD()
          GLO:DropControl = ?CLA:TeacherNumber
          SelectTeachersThread = START(SelectTeachers)
          GLO:ThreadRef &= SelectTeachersThread
        END
      OF EVENT:Drop
        CLA:TeacherNumber = DROPID()
        DISPLAY
        POST(EVENT:CloseWindow,,SelectTeachersThread)
        SelectTeachersThread = 0
        DO RefreshWindow
        PRESSKEY(TabKey)
        PRESSKEY(TabKey)
      END
    OF ?Button7:2
      CASE EVENT()
      OF EVENT:Accepted
        !Call drag toolbox
        POST(EVENT:AlertKey,?CLA:TeacherNumber)
        DO SyncWindow
      END
    OF ?Browse:2
      CASE EVENT()
      OF EVENT:NewSelection
        DO BRW2::NewSelection
      OF EVENT:ScrollUp
        DO BRW2::ProcessScroll
      OF EVENT:ScrollDown
        DO BRW2::ProcessScroll
      OF EVENT:PageUp
        DO BRW2::ProcessScroll
      OF EVENT:PageDown
        DO BRW2::ProcessScroll
      OF EVENT:ScrollTop
        DO BRW2::ProcessScroll
      OF EVENT:ScrollBottom
        DO BRW2::ProcessScroll
      OF EVENT:ScrollDrag
        DO BRW2::ScrollDrag
      OF EVENT:AlertKey
        DO BRW2::AlertKey
      END
    OF ?Insert:3
      CASE EVENT()
      OF EVENT:Accepted
        DO SyncWindow
        DO BRW2::ButtonInsert
      END
    OF ?Change:3
      CASE EVENT()
      OF EVENT:Accepted
        DO SyncWindow
        DO BRW2::ButtonChange
      END
    OF ?Delete:3
      CASE EVENT()
      OF EVENT:Accepted
        DO SyncWindow
        DO BRW2::ButtonDelete
      END
    OF ?OK
      CASE EVENT()
      OF EVENT:Accepted
        DO SyncWindow
        IF LocalRequest <> ViewRecord THEN
          IF OriginalRequest = ChangeRecord OR OriginalRequest = InsertRecord
            SELECT()
          ELSE
            POST(EVENT:Completed)
          END
        END
      END
    OF ?Cancel
      CASE EVENT()
      OF EVENT:Accepted
        DO SyncWindow
        LocalResponse = RequestCancelled
        VCRRequest = VCRNone
        POST(EVENT:CloseWindow)
      END
    END
  END
  DO ProcedureReturn
!---------------------------------------------------------------------------
PrepareProcedure ROUTINE
  IF Classes::Used = 0
    CheckOpen(Classes,1)
  END
  Classes::Used += 1
  IF Courses::Used = 0
    CheckOpen(Courses,1)
  END
  Courses::Used += 1
  IF Enrollment::Used = 0
    CheckOpen(Enrollment,1)
  END
  Enrollment::Used += 1
  IF Students::Used = 0
    CheckOpen(Students,1)
  END
  Students::Used += 1
  IF Teachers::Used = 0
    CheckOpen(Teachers,1)
  END
  Teachers::Used += 1
  FilesOpened = TRUE
  DO BindFields
  RISnap:Classes
  SAV::CLA:Record = CLA:Record
  IF LocalRequest = InsertRecord
    LocalResponse = RequestCompleted
    DO PrimeFields
    IF LocalResponse = RequestCancelled
      DO ProcedureReturn
    END
    LocalResponse = RequestCancelled
  END
  IF LocalRequest = DeleteRecord
    IF StandardWarning(Warn:StandardDelete) = Button:OK
      LOOP
        LocalResponse = RequestCancelled
        SETCURSOR(CURSOR:Wait)
        IF RIDelete:Classes()
          SETCURSOR()
          CASE StandardWarning(Warn:DeleteError)
          OF Button:Yes
            CYCLE
          OF Button:No OROF Button:Cancel
            BREAK
          END
        ELSE
          SETCURSOR()
          LocalResponse = RequestCompleted
        END
        BREAK
      END
    END
    DO ProcedureReturn
  END
  OPEN(QuickWindow)
  WindowOpened=True
  BRW2::AddQueue = True
  BRW2::RecordCount = 0
  IF WindowPosInit THEN
    SETPOSITION(0,WindowXPos,WindowYPos)
  ELSE
    GETPOSITION(0,WindowXPos,WindowYPos)
    WindowPosInit=True
  END
  Do DefineListboxStyle
  ?Browse:2{PROP:Alrt,252} = MouseLeft2
  ?Browse:2{PROP:Alrt,255} = AppsKey
  ?Browse:2{PROP:Alrt,253} = MouseRight
  ?Browse:2{PROP:Alrt,255} = InsertKey
  ?Browse:2{PROP:Alrt,254} = DeleteKey
  ?Browse:2{PROP:Alrt,253} = CtrlEnter
  ?Browse:2{PROP:Alrt,252} = MouseLeft2
  ?CLA:ClassNumber{PROP:Alrt,255} = 734
  ?CLA:CourseNumber{PROP:Alrt,255} = 734
  ?CLA:TeacherNumber{PROP:Alrt,255} = 734
  ?CLA:RoomNumber{PROP:Alrt,255} = 734
  ?CLA:ScheduledTime{PROP:Alrt,255} = 734

!---------------------------------------------------------------------------
BindFields ROUTINE
  BIND(CLA:RECORD)
  BIND(COU:RECORD)
  BIND('COU:CompleteDescription',COU:CompleteDescription)
  BIND(ENR:RECORD)
  BIND(STU:RECORD)
  BIND(TEA:RECORD)
  BIND('BRW2::Sort1:Reset:CLA:ClassNumber',BRW2::Sort1:Reset:CLA:ClassNumber) ! Added by: BrowseBox(Clarion)
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
    Classes::Used -= 1
    IF Classes::Used = 0 THEN CLOSE(Classes).
    Courses::Used -= 1
    IF Courses::Used = 0 THEN CLOSE(Courses).
    Enrollment::Used -= 1
    IF Enrollment::Used = 0 THEN CLOSE(Enrollment).
    Students::Used -= 1
    IF Students::Used = 0 THEN CLOSE(Students).
    Teachers::Used -= 1
    IF Teachers::Used = 0 THEN CLOSE(Teachers).
  END
  IF WindowOpened
    CLOSE(QuickWindow)
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
  IF QuickWindow{Prop:AcceptAll} THEN EXIT.
  TEA:Number = CLA:TeacherNumber                           ! Assign linking field value
  GET(Teachers,TEA:KeyTeacherNumber)                       ! Lookup record
  IF ERRORCODE()
    CLEAR(TEA:Record)                                      ! Clear record if unsuccessful
  END
  COU:Number = CLA:CourseNumber                            ! Assign linking field value
  GET(Courses,COU:KeyNumber)                               ! Lookup record
  IF ERRORCODE()
    CLEAR(COU:Record)                                      ! Clear record if unsuccessful
    CLEAR(COU:CompleteDescription)
  END
  Do LookupRelated
  DO BRW2::SelectSort
  ?Browse:2{Prop:VScrollPos} = BRW2::CurrentScroll
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
  DO BRW2::GetRecord
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
BRW2::AssignButtons ROUTINE
  CLEAR(BrowseButtons)
  BrowseButtons.ListBox = ?Browse:2
  BrowseButtons.InsertButton = ?Insert:3
  BrowseButtons.ChangeButton = ?Change:3
  BrowseButtons.DeleteButton = ?Delete:3
  DO DisplayBrowseToolbar
!----------------------------------------------------------------------
BRW2::SelectSort ROUTINE
!|
!| This routine is called during the RefreshWindow ROUTINE present in every window procedure.
!| The purpose of this routine is to make certain that the BrowseBox is always current with your
!| user's selections. This routine...
!|
!| 1. Checks to see if any of your specified sort-order conditions are met, and if so, changes the sort order.
!| 2. If no sort order change is necessary, this routine checks to see if any of your Reset Fields has changed.
!| 3. If the sort order has changed, or if a reset field has changed, or if the ForceRefresh flag is set...
!|    a. The current record is retrieved from the disk.
!|    b. If the BrowseBox is accessed for the first time, and the Browse has been called to select a record,
!|       the page containing the current record is loaded.
!|    c. If the BrowseBox is accessed for the first time, and the Browse has not been called to select a
!|       record, the first page of information is loaded.
!|    d. If the BrowseBox is not being accessed for the first time, and the Browse sort order has changed, the
!|       new "first" page of information is loaded.
!|    e. If the BrowseBox is not being accessed for the first time, and the Browse sort order hasn't changes,
!|       the page containing the current record is reloaded.
!|    f. The record buffer is refilled from the currently highlighted BrowseBox item.
!|    f. The BrowseBox is reinitialized (BRW2::InitializeBrowse ROUTINE).
!| 4. If step 3 is not necessary, the record buffer is refilled from the currently highlighted BrowseBox item.
!|
  BRW2::LastSortOrder = BRW2::SortOrder
  BRW2::Changed = False
  IF BRW2::SortOrder = 0
    BRW2::SortOrder = 1
  END
  IF BRW2::SortOrder = BRW2::LastSortOrder
    CASE BRW2::SortOrder
    OF 1
      IF BRW2::Sort1:Reset:CLA:ClassNumber <> CLA:ClassNumber
        BRW2::Changed = True
      END
    END
  ELSE
  END
  IF BRW2::SortOrder <> BRW2::LastSortOrder OR BRW2::Changed OR ForceRefresh OR (BRW2::LoadPending AND ?Browse:2{PROP:VISIBLE})
    CASE BRW2::SortOrder
    OF 1
      BRW2::Sort1:Reset:CLA:ClassNumber = CLA:ClassNumber
    END
    DO BRW2::GetRecord
    DO BRW2::Reset
    IF BRW2::LastSortOrder = 0
      IF LocalRequest = SelectRecord
        BRW2::LocateMode = LocateOnValue
        DO BRW2::LocateRecord
      ELSE
        FREE(Queue:Browse:2)
        BRW2::RefreshMode = RefreshOnTop
        DO BRW2::RefreshPage
        DO BRW2::PostNewSelection
      END
    ELSE
      IF BRW2::Changed
        FREE(Queue:Browse:2)
        BRW2::RefreshMode = RefreshOnTop
        DO BRW2::RefreshPage
        DO BRW2::PostNewSelection
      ELSE
        BRW2::LocateMode = LocateOnValue
        DO BRW2::LocateRecord
      END
    END
    IF BRW2::RecordCount
      GET(Queue:Browse:2,BRW2::CurrentChoice)
      DO BRW2::FillBuffer
    END
    DO BRW2::InitializeBrowse
  ELSE
    IF BRW2::RecordCount
      GET(Queue:Browse:2,BRW2::CurrentChoice)
      DO BRW2::FillBuffer
    END
  END
!----------------------------------------------------------------------
BRW2::InitializeBrowse ROUTINE
!|
!| This routine initializes the BrowseBox control template. This routine is called when...
!|
!| The BrowseBox sort order has changed. This includes the first time the BrowseBox is accessed.
!| The BrowseBox returns from a record update.
!|
!| This routine performs two main functions.
!|   1. Computes all BrowseBox totals. All records that satisfy the current selection criteria
!|      are read, and totals computed. If no totals are present, this section is not generated,
!|      and may not be present in the code below.
!|   2. Calculates any runtime scrollbar positions. Again, if runtime scrollbars are not used,
!|      the code for runtime scrollbar computation will not be present.
!|
  IF NOT BRW2::ActiveInvisible THEN
     IF NOT ?Browse:2{PROP:Visible} THEN
        BRW2::LoadPending = True
        EXIT
     END
  END
  DO BRW2::Reset
  PREVIOUS(BRW2::View:Browse)
  IF ERRORCODE()
    IF ERRORCODE() = BadRecErr
      DO BRW2::RestoreResetValues
    ELSE
      StandardWarning(Warn:RecordFetchError,'Enrollment')
      POST(Event:CloseWindow)
    END
    EXIT
  END
  CASE BRW2::SortOrder
  OF 1
    BRW2::Sort1:HighValue = ENR:StudentNumber
  END
  DO BRW2::Reset
  NEXT(BRW2::View:Browse)
  IF ERRORCODE()
    IF ERRORCODE() = BadRecErr
      DO BRW2::RestoreResetValues
    ELSE
      StandardWarning(Warn:RecordFetchError,'Enrollment')
      POST(Event:CloseWindow)
    END
    EXIT
  END
  CASE BRW2::SortOrder
  OF 1
    BRW2::Sort1:LowValue = ENR:StudentNumber
    SetupRealStops(BRW2::Sort1:LowValue,BRW2::Sort1:HighValue)
    LOOP BRW2::ScrollRecordCount = 1 TO 100
      BRW2::Sort1:KeyDistribution[BRW2::ScrollRecordCount] = NextRealStop()
    END
  END
!----------------------------------------------------------------------
BRW2::FillBuffer ROUTINE
!|
!| This routine fills the record buffer from the BrowseBox queue. This gives the appearance
!| that the record is "fresh" from the disk, without the disk access required.
!|
  ENR:StudentNumber = BRW2::ENR:StudentNumber
  STU:LastName = BRW2::STU:LastName
  STU:FirstName = BRW2::STU:FirstName
  ENR:MidtermExam = BRW2::ENR:MidtermExam
  ENR:FinalExam = BRW2::ENR:FinalExam
  ENR:TermPaper = BRW2::ENR:TermPaper
  ENR:ClassNumber = BRW2::ENR:ClassNumber
  STU:Number = BRW2::STU:Number
!----------------------------------------------------------------------
BRW2::FillQueue ROUTINE
!|
!| This routine is used to fill the BrowseBox QUEUE from several sources.
!|
!| First, all Format Browse formulae are processed.
!|
!| Next, each field of the BrowseBox is processed. For each field...
!|
!|    The value of the field is placed in the BrowseBox queue.
!|
!| Finally, the POSITION of the current VIEW record is added to the QUEUE
!|
  BRW2::ENR:StudentNumber = ENR:StudentNumber
  BRW2::STU:LastName = STU:LastName
  BRW2::STU:FirstName = STU:FirstName
  BRW2::ENR:MidtermExam = ENR:MidtermExam
  BRW2::ENR:FinalExam = ENR:FinalExam
  BRW2::ENR:TermPaper = ENR:TermPaper
  BRW2::ENR:ClassNumber = ENR:ClassNumber
  BRW2::STU:Number = STU:Number
  BRW2::Position = POSITION(BRW2::View:Browse)
!----------------------------------------------------------------------
BRW2::PostNewSelection ROUTINE
!|
!| This routine is used to post the NewSelection EVENT to the window. Because we only want this
!| EVENT processed once, and becuase there are several routines that need to initiate a NewSelection
!| EVENT, we keep a flag that tells us if the EVENT is already waiting to be processed. The EVENT is
!| only POSTed if this flag is false.
!|
  IF NOT BRW2::NewSelectPosted
    BRW2::NewSelectPosted = True
    POST(EVENT:NewSelection,?Browse:2)
  END
!----------------------------------------------------------------------
BRW2::NewSelection ROUTINE
!|
!| This routine performs any window bookkeeping necessary when a new record is selected in the
!| BrowseBox.
!| 1. If the new selection is made with the right mouse button, the popup menu (if applicable) is
!|    processed.
!| 2. The current record is retrieved into the buffer using the BRW2::FillBuffer ROUTINE.
!|    After this, the current vertical scrollbar position is computed, and the scrollbar positioned.
!|
  IF NOT BRW2::ActiveInvisible THEN
     IF NOT ?Browse:2{PROP:Visible} THEN
        BRW2::LoadPending = True
        EXIT
     END
  END
  BRW2::NewSelectPosted = False
  IF KEYCODE() = MouseRight OR KEYCODE() = AppsKey
     SETKEYCODE(0)
    BRW2::PopupText = ''
    IF BRW2::RecordCount
      IF BRW2::PopupText
        BRW2::PopupText = '&Insert|&Change|&Delete|-|' & BRW2::PopupText
      ELSE
        BRW2::PopupText = '&Insert|&Change|&Delete'
      END
    ELSE
      IF BRW2::PopupText
        BRW2::PopupText = '&Insert|~&Change|~&Delete|-|' & BRW2::PopupText
      ELSE
        BRW2::PopupText = '&Insert|~&Change|~&Delete'
      END
    END
    EXECUTE(POPUP(BRW2::PopupText))
      POST(EVENT:Accepted,?Insert:3)
      POST(EVENT:Accepted,?Change:3)
      POST(EVENT:Accepted,?Delete:3)
    END
  ELSIF BRW2::RecordCount
    BRW2::CurrentChoice = CHOICE(?Browse:2)
    GET(Queue:Browse:2,BRW2::CurrentChoice)
    DO BRW2::FillBuffer
    IF BRW2::RecordCount = ?Browse:2{PROP:Items}
      IF NOT ?Browse:2{PROP:VScroll}
        ?Browse:2{PROP:VScroll} = True
      END
      CASE BRW2::SortOrder
      OF 1
        LOOP BRW2::CurrentScroll = 1 TO 100
          IF BRW2::Sort1:KeyDistribution[BRW2::CurrentScroll] => ENR:StudentNumber
            IF BRW2::CurrentScroll <= 1
              BRW2::CurrentScroll = 0
            ELSIF BRW2::CurrentScroll = 100
              BRW2::CurrentScroll = 100
            ELSE
            END
            BREAK
          END
        END
      END
    ELSE
      IF ?Browse:2{PROP:VScroll}
        ?Browse:2{PROP:VScroll} = False
      END
    END
    DO RefreshWindow
  END
!---------------------------------------------------------------------
BRW2::ProcessScroll ROUTINE
!|
!| This routine processes any of the six scrolling EVENTs handled by the BrowseBox.
!| If one record is to be scrolled, the ROUTINE BRW2::ScrollOne is called.
!| If a page of records is to be scrolled, the ROUTINE BRW2::ScrollPage is called.
!| If the first or last page is to be displayed, the ROUTINE BRW2::ScrollEnd is called.
!|
!| If an incremental locator is in use, the value of that locator is cleared.
!| Finally, if a Fixed Thumb vertical scroll bar is used, the thumb is positioned.
!|
  IF BRW2::RecordCount
    BRW2::CurrentEvent = EVENT()
    CASE BRW2::CurrentEvent
    OF EVENT:ScrollUp OROF EVENT:ScrollDown
      DO BRW2::ScrollOne
    OF EVENT:PageUp OROF EVENT:PageDown
      DO BRW2::ScrollPage
    OF EVENT:ScrollTop OROF EVENT:ScrollBottom
      DO BRW2::ScrollEnd
    END
    ?Browse:2{PROP:SelStart} = BRW2::CurrentChoice
    DO BRW2::PostNewSelection
  END
!----------------------------------------------------------------------
BRW2::ScrollOne ROUTINE
!|
!| This routine is used to scroll a single record on the BrowseBox. Since the BrowseBox is an IMM
!| listbox, all scrolling must be handled in code. When called, this routine...
!|
!| 1. Sees if scrolling in the intended direction will cause the listbox display to shift. If not,
!|    the routine moves the list box cursor and exits.
!| 2. Calls BRW2::FillRecord to retrieve one record in the direction required.
!|
  IF BRW2::CurrentEvent = EVENT:ScrollUp AND BRW2::CurrentChoice > 1
    BRW2::CurrentChoice -= 1
    EXIT
  ELSIF BRW2::CurrentEvent = EVENT:ScrollDown AND BRW2::CurrentChoice < BRW2::RecordCount
    BRW2::CurrentChoice += 1
    EXIT
  END
  BRW2::ItemsToFill = 1
  BRW2::FillDirection = BRW2::CurrentEvent - 2
  DO BRW2::FillRecord
!----------------------------------------------------------------------
BRW2::ScrollPage ROUTINE
!|
!| This routine is used to scroll a single page of records on the BrowseBox. Since the BrowseBox is
!| an IMM listbox, all scrolling must be handled in code. When called, this routine...
!|
!| 1. Calls BRW2::FillRecord to retrieve one page of records in the direction required.
!| 2. If BRW2::FillRecord doesn't fill a page (BRW2::ItemsToFill > 0), then
!|    the list-box cursor ia shifted.
!|
  BRW2::ItemsToFill = ?Browse:2{PROP:Items}
  BRW2::FillDirection = BRW2::CurrentEvent - 4
  DO BRW2::FillRecord                           ! Fill with next read(s)
  IF BRW2::ItemsToFill
    IF BRW2::CurrentEvent = EVENT:PageUp
      BRW2::CurrentChoice -= BRW2::ItemsToFill
      IF BRW2::CurrentChoice < 1
        BRW2::CurrentChoice = 1
      END
    ELSE
      BRW2::CurrentChoice += BRW2::ItemsToFill
      IF BRW2::CurrentChoice > BRW2::RecordCount
        BRW2::CurrentChoice = BRW2::RecordCount
      END
    END
  END
!----------------------------------------------------------------------
BRW2::ScrollEnd ROUTINE
!|
!| This routine is used to load the first or last page of the displayable set of records.
!| Since the BrowseBox is an IMM listbox, all scrolling must be handled in code. When called,
!| this routine...
!|
!| 1. Resets the BrowseBox VIEW to insure that it reads from the end of the current sort order.
!| 2. Calls BRW2::FillRecord to retrieve one page of records.
!| 3. Selects the record that represents the end of the view. That is, if the first page was loaded,
!|    the first record is highlighted. If the last was loaded, the last record is highlighted.
!|
  FREE(Queue:Browse:2)
  BRW2::RecordCount = 0
  DO BRW2::Reset
  BRW2::ItemsToFill = ?Browse:2{PROP:Items}
  IF BRW2::CurrentEvent = EVENT:ScrollTop
    BRW2::FillDirection = FillForward
  ELSE
    BRW2::FillDirection = FillBackward
  END
  DO BRW2::FillRecord                           ! Fill with next read(s)
  IF BRW2::CurrentEvent = EVENT:ScrollTop
    BRW2::CurrentChoice = 1
  ELSE
    BRW2::CurrentChoice = BRW2::RecordCount
  END
!----------------------------------------------------------------------
BRW2::AlertKey ROUTINE
!|
!| This routine processes any KEYCODEs experienced by the BrowseBox.
!| NOTE: The cursor movement keys are not processed as KEYCODEs. They are processed as the
!|       appropriate BrowseBox scrolling and selection EVENTs.
!| This routine includes handling for double-click. Actually, this handling is in the form of
!| EMBEDs, which are filled by child-control templates.
!| This routine also includes the BrowseBox's locator handling.
!| After a value is entered for locating, this routine sets BRW2::LocateMode to a value
!| of 2 -- EQUATEd to LocateOnValue -- and calls the routine BRW2::LocateRecord.
!|
  IF BRW2::RecordCount
    CASE KEYCODE()                                         ! What keycode was hit
    OF AppsKey
    OROF MouseRight
       IF ?Browse:2{PROPLIST:MouseDownRow}>0
         ?Browse:2{PROP:Selected} = ?Browse:2{PROPLIST:MouseDownRow}
         BRW2::CurrentChoice = CHOICE(?Browse:2)
       END
       DO BRW2::NewSelection
    OF MouseLeft2
      POST(EVENT:Accepted,?Change:3)
      DO BRW2::FillBuffer
    OF InsertKey
      POST(EVENT:Accepted,?Insert:3)
    OF DeleteKey
      POST(EVENT:Accepted,?Delete:3)
    OF CtrlEnter
      POST(EVENT:Accepted,?Change:3)
    END                                                    ! END (What keycode was hit)
  ELSE
    CASE KEYCODE()                                         ! What keycode was hit
    OF AppsKey
    OROF MouseRight
       DO BRW2::NewSelection
    OF InsertKey
      POST(EVENT:Accepted,?Insert:3)
    ELSE                                                   ! ELSE (What keycode was hit)
      CASE BRW2::SortOrder
      OF 1
      END
    END
  END
  DO BRW2::PostNewSelection
!----------------------------------------------------------------------
BRW2::ScrollDrag ROUTINE
!|
!| This routine processes the Vertical Scroll Bar arrays to find the free key field value
!| that corresponds to the current scroll bar position.
!|
!| After the scroll position is computed, and the scroll value found, this routine sets
!| BRW2::LocateMode to that scroll value of 2 -- EQUATEd to LocateOnValue --
!| and calls the routine BRW2::LocateRecord.
!|
  IF ?Browse:2{PROP:VScrollPos} <= 1
    POST(EVENT:ScrollTop,?Browse:2)
  ELSIF ?Browse:2{PROP:VScrollPos} = 100
    POST(EVENT:ScrollBottom,?Browse:2)
  ELSE
    CASE BRW2::SortOrder
    OF 1
      ENR:StudentNumber = BRW2::Sort1:KeyDistribution[?Browse:2{PROP:VScrollPos}]
      BRW2::LocateMode = LocateOnValue
      DO BRW2::LocateRecord
    END
  END
!----------------------------------------------------------------------
BRW2::FillRecord ROUTINE
!|
!| This routine is used to retrieve a number of records from the VIEW. The number of records
!| retrieved is held in the variable BRW2::ItemsToFill. If more than one record is
!| to be retrieved, QuickScan is used to minimize reads from the disk.
!|
!| If records exist in the queue (in other words, if the browse has been used before), the record
!| at the appropriate end of the list box is retrieved, and the VIEW is reset to read starting
!| at that record.
!|
!| Next, the VIEW is accessed to retrieve BRW2::ItemsToFill records. Normally, this will
!| result in BRW2::ItemsToFill records being read from the VIEW, but if custom filtering
!| or range limiting is used (via the BRW2::ValidateRecord routine) then any number of records
!| might be read.
!|
!| For each good record, if BRW2::AddQueue is true, the queue is filled using the BRW2::FillQueue
!| routine. The record is then added to the queue. If adding this record causes the BrowseBox queue
!| to contain more records than can be displayed, the record at the opposite end of the queue is
!| deleted.
!|
!| The only time BRW2::AddQueue is false is when the BRW2::LocateRecord routine needs to
!| get the closest record to its record to be located. At this time, the record doesn't need to be
!| added to the queue, so it isn't.
!|
  IF BRW2::RecordCount
    IF BRW2::FillDirection = FillForward
      GET(Queue:Browse:2,BRW2::RecordCount)                ! Get the first queue item
    ELSE
      GET(Queue:Browse:2,1)                                ! Get the first queue item
    END
    RESET(BRW2::View:Browse,BRW2::Position)                ! Reset for sequential processing
    BRW2::SkipFirst = TRUE
  ELSE
    BRW2::SkipFirst = FALSE
  END
  LOOP WHILE BRW2::ItemsToFill
    IF BRW2::View:Browse{PROP:IPRequestCount} = 0
       BRW2::View:Browse{PROP:IPRequestCount} = BRW2::ItemsToFill
    END
    IF BRW2::FillDirection = FillForward
      NEXT(BRW2::View:Browse)
    ELSE
      PREVIOUS(BRW2::View:Browse)
    END
    IF ERRORCODE()
      IF ERRORCODE() = BadRecErr
        DO BRW2::RestoreResetValues
        BREAK
      ELSE
        StandardWarning(Warn:RecordFetchError,'Enrollment')
        POST(EVENT:CloseWindow)
        EXIT
      END
    END
    IF BRW2::SkipFirst
       BRW2::SkipFirst = FALSE
       IF POSITION(BRW2::View:Browse) = BRW2::Position
          CYCLE
       END
    END
    IF BRW2::AddQueue
      IF BRW2::RecordCount = ?Browse:2{PROP:Items}
        IF BRW2::FillDirection = FillForward
          GET(Queue:Browse:2,1)                            ! Get the first queue item
        ELSE
          GET(Queue:Browse:2,BRW2::RecordCount)            ! Get the first queue item
        END
        DELETE(Queue:Browse:2)
        BRW2::RecordCount -= 1
      END
      DO BRW2::FillQueue
      IF BRW2::FillDirection = FillForward
        ADD(Queue:Browse:2)
      ELSE
        ADD(Queue:Browse:2,1)
      END
      BRW2::RecordCount += 1
    END
    BRW2::ItemsToFill -= 1
  END
  BRW2::AddQueue = True
  EXIT
!----------------------------------------------------------------------
BRW2::LocateRecord ROUTINE
!|
!| This routine is used to find a record in the VIEW, and to display that record
!| in the BrowseBox.
!|
!| This routine has three different modes of operation, which are invoked based on
!| the setting of BRW2::LocateMode. These modes are...
!|
!|   LocateOnPosition (1) - This mode is still supported for 1.5 compatability. This mode
!|                          is the same as LocateOnEdit.
!|   LocateOnValue    (2) - The values of the current sort order key are used. This mode
!|                          used for Locators and when the BrowseBox is called to select
!|                          a record.
!|   LocateOnEdit     (3) - The current record of the VIEW is used. This mode assumes
!|                          that there is an active VIEW record. This mode is used when
!|                          the sort order of the BrowseBox has changed
!|
!| If an appropriate record has been located, the BRW2::RefreshPage routine is
!| called to load the page containing the located record.
!|
!| If an appropriate record is not locate, the last page of the BrowseBox is loaded.
!|
  IF BRW2::LocateMode = LocateOnPosition
    BRW2::LocateMode = LocateOnEdit
  END
  CLOSE(BRW2::View:Browse)
  CASE BRW2::SortOrder
  OF 1
    IF BRW2::UsingAdditionalSortOrder THEN
       BRW2::UsingAdditionalSortOrder = False
       BRW2::View:Browse{PROP:Order} = '+ENR:ClassNumber,+ENR:StudentNumber'
    END
    IF BRW2::LocateMode = LocateOnEdit
      BRW2::HighlightedPosition = POSITION(ENR:SeqStu)
      RESET(ENR:SeqStu,BRW2::HighlightedPosition)
    ELSE
      ENR:ClassNumber = CLA:ClassNumber
      SET(ENR:SeqStu,ENR:SeqStu)
    END
    BRW2::View:Browse{Prop:Filter} = 'ENR:ClassNumber = BRW2::Sort1:Reset:CLA:ClassNumber'
  END
  IF ERRORCODE()
    StandardWarning(Warn:ViewOpenError)
  END
  OPEN(BRW2::View:Browse)
  IF ERRORCODE()
    StandardWarning(Warn:ViewOpenError)
  END
  IF BRW2::UsingAdditionalSortOrder = True
    CASE BRW2::SortOrder
    OF 1
       SET(BRW2::View:Browse,2)
    END
  END
  FREE(Queue:Browse:2)
  BRW2::RecordCount = 0
  BRW2::ItemsToFill = 1
  BRW2::FillDirection = FillForward                        ! Fill with next read(s)
  BRW2::AddQueue = False
  DO BRW2::FillRecord                                      ! Fill with next read(s)
  BRW2::AddQueue = True
  IF BRW2::ItemsToFill
    BRW2::RefreshMode = RefreshOnBottom
    DO BRW2::RefreshPage
  ELSE
    BRW2::RefreshMode = RefreshOnPosition
    DO BRW2::RefreshPage
  END
  DO BRW2::PostNewSelection
  BRW2::LocateMode = 0
  EXIT
!----------------------------------------------------------------------
BRW2::RefreshPage ROUTINE
!|
!| This routine is used to load a single page of the BrowseBox.
!|
!| If this routine is called with a BRW2::RefreshMode of RefreshOnPosition,
!| the active VIEW record is loaded at the top of the page. Otherwise, if there are
!| records in the browse queue (Queue:Browse:2), then the current page is reloaded, and the
!| currently selected item remains selected.
!|
  IF NOT BRW2::ActiveInvisible THEN
     IF NOT ?Browse:2{PROP:Visible} THEN
        BRW2::LoadPending = True
        EXIT
     END
  END
  SETCURSOR(Cursor:Wait)
  IF BRW2::RefreshMode = RefreshOnPosition
    BRW2::HighlightedPosition = POSITION(BRW2::View:Browse)
    RESET(BRW2::View:Browse,BRW2::HighlightedPosition)
    BRW2::RefreshMode = RefreshOnTop
  ELSIF RECORDS(Queue:Browse:2)
    GET(Queue:Browse:2,BRW2::CurrentChoice)
    IF ERRORCODE()
      GET(Queue:Browse:2,RECORDS(Queue:Browse:2))
    END
    BRW2::HighlightedPosition = BRW2::Position
    GET(Queue:Browse:2,1)
    RESET(BRW2::View:Browse,BRW2::Position)
    BRW2::RefreshMode = RefreshOnCurrent
  ELSE
    BRW2::HighlightedPosition = ''
    DO BRW2::Reset
  END
  FREE(Queue:Browse:2)
  BRW2::RecordCount = 0
  BRW2::ItemsToFill = ?Browse:2{PROP:Items}
  IF BRW2::RefreshMode = RefreshOnBottom
    BRW2::FillDirection = FillBackward
  ELSE
    BRW2::FillDirection = FillForward
  END
  DO BRW2::FillRecord                                      ! Fill with next read(s)
  IF BRW2::HighlightedPosition
    IF BRW2::ItemsToFill
      IF NOT BRW2::RecordCount
        DO BRW2::Reset
      END
      IF BRW2::RefreshMode = RefreshOnBottom
        BRW2::FillDirection = FillForward
      ELSE
        BRW2::FillDirection = FillBackward
      END
      DO BRW2::FillRecord
    END
  END
  IF BRW2::RecordCount
    IF BRW2::HighlightedPosition
      LOOP BRW2::CurrentChoice = 1 TO BRW2::RecordCount
        GET(Queue:Browse:2,BRW2::CurrentChoice)
        IF BRW2::Position = BRW2::HighlightedPosition THEN BREAK.
      END
      IF BRW2::CurrentChoice > BRW2::RecordCount
        BRW2::CurrentChoice = BRW2::RecordCount
      END
    ELSE
      IF BRW2::RefreshMode = RefreshOnBottom
        BRW2::CurrentChoice = RECORDS(Queue:Browse:2)
      ELSE
        BRW2::CurrentChoice = 1
      END
    END
    ?Browse:2{Prop:Selected} = BRW2::CurrentChoice
    DO BRW2::FillBuffer
    ?Change:3{PROP:Disable} = 0
    ?Delete:3{PROP:Disable} = 0
  ELSE
    CLEAR(ENR:Record)
    BRW2::CurrentChoice = 0
    ?Change:3{PROP:Disable} = 1
    ?Delete:3{PROP:Disable} = 1
  END
  SETCURSOR()
  BRW2::RefreshMode = 0
  EXIT
BRW2::Reset ROUTINE
!|
!| This routine is used to reset the VIEW used by the BrowseBox.
!|
  IF NOT BRW2::ActiveInvisible THEN
     IF NOT ?Browse:2{PROP:Visible} THEN
        BRW2::LoadPending = True
        EXIT
     END
  END
  BRW2::LoadPending = False
  CLOSE(BRW2::View:Browse)
  CASE BRW2::SortOrder
  OF 1
    IF BRW2::UsingAdditionalSortOrder THEN
       BRW2::UsingAdditionalSortOrder = False
       BRW2::View:Browse{PROP:Order} = '+ENR:ClassNumber,+ENR:StudentNumber'
    END
    ENR:ClassNumber = CLA:ClassNumber
    SET(ENR:SeqStu)
    BRW2::View:Browse{Prop:Filter} = 'ENR:ClassNumber = BRW2::Sort1:Reset:CLA:ClassNumber'
  END
  IF ERRORCODE()
    StandardWarning(Warn:ViewOpenError)
  END
  OPEN(BRW2::View:Browse)
  IF ERRORCODE()
    StandardWarning(Warn:ViewOpenError)
  END
  SET(BRW2::View:Browse)
!----------------------------------------------------------------------
BRW2::GetRecord ROUTINE
!|
!| This routine is used to retrieve the VIEW record that corresponds to a
!| chosen listbox record.
!|
  IF BRW2::RecordCount
    BRW2::CurrentChoice = CHOICE(?Browse:2)
    GET(Queue:Browse:2,BRW2::CurrentChoice)
    WATCH(BRW2::View:Browse)
    REGET(BRW2::View:Browse,BRW2::Position)
  END
!----------------------------------------------------------------------
BRW2::RestoreResetValues ROUTINE
!|
!| This routine is used to restore reset values to their saved value
!| after a bad record access from the VIEW.
!|
  CASE BRW2::SortOrder
  OF 1
    CLA:ClassNumber = BRW2::Sort1:Reset:CLA:ClassNumber
  END
!--------------------------------------------------------------------------
DisplayBrowseToolbar      ROUTINE
  ENABLE(TBarBrwBottom,TBarBrwLocate)
  TBarBrwHelp{PROP:DISABLE}=?Help{PROP:DISABLE}
  IF BrowseButtons.InsertButton THEN
    TBarBrwInsert{PROP:Disable} = BrowseButtons.InsertButton{PROP:Disable}
  END
  IF BrowseButtons.ChangeButton THEN
    TBarBrwChange{PROP:Disable} = BrowseButtons.ChangeButton{PROP:Disable}
  END
  IF BrowseButtons.DeleteButton THEN
    TBarBrwDelete{PROP:Disable} = BrowseButtons.DeleteButton{PROP:Disable}
  END
  DISABLE(TBarBrwHistory)
  ToolBarMode = BrowseMode
  TBarBrwDown{PROP:ToolTip} = 'Go to the Next Record'
  TBarBrwBottom{PROP:ToolTip} = 'Go to the Last Page'
  TBarBrwTop{PROP:ToolTip} = 'Go to the First Page'
  TBarBrwPageDown{PROP:ToolTip} = 'Go to the Next Page'
  TBarBrwPageUp{PROP:ToolTip} = 'Go to the Prior Page'
  TBarBrwDown{PROP:ToolTip} = 'Go to the Next Record'
  TBarBrwUP{PROP:ToolTip} = 'Go to the Prior Record'
  TBarBrwInsert{PROP:ToolTip} = 'Insert a new Record'
  DISPLAY(TBarBrwFirst,TBarBrwLast)
!--------------------------------------------------------------------------
ListBoxDispatch ROUTINE
  DO DisplayBrowseToolbar
  IF FOCUS() <> BrowseButtons.ListBox THEN  ! List box must have focus when on update form
    EXIT
  END
  IF ACCEPTED() THEN            !trap remote browse box control calls
    EXECUTE(ACCEPTED()-TBarBrwBottom+1)
      POST(EVENT:ScrollBottom,BrowseButtons.ListBox)
      POST(EVENT:ScrollTop,BrowseButtons.ListBox)
      POST(EVENT:PageDown,BrowseButtons.ListBox)
      POST(EVENT:PageUp,BrowseButtons.ListBox)
      POST(EVENT:ScrollDown,BrowseButtons.ListBox)
      POST(EVENT:ScrollUp,BrowseButtons.ListBox)
      POST(EVENT:Locate,BrowseButtons.ListBox)
      BEGIN                     !EXECUTE Place Holder - Ditto has no effect on a browse
      END
      PRESSKEY(F1Key)
    END
  END

UpdateDispatch ROUTINE
  DISABLE(TBarBrwDelete)
  DISABLE(TBarBrwChange)
  IF BrowseButtons.DeleteButton AND BrowseButtons.DeleteButton{PROP:Disable} = 0 THEN
    ENABLE(TBarBrwDelete)
  END
  IF BrowseButtons.ChangeButton AND BrowseButtons.ChangeButton{PROP:Disable} = 0 THEN
    ENABLE(TBarBrwChange)
  END
  IF FOCUS() <> BrowseButtons.ListBox THEN  ! List box must have focus when on update form
    EXIT
  END
  IF INRANGE(ACCEPTED(),TBarBrwInsert,TBarBrwDelete) THEN         !trap remote browse update control calls
    EXECUTE(ACCEPTED()-TBarBrwInsert+1)
      POST(EVENT:Accepted,BrowseButtons.InsertButton)
      POST(EVENT:Accepted,BrowseButtons.ChangeButton)
      POST(EVENT:Accepted,BrowseButtons.DeleteButton)
    END
  END
!----------------------------------------------------------------
BRW2::ButtonInsert ROUTINE
!|
!| This routine calls the BrowseBox's update procedure (as specified in the
!| BrowseUpdateButtons control template) to insert a new record.
!|
!| First, the primary file's record  buffer is cleared, as well as any memos
!| or BLOBs. Next, any range-limit values are restored so that the inserted
!| record defaults to being added to the current display set.
!|
!| Next, LocalRequest is set to InsertRecord, and the BRW2::CallRecord routine
!| is called. This routine performs the actual call to the update procedure.
!|
!| If the insert is successful (GlobalRequest = RequestCompleted) then the newly added
!| record is displayed in the BrowseBox, at the top of the listbox.
!|
!| If the insert is not successful, the current page of the browse is refreshed.
!|
!| Finally, The BrowseBox is re-initialized, resetting scroll bars and totals.
!|
  GET(Enrollment,0)
  CLEAR(ENR:Record,0)
  CASE BRW2::SortOrder
  OF 1
    ENR:ClassNumber = BRW2::Sort1:Reset:CLA:ClassNumber
  END
  LocalRequest = InsertRecord
  DO BRW2::CallUpdate
  IF GlobalResponse = RequestCompleted
    BRW2::LocateMode = LocateOnEdit
    DO BRW2::LocateRecord
  ELSE
    BRW2::RefreshMode = RefreshOnQueue
    DO BRW2::RefreshPage
  END
  DO BRW2::InitializeBrowse
  DO BRW2::PostNewSelection
  SELECT(?Browse:2)
  LocalRequest = OriginalRequest
  LocalResponse = RequestCancelled
  DO RefreshWindow
!----------------------------------------------------------------
BRW2::ButtonChange ROUTINE
!|
!| This routine calls the BrowseBox's update procedure (as specified in the
!| BrowseUpdateButtons control template) to change a selected record.
!|
!| Whenever a button is pressed, the first thing that happens is that the
!| SyncWindow routine is called. This routine insures that the BrowseBox's
!| VIEW corresponds to the highlighted record by calling the BRW2::GetRecord routine.
!|
!| First, LocalRequest is set to ChangeRecord, and the BRW2::CallRecord routine
!| is called. This routine performs the actual call to the update procedure.
!|
!| If the change is successful (GlobalRequest = RequestCompleted) then the newly changed
!| record is displayed in the BrowseBox.
!|
!| If the change is not successful, the current page of the browse is refreshed.
!|
!| Finally, The BrowseBox is re-initialized, resetting scroll bars and totals.
!|
  LocalRequest = ChangeRecord
  DO BRW2::CallUpdate
  IF GlobalResponse = RequestCompleted
    BRW2::LocateMode = LocateOnEdit
    DO BRW2::LocateRecord
  ELSE
    BRW2::RefreshMode = RefreshOnQueue
    DO BRW2::RefreshPage
  END
  DO BRW2::InitializeBrowse
  DO BRW2::PostNewSelection
  SELECT(?Browse:2)
  LocalRequest = OriginalRequest
  LocalResponse = RequestCancelled
  DO RefreshWindow
!----------------------------------------------------------------
BRW2::ButtonDelete ROUTINE
!|
!| This routine calls the BrowseBox's update procedure (as specified in the
!| BrowseUpdateButtons control template) to delete a selected record.
!|
!| Whenever a button is pressed, the first thing that happens is that the
!| SyncWindow routine is called. This routine insures that the BrowseBox's
!| VIEW corresponds to the highlighted record by calling the BRW2::GetRecord routine.
!|
!| First, LocalRequest is set to DeleteRecord, and the BRW2::CallRecord routine
!| is called. This routine performs the actual call to the update procedure.
!|
!| If the delete is successful (GlobalRequest = RequestCompleted) then the deleted record is
!| removed from the queue.
!|
!| Next, the BrowseBox is refreshed, redisplaying the current page.
!|
!| Finally, The BrowseBox is re-initialized, resetting scroll bars and totals.
!|
  LocalRequest = DeleteRecord
  DO BRW2::CallUpdate
  IF GlobalResponse = RequestCompleted
    DELETE(Queue:Browse:2)
    BRW2::RecordCount -= 1
  END
  BRW2::RefreshMode = RefreshOnQueue
  DO BRW2::RefreshPage
  DO BRW2::InitializeBrowse
  DO BRW2::PostNewSelection
  SELECT(?Browse:2)
  LocalRequest = OriginalRequest
  LocalResponse = RequestCancelled
  DO RefreshWindow
!----------------------------------------------------------------
BRW2::CallUpdate ROUTINE
!|
!| This routine performs the actual call to the update procedure.
!|
!| The first thing that happens is that the VIEW is closed. This is performed just in case
!| the VIEW is still open.
!|
!| Next, GlobalRequest is set the the value of LocalRequest, and the update procedure
!| (UpdateEnrollment) is called.
!|
!| Upon return from the update, the routine BRW2::Reset is called to reset the VIEW
!| and reopen it.
!|
  CLOSE(BRW2::View:Browse)
  LOOP
    GlobalRequest = LocalRequest
    VCRRequest = VCRNone
    UpdateEnrollment
    LocalResponse = GlobalResponse
    CASE VCRRequest
    OF VCRNone
      BREAK
    OF VCRInsert
      IF LocalRequest = ChangeRecord THEN
        LocalRequest = InsertRecord
      END
    OROF VCRForward
      IF LocalRequest = InsertRecord THEN
        GET(Enrollment,0)
        CLEAR(ENR:Record,0)
      ELSE
        DO BRW2::PostVCREdit1
        BRW2::CurrentEvent = EVENT:ScrollDown
        DO BRW2::ScrollOne
        DO BRW2::PostVCREdit2
      END
    OF VCRBackward
      DO BRW2::PostVCREdit1
      BRW2::CurrentEvent = EVENT:ScrollUp
      DO BRW2::ScrollOne
      DO BRW2::PostVCREdit2
    OF VCRPageForward
      DO BRW2::PostVCREdit1
      BRW2::CurrentEvent = EVENT:PageDown
      DO BRW2::ScrollPage
      DO BRW2::PostVCREdit2
    OF VCRPageBackward
      DO BRW2::PostVCREdit1
      BRW2::CurrentEvent = EVENT:PageUp
      DO BRW2::ScrollPage
      DO BRW2::PostVCREdit2
    OF VCRFirst
      DO BRW2::PostVCREdit1
      BRW2::CurrentEvent = EVENT:ScrollTop
      DO BRW2::ScrollEnd
      DO BRW2::PostVCREdit2
    OF VCRLast
      DO BRW2::PostVCREdit1
      BRW2::CurrentEvent = EVENT:ScrollBottom
      DO BRW2::ScrollEND
      DO BRW2::PostVCREdit2
    END
  END
  DO BRW2::Reset

BRW2::PostVCREdit1 ROUTINE
  DO BRW2::Reset
  BRW2::LocateMode = LocateOnEdit
  DO BRW2::LocateRecord
  DO RefreshWindow

BRW2::PostVCREdit2 ROUTINE
  ?Browse:2{PROP:SelStart} = BRW2::CurrentChoice
  DO BRW2::NewSelection
  REGET(BRW2::View:Browse,BRW2::Position)
  CLOSE(BRW2::View:Browse)

!|
!| Copies a field from save buffer to actual buffer switched on current field
!|
HistoryField  ROUTINE
  CASE FOCUS()
  OF ?CLA:ClassNumber
    CLA:ClassNumber = History::CLA:Record.ClassNumber
  OF ?CLA:CourseNumber
    CLA:CourseNumber = History::CLA:Record.CourseNumber
  OF ?CLA:TeacherNumber
    CLA:TeacherNumber = History::CLA:Record.TeacherNumber
  OF ?CLA:RoomNumber
    CLA:RoomNumber = History::CLA:Record.RoomNumber
  OF ?CLA:ScheduledTime
    CLA:ScheduledTime = History::CLA:Record.ScheduledTime
  END
  DISPLAY()
!---------------------------------------------------------------
PrimeFields ROUTINE
!|
!| This routine is called whenever the procedure is called to insert a record.
!|
!| This procedure performs three functions. These functions are..
!|
!|   1. Prime the new record with initial values specified in the dictionary
!|      and under the Field priming on Insert button.
!|   2. Generates any Auto-Increment values needed.
!|   3. Saves a copy of the new record, as primed, for use in batch-adds.
!|
!| If an auto-increment value is generated, this routine will add the new record
!| at this point, keeping its place in the file.
!|
  CLA:Record = SAV::CLA:Record
  SAV::CLA:Record = CLA:Record
ClosingWindow ROUTINE
  Update::Reloop = 0
  IF LocalResponse <> RequestCompleted
    DO CancelAutoIncrement
    IF Save:LastInsertedPosition
      LocalResponse = RequestCompleted
      REGET(Classes,Save:LastInsertedPosition)
    END
  END

CancelAutoIncrement ROUTINE
  IF LocalResponse <> RequestCompleted
  END
FORM::AssignButtons ROUTINE
  ToolBarMode=FormMode
  DISABLE(TBarBrwFirst,TBarBrwLast)
  ENABLE(TBarBrwHistory)
  CASE OriginalRequest
  OF InsertRecord
    ENABLE(TBarBrwDown)
    ENABLE(TBarBrwInsert)
    TBarBrwDown{PROP:ToolTip}='Save record and add another'
    TBarBrwInsert{PROP:ToolTip}=TBarBrwDown{PROP:ToolTip}
  OF ChangeRecord
    ENABLE(TBarBrwBottom,TBarBrwUp)
    ENABLE(TBarBrwInsert)
    TBarBrwBottom{PROP:ToolTip}='Save changes and go to last record'
    TBarBrwTop{PROP:ToolTip}='Save changes and go to first record'
    TBarBrwPageDown{PROP:ToolTip}='Save changes and page down to record'
    TBarBrwPageUp{PROP:ToolTip}='Save changes and page up to record'
    TBarBrwDown{PROP:ToolTip}='Save changes and go to next record'
    TBarBrwUP{PROP:ToolTip}='Save changes and go to previous record'
    TBarBrwInsert{PROP:ToolTip}='Save this record and add a new one'
  END
  TBarBrwHelp{PROP:Disable}=?Help{PROP:Disable}
  DISPLAY(TBarBrwFirst,TBarBrwLast)

!!! <summary>
!!! Generated from procedure template - Form
!!! Update the Enrollment File
!!! </summary>
UpdateEnrollment PROCEDURE

CurrentTab           STRING(80)                            ! 
LocalRequest         LONG                                  ! 
OriginalRequest      LONG                                  ! 
LocalResponse        LONG                                  ! 
FilesOpened          LONG                                  ! 
WindowOpened         LONG                                  ! 
WindowInitialized    LONG                                  ! 
ForceRefresh         LONG                                  ! 
ActionMessage        CSTRING(40)                           ! 
RecordChanged        BYTE,AUTO                             ! 
SelectStudentsThread LONG                                  ! 
SelectClassesThread  LONG                                  ! 
Update::Reloop  BYTE
Update::Error   BYTE
History::ENR:Record LIKE(ENR:Record),STATIC
SAV::ENR:Record      LIKE(ENR:Record)
Save:LastInsertedPosition STRING(512)
ToolBarMode     UNSIGNED,AUTO
WindowXPos      SIGNED,AUTO,STATIC
WindowYPos      SIGNED,AUTO,STATIC
WindowPosInit   BOOL(False),STATIC
QuickWindow          WINDOW('Update the Enrollment File'),AT(1,1,263,102),FONT('MS Sans Serif',8,COLOR:Black),GRAY, |
  IMM,MDI,HLP('~UpdateEnrollment'),SYSTEM
                       PROMPT('&Student Number:'),AT(8,10),USE(?ENR:StudentNumber:Prompt)
                       ENTRY(@p###-##-####p),AT(64,10,52,10),USE(ENR:StudentNumber),RIGHT(1),ALRT(F10Key),ALRT(MouseRight), |
  DROPID('Students'),MSG('Press F10 to drag a Student'),TIP('Press F10 to drag a Student')
                       BUTTON('...'),AT(118,9,12,12),USE(?Button4)
                       STRING(@S20),AT(133,10),USE(STU:LastName)
                       PROMPT('&Class Number'),AT(8,31),USE(?ENR:ClassNumber:Prompt)
                       ENTRY(@p##-#####p),AT(64,31,40,10),USE(ENR:ClassNumber),RIGHT(1),ALRT(F10Key),ALRT(MouseRight), |
  DROPID('Classes'),MSG('Press F10 to drag a Class'),TIP('Press F10 to drag a Class')
                       BUTTON('...'),AT(107,30,12,12),USE(?Button5)
                       STRING(@S30),AT(133,31),USE(COU:Description)
                       STRING(@s20),AT(133,42),USE(CLA:ScheduledTime)
                       PROMPT('Midterm:'),AT(11,63),USE(?ENR:MidtermExam:Prompt)
                       ENTRY(@n3),AT(41,63,40,10),USE(ENR:MidtermExam)
                       PROMPT('Final:'),AT(96,63),USE(?ENR:FinalExam:Prompt)
                       ENTRY(@n3),AT(115,63,40,10),USE(ENR:FinalExam)
                       PROMPT('Term Paper:'),AT(170,63),USE(?ENR:TermPaper:Prompt)
                       ENTRY(@n3),AT(212,63,40,10),USE(ENR:TermPaper)
                       BUTTON('OK'),AT(4,84,45,14),USE(?OK),DEFAULT
                       BUTTON('Cancel'),AT(53,84,45,14),USE(?Cancel)
                       BUTTON('Help'),AT(102,84,45,14),USE(?Help),STD(STD:Help)
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
  CASE LocalRequest
  OF ViewRecord
    ActionMessage = 'View Record'
  OF InsertRecord
    CLEAR(Save:LastInsertedPosition)
    ActionMessage = 'Adding a Enrollment Record'
  OF ChangeRecord
    ActionMessage = 'Changing a Enrollment Record'
  OF DeleteRecord
  END
  QuickWindow{PROP:Text} = ActionMessage
  IF LocalRequest <> ViewRecord THEN
    ENABLE(TBarBrwHistory)
  END
  ACCEPT
    CASE EVENT()
    OF EVENT:AlertKey
      IF KEYCODE() = 734 AND LocalRequest <> ViewRecord THEN
        DO HistoryField
      END
    OF EVENT:CloseDown
      DO ClosingWindow
      IF Update::Reloop THEN CYCLE.
    OF EVENT:CloseWindow
      !Close drag toolbox
      IF SelectStudentsThread
        POST(EVENT:CloseWindow,,SelectStudentsThread)
      END
      IF SelectClassesThread
        POST(EVENT:CloseWindow,,SelectClassesThread)
      END
      DO ClosingWindow
      IF Update::Reloop THEN CYCLE.
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
    OF EVENT:Moved
      GETPOSITION(0,WindowXPos,WindowYPos)
    OF EVENT:OpenWindow
      DO FORM::AssignButtons
      IF NOT WindowInitialized
        DO InitializeWindow
        WindowInitialized = True
      END
      SELECT(?ENR:StudentNumber:Prompt)
    OF EVENT:Sized
      POST(EVENT:DoResize,0,THREAD())
    OF Event:Rejected
      BEEP
      DISPLAY(?)
      SELECT(?)
    ELSE
      IF LocalRequest <> ViewRecord THEN
        IF ACCEPTED() = TbarBrwHistory
          DO HistoryField
        END
      END
      IF EVENT() = EVENT:Completed
        History::ENR:Record = ENR:Record
        CASE LocalRequest
        OF InsertRecord
          ADD(Enrollment)
          CASE ERRORCODE()
          OF NoError
            IF StandardWarning(Warn:NewRecordAdded) = Button:Yes
              Save:LastInsertedPosition = POSITION(Enrollment)
              DO PrimeFields
              DISPLAY
              SELECT(?ENR:StudentNumber:Prompt)
              CYCLE
            ELSE
              LocalResponse = RequestCompleted
              POST(EVENT:CloseWindow)
            END
          OF DupKeyErr
            IF DUPLICATE(ENR:StuSeq)
              IF StandardWarning(Warn:DuplicateKey,'ENR:StuSeq')
                SELECT(?ENR:StudentNumber:Prompt)
                VCRRequest = VCRNone
                CYCLE
              END
            END
            IF DUPLICATE(ENR:SeqStu)
              IF StandardWarning(Warn:DuplicateKey,'ENR:SeqStu')
                SELECT(?ENR:StudentNumber:Prompt)
                VCRRequest = VCRNone
                CYCLE
              END
            END
          ELSE
            IF StandardWarning(Warn:InsertError)
              SELECT(?ENR:StudentNumber:Prompt)
              VCRRequest = VCRNone
              CYCLE
            END
          END
        OF ChangeRecord
          LOOP
            LocalResponse = RequestCancelled
            SETCURSOR(CURSOR:Wait)
            RecordChanged = FALSE
            IF (SAV::ENR:Record <> ENR:Record) 
               RecordChanged = TRUE
            END
            IF RecordChanged THEN
              Update::Error = RIUpdate:Enrollment(1)
            ELSE
              Update::Error = 0
            END
            SETCURSOR()
            IF Update::Error THEN
              IF Update::Error = 1 THEN
                CASE StandardWarning(Warn:UpdateError)
                OF Button:Yes
                  CYCLE
                OF Button:No
                  POST(EVENT:CloseWindow)
                  BREAK
                END
              END
              DISPLAY
              SELECT(?ENR:StudentNumber:Prompt)
              VCRRequest = VCRNone
            ELSE
              IF RecordChanged OR VCRRequest = VCRNone THEN
                LocalResponse = RequestCompleted
              END
              POST(EVENT:CloseWindow)
            END
            BREAK
          END
        END
      END
      IF ToolbarMode = FormMode THEN
        CASE ACCEPTED()
        OF TBarBrwBottom TO TBarBrwUp
        OROF TBarBrwInsert
          VCRRequest=ACCEPTED()
          POST(EVENT:Completed)
        OF TBarBrwHelp
          PRESSKEY(F1Key)
        END
      END
    END
    CASE FIELD()
    OF ?ENR:StudentNumber
      CASE EVENT()
      OF EVENT:Accepted
        STU:Number = ENR:StudentNumber
        GET(Students,STU:KeyStudentNumber)
        IF ERRORCODE()
          IF StandardWarning(Warn:NotInFile,'ENR:StudentNumber','Students')
            SELECT(?ENR:StudentNumber)
            QuickWindow{PROP:AcceptAll} = False
            CYCLE
          END
        END
      OF EVENT:Selected
        !Call drag toolbox
        IF OriginalRequest = InsertRecord AND NOT ENR:StudentNumber
          GLO:DropThread = THREAD()
          GLO:DropControl = ?ENR:StudentNumber
          SelectStudentsThread = START(SelectStudents)
          GLO:ThreadRef &= SelectStudentsThread
        END
      OF EVENT:AlertKey
        !Call drag toolbox
        IF NOT SelectStudentsThread
          GLO:DropThread = THREAD()
          GLO:DropControl = ?ENR:StudentNumber
          SelectStudentsThread = START(SelectStudents)
          GLO:ThreadRef &= SelectStudentsThread
        END
        
      OF EVENT:Drop
        !Receive dropped data and close drag toolbox
        ENR:StudentNumber = DEFORMAT(DROPID(),@P###-##-####P)
        DISPLAY
        POST(EVENT:CloseWindow,,SelectStudentsThread)
        SelectStudentsThread = 0
        DO RefreshWindow
        PRESSKEY(TabKey)
        PRESSKEY(TabKey)
      END
    OF ?Button4
      CASE EVENT()
      OF EVENT:Accepted
        !Call drag toolbox
        POST(EVENT:AlertKey,?ENR:StudentNumber)
        DO SyncWindow
      END
    OF ?ENR:ClassNumber
      CASE EVENT()
      OF EVENT:Accepted
        IF NOT QuickWindow{PROP:AcceptAll}
          CLA:ClassNumber = ENR:ClassNumber
          GET(Classes,CLA:KeyClassNumber)
          IF ERRORCODE()
            GlobalRequest = SelectRecord
            SelectClasses
            LocalResponse = GlobalResponse
            GlobalResponse = RequestCancelled
            IF LocalResponse = RequestCompleted
              ENR:ClassNumber = CLA:ClassNumber
            ELSE
              SELECT(?ENR:ClassNumber)
              CYCLE
            END
          END
        END
        LocalRequest = OriginalRequest
        DO RefreshWindow
        CLA:ClassNumber = ENR:ClassNumber
        GET(Classes,CLA:KeyClassNumber)
        IF ERRORCODE()
          IF StandardWarning(Warn:NotInFile,'ENR:ClassNumber','Classes')
            SELECT(?ENR:ClassNumber)
            QuickWindow{PROP:AcceptAll} = False
            CYCLE
          END
        END
      OF EVENT:Selected
        !Call drag toolbox
        IF OriginalRequest = InsertRecord AND NOT ENR:ClassNumber
          GLO:DropThread = THREAD()
          GLO:DropControl = ?ENR:ClassNumber
          SelectClassesThread = START(SelectClasses)
          GLO:ThreadRef &= SelectClassesThread
        END
        
      OF EVENT:AlertKey
        !Call drag toolbox
        IF NOT SelectClassesThread
          GLO:DropThread = THREAD()
          GLO:DropControl = ?ENR:ClassNumber
          SelectClassesThread = START(SelectClasses)
          GLO:ThreadRef &= SelectClassesThread
        END
      OF EVENT:Drop
        !Receive dropped data and close drag toolbox
        ENR:ClassNumber = DEFORMAT(DROPID(),@P##-#####P)
        DISPLAY
        POST(EVENT:CloseWindow,,SelectClassesThread)
        SelectClassesThread = 0
        DO RefreshWindow
        PRESSKEY(TabKey)
        PRESSKEY(TabKey)
      END
    OF ?Button5
      CASE EVENT()
      OF EVENT:Accepted
        !Call drag toolbox
        POST(EVENT:AlertKey,?ENR:ClassNumber)
        DO SyncWindow
      END
    OF ?ENR:MidtermExam
      CASE EVENT()
      OF EVENT:Accepted
        IF NOT INRANGE(ENR:MidtermExam,0,100)
          IF StandardWarning(Warn:OutOfRange,'ENR:MidtermExam','0 and 100')
            SELECT(?ENR:MidtermExam)
            QuickWindow{PROP:AcceptAll} = False
            CYCLE
          END
        END
      END
    OF ?ENR:FinalExam
      CASE EVENT()
      OF EVENT:Accepted
        IF NOT INRANGE(ENR:FinalExam,0,100)
          IF StandardWarning(Warn:OutOfRange,'ENR:FinalExam','0 and 100')
            SELECT(?ENR:FinalExam)
            QuickWindow{PROP:AcceptAll} = False
            CYCLE
          END
        END
      END
    OF ?ENR:TermPaper
      CASE EVENT()
      OF EVENT:Accepted
        IF NOT INRANGE(ENR:TermPaper,0,100)
          IF StandardWarning(Warn:OutOfRange,'ENR:TermPaper','0 and 100')
            SELECT(?ENR:TermPaper)
            QuickWindow{PROP:AcceptAll} = False
            CYCLE
          END
        END
      END
    OF ?OK
      CASE EVENT()
      OF EVENT:Accepted
        DO SyncWindow
        IF LocalRequest <> ViewRecord THEN
          IF OriginalRequest = ChangeRecord OR OriginalRequest = InsertRecord
            SELECT()
          ELSE
            POST(EVENT:Completed)
          END
        END
      END
    OF ?Cancel
      CASE EVENT()
      OF EVENT:Accepted
        DO SyncWindow
        LocalResponse = RequestCancelled
        VCRRequest = VCRNone
        POST(EVENT:CloseWindow)
      END
    END
  END
  DO ProcedureReturn
!---------------------------------------------------------------------------
PrepareProcedure ROUTINE
  IF Classes::Used = 0
    CheckOpen(Classes,1)
  END
  Classes::Used += 1
  IF Courses::Used = 0
    CheckOpen(Courses,1)
  END
  Courses::Used += 1
  IF Enrollment::Used = 0
    CheckOpen(Enrollment,1)
  END
  Enrollment::Used += 1
  IF Students::Used = 0
    CheckOpen(Students,1)
  END
  Students::Used += 1
  FilesOpened = TRUE
  DO BindFields
  RISnap:Enrollment
  SAV::ENR:Record = ENR:Record
  IF LocalRequest = InsertRecord
    LocalResponse = RequestCompleted
    DO PrimeFields
    IF LocalResponse = RequestCancelled
      DO ProcedureReturn
    END
    LocalResponse = RequestCancelled
  END
  IF LocalRequest = DeleteRecord
    IF StandardWarning(Warn:StandardDelete) = Button:OK
      LOOP
        LocalResponse = RequestCancelled
        SETCURSOR(CURSOR:Wait)
        IF RIDelete:Enrollment()
          SETCURSOR()
          CASE StandardWarning(Warn:DeleteError)
          OF Button:Yes
            CYCLE
          OF Button:No OROF Button:Cancel
            BREAK
          END
        ELSE
          SETCURSOR()
          LocalResponse = RequestCompleted
        END
        BREAK
      END
    END
    DO ProcedureReturn
  END
  OPEN(QuickWindow)
  WindowOpened=True
  IF WindowPosInit THEN
    SETPOSITION(0,WindowXPos,WindowYPos)
  ELSE
    GETPOSITION(0,WindowXPos,WindowYPos)
    WindowPosInit=True
  END
  Do DefineListboxStyle
  ?ENR:StudentNumber{PROP:Alrt,255} = 734
  ?ENR:ClassNumber{PROP:Alrt,255} = 734
  ?ENR:MidtermExam{PROP:Alrt,255} = 734
  ?ENR:FinalExam{PROP:Alrt,255} = 734
  ?ENR:TermPaper{PROP:Alrt,255} = 734

!---------------------------------------------------------------------------
BindFields ROUTINE
  BIND(CLA:RECORD)
  BIND(COU:RECORD)
  BIND('COU:CompleteDescription',COU:CompleteDescription)
  BIND(ENR:RECORD)
  BIND(STU:RECORD)
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
    Classes::Used -= 1
    IF Classes::Used = 0 THEN CLOSE(Classes).
    Courses::Used -= 1
    IF Courses::Used = 0 THEN CLOSE(Courses).
    Enrollment::Used -= 1
    IF Enrollment::Used = 0 THEN CLOSE(Enrollment).
    Students::Used -= 1
    IF Students::Used = 0 THEN CLOSE(Students).
  END
  IF WindowOpened
    CLOSE(QuickWindow)
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
  IF QuickWindow{Prop:AcceptAll} THEN EXIT.
  STU:Number = ENR:StudentNumber                           ! Assign linking field value
  GET(Students,STU:KeyStudentNumber)                       ! Lookup record
  IF ERRORCODE()
    CLEAR(STU:Record)                                      ! Clear record if unsuccessful
  END
  CLA:ClassNumber = ENR:ClassNumber                        ! Assign linking field value
  GET(Classes,CLA:KeyClassNumber)                          ! Lookup record
  IF ERRORCODE()
    CLEAR(CLA:Record)                                      ! Clear record if unsuccessful
  END
  COU:Number = CLA:CourseNumber                            ! Assign linking field value
  GET(Courses,COU:KeyNumber)                               ! Lookup record
  IF ERRORCODE()
    CLEAR(COU:Record)                                      ! Clear record if unsuccessful
    CLEAR(COU:CompleteDescription)
  END
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
!|
!| Copies a field from save buffer to actual buffer switched on current field
!|
HistoryField  ROUTINE
  CASE FOCUS()
  OF ?ENR:StudentNumber
    ENR:StudentNumber = History::ENR:Record.StudentNumber
  OF ?ENR:ClassNumber
    ENR:ClassNumber = History::ENR:Record.ClassNumber
  OF ?ENR:MidtermExam
    ENR:MidtermExam = History::ENR:Record.MidtermExam
  OF ?ENR:FinalExam
    ENR:FinalExam = History::ENR:Record.FinalExam
  OF ?ENR:TermPaper
    ENR:TermPaper = History::ENR:Record.TermPaper
  END
  DISPLAY()
!---------------------------------------------------------------
PrimeFields ROUTINE
!|
!| This routine is called whenever the procedure is called to insert a record.
!|
!| This procedure performs three functions. These functions are..
!|
!|   1. Prime the new record with initial values specified in the dictionary
!|      and under the Field priming on Insert button.
!|   2. Generates any Auto-Increment values needed.
!|   3. Saves a copy of the new record, as primed, for use in batch-adds.
!|
!| If an auto-increment value is generated, this routine will add the new record
!| at this point, keeping its place in the file.
!|
  ENR:Record = SAV::ENR:Record
  SAV::ENR:Record = ENR:Record
ClosingWindow ROUTINE
  Update::Reloop = 0
  IF LocalResponse <> RequestCompleted
    DO CancelAutoIncrement
    IF Save:LastInsertedPosition
      LocalResponse = RequestCompleted
      REGET(Enrollment,Save:LastInsertedPosition)
    END
  END

CancelAutoIncrement ROUTINE
  IF LocalResponse <> RequestCompleted
  END
FORM::AssignButtons ROUTINE
  ToolBarMode=FormMode
  DISABLE(TBarBrwFirst,TBarBrwLast)
  ENABLE(TBarBrwHistory)
  CASE OriginalRequest
  OF InsertRecord
    ENABLE(TBarBrwDown)
    ENABLE(TBarBrwInsert)
    TBarBrwDown{PROP:ToolTip}='Save record and add another'
    TBarBrwInsert{PROP:ToolTip}=TBarBrwDown{PROP:ToolTip}
  OF ChangeRecord
    ENABLE(TBarBrwBottom,TBarBrwUp)
    ENABLE(TBarBrwInsert)
    TBarBrwBottom{PROP:ToolTip}='Save changes and go to last record'
    TBarBrwTop{PROP:ToolTip}='Save changes and go to first record'
    TBarBrwPageDown{PROP:ToolTip}='Save changes and page down to record'
    TBarBrwPageUp{PROP:ToolTip}='Save changes and page up to record'
    TBarBrwDown{PROP:ToolTip}='Save changes and go to next record'
    TBarBrwUP{PROP:ToolTip}='Save changes and go to previous record'
    TBarBrwInsert{PROP:ToolTip}='Save this record and add a new one'
  END
  TBarBrwHelp{PROP:Disable}=?Help{PROP:Disable}
  DISPLAY(TBarBrwFirst,TBarBrwLast)

!!! <summary>
!!! Generated from procedure template - Form
!!! Update the Teachers File
!!! </summary>
UpdateTeachers PROCEDURE

CurrentTab           STRING(80)                            ! 
LocalRequest         LONG                                  ! 
OriginalRequest      LONG                                  ! 
LocalResponse        LONG                                  ! 
FilesOpened          LONG                                  ! 
WindowOpened         LONG                                  ! 
WindowInitialized    LONG                                  ! 
ForceRefresh         LONG                                  ! 
RecordFiltered       LONG                                  ! 
ActionMessage        CSTRING(40)                           ! 
RecordChanged        BYTE,AUTO                             ! 
SelectMajorsThread   LONG                                  ! 

BRW2::View:Browse    VIEW(Classes)
                       PROJECT(CLA:ClassNumber)
                       PROJECT(CLA:RoomNumber)
                       PROJECT(CLA:ScheduledTime)
                       PROJECT(CLA:TeacherNumber)
                       PROJECT(CLA:CourseNumber)
                       JOIN(COU:KeyNumber,CLA:CourseNumber)
                         PROJECT(COU:Description)
                         PROJECT(COU:Number)
                       END
                     END
Queue:Browse:2       QUEUE,PRE()                           ! Browsing Queue
BRW2::CLA:ClassNumber  LIKE(CLA:ClassNumber)               ! Queue Display field
BRW2::COU:Description  LIKE(COU:Description)               ! Queue Display field
BRW2::CLA:RoomNumber   LIKE(CLA:RoomNumber)                ! Queue Display field
BRW2::CLA:ScheduledTime LIKE(CLA:ScheduledTime)            ! Queue Display field
BRW2::CLA:TeacherNumber LIKE(CLA:TeacherNumber)            ! Queue Display field
BRW2::COU:Number       LIKE(COU:Number)                    ! Queue Display field
BRW2::Mark             BYTE                                ! Record mark flag
BRW2::Position         STRING(1024)                        ! Queue POSITION information
                     END                                   ! END (Browsing Queue)
BRW2::UsingAdditionalSortOrder BYTE                        ! When true the view is using PROP:Order
BRW2::CurrentScroll  BYTE                                  ! Queue position of scroll thumb
BRW2::ScrollRecordCount LONG                               ! Queue position of scroll thumb
BRW2::SkipFirst      BYTE                                  ! Skip first retrieved record in page fill
BRW2::Sort1:KeyDistribution LIKE(CLA:TeacherNumber),DIM(100)
BRW2::Sort1:LowValue LIKE(CLA:TeacherNumber)               ! Queue position of scroll thumb
BRW2::Sort1:HighValue LIKE(CLA:TeacherNumber)              ! Queue position of scroll thumb
BRW2::Sort1:Reset:TEA:Number LIKE(TEA:Number)
BRW2::CurrentEvent   LONG                                  !
BRW2::CurrentChoice  LONG                                  !
BRW2::RecordCount    LONG                                  !
BRW2::SortOrder      BYTE                                  !
BRW2::LocateMode     BYTE                                  !
BRW2::RefreshMode    BYTE                                  !
BRW2::LastSortOrder  BYTE                                  !
BRW2::FillDirection  BYTE                                  !
BRW2::AddQueue       BYTE                                  !
BRW2::Changed        BYTE                                  !
BRW2::RecordStatus   BYTE                                  ! Flag for Range/Filter test
BRW2::ItemsToFill    LONG                                  ! Controls records retrieved
BRW2::MaxItemsInList LONG                                  ! Retrieved after window opened
BRW2::HighlightedPosition STRING(1024)                     ! POSITION of located record
BRW2::NewSelectPosted BYTE                                 ! Queue position of located record
BRW2::PopupText      CSTRING(10000)                        !
BRW2::ActiveInvisible BYTE(1)                              !
BRW2::LoadPending    BYTE                                  !
ToolBarMode          UNSIGNED,AUTO
BrowseButtons        GROUP                      !info for current browse with focus
ListBox                SIGNED                   !Browse list control
InsertButton           SIGNED                   !Browse insert button
ChangeButton           SIGNED                   !Browse change button
DeleteButton           SIGNED                   !Browse delete button
SelectButton           SIGNED                   !Browse select button
                     END
Update::Reloop  BYTE
Update::Error   BYTE
History::TEA:Record LIKE(TEA:Record),STATIC
SAV::TEA:Record      LIKE(TEA:Record)
Save:LastInsertedPosition STRING(512)
WindowXPos      SIGNED,AUTO,STATIC
WindowYPos      SIGNED,AUTO,STATIC
WindowPosInit   BOOL(False),STATIC
QuickWindow          WINDOW('Update the Teachers File'),AT(,,159,159),FONT('MS Sans Serif',8,COLOR:Black),GRAY, |
  IMM,MDI,HLP('~UpdateTeachers'),SYSTEM
                       SHEET,AT(4,4,151,133),USE(?CurrentTab)
                         TAB('General')
                           PROMPT('&First Name:'),AT(8,20),USE(?Tea:FirstName:Prompt)
                           ENTRY(@S20),AT(53,20,84,10),USE(TEA:FirstName)
                           PROMPT('&Last Name:'),AT(8,34),USE(?Tea:LastName:Prompt)
                           ENTRY(@S20),AT(53,34,84,10),USE(TEA:LastName)
                           PROMPT('&Address:'),AT(8,48),USE(?Tea:Address:Prompt)
                           ENTRY(@S20),AT(53,48,84,10),USE(TEA:Address)
                           PROMPT('&City:'),AT(8,62),USE(?Tea:City:Prompt)
                           ENTRY(@S20),AT(53,62,84,10),USE(TEA:City)
                           PROMPT('&State:'),AT(8,76),USE(?Tea:State:Prompt)
                           ENTRY(@S2),AT(53,76,40,10),USE(TEA:State)
                           PROMPT('&Zip:'),AT(8,90),USE(?Tea:Zip:Prompt)
                           ENTRY(@n05),AT(53,90,40,10),USE(TEA:Zip)
                           PROMPT('&Telephone:'),AT(8,104),USE(?Tea:Telephone:Prompt)
                           ENTRY(@s12),AT(53,104,52,10),USE(TEA:Telephone)
                           PROMPT('Department:'),AT(8,119),USE(?Tea:Department:Prompt)
                           ENTRY(@n4),AT(53,119,21,10),USE(TEA:Department),ALRT(F10Key),DROPID('Majors')
                           BUTTON('...'),AT(77,118,12,12),USE(?Button7)
                           STRING(@S15),AT(90,119),USE(MAJ:Description)
                         END
                         TAB('Classes'),USE(?ClassesTab)
                           LIST,AT(8,20,143,111),USE(?Browse:2),HVSCROLL,FORMAT('52L(2)|M~Class Number~@P##-#####P' & |
  '@120L(2)|M~Description~@S30@48R(2)|M~Room Number~C(0)@n4@80L(2)|M~Scheduled Time~@s20@'), |
  FROM(Queue:Browse:2),IMM,MSG('Browsing Records')
                           BUTTON('&Insert'),AT(8,100,45,14),USE(?Insert:3),HIDE
                           BUTTON('&Change'),AT(57,100,45,14),USE(?Change:3),HIDE
                           BUTTON('&Delete'),AT(106,100,45,14),USE(?Delete:3),HIDE
                         END
                       END
                       BUTTON('OK'),AT(12,140,45,14),USE(?OK),DEFAULT
                       BUTTON('Cancel'),AT(61,140,45,14),USE(?Cancel)
                       BUTTON('Help'),AT(110,140,45,14),USE(?Help),STD(STD:Help)
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
  IF LocalRequest = InsertRecord
    ?ClassesTab{PROP:Hide} = TRUE
  END
  CASE LocalRequest
  OF ViewRecord
    ActionMessage = 'View Record'
  OF InsertRecord
    CLEAR(Save:LastInsertedPosition)
    ActionMessage = 'Adding a Teachers Record'
  OF ChangeRecord
    ActionMessage = 'Changing a Teachers Record'
  OF DeleteRecord
  END
  QuickWindow{PROP:Text} = ActionMessage
  IF LocalRequest <> ViewRecord THEN
    ENABLE(TBarBrwHistory)
  END
  ACCEPT
    CASE EVENT()
    OF EVENT:AlertKey
      IF KEYCODE() = 734 AND LocalRequest <> ViewRecord THEN
        DO HistoryField
      END
    OF EVENT:CloseDown
      DO ClosingWindow
      IF Update::Reloop THEN CYCLE.
    OF EVENT:CloseWindow
      !Close drag toolbox
      IF SelectMajorsThread
        POST(EVENT:CloseWindow,,SelectMajorsThread)
      END
      DO ClosingWindow
      IF Update::Reloop THEN CYCLE.
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
    OF EVENT:Moved
      GETPOSITION(0,WindowXPos,WindowYPos)
    OF EVENT:OpenWindow
      DO BRW2::AssignButtons
      DO FORM::AssignButtons
      IF NOT WindowInitialized
        DO InitializeWindow
        WindowInitialized = True
      END
      SELECT(?Tea:FirstName:Prompt)
    OF EVENT:Sized
      POST(EVENT:DoResize,0,THREAD())
    OF Event:Rejected
      BEEP
      DISPLAY(?)
      SELECT(?)
    ELSE
      IF ToolBarMode=BrowseMode THEN
        DO ListBoxDispatch
      END
      IF ToolBarMode = BrowseMode THEN
        DO UpdateDispatch
      END
      IF LocalRequest <> ViewRecord THEN
        IF ACCEPTED() = TbarBrwHistory
          DO HistoryField
        END
      END
      IF EVENT() = EVENT:Completed
        History::TEA:Record = TEA:Record
        CASE LocalRequest
        OF InsertRecord
          ADD(Teachers)
          CASE ERRORCODE()
          OF NoError
            IF StandardWarning(Warn:NewRecordAdded) = Button:Yes
              Save:LastInsertedPosition = POSITION(Teachers)
              DO PrimeFields
              DISPLAY
              SELECT(?Tea:FirstName:Prompt)
              CYCLE
            ELSE
              LocalResponse = RequestCompleted
              POST(EVENT:CloseWindow)
            END
          OF DupKeyErr
            IF DUPLICATE(TEA:KeyTeacherNumber)
              IF StandardWarning(Warn:DuplicateKey,'TEA:KeyTeacherNumber')
                SELECT(?Tea:FirstName:Prompt)
                VCRRequest = VCRNone
                CYCLE
              END
            END
          ELSE
            IF StandardWarning(Warn:InsertError)
              SELECT(?Tea:FirstName:Prompt)
              VCRRequest = VCRNone
              CYCLE
            END
          END
        OF ChangeRecord
          LOOP
            LocalResponse = RequestCancelled
            SETCURSOR(CURSOR:Wait)
            RecordChanged = FALSE
            IF (SAV::TEA:Record <> TEA:Record) 
               RecordChanged = TRUE
            END
            IF RecordChanged THEN
              Update::Error = RIUpdate:Teachers(1)
            ELSE
              Update::Error = 0
            END
            SETCURSOR()
            IF Update::Error THEN
              IF Update::Error = 1 THEN
                CASE StandardWarning(Warn:UpdateError)
                OF Button:Yes
                  CYCLE
                OF Button:No
                  POST(EVENT:CloseWindow)
                  BREAK
                END
              END
              DISPLAY
              SELECT(?Tea:FirstName:Prompt)
              VCRRequest = VCRNone
            ELSE
              IF RecordChanged OR VCRRequest = VCRNone THEN
                LocalResponse = RequestCompleted
              END
              POST(EVENT:CloseWindow)
            END
            BREAK
          END
        END
      END
      IF ToolbarMode = FormMode THEN
        CASE ACCEPTED()
        OF TBarBrwBottom TO TBarBrwUp
        OROF TBarBrwInsert
          VCRRequest=ACCEPTED()
          POST(EVENT:Completed)
        OF TBarBrwHelp
          PRESSKEY(F1Key)
        END
      END
    END
    CASE FIELD()
    OF ?CurrentTab
      CASE EVENT()
      OF EVENT:Accepted
        DO RefreshWindow
      OF EVENT:Selected
        DO RefreshWindow
      OF EVENT:Selecting
        DO RefreshWindow
      OF EVENT:NewSelection
          !Code to assign button control based upon current tab selection
          CASE CHOICE(?CurrentTab)
          OF 1
            DO FORM::AssignButtons
          OF 2
            DO BRW2::AssignButtons
          END
        DO RefreshWindow
      OF EVENT:TabChanging
        DO RefreshWindow
      END
    OF ?TEA:Department
      CASE EVENT()
      OF EVENT:Accepted
        MAJ:Number = TEA:Department
        GET(Majors,MAJ:KeyNumber)
        IF ERRORCODE()
          IF StandardWarning(Warn:NotInFile,'TEA:Department','Majors')
            SELECT(?TEA:Department)
            QuickWindow{PROP:AcceptAll} = False
            CYCLE
          END
        END
      OF EVENT:Selected
        !Call drag toolbox
        IF OriginalRequest = InsertRecord AND NOT Tea:Department
          GLO:DropThread = THREAD()
          GLO:DropControl = ?Tea:Department
          SelectMajorsThread = START(SelectMajors)
          GLO:ThreadRef &= SelectMajorsThread
        END
      OF EVENT:AlertKey
        !Call drag toolbox
        IF NOT SelectMajorsThread
          GLO:DropThread = THREAD()
          GLO:DropControl = ?Tea:Department
          SelectMajorsThread = START(SelectMajors)
          GLO:ThreadRef &= SelectMajorsThread
        END
      OF EVENT:Drop
        !Receive dropped data and close drag toolbox
        Tea:Department = DROPID()
        DISPLAY
        POST(EVENT:CloseWindow,,SelectMajorsThread)
        SelectMajorsThread = 0
        DO RefreshWindow
        PRESSKEY(TabKey)
        PRESSKEY(TabKey)
      END
    OF ?Button7
      CASE EVENT()
      OF EVENT:Accepted
        !Call drag toolbox
        POST(EVENT:AlertKey,?Tea:Department)
        DO SyncWindow
      END
    OF ?Browse:2
      CASE EVENT()
      OF EVENT:NewSelection
        DO BRW2::NewSelection
      OF EVENT:ScrollUp
        DO BRW2::ProcessScroll
      OF EVENT:ScrollDown
        DO BRW2::ProcessScroll
      OF EVENT:PageUp
        DO BRW2::ProcessScroll
      OF EVENT:PageDown
        DO BRW2::ProcessScroll
      OF EVENT:ScrollTop
        DO BRW2::ProcessScroll
      OF EVENT:ScrollBottom
        DO BRW2::ProcessScroll
      OF EVENT:ScrollDrag
        DO BRW2::ScrollDrag
      OF EVENT:AlertKey
        DO BRW2::AlertKey
      END
    OF ?Insert:3
      CASE EVENT()
      OF EVENT:Accepted
        DO SyncWindow
        DO BRW2::ButtonInsert
      END
    OF ?Change:3
      CASE EVENT()
      OF EVENT:Accepted
        DO SyncWindow
        DO BRW2::ButtonChange
      END
    OF ?Delete:3
      CASE EVENT()
      OF EVENT:Accepted
        DO SyncWindow
        DO BRW2::ButtonDelete
      END
    OF ?OK
      CASE EVENT()
      OF EVENT:Accepted
        DO SyncWindow
        IF LocalRequest <> ViewRecord THEN
          IF OriginalRequest = ChangeRecord OR OriginalRequest = InsertRecord
            SELECT()
          ELSE
            POST(EVENT:Completed)
          END
        END
      END
    OF ?Cancel
      CASE EVENT()
      OF EVENT:Accepted
        DO SyncWindow
        LocalResponse = RequestCancelled
        VCRRequest = VCRNone
        POST(EVENT:CloseWindow)
      END
    END
  END
  DO ProcedureReturn
!---------------------------------------------------------------------------
PrepareProcedure ROUTINE
  IF Classes::Used = 0
    CheckOpen(Classes,1)
  END
  Classes::Used += 1
  IF Courses::Used = 0
    CheckOpen(Courses,1)
  END
  Courses::Used += 1
  IF Majors::Used = 0
    CheckOpen(Majors,1)
  END
  Majors::Used += 1
  IF Teachers::Used = 0
    CheckOpen(Teachers,1)
  END
  Teachers::Used += 1
  FilesOpened = TRUE
  DO BindFields
  RISnap:Teachers
  SAV::TEA:Record = TEA:Record
  IF LocalRequest = InsertRecord
    LocalResponse = RequestCompleted
    DO PrimeFields
    IF LocalResponse = RequestCancelled
      DO ProcedureReturn
    END
    LocalResponse = RequestCancelled
  END
  IF LocalRequest = DeleteRecord
    IF StandardWarning(Warn:StandardDelete) = Button:OK
      LOOP
        LocalResponse = RequestCancelled
        SETCURSOR(CURSOR:Wait)
        IF RIDelete:Teachers()
          SETCURSOR()
          CASE StandardWarning(Warn:DeleteError)
          OF Button:Yes
            CYCLE
          OF Button:No OROF Button:Cancel
            BREAK
          END
        ELSE
          SETCURSOR()
          LocalResponse = RequestCompleted
        END
        BREAK
      END
    END
    DO ProcedureReturn
  END
  OPEN(QuickWindow)
  WindowOpened=True
  BRW2::AddQueue = True
  BRW2::RecordCount = 0
  IF WindowPosInit THEN
    SETPOSITION(0,WindowXPos,WindowYPos)
  ELSE
    GETPOSITION(0,WindowXPos,WindowYPos)
    WindowPosInit=True
  END
  Do DefineListboxStyle
  ?Browse:2{PROP:Alrt,252} = MouseLeft2
  ?Browse:2{PROP:Alrt,255} = AppsKey
  ?Browse:2{PROP:Alrt,253} = MouseRight
  ?Browse:2{PROP:Alrt,255} = InsertKey
  ?Browse:2{PROP:Alrt,254} = DeleteKey
  ?Browse:2{PROP:Alrt,253} = CtrlEnter
  ?Browse:2{PROP:Alrt,252} = MouseLeft2
  ?TEA:FirstName{PROP:Alrt,255} = 734
  ?TEA:LastName{PROP:Alrt,255} = 734
  ?TEA:Address{PROP:Alrt,255} = 734
  ?TEA:City{PROP:Alrt,255} = 734
  ?TEA:State{PROP:Alrt,255} = 734
  ?TEA:Zip{PROP:Alrt,255} = 734
  ?TEA:Telephone{PROP:Alrt,255} = 734
  ?TEA:Department{PROP:Alrt,255} = 734

!---------------------------------------------------------------------------
BindFields ROUTINE
  BIND(CLA:RECORD)
  BIND(COU:RECORD)
  BIND('COU:CompleteDescription',COU:CompleteDescription)
  BIND(MAJ:RECORD)
  BIND(TEA:RECORD)
  BIND('BRW2::Sort1:Reset:TEA:Number',BRW2::Sort1:Reset:TEA:Number) ! Added by: BrowseBox(Clarion)
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
    Classes::Used -= 1
    IF Classes::Used = 0 THEN CLOSE(Classes).
    Courses::Used -= 1
    IF Courses::Used = 0 THEN CLOSE(Courses).
    Majors::Used -= 1
    IF Majors::Used = 0 THEN CLOSE(Majors).
    Teachers::Used -= 1
    IF Teachers::Used = 0 THEN CLOSE(Teachers).
  END
  IF WindowOpened
    CLOSE(QuickWindow)
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
  IF QuickWindow{Prop:AcceptAll} THEN EXIT.
  MAJ:Number = TEA:Department                              ! Assign linking field value
  GET(Majors,MAJ:KeyNumber)                                ! Lookup record
  IF ERRORCODE()
    CLEAR(MAJ:Record)                                      ! Clear record if unsuccessful
  END
  Do LookupRelated
  DO BRW2::SelectSort
  ?Browse:2{Prop:VScrollPos} = BRW2::CurrentScroll
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
  DO BRW2::GetRecord
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
BRW2::AssignButtons ROUTINE
  CLEAR(BrowseButtons)
  BrowseButtons.ListBox = ?Browse:2
  BrowseButtons.InsertButton = ?Insert:3
  BrowseButtons.ChangeButton = ?Change:3
  BrowseButtons.DeleteButton = ?Delete:3
  DO DisplayBrowseToolbar
!----------------------------------------------------------------------
BRW2::SelectSort ROUTINE
!|
!| This routine is called during the RefreshWindow ROUTINE present in every window procedure.
!| The purpose of this routine is to make certain that the BrowseBox is always current with your
!| user's selections. This routine...
!|
!| 1. Checks to see if any of your specified sort-order conditions are met, and if so, changes the sort order.
!| 2. If no sort order change is necessary, this routine checks to see if any of your Reset Fields has changed.
!| 3. If the sort order has changed, or if a reset field has changed, or if the ForceRefresh flag is set...
!|    a. The current record is retrieved from the disk.
!|    b. If the BrowseBox is accessed for the first time, and the Browse has been called to select a record,
!|       the page containing the current record is loaded.
!|    c. If the BrowseBox is accessed for the first time, and the Browse has not been called to select a
!|       record, the first page of information is loaded.
!|    d. If the BrowseBox is not being accessed for the first time, and the Browse sort order has changed, the
!|       new "first" page of information is loaded.
!|    e. If the BrowseBox is not being accessed for the first time, and the Browse sort order hasn't changes,
!|       the page containing the current record is reloaded.
!|    f. The record buffer is refilled from the currently highlighted BrowseBox item.
!|    f. The BrowseBox is reinitialized (BRW2::InitializeBrowse ROUTINE).
!| 4. If step 3 is not necessary, the record buffer is refilled from the currently highlighted BrowseBox item.
!|
  BRW2::LastSortOrder = BRW2::SortOrder
  BRW2::Changed = False
  IF BRW2::SortOrder = 0
    BRW2::SortOrder = 1
  END
  IF BRW2::SortOrder = BRW2::LastSortOrder
    CASE BRW2::SortOrder
    OF 1
      IF BRW2::Sort1:Reset:TEA:Number <> TEA:Number
        BRW2::Changed = True
      END
    END
  ELSE
  END
  IF BRW2::SortOrder <> BRW2::LastSortOrder OR BRW2::Changed OR ForceRefresh OR (BRW2::LoadPending AND ?Browse:2{PROP:VISIBLE})
    CASE BRW2::SortOrder
    OF 1
      BRW2::Sort1:Reset:TEA:Number = TEA:Number
    END
    DO BRW2::GetRecord
    DO BRW2::Reset
    IF BRW2::LastSortOrder = 0
      IF LocalRequest = SelectRecord
        BRW2::LocateMode = LocateOnValue
        DO BRW2::LocateRecord
      ELSE
        FREE(Queue:Browse:2)
        BRW2::RefreshMode = RefreshOnTop
        DO BRW2::RefreshPage
        DO BRW2::PostNewSelection
      END
    ELSE
      IF BRW2::Changed
        FREE(Queue:Browse:2)
        BRW2::RefreshMode = RefreshOnTop
        DO BRW2::RefreshPage
        DO BRW2::PostNewSelection
      ELSE
        BRW2::LocateMode = LocateOnValue
        DO BRW2::LocateRecord
      END
    END
    IF BRW2::RecordCount
      GET(Queue:Browse:2,BRW2::CurrentChoice)
      DO BRW2::FillBuffer
    END
    DO BRW2::InitializeBrowse
  ELSE
    IF BRW2::RecordCount
      GET(Queue:Browse:2,BRW2::CurrentChoice)
      DO BRW2::FillBuffer
    END
  END
!----------------------------------------------------------------------
BRW2::InitializeBrowse ROUTINE
!|
!| This routine initializes the BrowseBox control template. This routine is called when...
!|
!| The BrowseBox sort order has changed. This includes the first time the BrowseBox is accessed.
!| The BrowseBox returns from a record update.
!|
!| This routine performs two main functions.
!|   1. Computes all BrowseBox totals. All records that satisfy the current selection criteria
!|      are read, and totals computed. If no totals are present, this section is not generated,
!|      and may not be present in the code below.
!|   2. Calculates any runtime scrollbar positions. Again, if runtime scrollbars are not used,
!|      the code for runtime scrollbar computation will not be present.
!|
  IF NOT BRW2::ActiveInvisible THEN
     IF NOT ?Browse:2{PROP:Visible} THEN
        BRW2::LoadPending = True
        EXIT
     END
  END
  DO BRW2::Reset
  PREVIOUS(BRW2::View:Browse)
  IF ERRORCODE()
    IF ERRORCODE() = BadRecErr
      DO BRW2::RestoreResetValues
    ELSE
      StandardWarning(Warn:RecordFetchError,'Classes')
      POST(Event:CloseWindow)
    END
    EXIT
  END
  CASE BRW2::SortOrder
  OF 1
    BRW2::Sort1:HighValue = CLA:TeacherNumber
  END
  DO BRW2::Reset
  NEXT(BRW2::View:Browse)
  IF ERRORCODE()
    IF ERRORCODE() = BadRecErr
      DO BRW2::RestoreResetValues
    ELSE
      StandardWarning(Warn:RecordFetchError,'Classes')
      POST(Event:CloseWindow)
    END
    EXIT
  END
  CASE BRW2::SortOrder
  OF 1
    BRW2::Sort1:LowValue = CLA:TeacherNumber
    SetupRealStops(BRW2::Sort1:LowValue,BRW2::Sort1:HighValue)
    LOOP BRW2::ScrollRecordCount = 1 TO 100
      BRW2::Sort1:KeyDistribution[BRW2::ScrollRecordCount] = NextRealStop()
    END
  END
!----------------------------------------------------------------------
BRW2::FillBuffer ROUTINE
!|
!| This routine fills the record buffer from the BrowseBox queue. This gives the appearance
!| that the record is "fresh" from the disk, without the disk access required.
!|
  CLA:ClassNumber = BRW2::CLA:ClassNumber
  COU:Description = BRW2::COU:Description
  CLA:RoomNumber = BRW2::CLA:RoomNumber
  CLA:ScheduledTime = BRW2::CLA:ScheduledTime
  CLA:TeacherNumber = BRW2::CLA:TeacherNumber
  COU:Number = BRW2::COU:Number
!----------------------------------------------------------------------
BRW2::FillQueue ROUTINE
!|
!| This routine is used to fill the BrowseBox QUEUE from several sources.
!|
!| First, all Format Browse formulae are processed.
!|
!| Next, each field of the BrowseBox is processed. For each field...
!|
!|    The value of the field is placed in the BrowseBox queue.
!|
!| Finally, the POSITION of the current VIEW record is added to the QUEUE
!|
  BRW2::CLA:ClassNumber = CLA:ClassNumber
  BRW2::COU:Description = COU:Description
  BRW2::CLA:RoomNumber = CLA:RoomNumber
  BRW2::CLA:ScheduledTime = CLA:ScheduledTime
  BRW2::CLA:TeacherNumber = CLA:TeacherNumber
  BRW2::COU:Number = COU:Number
  BRW2::Position = POSITION(BRW2::View:Browse)
!----------------------------------------------------------------------
BRW2::PostNewSelection ROUTINE
!|
!| This routine is used to post the NewSelection EVENT to the window. Because we only want this
!| EVENT processed once, and becuase there are several routines that need to initiate a NewSelection
!| EVENT, we keep a flag that tells us if the EVENT is already waiting to be processed. The EVENT is
!| only POSTed if this flag is false.
!|
  IF NOT BRW2::NewSelectPosted
    BRW2::NewSelectPosted = True
    POST(EVENT:NewSelection,?Browse:2)
  END
!----------------------------------------------------------------------
BRW2::NewSelection ROUTINE
!|
!| This routine performs any window bookkeeping necessary when a new record is selected in the
!| BrowseBox.
!| 1. If the new selection is made with the right mouse button, the popup menu (if applicable) is
!|    processed.
!| 2. The current record is retrieved into the buffer using the BRW2::FillBuffer ROUTINE.
!|    After this, the current vertical scrollbar position is computed, and the scrollbar positioned.
!|
  IF NOT BRW2::ActiveInvisible THEN
     IF NOT ?Browse:2{PROP:Visible} THEN
        BRW2::LoadPending = True
        EXIT
     END
  END
  BRW2::NewSelectPosted = False
  IF KEYCODE() = MouseRight OR KEYCODE() = AppsKey
     SETKEYCODE(0)
    BRW2::PopupText = ''
    IF BRW2::RecordCount
      IF BRW2::PopupText
        BRW2::PopupText = '&Insert|&Change|&Delete|-|' & BRW2::PopupText
      ELSE
        BRW2::PopupText = '&Insert|&Change|&Delete'
      END
    ELSE
      IF BRW2::PopupText
        BRW2::PopupText = '&Insert|~&Change|~&Delete|-|' & BRW2::PopupText
      ELSE
        BRW2::PopupText = '&Insert|~&Change|~&Delete'
      END
    END
    EXECUTE(POPUP(BRW2::PopupText))
      POST(EVENT:Accepted,?Insert:3)
      POST(EVENT:Accepted,?Change:3)
      POST(EVENT:Accepted,?Delete:3)
    END
  ELSIF BRW2::RecordCount
    BRW2::CurrentChoice = CHOICE(?Browse:2)
    GET(Queue:Browse:2,BRW2::CurrentChoice)
    DO BRW2::FillBuffer
    IF BRW2::RecordCount = ?Browse:2{PROP:Items}
      IF NOT ?Browse:2{PROP:VScroll}
        ?Browse:2{PROP:VScroll} = True
      END
      CASE BRW2::SortOrder
      OF 1
        LOOP BRW2::CurrentScroll = 1 TO 100
          IF BRW2::Sort1:KeyDistribution[BRW2::CurrentScroll] => CLA:TeacherNumber
            IF BRW2::CurrentScroll <= 1
              BRW2::CurrentScroll = 0
            ELSIF BRW2::CurrentScroll = 100
              BRW2::CurrentScroll = 100
            ELSE
            END
            BREAK
          END
        END
      END
    ELSE
      IF ?Browse:2{PROP:VScroll}
        ?Browse:2{PROP:VScroll} = False
      END
    END
    DO RefreshWindow
  END
!---------------------------------------------------------------------
BRW2::ProcessScroll ROUTINE
!|
!| This routine processes any of the six scrolling EVENTs handled by the BrowseBox.
!| If one record is to be scrolled, the ROUTINE BRW2::ScrollOne is called.
!| If a page of records is to be scrolled, the ROUTINE BRW2::ScrollPage is called.
!| If the first or last page is to be displayed, the ROUTINE BRW2::ScrollEnd is called.
!|
!| If an incremental locator is in use, the value of that locator is cleared.
!| Finally, if a Fixed Thumb vertical scroll bar is used, the thumb is positioned.
!|
  IF BRW2::RecordCount
    BRW2::CurrentEvent = EVENT()
    CASE BRW2::CurrentEvent
    OF EVENT:ScrollUp OROF EVENT:ScrollDown
      DO BRW2::ScrollOne
    OF EVENT:PageUp OROF EVENT:PageDown
      DO BRW2::ScrollPage
    OF EVENT:ScrollTop OROF EVENT:ScrollBottom
      DO BRW2::ScrollEnd
    END
    ?Browse:2{PROP:SelStart} = BRW2::CurrentChoice
    DO BRW2::PostNewSelection
  END
!----------------------------------------------------------------------
BRW2::ScrollOne ROUTINE
!|
!| This routine is used to scroll a single record on the BrowseBox. Since the BrowseBox is an IMM
!| listbox, all scrolling must be handled in code. When called, this routine...
!|
!| 1. Sees if scrolling in the intended direction will cause the listbox display to shift. If not,
!|    the routine moves the list box cursor and exits.
!| 2. Calls BRW2::FillRecord to retrieve one record in the direction required.
!|
  IF BRW2::CurrentEvent = EVENT:ScrollUp AND BRW2::CurrentChoice > 1
    BRW2::CurrentChoice -= 1
    EXIT
  ELSIF BRW2::CurrentEvent = EVENT:ScrollDown AND BRW2::CurrentChoice < BRW2::RecordCount
    BRW2::CurrentChoice += 1
    EXIT
  END
  BRW2::ItemsToFill = 1
  BRW2::FillDirection = BRW2::CurrentEvent - 2
  DO BRW2::FillRecord
!----------------------------------------------------------------------
BRW2::ScrollPage ROUTINE
!|
!| This routine is used to scroll a single page of records on the BrowseBox. Since the BrowseBox is
!| an IMM listbox, all scrolling must be handled in code. When called, this routine...
!|
!| 1. Calls BRW2::FillRecord to retrieve one page of records in the direction required.
!| 2. If BRW2::FillRecord doesn't fill a page (BRW2::ItemsToFill > 0), then
!|    the list-box cursor ia shifted.
!|
  BRW2::ItemsToFill = ?Browse:2{PROP:Items}
  BRW2::FillDirection = BRW2::CurrentEvent - 4
  DO BRW2::FillRecord                           ! Fill with next read(s)
  IF BRW2::ItemsToFill
    IF BRW2::CurrentEvent = EVENT:PageUp
      BRW2::CurrentChoice -= BRW2::ItemsToFill
      IF BRW2::CurrentChoice < 1
        BRW2::CurrentChoice = 1
      END
    ELSE
      BRW2::CurrentChoice += BRW2::ItemsToFill
      IF BRW2::CurrentChoice > BRW2::RecordCount
        BRW2::CurrentChoice = BRW2::RecordCount
      END
    END
  END
!----------------------------------------------------------------------
BRW2::ScrollEnd ROUTINE
!|
!| This routine is used to load the first or last page of the displayable set of records.
!| Since the BrowseBox is an IMM listbox, all scrolling must be handled in code. When called,
!| this routine...
!|
!| 1. Resets the BrowseBox VIEW to insure that it reads from the end of the current sort order.
!| 2. Calls BRW2::FillRecord to retrieve one page of records.
!| 3. Selects the record that represents the end of the view. That is, if the first page was loaded,
!|    the first record is highlighted. If the last was loaded, the last record is highlighted.
!|
  FREE(Queue:Browse:2)
  BRW2::RecordCount = 0
  DO BRW2::Reset
  BRW2::ItemsToFill = ?Browse:2{PROP:Items}
  IF BRW2::CurrentEvent = EVENT:ScrollTop
    BRW2::FillDirection = FillForward
  ELSE
    BRW2::FillDirection = FillBackward
  END
  DO BRW2::FillRecord                           ! Fill with next read(s)
  IF BRW2::CurrentEvent = EVENT:ScrollTop
    BRW2::CurrentChoice = 1
  ELSE
    BRW2::CurrentChoice = BRW2::RecordCount
  END
!----------------------------------------------------------------------
BRW2::AlertKey ROUTINE
!|
!| This routine processes any KEYCODEs experienced by the BrowseBox.
!| NOTE: The cursor movement keys are not processed as KEYCODEs. They are processed as the
!|       appropriate BrowseBox scrolling and selection EVENTs.
!| This routine includes handling for double-click. Actually, this handling is in the form of
!| EMBEDs, which are filled by child-control templates.
!| This routine also includes the BrowseBox's locator handling.
!| After a value is entered for locating, this routine sets BRW2::LocateMode to a value
!| of 2 -- EQUATEd to LocateOnValue -- and calls the routine BRW2::LocateRecord.
!|
  IF BRW2::RecordCount
    CASE KEYCODE()                                         ! What keycode was hit
    OF AppsKey
    OROF MouseRight
       IF ?Browse:2{PROPLIST:MouseDownRow}>0
         ?Browse:2{PROP:Selected} = ?Browse:2{PROPLIST:MouseDownRow}
         BRW2::CurrentChoice = CHOICE(?Browse:2)
       END
       DO BRW2::NewSelection
    OF MouseLeft2
      POST(EVENT:Accepted,?Change:3)
      DO BRW2::FillBuffer
    OF InsertKey
      POST(EVENT:Accepted,?Insert:3)
    OF DeleteKey
      POST(EVENT:Accepted,?Delete:3)
    OF CtrlEnter
      POST(EVENT:Accepted,?Change:3)
    END                                                    ! END (What keycode was hit)
  ELSE
    CASE KEYCODE()                                         ! What keycode was hit
    OF AppsKey
    OROF MouseRight
       DO BRW2::NewSelection
    OF InsertKey
      POST(EVENT:Accepted,?Insert:3)
    ELSE                                                   ! ELSE (What keycode was hit)
      CASE BRW2::SortOrder
      OF 1
      END
    END
  END
  DO BRW2::PostNewSelection
!----------------------------------------------------------------------
BRW2::ScrollDrag ROUTINE
!|
!| This routine processes the Vertical Scroll Bar arrays to find the free key field value
!| that corresponds to the current scroll bar position.
!|
!| After the scroll position is computed, and the scroll value found, this routine sets
!| BRW2::LocateMode to that scroll value of 2 -- EQUATEd to LocateOnValue --
!| and calls the routine BRW2::LocateRecord.
!|
  IF ?Browse:2{PROP:VScrollPos} <= 1
    POST(EVENT:ScrollTop,?Browse:2)
  ELSIF ?Browse:2{PROP:VScrollPos} = 100
    POST(EVENT:ScrollBottom,?Browse:2)
  ELSE
    CASE BRW2::SortOrder
    OF 1
      CLA:TeacherNumber = BRW2::Sort1:KeyDistribution[?Browse:2{PROP:VScrollPos}]
      BRW2::LocateMode = LocateOnValue
      DO BRW2::LocateRecord
    END
  END
!----------------------------------------------------------------------
BRW2::FillRecord ROUTINE
!|
!| This routine is used to retrieve a number of records from the VIEW. The number of records
!| retrieved is held in the variable BRW2::ItemsToFill. If more than one record is
!| to be retrieved, QuickScan is used to minimize reads from the disk.
!|
!| If records exist in the queue (in other words, if the browse has been used before), the record
!| at the appropriate end of the list box is retrieved, and the VIEW is reset to read starting
!| at that record.
!|
!| Next, the VIEW is accessed to retrieve BRW2::ItemsToFill records. Normally, this will
!| result in BRW2::ItemsToFill records being read from the VIEW, but if custom filtering
!| or range limiting is used (via the BRW2::ValidateRecord routine) then any number of records
!| might be read.
!|
!| For each good record, if BRW2::AddQueue is true, the queue is filled using the BRW2::FillQueue
!| routine. The record is then added to the queue. If adding this record causes the BrowseBox queue
!| to contain more records than can be displayed, the record at the opposite end of the queue is
!| deleted.
!|
!| The only time BRW2::AddQueue is false is when the BRW2::LocateRecord routine needs to
!| get the closest record to its record to be located. At this time, the record doesn't need to be
!| added to the queue, so it isn't.
!|
  IF BRW2::RecordCount
    IF BRW2::FillDirection = FillForward
      GET(Queue:Browse:2,BRW2::RecordCount)                ! Get the first queue item
    ELSE
      GET(Queue:Browse:2,1)                                ! Get the first queue item
    END
    RESET(BRW2::View:Browse,BRW2::Position)                ! Reset for sequential processing
    BRW2::SkipFirst = TRUE
  ELSE
    BRW2::SkipFirst = FALSE
  END
  LOOP WHILE BRW2::ItemsToFill
    IF BRW2::View:Browse{PROP:IPRequestCount} = 0
       BRW2::View:Browse{PROP:IPRequestCount} = BRW2::ItemsToFill
    END
    IF BRW2::FillDirection = FillForward
      NEXT(BRW2::View:Browse)
    ELSE
      PREVIOUS(BRW2::View:Browse)
    END
    IF ERRORCODE()
      IF ERRORCODE() = BadRecErr
        DO BRW2::RestoreResetValues
        BREAK
      ELSE
        StandardWarning(Warn:RecordFetchError,'Classes')
        POST(EVENT:CloseWindow)
        EXIT
      END
    END
    IF BRW2::SkipFirst
       BRW2::SkipFirst = FALSE
       IF POSITION(BRW2::View:Browse) = BRW2::Position
          CYCLE
       END
    END
    IF BRW2::AddQueue
      IF BRW2::RecordCount = ?Browse:2{PROP:Items}
        IF BRW2::FillDirection = FillForward
          GET(Queue:Browse:2,1)                            ! Get the first queue item
        ELSE
          GET(Queue:Browse:2,BRW2::RecordCount)            ! Get the first queue item
        END
        DELETE(Queue:Browse:2)
        BRW2::RecordCount -= 1
      END
      DO BRW2::FillQueue
      IF BRW2::FillDirection = FillForward
        ADD(Queue:Browse:2)
      ELSE
        ADD(Queue:Browse:2,1)
      END
      BRW2::RecordCount += 1
    END
    BRW2::ItemsToFill -= 1
  END
  BRW2::AddQueue = True
  EXIT
!----------------------------------------------------------------------
BRW2::LocateRecord ROUTINE
!|
!| This routine is used to find a record in the VIEW, and to display that record
!| in the BrowseBox.
!|
!| This routine has three different modes of operation, which are invoked based on
!| the setting of BRW2::LocateMode. These modes are...
!|
!|   LocateOnPosition (1) - This mode is still supported for 1.5 compatability. This mode
!|                          is the same as LocateOnEdit.
!|   LocateOnValue    (2) - The values of the current sort order key are used. This mode
!|                          used for Locators and when the BrowseBox is called to select
!|                          a record.
!|   LocateOnEdit     (3) - The current record of the VIEW is used. This mode assumes
!|                          that there is an active VIEW record. This mode is used when
!|                          the sort order of the BrowseBox has changed
!|
!| If an appropriate record has been located, the BRW2::RefreshPage routine is
!| called to load the page containing the located record.
!|
!| If an appropriate record is not locate, the last page of the BrowseBox is loaded.
!|
  IF BRW2::LocateMode = LocateOnPosition
    BRW2::LocateMode = LocateOnEdit
  END
  CLOSE(BRW2::View:Browse)
  CASE BRW2::SortOrder
  OF 1
    IF BRW2::UsingAdditionalSortOrder THEN
       BRW2::UsingAdditionalSortOrder = False
       BRW2::View:Browse{PROP:Order} = '+CLA:TeacherNumber'
    END
    IF BRW2::LocateMode = LocateOnEdit
      BRW2::HighlightedPosition = POSITION(CLA:KeyTeacherNumber)
      RESET(CLA:KeyTeacherNumber,BRW2::HighlightedPosition)
    ELSE
      CLA:TeacherNumber = TEA:Number
      SET(CLA:KeyTeacherNumber,CLA:KeyTeacherNumber)
    END
    BRW2::View:Browse{Prop:Filter} = 'CLA:TeacherNumber = BRW2::Sort1:Reset:TEA:Number'
  END
  IF ERRORCODE()
    StandardWarning(Warn:ViewOpenError)
  END
  OPEN(BRW2::View:Browse)
  IF ERRORCODE()
    StandardWarning(Warn:ViewOpenError)
  END
  IF BRW2::UsingAdditionalSortOrder = True
    CASE BRW2::SortOrder
    OF 1
       SET(BRW2::View:Browse,1)
    END
  END
  FREE(Queue:Browse:2)
  BRW2::RecordCount = 0
  BRW2::ItemsToFill = 1
  BRW2::FillDirection = FillForward                        ! Fill with next read(s)
  BRW2::AddQueue = False
  DO BRW2::FillRecord                                      ! Fill with next read(s)
  BRW2::AddQueue = True
  IF BRW2::ItemsToFill
    BRW2::RefreshMode = RefreshOnBottom
    DO BRW2::RefreshPage
  ELSE
    BRW2::RefreshMode = RefreshOnPosition
    DO BRW2::RefreshPage
  END
  DO BRW2::PostNewSelection
  BRW2::LocateMode = 0
  EXIT
!----------------------------------------------------------------------
BRW2::RefreshPage ROUTINE
!|
!| This routine is used to load a single page of the BrowseBox.
!|
!| If this routine is called with a BRW2::RefreshMode of RefreshOnPosition,
!| the active VIEW record is loaded at the top of the page. Otherwise, if there are
!| records in the browse queue (Queue:Browse:2), then the current page is reloaded, and the
!| currently selected item remains selected.
!|
  IF NOT BRW2::ActiveInvisible THEN
     IF NOT ?Browse:2{PROP:Visible} THEN
        BRW2::LoadPending = True
        EXIT
     END
  END
  SETCURSOR(Cursor:Wait)
  IF BRW2::RefreshMode = RefreshOnPosition
    BRW2::HighlightedPosition = POSITION(BRW2::View:Browse)
    RESET(BRW2::View:Browse,BRW2::HighlightedPosition)
    BRW2::RefreshMode = RefreshOnTop
  ELSIF RECORDS(Queue:Browse:2)
    GET(Queue:Browse:2,BRW2::CurrentChoice)
    IF ERRORCODE()
      GET(Queue:Browse:2,RECORDS(Queue:Browse:2))
    END
    BRW2::HighlightedPosition = BRW2::Position
    GET(Queue:Browse:2,1)
    RESET(BRW2::View:Browse,BRW2::Position)
    BRW2::RefreshMode = RefreshOnCurrent
  ELSE
    BRW2::HighlightedPosition = ''
    DO BRW2::Reset
  END
  FREE(Queue:Browse:2)
  BRW2::RecordCount = 0
  BRW2::ItemsToFill = ?Browse:2{PROP:Items}
  IF BRW2::RefreshMode = RefreshOnBottom
    BRW2::FillDirection = FillBackward
  ELSE
    BRW2::FillDirection = FillForward
  END
  DO BRW2::FillRecord                                      ! Fill with next read(s)
  IF BRW2::HighlightedPosition
    IF BRW2::ItemsToFill
      IF NOT BRW2::RecordCount
        DO BRW2::Reset
      END
      IF BRW2::RefreshMode = RefreshOnBottom
        BRW2::FillDirection = FillForward
      ELSE
        BRW2::FillDirection = FillBackward
      END
      DO BRW2::FillRecord
    END
  END
  IF BRW2::RecordCount
    IF BRW2::HighlightedPosition
      LOOP BRW2::CurrentChoice = 1 TO BRW2::RecordCount
        GET(Queue:Browse:2,BRW2::CurrentChoice)
        IF BRW2::Position = BRW2::HighlightedPosition THEN BREAK.
      END
      IF BRW2::CurrentChoice > BRW2::RecordCount
        BRW2::CurrentChoice = BRW2::RecordCount
      END
    ELSE
      IF BRW2::RefreshMode = RefreshOnBottom
        BRW2::CurrentChoice = RECORDS(Queue:Browse:2)
      ELSE
        BRW2::CurrentChoice = 1
      END
    END
    ?Browse:2{Prop:Selected} = BRW2::CurrentChoice
    DO BRW2::FillBuffer
    ?Change:3{PROP:Disable} = 0
    ?Delete:3{PROP:Disable} = 0
  ELSE
    CLEAR(CLA:Record)
    BRW2::CurrentChoice = 0
    ?Change:3{PROP:Disable} = 1
    ?Delete:3{PROP:Disable} = 1
  END
  SETCURSOR()
  BRW2::RefreshMode = 0
  EXIT
BRW2::Reset ROUTINE
!|
!| This routine is used to reset the VIEW used by the BrowseBox.
!|
  IF NOT BRW2::ActiveInvisible THEN
     IF NOT ?Browse:2{PROP:Visible} THEN
        BRW2::LoadPending = True
        EXIT
     END
  END
  BRW2::LoadPending = False
  CLOSE(BRW2::View:Browse)
  CASE BRW2::SortOrder
  OF 1
    IF BRW2::UsingAdditionalSortOrder THEN
       BRW2::UsingAdditionalSortOrder = False
       BRW2::View:Browse{PROP:Order} = '+CLA:TeacherNumber'
    END
    CLA:TeacherNumber = TEA:Number
    SET(CLA:KeyTeacherNumber)
    BRW2::View:Browse{Prop:Filter} = 'CLA:TeacherNumber = BRW2::Sort1:Reset:TEA:Number'
  END
  IF ERRORCODE()
    StandardWarning(Warn:ViewOpenError)
  END
  OPEN(BRW2::View:Browse)
  IF ERRORCODE()
    StandardWarning(Warn:ViewOpenError)
  END
  SET(BRW2::View:Browse)
!----------------------------------------------------------------------
BRW2::GetRecord ROUTINE
!|
!| This routine is used to retrieve the VIEW record that corresponds to a
!| chosen listbox record.
!|
  IF BRW2::RecordCount
    BRW2::CurrentChoice = CHOICE(?Browse:2)
    GET(Queue:Browse:2,BRW2::CurrentChoice)
    WATCH(BRW2::View:Browse)
    REGET(BRW2::View:Browse,BRW2::Position)
  END
!----------------------------------------------------------------------
BRW2::RestoreResetValues ROUTINE
!|
!| This routine is used to restore reset values to their saved value
!| after a bad record access from the VIEW.
!|
  CASE BRW2::SortOrder
  OF 1
    TEA:Number = BRW2::Sort1:Reset:TEA:Number
  END
!--------------------------------------------------------------------------
DisplayBrowseToolbar      ROUTINE
  ENABLE(TBarBrwBottom,TBarBrwLocate)
  TBarBrwHelp{PROP:DISABLE}=?Help{PROP:DISABLE}
  IF BrowseButtons.InsertButton THEN
    TBarBrwInsert{PROP:Disable} = BrowseButtons.InsertButton{PROP:Disable}
  END
  IF BrowseButtons.ChangeButton THEN
    TBarBrwChange{PROP:Disable} = BrowseButtons.ChangeButton{PROP:Disable}
  END
  IF BrowseButtons.DeleteButton THEN
    TBarBrwDelete{PROP:Disable} = BrowseButtons.DeleteButton{PROP:Disable}
  END
  DISABLE(TBarBrwHistory)
  ToolBarMode = BrowseMode
  TBarBrwDown{PROP:ToolTip} = 'Go to the Next Record'
  TBarBrwBottom{PROP:ToolTip} = 'Go to the Last Page'
  TBarBrwTop{PROP:ToolTip} = 'Go to the First Page'
  TBarBrwPageDown{PROP:ToolTip} = 'Go to the Next Page'
  TBarBrwPageUp{PROP:ToolTip} = 'Go to the Prior Page'
  TBarBrwDown{PROP:ToolTip} = 'Go to the Next Record'
  TBarBrwUP{PROP:ToolTip} = 'Go to the Prior Record'
  TBarBrwInsert{PROP:ToolTip} = 'Insert a new Record'
  DISPLAY(TBarBrwFirst,TBarBrwLast)
!--------------------------------------------------------------------------
ListBoxDispatch ROUTINE
  DO DisplayBrowseToolbar
  IF FOCUS() <> BrowseButtons.ListBox THEN  ! List box must have focus when on update form
    EXIT
  END
  IF ACCEPTED() THEN            !trap remote browse box control calls
    EXECUTE(ACCEPTED()-TBarBrwBottom+1)
      POST(EVENT:ScrollBottom,BrowseButtons.ListBox)
      POST(EVENT:ScrollTop,BrowseButtons.ListBox)
      POST(EVENT:PageDown,BrowseButtons.ListBox)
      POST(EVENT:PageUp,BrowseButtons.ListBox)
      POST(EVENT:ScrollDown,BrowseButtons.ListBox)
      POST(EVENT:ScrollUp,BrowseButtons.ListBox)
      POST(EVENT:Locate,BrowseButtons.ListBox)
      BEGIN                     !EXECUTE Place Holder - Ditto has no effect on a browse
      END
      PRESSKEY(F1Key)
    END
  END

UpdateDispatch ROUTINE
  DISABLE(TBarBrwDelete)
  DISABLE(TBarBrwChange)
  IF BrowseButtons.DeleteButton AND BrowseButtons.DeleteButton{PROP:Disable} = 0 THEN
    ENABLE(TBarBrwDelete)
  END
  IF BrowseButtons.ChangeButton AND BrowseButtons.ChangeButton{PROP:Disable} = 0 THEN
    ENABLE(TBarBrwChange)
  END
  IF FOCUS() <> BrowseButtons.ListBox THEN  ! List box must have focus when on update form
    EXIT
  END
  IF INRANGE(ACCEPTED(),TBarBrwInsert,TBarBrwDelete) THEN         !trap remote browse update control calls
    EXECUTE(ACCEPTED()-TBarBrwInsert+1)
      POST(EVENT:Accepted,BrowseButtons.InsertButton)
      POST(EVENT:Accepted,BrowseButtons.ChangeButton)
      POST(EVENT:Accepted,BrowseButtons.DeleteButton)
    END
  END
!----------------------------------------------------------------
BRW2::ButtonInsert ROUTINE
!|
!| This routine calls the BrowseBox's update procedure (as specified in the
!| BrowseUpdateButtons control template) to insert a new record.
!|
!| First, the primary file's record  buffer is cleared, as well as any memos
!| or BLOBs. Next, any range-limit values are restored so that the inserted
!| record defaults to being added to the current display set.
!|
!| Next, LocalRequest is set to InsertRecord, and the BRW2::CallRecord routine
!| is called. This routine performs the actual call to the update procedure.
!|
!| If the insert is successful (GlobalRequest = RequestCompleted) then the newly added
!| record is displayed in the BrowseBox, at the top of the listbox.
!|
!| If the insert is not successful, the current page of the browse is refreshed.
!|
!| Finally, The BrowseBox is re-initialized, resetting scroll bars and totals.
!|
  GET(Classes,0)
  CLEAR(CLA:Record,0)
  CASE BRW2::SortOrder
  OF 1
    CLA:TeacherNumber = BRW2::Sort1:Reset:TEA:Number
  END
  LocalRequest = InsertRecord
  DO BRW2::CallUpdate
  IF GlobalResponse = RequestCompleted
    BRW2::LocateMode = LocateOnEdit
    DO BRW2::LocateRecord
  ELSE
    BRW2::RefreshMode = RefreshOnQueue
    DO BRW2::RefreshPage
  END
  DO BRW2::InitializeBrowse
  DO BRW2::PostNewSelection
  SELECT(?Browse:2)
  LocalRequest = OriginalRequest
  LocalResponse = RequestCancelled
  DO RefreshWindow
!----------------------------------------------------------------
BRW2::ButtonChange ROUTINE
!|
!| This routine calls the BrowseBox's update procedure (as specified in the
!| BrowseUpdateButtons control template) to change a selected record.
!|
!| Whenever a button is pressed, the first thing that happens is that the
!| SyncWindow routine is called. This routine insures that the BrowseBox's
!| VIEW corresponds to the highlighted record by calling the BRW2::GetRecord routine.
!|
!| First, LocalRequest is set to ChangeRecord, and the BRW2::CallRecord routine
!| is called. This routine performs the actual call to the update procedure.
!|
!| If the change is successful (GlobalRequest = RequestCompleted) then the newly changed
!| record is displayed in the BrowseBox.
!|
!| If the change is not successful, the current page of the browse is refreshed.
!|
!| Finally, The BrowseBox is re-initialized, resetting scroll bars and totals.
!|
  LocalRequest = ChangeRecord
  DO BRW2::CallUpdate
  IF GlobalResponse = RequestCompleted
    BRW2::LocateMode = LocateOnEdit
    DO BRW2::LocateRecord
  ELSE
    BRW2::RefreshMode = RefreshOnQueue
    DO BRW2::RefreshPage
  END
  DO BRW2::InitializeBrowse
  DO BRW2::PostNewSelection
  SELECT(?Browse:2)
  LocalRequest = OriginalRequest
  LocalResponse = RequestCancelled
  DO RefreshWindow
!----------------------------------------------------------------
BRW2::ButtonDelete ROUTINE
!|
!| This routine calls the BrowseBox's update procedure (as specified in the
!| BrowseUpdateButtons control template) to delete a selected record.
!|
!| Whenever a button is pressed, the first thing that happens is that the
!| SyncWindow routine is called. This routine insures that the BrowseBox's
!| VIEW corresponds to the highlighted record by calling the BRW2::GetRecord routine.
!|
!| First, LocalRequest is set to DeleteRecord, and the BRW2::CallRecord routine
!| is called. This routine performs the actual call to the update procedure.
!|
!| If the delete is successful (GlobalRequest = RequestCompleted) then the deleted record is
!| removed from the queue.
!|
!| Next, the BrowseBox is refreshed, redisplaying the current page.
!|
!| Finally, The BrowseBox is re-initialized, resetting scroll bars and totals.
!|
  LocalRequest = DeleteRecord
  DO BRW2::CallUpdate
  IF GlobalResponse = RequestCompleted
    DELETE(Queue:Browse:2)
    BRW2::RecordCount -= 1
  END
  BRW2::RefreshMode = RefreshOnQueue
  DO BRW2::RefreshPage
  DO BRW2::InitializeBrowse
  DO BRW2::PostNewSelection
  SELECT(?Browse:2)
  LocalRequest = OriginalRequest
  LocalResponse = RequestCancelled
  DO RefreshWindow
!----------------------------------------------------------------
BRW2::CallUpdate ROUTINE
!|
!| This routine performs the actual call to the update procedure.
!|
!| The first thing that happens is that the VIEW is closed. This is performed just in case
!| the VIEW is still open.
!|
!| Next, GlobalRequest is set the the value of LocalRequest, and the update procedure
!| (UpdateClasses) is called.
!|
!| Upon return from the update, the routine BRW2::Reset is called to reset the VIEW
!| and reopen it.
!|
  CLOSE(BRW2::View:Browse)
  LOOP
    GlobalRequest = LocalRequest
    VCRRequest = VCRNone
    UpdateClasses
    LocalResponse = GlobalResponse
    CASE VCRRequest
    OF VCRNone
      BREAK
    OF VCRInsert
      IF LocalRequest = ChangeRecord THEN
        LocalRequest = InsertRecord
      END
    OROF VCRForward
      IF LocalRequest = InsertRecord THEN
        GET(Classes,0)
        CLEAR(CLA:Record,0)
      ELSE
        DO BRW2::PostVCREdit1
        BRW2::CurrentEvent = EVENT:ScrollDown
        DO BRW2::ScrollOne
        DO BRW2::PostVCREdit2
      END
    OF VCRBackward
      DO BRW2::PostVCREdit1
      BRW2::CurrentEvent = EVENT:ScrollUp
      DO BRW2::ScrollOne
      DO BRW2::PostVCREdit2
    OF VCRPageForward
      DO BRW2::PostVCREdit1
      BRW2::CurrentEvent = EVENT:PageDown
      DO BRW2::ScrollPage
      DO BRW2::PostVCREdit2
    OF VCRPageBackward
      DO BRW2::PostVCREdit1
      BRW2::CurrentEvent = EVENT:PageUp
      DO BRW2::ScrollPage
      DO BRW2::PostVCREdit2
    OF VCRFirst
      DO BRW2::PostVCREdit1
      BRW2::CurrentEvent = EVENT:ScrollTop
      DO BRW2::ScrollEnd
      DO BRW2::PostVCREdit2
    OF VCRLast
      DO BRW2::PostVCREdit1
      BRW2::CurrentEvent = EVENT:ScrollBottom
      DO BRW2::ScrollEND
      DO BRW2::PostVCREdit2
    END
  END
  DO BRW2::Reset

BRW2::PostVCREdit1 ROUTINE
  DO BRW2::Reset
  BRW2::LocateMode = LocateOnEdit
  DO BRW2::LocateRecord
  DO RefreshWindow

BRW2::PostVCREdit2 ROUTINE
  ?Browse:2{PROP:SelStart} = BRW2::CurrentChoice
  DO BRW2::NewSelection
  REGET(BRW2::View:Browse,BRW2::Position)
  CLOSE(BRW2::View:Browse)

!|
!| Copies a field from save buffer to actual buffer switched on current field
!|
HistoryField  ROUTINE
  CASE FOCUS()
  OF ?TEA:FirstName
    TEA:FirstName = History::TEA:Record.FirstName
  OF ?TEA:LastName
    TEA:LastName = History::TEA:Record.LastName
  OF ?TEA:Address
    TEA:Address = History::TEA:Record.Address
  OF ?TEA:City
    TEA:City = History::TEA:Record.City
  OF ?TEA:State
    TEA:State = History::TEA:Record.State
  OF ?TEA:Zip
    TEA:Zip = History::TEA:Record.Zip
  OF ?TEA:Telephone
    TEA:Telephone = History::TEA:Record.Telephone
  OF ?TEA:Department
    TEA:Department = History::TEA:Record.Department
  END
  DISPLAY()
!---------------------------------------------------------------
PrimeFields ROUTINE
!|
!| This routine is called whenever the procedure is called to insert a record.
!|
!| This procedure performs three functions. These functions are..
!|
!|   1. Prime the new record with initial values specified in the dictionary
!|      and under the Field priming on Insert button.
!|   2. Generates any Auto-Increment values needed.
!|   3. Saves a copy of the new record, as primed, for use in batch-adds.
!|
!| If an auto-increment value is generated, this routine will add the new record
!| at this point, keeping its place in the file.
!|
  TEA:Record = SAV::TEA:Record
  SAV::TEA:Record = TEA:Record
ClosingWindow ROUTINE
  Update::Reloop = 0
  IF LocalResponse <> RequestCompleted
    DO CancelAutoIncrement
    IF Save:LastInsertedPosition
      LocalResponse = RequestCompleted
      REGET(Teachers,Save:LastInsertedPosition)
    END
  END

CancelAutoIncrement ROUTINE
  IF LocalResponse <> RequestCompleted
  END
FORM::AssignButtons ROUTINE
  ToolBarMode=FormMode
  DISABLE(TBarBrwFirst,TBarBrwLast)
  ENABLE(TBarBrwHistory)
  CASE OriginalRequest
  OF InsertRecord
    ENABLE(TBarBrwDown)
    ENABLE(TBarBrwInsert)
    TBarBrwDown{PROP:ToolTip}='Save record and add another'
    TBarBrwInsert{PROP:ToolTip}=TBarBrwDown{PROP:ToolTip}
  OF ChangeRecord
    ENABLE(TBarBrwBottom,TBarBrwUp)
    ENABLE(TBarBrwInsert)
    TBarBrwBottom{PROP:ToolTip}='Save changes and go to last record'
    TBarBrwTop{PROP:ToolTip}='Save changes and go to first record'
    TBarBrwPageDown{PROP:ToolTip}='Save changes and page down to record'
    TBarBrwPageUp{PROP:ToolTip}='Save changes and page up to record'
    TBarBrwDown{PROP:ToolTip}='Save changes and go to next record'
    TBarBrwUP{PROP:ToolTip}='Save changes and go to previous record'
    TBarBrwInsert{PROP:ToolTip}='Save this record and add a new one'
  END
  TBarBrwHelp{PROP:Disable}=?Help{PROP:Disable}
  DISPLAY(TBarBrwFirst,TBarBrwLast)

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
SplashProcedureThread LONG
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
                         MENU('&Trees'),USE(?Trees)
                           ITEM('&Class Tree'),USE(?BrowseClassTree)
                           ITEM('&Students Tree'),USE(?BrowseStudentsTree)
                         END
                         MENU('&Browse')
                           ITEM('Students '),USE(?BrowseStudents),MSG('Browse Students')
                           ITEM('Teachers '),USE(?BrowseTeachers),MSG('Browse Teachers')
                           ITEM('Classes '),USE(?BrowseClasses),MSG('Browse Classes')
                           ITEM('Enrollment '),USE(?BrowseEnrollment),MSG('Browse Enrollment')
                           ITEM('Courses '),USE(?BrowseCourses),MSG('Browse Courses')
                           ITEM('Majors '),USE(?BrowseMajors),MSG('Browse Majors')
                           ITEM('Update Grades'),USE(?BrowseUpdateGrades)
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
                           ITEM('&About School...'),USE(?HelpAbout)
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
    OF ?BrowseUpdateGrades
      START(UpdateGrades, 25000)
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
