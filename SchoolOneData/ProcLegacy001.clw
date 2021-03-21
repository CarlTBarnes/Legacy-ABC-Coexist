

                     MEMBER('ProcLegacy.clw')              ! This is a MEMBER module

!!! <summary>
!!! Generated from procedure template - Browse
!!! Browse the Classes File
!!! </summary>
BrowseClasses PROCEDURE

CurrentTab           STRING(80)                            ! 
LocalRequest         LONG                                  ! 
OriginalRequest      LONG                                  ! 
LocalResponse        LONG                                  ! 
FilesOpened          LONG                                  ! 
WindowOpened         LONG                                  ! 
WindowInitialized    LONG                                  ! 
ForceRefresh         LONG                                  ! 
RecordFiltered       LONG                                  ! 

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
WinResize            WindowResizeType
QuickWindow          WINDOW('Browse the Classes File'),AT(,,255,198),FONT('MS Sans Serif',8,COLOR:Black),RESIZE, |
  GRAY,IMM,MDI,HLP('~BrowseClasses'),SYSTEM
                       LIST,AT(8,30,239,143),USE(?Browse:1),HVSCROLL,FORMAT('[49L(1)|M~Class Number~R@P##-####' & |
  '#P@120L|M~Course~@S30@/49R(1)|M~Room~@n4@80R(1)|M~Scheduled Time~R(0)@s20@]|M[80L|M~' & |
  'Instructor~L(2)@S20@/]|M'),FROM(Queue:Browse:1),IMM,MSG('Browsing Records')
                       BUTTON('&Insert'),AT(57,158,45,14),USE(?Insert:2),HIDE
                       BUTTON('&Change'),AT(106,158,45,14),USE(?Change:2),DEFAULT,HIDE
                       BUTTON('&Delete'),AT(155,158,45,14),USE(?Delete:2),HIDE
                       SHEET,AT(4,4,247,172),USE(?CurrentTab)
                         TAB('by Class Number')
                         END
                         TAB('by Course Number')
                         END
                       END
                       BUTTON('Close'),AT(155,180,45,14),USE(?Close)
                       BUTTON('Help'),AT(205,180,45,14),USE(?Help),STD(STD:Help)
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
      IF ToolBarMode = BrowseMode THEN
        DO UpdateDispatch
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
      END
    OF ?Insert:2
      CASE EVENT()
      OF EVENT:Accepted
        DO SyncWindow
        DO BRW1::ButtonInsert
      END
    OF ?Change:2
      CASE EVENT()
      OF EVENT:Accepted
        DO SyncWindow
        DO BRW1::ButtonChange
      END
    OF ?Delete:2
      CASE EVENT()
      OF EVENT:Accepted
        DO SyncWindow
        DO BRW1::ButtonDelete
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
  WinResize.Init(AppStrategy:Spread)
  Do DefineListboxStyle
  ?Browse:1{PROP:Alrt,252} = MouseLeft2
  ?Browse:1{PROP:Alrt,255} = AppsKey
  ?Browse:1{PROP:Alrt,253} = MouseRight
  ?Browse:1{PROP:Alrt,255} = InsertKey
  ?Browse:1{PROP:Alrt,254} = DeleteKey
  ?Browse:1{PROP:Alrt,253} = CtrlEnter
  ?Browse:1{PROP:Alrt,252} = MouseLeft2

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
  BrowseButtons.InsertButton = ?Insert:2
  BrowseButtons.ChangeButton = ?Change:2
  BrowseButtons.DeleteButton = ?Delete:2
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
      IF BRW1::PopupText
        BRW1::PopupText = '&Insert|&Change|&Delete|-|' & BRW1::PopupText
      ELSE
        BRW1::PopupText = '&Insert|&Change|&Delete'
      END
    ELSE
      IF BRW1::PopupText
        BRW1::PopupText = '&Insert|~&Change|~&Delete|-|' & BRW1::PopupText
      ELSE
        BRW1::PopupText = '&Insert|~&Change|~&Delete'
      END
    END
    EXECUTE(POPUP(BRW1::PopupText))
      POST(EVENT:Accepted,?Insert:2)
      POST(EVENT:Accepted,?Change:2)
      POST(EVENT:Accepted,?Delete:2)
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
      POST(EVENT:Accepted,?Change:2)
      DO BRW1::FillBuffer
    OF InsertKey
      POST(EVENT:Accepted,?Insert:2)
    OF DeleteKey
      POST(EVENT:Accepted,?Delete:2)
    OF CtrlEnter
      POST(EVENT:Accepted,?Change:2)
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
    OF InsertKey
      POST(EVENT:Accepted,?Insert:2)
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
    ?Change:2{PROP:Disable} = 0
    ?Delete:2{PROP:Disable} = 0
  ELSE
    CLEAR(CLA:Record)
    BRW1::CurrentChoice = 0
    ?Change:2{PROP:Disable} = 1
    ?Delete:2{PROP:Disable} = 1
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
  IF INRANGE(ACCEPTED(),TBarBrwInsert,TBarBrwDelete) THEN         !trap remote browse update control calls
    EXECUTE(ACCEPTED()-TBarBrwInsert+1)
      POST(EVENT:Accepted,BrowseButtons.InsertButton)
      POST(EVENT:Accepted,BrowseButtons.ChangeButton)
      POST(EVENT:Accepted,BrowseButtons.DeleteButton)
    END
  END
!----------------------------------------------------------------
BRW1::ButtonInsert ROUTINE
!|
!| This routine calls the BrowseBox's update procedure (as specified in the
!| BrowseUpdateButtons control template) to insert a new record.
!|
!| First, the primary file's record  buffer is cleared, as well as any memos
!| or BLOBs. Next, any range-limit values are restored so that the inserted
!| record defaults to being added to the current display set.
!|
!| Next, LocalRequest is set to InsertRecord, and the BRW1::CallRecord routine
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
  LocalRequest = InsertRecord
  DO BRW1::CallUpdate
  IF GlobalResponse = RequestCompleted
    BRW1::LocateMode = LocateOnEdit
    DO BRW1::LocateRecord
  ELSE
    BRW1::RefreshMode = RefreshOnQueue
    DO BRW1::RefreshPage
  END
  DO BRW1::InitializeBrowse
  DO BRW1::PostNewSelection
  SELECT(?Browse:1)
  LocalRequest = OriginalRequest
  LocalResponse = RequestCancelled
  DO RefreshWindow
!----------------------------------------------------------------
BRW1::ButtonChange ROUTINE
!|
!| This routine calls the BrowseBox's update procedure (as specified in the
!| BrowseUpdateButtons control template) to change a selected record.
!|
!| Whenever a button is pressed, the first thing that happens is that the
!| SyncWindow routine is called. This routine insures that the BrowseBox's
!| VIEW corresponds to the highlighted record by calling the BRW1::GetRecord routine.
!|
!| First, LocalRequest is set to ChangeRecord, and the BRW1::CallRecord routine
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
  DO BRW1::CallUpdate
  IF GlobalResponse = RequestCompleted
    BRW1::LocateMode = LocateOnEdit
    DO BRW1::LocateRecord
  ELSE
    BRW1::RefreshMode = RefreshOnQueue
    DO BRW1::RefreshPage
  END
  DO BRW1::InitializeBrowse
  DO BRW1::PostNewSelection
  SELECT(?Browse:1)
  LocalRequest = OriginalRequest
  LocalResponse = RequestCancelled
  DO RefreshWindow
!----------------------------------------------------------------
BRW1::ButtonDelete ROUTINE
!|
!| This routine calls the BrowseBox's update procedure (as specified in the
!| BrowseUpdateButtons control template) to delete a selected record.
!|
!| Whenever a button is pressed, the first thing that happens is that the
!| SyncWindow routine is called. This routine insures that the BrowseBox's
!| VIEW corresponds to the highlighted record by calling the BRW1::GetRecord routine.
!|
!| First, LocalRequest is set to DeleteRecord, and the BRW1::CallRecord routine
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
  DO BRW1::CallUpdate
  IF GlobalResponse = RequestCompleted
    DELETE(Queue:Browse:1)
    BRW1::RecordCount -= 1
  END
  BRW1::RefreshMode = RefreshOnQueue
  DO BRW1::RefreshPage
  DO BRW1::InitializeBrowse
  DO BRW1::PostNewSelection
  SELECT(?Browse:1)
  LocalRequest = OriginalRequest
  LocalResponse = RequestCancelled
  DO RefreshWindow
!----------------------------------------------------------------
BRW1::CallUpdate ROUTINE
!|
!| This routine performs the actual call to the update procedure.
!|
!| The first thing that happens is that the VIEW is closed. This is performed just in case
!| the VIEW is still open.
!|
!| Next, GlobalRequest is set the the value of LocalRequest, and the update procedure
!| (UpdateClasses) is called.
!|
!| Upon return from the update, the routine BRW1::Reset is called to reset the VIEW
!| and reopen it.
!|
  CLOSE(BRW1::View:Browse)
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
        DO BRW1::PostVCREdit1
        BRW1::CurrentEvent = EVENT:ScrollDown
        DO BRW1::ScrollOne
        DO BRW1::PostVCREdit2
      END
    OF VCRBackward
      DO BRW1::PostVCREdit1
      BRW1::CurrentEvent = EVENT:ScrollUp
      DO BRW1::ScrollOne
      DO BRW1::PostVCREdit2
    OF VCRPageForward
      DO BRW1::PostVCREdit1
      BRW1::CurrentEvent = EVENT:PageDown
      DO BRW1::ScrollPage
      DO BRW1::PostVCREdit2
    OF VCRPageBackward
      DO BRW1::PostVCREdit1
      BRW1::CurrentEvent = EVENT:PageUp
      DO BRW1::ScrollPage
      DO BRW1::PostVCREdit2
    OF VCRFirst
      DO BRW1::PostVCREdit1
      BRW1::CurrentEvent = EVENT:ScrollTop
      DO BRW1::ScrollEnd
      DO BRW1::PostVCREdit2
    OF VCRLast
      DO BRW1::PostVCREdit1
      BRW1::CurrentEvent = EVENT:ScrollBottom
      DO BRW1::ScrollEND
      DO BRW1::PostVCREdit2
    END
  END
  DO BRW1::Reset

BRW1::PostVCREdit1 ROUTINE
  DO BRW1::Reset
  BRW1::LocateMode = LocateOnEdit
  DO BRW1::LocateRecord
  DO RefreshWindow

BRW1::PostVCREdit2 ROUTINE
  ?Browse:1{PROP:SelStart} = BRW1::CurrentChoice
  DO BRW1::NewSelection
  REGET(BRW1::View:Browse,BRW1::Position)
  CLOSE(BRW1::View:Browse)

!!! <summary>
!!! Generated from procedure template - Browse
!!! Browse the Courses File
!!! </summary>
BrowseCourses PROCEDURE

CurrentTab           STRING(80)                            ! 
LocalRequest         LONG                                  ! 
OriginalRequest      LONG                                  ! 
LocalResponse        LONG                                  ! 
FilesOpened          LONG                                  ! 
WindowOpened         LONG                                  ! 
WindowInitialized    LONG                                  ! 
ForceRefresh         LONG                                  ! 
RecordFiltered       LONG                                  ! 

BRW1::View:Browse    VIEW(Courses)
                       PROJECT(COU:Number)
                       PROJECT(COU:Description)
                     END
Queue:Browse:1       QUEUE,PRE()                           ! Browsing Queue
BRW1::COU:Number       LIKE(COU:Number)                    ! Queue Display field
BRW1::COU:Description  LIKE(COU:Description)               ! Queue Display field
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
WinResize            WindowResizeType
QuickWindow          WINDOW('Browse the Courses File'),AT(,,159,188),FONT('MS Sans Serif',8,COLOR:Black),RESIZE, |
  GRAY,IMM,MDI,HLP('~BrowseCourses'),SYSTEM
                       LIST,AT(8,20,143,144),USE(?Browse:1),HVSCROLL,FORMAT('32R(1)|M~Number~L(2)@n4@80L(2)|M~' & |
  'Description~@S30@'),FROM(Queue:Browse:1),IMM,MSG('Browsing Records')
                       BUTTON('&Insert'),AT(8,148,45,14),USE(?Insert:2),HIDE
                       BUTTON('&Change'),AT(57,148,45,14),USE(?Change:2),DEFAULT,HIDE
                       BUTTON('&Delete'),AT(106,148,45,14),USE(?Delete:2),HIDE
                       SHEET,AT(4,4,151,162),USE(?CurrentTab)
                         TAB('by Course Description')
                         END
                       END
                       BUTTON('Close'),AT(61,170,45,14),USE(?Close)
                       BUTTON('Help'),AT(110,170,45,14),USE(?Help),STD(STD:Help)
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
      IF ToolBarMode = BrowseMode THEN
        DO UpdateDispatch
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
      END
    OF ?Insert:2
      CASE EVENT()
      OF EVENT:Accepted
        DO SyncWindow
        DO BRW1::ButtonInsert
      END
    OF ?Change:2
      CASE EVENT()
      OF EVENT:Accepted
        DO SyncWindow
        DO BRW1::ButtonChange
      END
    OF ?Delete:2
      CASE EVENT()
      OF EVENT:Accepted
        DO SyncWindow
        DO BRW1::ButtonDelete
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
  WinResize.Init(AppStrategy:Spread)
  Do DefineListboxStyle
  ?Browse:1{PROP:Alrt,252} = MouseLeft2
  ?Browse:1{PROP:Alrt,255} = AppsKey
  ?Browse:1{PROP:Alrt,253} = MouseRight
  ?Browse:1{PROP:Alrt,255} = InsertKey
  ?Browse:1{PROP:Alrt,254} = DeleteKey
  ?Browse:1{PROP:Alrt,253} = CtrlEnter
  ?Browse:1{PROP:Alrt,252} = MouseLeft2

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
  BrowseButtons.InsertButton = ?Insert:2
  BrowseButtons.ChangeButton = ?Change:2
  BrowseButtons.DeleteButton = ?Delete:2
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
  COU:Number = BRW1::COU:Number
  COU:Description = BRW1::COU:Description
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
  BRW1::COU:Number = COU:Number
  BRW1::COU:Description = COU:Description
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
      IF BRW1::PopupText
        BRW1::PopupText = '&Insert|&Change|&Delete|-|' & BRW1::PopupText
      ELSE
        BRW1::PopupText = '&Insert|&Change|&Delete'
      END
    ELSE
      IF BRW1::PopupText
        BRW1::PopupText = '&Insert|~&Change|~&Delete|-|' & BRW1::PopupText
      ELSE
        BRW1::PopupText = '&Insert|~&Change|~&Delete'
      END
    END
    EXECUTE(POPUP(BRW1::PopupText))
      POST(EVENT:Accepted,?Insert:2)
      POST(EVENT:Accepted,?Change:2)
      POST(EVENT:Accepted,?Delete:2)
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
      POST(EVENT:Accepted,?Change:2)
      DO BRW1::FillBuffer
    OF InsertKey
      POST(EVENT:Accepted,?Insert:2)
    OF DeleteKey
      POST(EVENT:Accepted,?Delete:2)
    OF CtrlEnter
      POST(EVENT:Accepted,?Change:2)
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
    OF InsertKey
      POST(EVENT:Accepted,?Insert:2)
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
    ?Change:2{PROP:Disable} = 0
    ?Delete:2{PROP:Disable} = 0
  ELSE
    CLEAR(COU:Record)
    CLEAR(COU:CompleteDescription)
    BRW1::CurrentChoice = 0
    ?Change:2{PROP:Disable} = 1
    ?Delete:2{PROP:Disable} = 1
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
  IF INRANGE(ACCEPTED(),TBarBrwInsert,TBarBrwDelete) THEN         !trap remote browse update control calls
    EXECUTE(ACCEPTED()-TBarBrwInsert+1)
      POST(EVENT:Accepted,BrowseButtons.InsertButton)
      POST(EVENT:Accepted,BrowseButtons.ChangeButton)
      POST(EVENT:Accepted,BrowseButtons.DeleteButton)
    END
  END
!----------------------------------------------------------------
BRW1::ButtonInsert ROUTINE
!|
!| This routine calls the BrowseBox's update procedure (as specified in the
!| BrowseUpdateButtons control template) to insert a new record.
!|
!| First, the primary file's record  buffer is cleared, as well as any memos
!| or BLOBs. Next, any range-limit values are restored so that the inserted
!| record defaults to being added to the current display set.
!|
!| Next, LocalRequest is set to InsertRecord, and the BRW1::CallRecord routine
!| is called. This routine performs the actual call to the update procedure.
!|
!| If the insert is successful (GlobalRequest = RequestCompleted) then the newly added
!| record is displayed in the BrowseBox, at the top of the listbox.
!|
!| If the insert is not successful, the current page of the browse is refreshed.
!|
!| Finally, The BrowseBox is re-initialized, resetting scroll bars and totals.
!|
  GET(Courses,0)
  CLEAR(COU:Record,0)
  CLEAR(COU:CompleteDescription)
  LocalRequest = InsertRecord
  DO BRW1::CallUpdate
  IF GlobalResponse = RequestCompleted
    BRW1::LocateMode = LocateOnEdit
    DO BRW1::LocateRecord
  ELSE
    BRW1::RefreshMode = RefreshOnQueue
    DO BRW1::RefreshPage
  END
  DO BRW1::InitializeBrowse
  DO BRW1::PostNewSelection
  SELECT(?Browse:1)
  LocalRequest = OriginalRequest
  LocalResponse = RequestCancelled
  DO RefreshWindow
!----------------------------------------------------------------
BRW1::ButtonChange ROUTINE
!|
!| This routine calls the BrowseBox's update procedure (as specified in the
!| BrowseUpdateButtons control template) to change a selected record.
!|
!| Whenever a button is pressed, the first thing that happens is that the
!| SyncWindow routine is called. This routine insures that the BrowseBox's
!| VIEW corresponds to the highlighted record by calling the BRW1::GetRecord routine.
!|
!| First, LocalRequest is set to ChangeRecord, and the BRW1::CallRecord routine
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
  DO BRW1::CallUpdate
  IF GlobalResponse = RequestCompleted
    BRW1::LocateMode = LocateOnEdit
    DO BRW1::LocateRecord
  ELSE
    BRW1::RefreshMode = RefreshOnQueue
    DO BRW1::RefreshPage
  END
  DO BRW1::InitializeBrowse
  DO BRW1::PostNewSelection
  SELECT(?Browse:1)
  LocalRequest = OriginalRequest
  LocalResponse = RequestCancelled
  DO RefreshWindow
!----------------------------------------------------------------
BRW1::ButtonDelete ROUTINE
!|
!| This routine calls the BrowseBox's update procedure (as specified in the
!| BrowseUpdateButtons control template) to delete a selected record.
!|
!| Whenever a button is pressed, the first thing that happens is that the
!| SyncWindow routine is called. This routine insures that the BrowseBox's
!| VIEW corresponds to the highlighted record by calling the BRW1::GetRecord routine.
!|
!| First, LocalRequest is set to DeleteRecord, and the BRW1::CallRecord routine
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
  DO BRW1::CallUpdate
  IF GlobalResponse = RequestCompleted
    DELETE(Queue:Browse:1)
    BRW1::RecordCount -= 1
  END
  BRW1::RefreshMode = RefreshOnQueue
  DO BRW1::RefreshPage
  DO BRW1::InitializeBrowse
  DO BRW1::PostNewSelection
  SELECT(?Browse:1)
  LocalRequest = OriginalRequest
  LocalResponse = RequestCancelled
  DO RefreshWindow
!----------------------------------------------------------------
BRW1::CallUpdate ROUTINE
!|
!| This routine performs the actual call to the update procedure.
!|
!| The first thing that happens is that the VIEW is closed. This is performed just in case
!| the VIEW is still open.
!|
!| Next, GlobalRequest is set the the value of LocalRequest, and the update procedure
!| (UpdateCourses) is called.
!|
!| Upon return from the update, the routine BRW1::Reset is called to reset the VIEW
!| and reopen it.
!|
  CLOSE(BRW1::View:Browse)
  LOOP
    GlobalRequest = LocalRequest
    VCRRequest = VCRNone
    UpdateCourses
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
        GET(Courses,0)
        CLEAR(COU:Record,0)
        CLEAR(COU:CompleteDescription)
      ELSE
        DO BRW1::PostVCREdit1
        BRW1::CurrentEvent = EVENT:ScrollDown
        DO BRW1::ScrollOne
        DO BRW1::PostVCREdit2
      END
    OF VCRBackward
      DO BRW1::PostVCREdit1
      BRW1::CurrentEvent = EVENT:ScrollUp
      DO BRW1::ScrollOne
      DO BRW1::PostVCREdit2
    OF VCRPageForward
      DO BRW1::PostVCREdit1
      BRW1::CurrentEvent = EVENT:PageDown
      DO BRW1::ScrollPage
      DO BRW1::PostVCREdit2
    OF VCRPageBackward
      DO BRW1::PostVCREdit1
      BRW1::CurrentEvent = EVENT:PageUp
      DO BRW1::ScrollPage
      DO BRW1::PostVCREdit2
    OF VCRFirst
      DO BRW1::PostVCREdit1
      BRW1::CurrentEvent = EVENT:ScrollTop
      DO BRW1::ScrollEnd
      DO BRW1::PostVCREdit2
    OF VCRLast
      DO BRW1::PostVCREdit1
      BRW1::CurrentEvent = EVENT:ScrollBottom
      DO BRW1::ScrollEND
      DO BRW1::PostVCREdit2
    END
  END
  DO BRW1::Reset

BRW1::PostVCREdit1 ROUTINE
  DO BRW1::Reset
  BRW1::LocateMode = LocateOnEdit
  DO BRW1::LocateRecord
  DO RefreshWindow

BRW1::PostVCREdit2 ROUTINE
  ?Browse:1{PROP:SelStart} = BRW1::CurrentChoice
  DO BRW1::NewSelection
  REGET(BRW1::View:Browse,BRW1::Position)
  CLOSE(BRW1::View:Browse)

!!! <summary>
!!! Generated from procedure template - Browse
!!! Browse the Enrollment File
!!! </summary>
BrowseEnrollment PROCEDURE

CurrentTab           STRING(80)                            ! 
LocalRequest         LONG                                  ! 
OriginalRequest      LONG                                  ! 
LocalResponse        LONG                                  ! 
FilesOpened          LONG                                  ! 
WindowOpened         LONG                                  ! 
WindowInitialized    LONG                                  ! 
ForceRefresh         LONG                                  ! 
RecordFiltered       LONG                                  ! 

BRW1::View:Browse    VIEW(Enrollment)
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
Queue:Browse:1       QUEUE,PRE()                           ! Browsing Queue
BRW1::ENR:StudentNumber LIKE(ENR:StudentNumber)            ! Queue Display field
BRW1::STU:LastName     LIKE(STU:LastName)                  ! Queue Display field
BRW1::STU:FirstName    LIKE(STU:FirstName)                 ! Queue Display field
BRW1::ENR:MidtermExam  LIKE(ENR:MidtermExam)               ! Queue Display field
BRW1::ENR:FinalExam    LIKE(ENR:FinalExam)                 ! Queue Display field
BRW1::ENR:TermPaper    LIKE(ENR:TermPaper)                 ! Queue Display field
BRW1::ENR:ClassNumber  LIKE(ENR:ClassNumber)               ! Queue Display field
BRW1::STU:Number       LIKE(STU:Number)                    ! Queue Display field
BRW1::Mark             BYTE                                ! Record mark flag
BRW1::Position         STRING(1024)                        ! Queue POSITION information
                     END                                   ! END (Browsing Queue)
BRW1::UsingAdditionalSortOrder BYTE                        ! When true the view is using PROP:Order
BRW1::CurrentScroll  BYTE                                  ! Queue position of scroll thumb
BRW1::ScrollRecordCount LONG                               ! Queue position of scroll thumb
BRW1::SkipFirst      BYTE                                  ! Skip first retrieved record in page fill
BRW1::Sort1:KeyDistribution LIKE(ENR:ClassNumber),DIM(100)
BRW1::Sort1:LowValue LIKE(ENR:ClassNumber)                 ! Queue position of scroll thumb
BRW1::Sort1:HighValue LIKE(ENR:ClassNumber)                ! Queue position of scroll thumb
BRW1::Sort2:KeyDistribution LIKE(ENR:StudentNumber),DIM(100)
BRW1::Sort2:LowValue LIKE(ENR:StudentNumber)               ! Queue position of scroll thumb
BRW1::Sort2:HighValue LIKE(ENR:StudentNumber)              ! Queue position of scroll thumb
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
WinResize            WindowResizeType
QuickWindow          WINDOW('Browse the Enrollment File'),AT(,,260,188),FONT('MS Sans Serif',8,COLOR:Black),RESIZE, |
  GRAY,IMM,MDI,HLP('~BrowseEnrollment'),SYSTEM
                       LIST,AT(8,20,244,142),USE(?Browse:1),HVSCROLL,FORMAT('[60L(2)|M~Student Number~@p###-##' & |
  '-####p@80L(2)|M~Last Name~@S20@80L(2)|M~First Name~@S20@/60C(2)M~Midterm Exam~C(0)@n' & |
  '3@79C(2)M~Final Exam~C(0)@n3@44C(2)|M~Term Paper~C(0)@n3@]|M'),FROM(Queue:Browse:1),IMM, |
  MSG('Browsing Records')
                       BUTTON('&Insert'),AT(109,148,45,14),USE(?Insert:2),HIDE
                       BUTTON('&Change'),AT(158,148,45,14),USE(?Change:2),DEFAULT,HIDE
                       BUTTON('&Delete'),AT(207,148,45,14),USE(?Delete:2),HIDE
                       SHEET,AT(4,4,252,162),USE(?CurrentTab)
                         TAB('by Student Number')
                         END
                         TAB('by Class Number')
                         END
                       END
                       BUTTON('Close'),AT(162,170,45,14),USE(?Close)
                       BUTTON('Help'),AT(211,170,45,14),USE(?Help),STD(STD:Help)
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
      IF ToolBarMode = BrowseMode THEN
        DO UpdateDispatch
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
      END
    OF ?Insert:2
      CASE EVENT()
      OF EVENT:Accepted
        DO SyncWindow
        DO BRW1::ButtonInsert
      END
    OF ?Change:2
      CASE EVENT()
      OF EVENT:Accepted
        DO SyncWindow
        DO BRW1::ButtonChange
      END
    OF ?Delete:2
      CASE EVENT()
      OF EVENT:Accepted
        DO SyncWindow
        DO BRW1::ButtonDelete
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
  OPEN(QuickWindow)
  WindowOpened=True
  BRW1::AddQueue = True
  BRW1::RecordCount = 0
  WinResize.Init(AppStrategy:Spread)
  Do DefineListboxStyle
  ?Browse:1{PROP:Alrt,252} = MouseLeft2
  ?Browse:1{PROP:Alrt,255} = AppsKey
  ?Browse:1{PROP:Alrt,253} = MouseRight
  ?Browse:1{PROP:Alrt,255} = InsertKey
  ?Browse:1{PROP:Alrt,254} = DeleteKey
  ?Browse:1{PROP:Alrt,253} = CtrlEnter
  ?Browse:1{PROP:Alrt,252} = MouseLeft2

!---------------------------------------------------------------------------
BindFields ROUTINE
  BIND(CLA:RECORD)
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
  BrowseButtons.InsertButton = ?Insert:2
  BrowseButtons.ChangeButton = ?Change:2
  BrowseButtons.DeleteButton = ?Delete:2
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
      StandardWarning(Warn:RecordFetchError,'Enrollment')
      POST(Event:CloseWindow)
    END
    EXIT
  END
  CASE BRW1::SortOrder
  OF 1
    BRW1::Sort1:HighValue = ENR:ClassNumber
  OF 2
    BRW1::Sort2:HighValue = ENR:StudentNumber
  END
  DO BRW1::Reset
  NEXT(BRW1::View:Browse)
  IF ERRORCODE()
    IF ERRORCODE() = BadRecErr
      DO BRW1::RestoreResetValues
    ELSE
      StandardWarning(Warn:RecordFetchError,'Enrollment')
      POST(Event:CloseWindow)
    END
    EXIT
  END
  CASE BRW1::SortOrder
  OF 1
    BRW1::Sort1:LowValue = ENR:ClassNumber
    SetupRealStops(BRW1::Sort1:LowValue,BRW1::Sort1:HighValue)
    LOOP BRW1::ScrollRecordCount = 1 TO 100
      BRW1::Sort1:KeyDistribution[BRW1::ScrollRecordCount] = NextRealStop()
    END
  OF 2
    BRW1::Sort2:LowValue = ENR:StudentNumber
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
  ENR:StudentNumber = BRW1::ENR:StudentNumber
  STU:LastName = BRW1::STU:LastName
  STU:FirstName = BRW1::STU:FirstName
  ENR:MidtermExam = BRW1::ENR:MidtermExam
  ENR:FinalExam = BRW1::ENR:FinalExam
  ENR:TermPaper = BRW1::ENR:TermPaper
  ENR:ClassNumber = BRW1::ENR:ClassNumber
  STU:Number = BRW1::STU:Number
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
  BRW1::ENR:StudentNumber = ENR:StudentNumber
  BRW1::STU:LastName = STU:LastName
  BRW1::STU:FirstName = STU:FirstName
  BRW1::ENR:MidtermExam = ENR:MidtermExam
  BRW1::ENR:FinalExam = ENR:FinalExam
  BRW1::ENR:TermPaper = ENR:TermPaper
  BRW1::ENR:ClassNumber = ENR:ClassNumber
  BRW1::STU:Number = STU:Number
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
      IF BRW1::PopupText
        BRW1::PopupText = '&Insert|&Change|&Delete|-|' & BRW1::PopupText
      ELSE
        BRW1::PopupText = '&Insert|&Change|&Delete'
      END
    ELSE
      IF BRW1::PopupText
        BRW1::PopupText = '&Insert|~&Change|~&Delete|-|' & BRW1::PopupText
      ELSE
        BRW1::PopupText = '&Insert|~&Change|~&Delete'
      END
    END
    EXECUTE(POPUP(BRW1::PopupText))
      POST(EVENT:Accepted,?Insert:2)
      POST(EVENT:Accepted,?Change:2)
      POST(EVENT:Accepted,?Delete:2)
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
          IF BRW1::Sort1:KeyDistribution[BRW1::CurrentScroll] => ENR:ClassNumber
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
          IF BRW1::Sort2:KeyDistribution[BRW1::CurrentScroll] => ENR:StudentNumber
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
      POST(EVENT:Accepted,?Change:2)
      DO BRW1::FillBuffer
    OF InsertKey
      POST(EVENT:Accepted,?Insert:2)
    OF DeleteKey
      POST(EVENT:Accepted,?Delete:2)
    OF CtrlEnter
      POST(EVENT:Accepted,?Change:2)
    END                                                    ! END (What keycode was hit)
  ELSE
    CASE KEYCODE()                                         ! What keycode was hit
    OF AppsKey
    OROF MouseRight
       DO BRW1::NewSelection
    OF InsertKey
      POST(EVENT:Accepted,?Insert:2)
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
      ENR:ClassNumber = BRW1::Sort1:KeyDistribution[?Browse:1{PROP:VScrollPos}]
      BRW1::LocateMode = LocateOnValue
      DO BRW1::LocateRecord
    OF 2
      ENR:StudentNumber = BRW1::Sort2:KeyDistribution[?Browse:1{PROP:VScrollPos}]
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
        StandardWarning(Warn:RecordFetchError,'Enrollment')
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
       BRW1::View:Browse{PROP:Order} = '+ENR:ClassNumber,+ENR:StudentNumber'
    END
    IF BRW1::LocateMode = LocateOnEdit
      BRW1::HighlightedPosition = POSITION(ENR:SeqStu)
      RESET(ENR:SeqStu,BRW1::HighlightedPosition)
    ELSE
      SET(ENR:SeqStu,ENR:SeqStu)
    END
  OF 2
    IF BRW1::UsingAdditionalSortOrder THEN
       BRW1::UsingAdditionalSortOrder = False
       BRW1::View:Browse{PROP:Order} = '+ENR:StudentNumber,+ENR:ClassNumber'
    END
    IF BRW1::LocateMode = LocateOnEdit
      BRW1::HighlightedPosition = POSITION(ENR:StuSeq)
      RESET(ENR:StuSeq,BRW1::HighlightedPosition)
    ELSE
      SET(ENR:StuSeq,ENR:StuSeq)
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
    ?Change:2{PROP:Disable} = 0
    ?Delete:2{PROP:Disable} = 0
  ELSE
    CLEAR(ENR:Record)
    BRW1::CurrentChoice = 0
    ?Change:2{PROP:Disable} = 1
    ?Delete:2{PROP:Disable} = 1
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
       BRW1::View:Browse{PROP:Order} = '+ENR:ClassNumber,+ENR:StudentNumber'
    END
    SET(ENR:SeqStu)
  OF 2
    IF BRW1::UsingAdditionalSortOrder THEN
       BRW1::UsingAdditionalSortOrder = False
       BRW1::View:Browse{PROP:Order} = '+ENR:StudentNumber,+ENR:ClassNumber'
    END
    SET(ENR:StuSeq)
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
  IF INRANGE(ACCEPTED(),TBarBrwInsert,TBarBrwDelete) THEN         !trap remote browse update control calls
    EXECUTE(ACCEPTED()-TBarBrwInsert+1)
      POST(EVENT:Accepted,BrowseButtons.InsertButton)
      POST(EVENT:Accepted,BrowseButtons.ChangeButton)
      POST(EVENT:Accepted,BrowseButtons.DeleteButton)
    END
  END
!----------------------------------------------------------------
BRW1::ButtonInsert ROUTINE
!|
!| This routine calls the BrowseBox's update procedure (as specified in the
!| BrowseUpdateButtons control template) to insert a new record.
!|
!| First, the primary file's record  buffer is cleared, as well as any memos
!| or BLOBs. Next, any range-limit values are restored so that the inserted
!| record defaults to being added to the current display set.
!|
!| Next, LocalRequest is set to InsertRecord, and the BRW1::CallRecord routine
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
  LocalRequest = InsertRecord
  DO BRW1::CallUpdate
  IF GlobalResponse = RequestCompleted
    BRW1::LocateMode = LocateOnEdit
    DO BRW1::LocateRecord
  ELSE
    BRW1::RefreshMode = RefreshOnQueue
    DO BRW1::RefreshPage
  END
  DO BRW1::InitializeBrowse
  DO BRW1::PostNewSelection
  SELECT(?Browse:1)
  LocalRequest = OriginalRequest
  LocalResponse = RequestCancelled
  DO RefreshWindow
!----------------------------------------------------------------
BRW1::ButtonChange ROUTINE
!|
!| This routine calls the BrowseBox's update procedure (as specified in the
!| BrowseUpdateButtons control template) to change a selected record.
!|
!| Whenever a button is pressed, the first thing that happens is that the
!| SyncWindow routine is called. This routine insures that the BrowseBox's
!| VIEW corresponds to the highlighted record by calling the BRW1::GetRecord routine.
!|
!| First, LocalRequest is set to ChangeRecord, and the BRW1::CallRecord routine
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
  DO BRW1::CallUpdate
  IF GlobalResponse = RequestCompleted
    BRW1::LocateMode = LocateOnEdit
    DO BRW1::LocateRecord
  ELSE
    BRW1::RefreshMode = RefreshOnQueue
    DO BRW1::RefreshPage
  END
  DO BRW1::InitializeBrowse
  DO BRW1::PostNewSelection
  SELECT(?Browse:1)
  LocalRequest = OriginalRequest
  LocalResponse = RequestCancelled
  DO RefreshWindow
!----------------------------------------------------------------
BRW1::ButtonDelete ROUTINE
!|
!| This routine calls the BrowseBox's update procedure (as specified in the
!| BrowseUpdateButtons control template) to delete a selected record.
!|
!| Whenever a button is pressed, the first thing that happens is that the
!| SyncWindow routine is called. This routine insures that the BrowseBox's
!| VIEW corresponds to the highlighted record by calling the BRW1::GetRecord routine.
!|
!| First, LocalRequest is set to DeleteRecord, and the BRW1::CallRecord routine
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
  DO BRW1::CallUpdate
  IF GlobalResponse = RequestCompleted
    DELETE(Queue:Browse:1)
    BRW1::RecordCount -= 1
  END
  BRW1::RefreshMode = RefreshOnQueue
  DO BRW1::RefreshPage
  DO BRW1::InitializeBrowse
  DO BRW1::PostNewSelection
  SELECT(?Browse:1)
  LocalRequest = OriginalRequest
  LocalResponse = RequestCancelled
  DO RefreshWindow
!----------------------------------------------------------------
BRW1::CallUpdate ROUTINE
!|
!| This routine performs the actual call to the update procedure.
!|
!| The first thing that happens is that the VIEW is closed. This is performed just in case
!| the VIEW is still open.
!|
!| Next, GlobalRequest is set the the value of LocalRequest, and the update procedure
!| (UpdateEnrollment) is called.
!|
!| Upon return from the update, the routine BRW1::Reset is called to reset the VIEW
!| and reopen it.
!|
  CLOSE(BRW1::View:Browse)
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
        DO BRW1::PostVCREdit1
        BRW1::CurrentEvent = EVENT:ScrollDown
        DO BRW1::ScrollOne
        DO BRW1::PostVCREdit2
      END
    OF VCRBackward
      DO BRW1::PostVCREdit1
      BRW1::CurrentEvent = EVENT:ScrollUp
      DO BRW1::ScrollOne
      DO BRW1::PostVCREdit2
    OF VCRPageForward
      DO BRW1::PostVCREdit1
      BRW1::CurrentEvent = EVENT:PageDown
      DO BRW1::ScrollPage
      DO BRW1::PostVCREdit2
    OF VCRPageBackward
      DO BRW1::PostVCREdit1
      BRW1::CurrentEvent = EVENT:PageUp
      DO BRW1::ScrollPage
      DO BRW1::PostVCREdit2
    OF VCRFirst
      DO BRW1::PostVCREdit1
      BRW1::CurrentEvent = EVENT:ScrollTop
      DO BRW1::ScrollEnd
      DO BRW1::PostVCREdit2
    OF VCRLast
      DO BRW1::PostVCREdit1
      BRW1::CurrentEvent = EVENT:ScrollBottom
      DO BRW1::ScrollEND
      DO BRW1::PostVCREdit2
    END
  END
  DO BRW1::Reset

BRW1::PostVCREdit1 ROUTINE
  DO BRW1::Reset
  BRW1::LocateMode = LocateOnEdit
  DO BRW1::LocateRecord
  DO RefreshWindow

BRW1::PostVCREdit2 ROUTINE
  ?Browse:1{PROP:SelStart} = BRW1::CurrentChoice
  DO BRW1::NewSelection
  REGET(BRW1::View:Browse,BRW1::Position)
  CLOSE(BRW1::View:Browse)

!!! <summary>
!!! Generated from procedure template - Browse
!!! Browse the Majors File
!!! </summary>
BrowseMajors PROCEDURE

CurrentTab           STRING(80)                            ! 
LocalRequest         LONG                                  ! 
OriginalRequest      LONG                                  ! 
LocalResponse        LONG                                  ! 
FilesOpened          LONG                                  ! 
WindowOpened         LONG                                  ! 
WindowInitialized    LONG                                  ! 
ForceRefresh         LONG                                  ! 
RecordFiltered       LONG                                  ! 

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
WinResize            WindowResizeType
QuickWindow          WINDOW('Browse the Majors File'),AT(,,200,188),FONT('MS Sans Serif',8,COLOR:Black),RESIZE, |
  GRAY,IMM,MDI,HLP('~BrowseMajors'),SYSTEM
                       LIST,AT(8,20,184,143),USE(?Browse:1),HVSCROLL,FORMAT('80L(2)|M~Description~L(2)@S20@'),FROM(Queue:Browse:1), |
  IMM,MSG('Browsing Records')
                       BUTTON('&Insert'),AT(49,148,45,14),USE(?Insert:2),HIDE
                       BUTTON('&Change'),AT(98,148,45,14),USE(?Change:2),DEFAULT,HIDE
                       BUTTON('&Delete'),AT(147,148,45,14),USE(?Delete:2),HIDE
                       SHEET,AT(4,4,192,162),USE(?CurrentTab)
                         TAB('by Major Description'),USE(?TAB1)
                         END
                       END
                       BUTTON('Close Teachers to Test ABC'),AT(2,170,,14),USE(?CloseTeachers),TIP('Test of ABC' & |
  ' gets Mad when Teachers is Closed')
                       BUTTON('Close'),AT(147,170,45,14),USE(?Close)
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
      IF ToolBarMode = BrowseMode THEN
        DO UpdateDispatch
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
      END
    OF ?Insert:2
      CASE EVENT()
      OF EVENT:Accepted
        DO SyncWindow
        DO BRW1::ButtonInsert
      END
    OF ?Change:2
      CASE EVENT()
      OF EVENT:Accepted
        DO SyncWindow
        DO BRW1::ButtonChange
      END
    OF ?Delete:2
      CASE EVENT()
      OF EVENT:Accepted
        DO SyncWindow
        DO BRW1::ButtonDelete
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
    OF ?CloseTeachers
      CASE EVENT()
      OF EVENT:Accepted
        DO SyncWindow
            Teachers::Used -= 1 ; CLOSE(Teachers)
            
            
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
  WinResize.Init(AppStrategy:Spread)
  Do DefineListboxStyle
  ?Browse:1{PROP:Alrt,252} = MouseLeft2
  ?Browse:1{PROP:Alrt,255} = AppsKey
  ?Browse:1{PROP:Alrt,253} = MouseRight
  ?Browse:1{PROP:Alrt,255} = InsertKey
  ?Browse:1{PROP:Alrt,254} = DeleteKey
  ?Browse:1{PROP:Alrt,253} = CtrlEnter
  ?Browse:1{PROP:Alrt,252} = MouseLeft2

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
  BrowseButtons.InsertButton = ?Insert:2
  BrowseButtons.ChangeButton = ?Change:2
  BrowseButtons.DeleteButton = ?Delete:2
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
      IF BRW1::PopupText
        BRW1::PopupText = '&Insert|&Change|&Delete|-|' & BRW1::PopupText
      ELSE
        BRW1::PopupText = '&Insert|&Change|&Delete'
      END
    ELSE
      IF BRW1::PopupText
        BRW1::PopupText = '&Insert|~&Change|~&Delete|-|' & BRW1::PopupText
      ELSE
        BRW1::PopupText = '&Insert|~&Change|~&Delete'
      END
    END
    EXECUTE(POPUP(BRW1::PopupText))
      POST(EVENT:Accepted,?Insert:2)
      POST(EVENT:Accepted,?Change:2)
      POST(EVENT:Accepted,?Delete:2)
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
      POST(EVENT:Accepted,?Change:2)
      DO BRW1::FillBuffer
    OF InsertKey
      POST(EVENT:Accepted,?Insert:2)
    OF DeleteKey
      POST(EVENT:Accepted,?Delete:2)
    OF CtrlEnter
      POST(EVENT:Accepted,?Change:2)
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
    OF InsertKey
      POST(EVENT:Accepted,?Insert:2)
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
    ?Change:2{PROP:Disable} = 0
    ?Delete:2{PROP:Disable} = 0
  ELSE
    CLEAR(MAJ:Record)
    BRW1::CurrentChoice = 0
    ?Change:2{PROP:Disable} = 1
    ?Delete:2{PROP:Disable} = 1
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
  IF INRANGE(ACCEPTED(),TBarBrwInsert,TBarBrwDelete) THEN         !trap remote browse update control calls
    EXECUTE(ACCEPTED()-TBarBrwInsert+1)
      POST(EVENT:Accepted,BrowseButtons.InsertButton)
      POST(EVENT:Accepted,BrowseButtons.ChangeButton)
      POST(EVENT:Accepted,BrowseButtons.DeleteButton)
    END
  END
!----------------------------------------------------------------
BRW1::ButtonInsert ROUTINE
!|
!| This routine calls the BrowseBox's update procedure (as specified in the
!| BrowseUpdateButtons control template) to insert a new record.
!|
!| First, the primary file's record  buffer is cleared, as well as any memos
!| or BLOBs. Next, any range-limit values are restored so that the inserted
!| record defaults to being added to the current display set.
!|
!| Next, LocalRequest is set to InsertRecord, and the BRW1::CallRecord routine
!| is called. This routine performs the actual call to the update procedure.
!|
!| If the insert is successful (GlobalRequest = RequestCompleted) then the newly added
!| record is displayed in the BrowseBox, at the top of the listbox.
!|
!| If the insert is not successful, the current page of the browse is refreshed.
!|
!| Finally, The BrowseBox is re-initialized, resetting scroll bars and totals.
!|
  GET(Majors,0)
  CLEAR(MAJ:Record,0)
  LocalRequest = InsertRecord
  DO BRW1::CallUpdate
  IF GlobalResponse = RequestCompleted
    BRW1::LocateMode = LocateOnEdit
    DO BRW1::LocateRecord
  ELSE
    BRW1::RefreshMode = RefreshOnQueue
    DO BRW1::RefreshPage
  END
  DO BRW1::InitializeBrowse
  DO BRW1::PostNewSelection
  SELECT(?Browse:1)
  LocalRequest = OriginalRequest
  LocalResponse = RequestCancelled
  DO RefreshWindow
!----------------------------------------------------------------
BRW1::ButtonChange ROUTINE
!|
!| This routine calls the BrowseBox's update procedure (as specified in the
!| BrowseUpdateButtons control template) to change a selected record.
!|
!| Whenever a button is pressed, the first thing that happens is that the
!| SyncWindow routine is called. This routine insures that the BrowseBox's
!| VIEW corresponds to the highlighted record by calling the BRW1::GetRecord routine.
!|
!| First, LocalRequest is set to ChangeRecord, and the BRW1::CallRecord routine
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
  DO BRW1::CallUpdate
  IF GlobalResponse = RequestCompleted
    BRW1::LocateMode = LocateOnEdit
    DO BRW1::LocateRecord
  ELSE
    BRW1::RefreshMode = RefreshOnQueue
    DO BRW1::RefreshPage
  END
  DO BRW1::InitializeBrowse
  DO BRW1::PostNewSelection
  SELECT(?Browse:1)
  LocalRequest = OriginalRequest
  LocalResponse = RequestCancelled
  DO RefreshWindow
!----------------------------------------------------------------
BRW1::ButtonDelete ROUTINE
!|
!| This routine calls the BrowseBox's update procedure (as specified in the
!| BrowseUpdateButtons control template) to delete a selected record.
!|
!| Whenever a button is pressed, the first thing that happens is that the
!| SyncWindow routine is called. This routine insures that the BrowseBox's
!| VIEW corresponds to the highlighted record by calling the BRW1::GetRecord routine.
!|
!| First, LocalRequest is set to DeleteRecord, and the BRW1::CallRecord routine
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
  DO BRW1::CallUpdate
  IF GlobalResponse = RequestCompleted
    DELETE(Queue:Browse:1)
    BRW1::RecordCount -= 1
  END
  BRW1::RefreshMode = RefreshOnQueue
  DO BRW1::RefreshPage
  DO BRW1::InitializeBrowse
  DO BRW1::PostNewSelection
  SELECT(?Browse:1)
  LocalRequest = OriginalRequest
  LocalResponse = RequestCancelled
  DO RefreshWindow
!----------------------------------------------------------------
BRW1::CallUpdate ROUTINE
!|
!| This routine performs the actual call to the update procedure.
!|
!| The first thing that happens is that the VIEW is closed. This is performed just in case
!| the VIEW is still open.
!|
!| Next, GlobalRequest is set the the value of LocalRequest, and the update procedure
!| (UpdateMajors) is called.
!|
!| Upon return from the update, the routine BRW1::Reset is called to reset the VIEW
!| and reopen it.
!|
  CLOSE(BRW1::View:Browse)
  LOOP
    GlobalRequest = LocalRequest
    VCRRequest = VCRNone
    UpdateMajors
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
        GET(Majors,0)
        CLEAR(MAJ:Record,0)
      ELSE
        DO BRW1::PostVCREdit1
        BRW1::CurrentEvent = EVENT:ScrollDown
        DO BRW1::ScrollOne
        DO BRW1::PostVCREdit2
      END
    OF VCRBackward
      DO BRW1::PostVCREdit1
      BRW1::CurrentEvent = EVENT:ScrollUp
      DO BRW1::ScrollOne
      DO BRW1::PostVCREdit2
    OF VCRPageForward
      DO BRW1::PostVCREdit1
      BRW1::CurrentEvent = EVENT:PageDown
      DO BRW1::ScrollPage
      DO BRW1::PostVCREdit2
    OF VCRPageBackward
      DO BRW1::PostVCREdit1
      BRW1::CurrentEvent = EVENT:PageUp
      DO BRW1::ScrollPage
      DO BRW1::PostVCREdit2
    OF VCRFirst
      DO BRW1::PostVCREdit1
      BRW1::CurrentEvent = EVENT:ScrollTop
      DO BRW1::ScrollEnd
      DO BRW1::PostVCREdit2
    OF VCRLast
      DO BRW1::PostVCREdit1
      BRW1::CurrentEvent = EVENT:ScrollBottom
      DO BRW1::ScrollEND
      DO BRW1::PostVCREdit2
    END
  END
  DO BRW1::Reset

BRW1::PostVCREdit1 ROUTINE
  DO BRW1::Reset
  BRW1::LocateMode = LocateOnEdit
  DO BRW1::LocateRecord
  DO RefreshWindow

BRW1::PostVCREdit2 ROUTINE
  ?Browse:1{PROP:SelStart} = BRW1::CurrentChoice
  DO BRW1::NewSelection
  REGET(BRW1::View:Browse,BRW1::Position)
  CLOSE(BRW1::View:Browse)

!!! <summary>
!!! Generated from procedure template - Browse
!!! Browse the Students File
!!! </summary>
BrowseStudents PROCEDURE

CurrentTab           STRING(80)                            ! 
LocalRequest         LONG                                  ! 
OriginalRequest      LONG                                  ! 
LocalResponse        LONG                                  ! 
FilesOpened          LONG                                  ! 
WindowOpened         LONG                                  ! 
WindowInitialized    LONG                                  ! 
ForceRefresh         LONG                                  ! 
RecordFiltered       LONG                                  ! 
Junk                 STRING(20)                            ! 
Junk1                STRING(20),OVER(Junk)                 ! 
Junk2                STRING(20),OVER(Junk)                 ! 

BRW1::View:Browse    VIEW(Students)
                       PROJECT(STU:LastName)
                       PROJECT(STU:FirstName)
                       PROJECT(STU:GradYear)
                       PROJECT(STU:Address)
                       PROJECT(STU:Address2)
                       PROJECT(STU:City)
                       PROJECT(STU:State)
                       PROJECT(STU:Zip)
                       PROJECT(STU:Telephone)
                       PROJECT(STU:Number)
                       PROJECT(STU:Major)
                       JOIN(MAJ:KeyNumber,STU:Major)
                         PROJECT(MAJ:Description)
                         PROJECT(MAJ:Number)
                       END
                     END
Queue:Browse:1       QUEUE,PRE()                           ! Browsing Queue
BRW1::STU:LastName     LIKE(STU:LastName)                  ! Queue Display field
BRW1::STU:FirstName    LIKE(STU:FirstName)                 ! Queue Display field
BRW1::MAJ:Description  LIKE(MAJ:Description)               ! Queue Display field
BRW1::STU:GradYear     LIKE(STU:GradYear)                  ! Queue Display field
BRW1::Junk             LIKE(Junk)                          ! Queue Display field
BRW1::STU:Address      LIKE(STU:Address)                   ! Queue Display field
BRW1::STU:Address2     LIKE(STU:Address2)                  ! Queue Display field
BRW1::STU:City         LIKE(STU:City)                      ! Queue Display field
BRW1::STU:State        LIKE(STU:State)                     ! Queue Display field
BRW1::STU:Zip          LIKE(STU:Zip)                       ! Queue Display field
BRW1::STU:Telephone    LIKE(STU:Telephone)                 ! Queue Display field
BRW1::STU:Number       LIKE(STU:Number)                    ! Queue Display field
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
WinResize            WindowResizeType
QuickWindow          WINDOW('Browse the Students File'),AT(,,358,188),FONT('MS Sans Serif',8,COLOR:Black),RESIZE, |
  GRAY,IMM,MDI,HLP('~BrowseStudents'),SYSTEM
                       LIST,AT(8,20,342,143),USE(?Browse:1),HVSCROLL,FORMAT('[80L(2)M~Last Name~@S20@80L(2)|M~' & |
  'First Name~@S20@/80C(2)M~Major~C(0)@S20@40C(2)|M~Grad Year~C(0)@n4@/80C(2)|_M@s20@]|' & |
  'M[123L(2)|M@S20@/80L(2)|M@s20@/83L(2)_M@S20@16L(2)_M@S2@24R(2)|_M@n05@]|M~Address~[5' & |
  '2L(2)~Telephone~@s12@/]|M'),FROM(Queue:Browse:1),IMM,MSG('Browsing Records')
                       BUTTON('&Insert'),AT(207,148,45,14),USE(?Insert:2),HIDE
                       BUTTON('&Change'),AT(256,148,45,14),USE(?Change:2),DEFAULT,HIDE
                       BUTTON('&Delete'),AT(305,148,45,14),USE(?Delete:2),HIDE
                       SHEET,AT(4,4,350,162),USE(?CurrentTab)
                         TAB('by Major')
                         END
                         TAB('by Last Name')
                         END
                         TAB('by Grad Year')
                         END
                       END
                       BUTTON('Close'),AT(260,170,45,14),USE(?Close)
                       BUTTON('Help'),AT(309,170,45,14),USE(?Help),STD(STD:Help)
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
      IF ToolBarMode = BrowseMode THEN
        DO UpdateDispatch
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
      END
    OF ?Insert:2
      CASE EVENT()
      OF EVENT:Accepted
        DO SyncWindow
        DO BRW1::ButtonInsert
      END
    OF ?Change:2
      CASE EVENT()
      OF EVENT:Accepted
        DO SyncWindow
        DO BRW1::ButtonChange
      END
    OF ?Delete:2
      CASE EVENT()
      OF EVENT:Accepted
        DO SyncWindow
        DO BRW1::ButtonDelete
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
  WinResize.Init(AppStrategy:Spread)
  Do DefineListboxStyle
  ?Browse:1{PROP:Alrt,252} = MouseLeft2
  ?Browse:1{PROP:Alrt,255} = AppsKey
  ?Browse:1{PROP:Alrt,253} = MouseRight
  ?Browse:1{PROP:Alrt,255} = InsertKey
  ?Browse:1{PROP:Alrt,254} = DeleteKey
  ?Browse:1{PROP:Alrt,253} = CtrlEnter
  ?Browse:1{PROP:Alrt,252} = MouseLeft2

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
  BrowseButtons.InsertButton = ?Insert:2
  BrowseButtons.ChangeButton = ?Change:2
  BrowseButtons.DeleteButton = ?Delete:2
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
  MAJ:Description = BRW1::MAJ:Description
  STU:GradYear = BRW1::STU:GradYear
  Junk = BRW1::Junk
  STU:Address = BRW1::STU:Address
  STU:Address2 = BRW1::STU:Address2
  STU:City = BRW1::STU:City
  STU:State = BRW1::STU:State
  STU:Zip = BRW1::STU:Zip
  STU:Telephone = BRW1::STU:Telephone
  STU:Number = BRW1::STU:Number
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
  BRW1::MAJ:Description = MAJ:Description
  BRW1::STU:GradYear = STU:GradYear
  BRW1::Junk = Junk
  BRW1::STU:Address = STU:Address
  BRW1::STU:Address2 = STU:Address2
  BRW1::STU:City = STU:City
  BRW1::STU:State = STU:State
  BRW1::STU:Zip = STU:Zip
  BRW1::STU:Telephone = STU:Telephone
  BRW1::STU:Number = STU:Number
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
      IF BRW1::PopupText
        BRW1::PopupText = '&Insert|&Change|&Delete|-|' & BRW1::PopupText
      ELSE
        BRW1::PopupText = '&Insert|&Change|&Delete'
      END
    ELSE
      IF BRW1::PopupText
        BRW1::PopupText = '&Insert|~&Change|~&Delete|-|' & BRW1::PopupText
      ELSE
        BRW1::PopupText = '&Insert|~&Change|~&Delete'
      END
    END
    EXECUTE(POPUP(BRW1::PopupText))
      POST(EVENT:Accepted,?Insert:2)
      POST(EVENT:Accepted,?Change:2)
      POST(EVENT:Accepted,?Delete:2)
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
      POST(EVENT:Accepted,?Change:2)
      DO BRW1::FillBuffer
    OF InsertKey
      POST(EVENT:Accepted,?Insert:2)
    OF DeleteKey
      POST(EVENT:Accepted,?Delete:2)
    OF CtrlEnter
      POST(EVENT:Accepted,?Change:2)
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
    OF InsertKey
      POST(EVENT:Accepted,?Insert:2)
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
    ?Change:2{PROP:Disable} = 0
    ?Delete:2{PROP:Disable} = 0
  ELSE
    CLEAR(STU:Record)
    BRW1::CurrentChoice = 0
    ?Change:2{PROP:Disable} = 1
    ?Delete:2{PROP:Disable} = 1
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
  IF INRANGE(ACCEPTED(),TBarBrwInsert,TBarBrwDelete) THEN         !trap remote browse update control calls
    EXECUTE(ACCEPTED()-TBarBrwInsert+1)
      POST(EVENT:Accepted,BrowseButtons.InsertButton)
      POST(EVENT:Accepted,BrowseButtons.ChangeButton)
      POST(EVENT:Accepted,BrowseButtons.DeleteButton)
    END
  END
!----------------------------------------------------------------
BRW1::ButtonInsert ROUTINE
!|
!| This routine calls the BrowseBox's update procedure (as specified in the
!| BrowseUpdateButtons control template) to insert a new record.
!|
!| First, the primary file's record  buffer is cleared, as well as any memos
!| or BLOBs. Next, any range-limit values are restored so that the inserted
!| record defaults to being added to the current display set.
!|
!| Next, LocalRequest is set to InsertRecord, and the BRW1::CallRecord routine
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
  LocalRequest = InsertRecord
  DO BRW1::CallUpdate
  IF GlobalResponse = RequestCompleted
    BRW1::LocateMode = LocateOnEdit
    DO BRW1::LocateRecord
  ELSE
    BRW1::RefreshMode = RefreshOnQueue
    DO BRW1::RefreshPage
  END
  DO BRW1::InitializeBrowse
  DO BRW1::PostNewSelection
  SELECT(?Browse:1)
  LocalRequest = OriginalRequest
  LocalResponse = RequestCancelled
  DO RefreshWindow
!----------------------------------------------------------------
BRW1::ButtonChange ROUTINE
!|
!| This routine calls the BrowseBox's update procedure (as specified in the
!| BrowseUpdateButtons control template) to change a selected record.
!|
!| Whenever a button is pressed, the first thing that happens is that the
!| SyncWindow routine is called. This routine insures that the BrowseBox's
!| VIEW corresponds to the highlighted record by calling the BRW1::GetRecord routine.
!|
!| First, LocalRequest is set to ChangeRecord, and the BRW1::CallRecord routine
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
  DO BRW1::CallUpdate
  IF GlobalResponse = RequestCompleted
    BRW1::LocateMode = LocateOnEdit
    DO BRW1::LocateRecord
  ELSE
    BRW1::RefreshMode = RefreshOnQueue
    DO BRW1::RefreshPage
  END
  DO BRW1::InitializeBrowse
  DO BRW1::PostNewSelection
  SELECT(?Browse:1)
  LocalRequest = OriginalRequest
  LocalResponse = RequestCancelled
  DO RefreshWindow
!----------------------------------------------------------------
BRW1::ButtonDelete ROUTINE
!|
!| This routine calls the BrowseBox's update procedure (as specified in the
!| BrowseUpdateButtons control template) to delete a selected record.
!|
!| Whenever a button is pressed, the first thing that happens is that the
!| SyncWindow routine is called. This routine insures that the BrowseBox's
!| VIEW corresponds to the highlighted record by calling the BRW1::GetRecord routine.
!|
!| First, LocalRequest is set to DeleteRecord, and the BRW1::CallRecord routine
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
  DO BRW1::CallUpdate
  IF GlobalResponse = RequestCompleted
    DELETE(Queue:Browse:1)
    BRW1::RecordCount -= 1
  END
  BRW1::RefreshMode = RefreshOnQueue
  DO BRW1::RefreshPage
  DO BRW1::InitializeBrowse
  DO BRW1::PostNewSelection
  SELECT(?Browse:1)
  LocalRequest = OriginalRequest
  LocalResponse = RequestCancelled
  DO RefreshWindow
!----------------------------------------------------------------
BRW1::CallUpdate ROUTINE
!|
!| This routine performs the actual call to the update procedure.
!|
!| The first thing that happens is that the VIEW is closed. This is performed just in case
!| the VIEW is still open.
!|
!| Next, GlobalRequest is set the the value of LocalRequest, and the update procedure
!| (UpdateStudents) is called.
!|
!| Upon return from the update, the routine BRW1::Reset is called to reset the VIEW
!| and reopen it.
!|
  CLOSE(BRW1::View:Browse)
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
        DO BRW1::PostVCREdit1
        BRW1::CurrentEvent = EVENT:ScrollDown
        DO BRW1::ScrollOne
        DO BRW1::PostVCREdit2
      END
    OF VCRBackward
      DO BRW1::PostVCREdit1
      BRW1::CurrentEvent = EVENT:ScrollUp
      DO BRW1::ScrollOne
      DO BRW1::PostVCREdit2
    OF VCRPageForward
      DO BRW1::PostVCREdit1
      BRW1::CurrentEvent = EVENT:PageDown
      DO BRW1::ScrollPage
      DO BRW1::PostVCREdit2
    OF VCRPageBackward
      DO BRW1::PostVCREdit1
      BRW1::CurrentEvent = EVENT:PageUp
      DO BRW1::ScrollPage
      DO BRW1::PostVCREdit2
    OF VCRFirst
      DO BRW1::PostVCREdit1
      BRW1::CurrentEvent = EVENT:ScrollTop
      DO BRW1::ScrollEnd
      DO BRW1::PostVCREdit2
    OF VCRLast
      DO BRW1::PostVCREdit1
      BRW1::CurrentEvent = EVENT:ScrollBottom
      DO BRW1::ScrollEND
      DO BRW1::PostVCREdit2
    END
  END
  DO BRW1::Reset

BRW1::PostVCREdit1 ROUTINE
  DO BRW1::Reset
  BRW1::LocateMode = LocateOnEdit
  DO BRW1::LocateRecord
  DO RefreshWindow

BRW1::PostVCREdit2 ROUTINE
  ?Browse:1{PROP:SelStart} = BRW1::CurrentChoice
  DO BRW1::NewSelection
  REGET(BRW1::View:Browse,BRW1::Position)
  CLOSE(BRW1::View:Browse)

!!! <summary>
!!! Generated from procedure template - Browse
!!! Browse the Teachers File
!!! </summary>
BrowseTeachers PROCEDURE

CurrentTab           STRING(80)                            ! 
LocalRequest         LONG                                  ! 
OriginalRequest      LONG                                  ! 
LocalResponse        LONG                                  ! 
FilesOpened          LONG                                  ! 
WindowOpened         LONG                                  ! 
WindowInitialized    LONG                                  ! 
ForceRefresh         LONG                                  ! 
RecordFiltered       LONG                                  ! 

BRW1::View:Browse    VIEW(Teachers)
                       PROJECT(TEA:LastName)
                       PROJECT(TEA:FirstName)
                       PROJECT(TEA:Address)
                       PROJECT(TEA:City)
                       PROJECT(TEA:State)
                       PROJECT(TEA:Zip)
                       PROJECT(TEA:Telephone)
                       PROJECT(TEA:Number)
                       PROJECT(TEA:Department)
                       JOIN(MAJ:KeyNumber,TEA:Department)
                         PROJECT(MAJ:Description)
                         PROJECT(MAJ:Number)
                       END
                     END
Queue:Browse:1       QUEUE,PRE()                           ! Browsing Queue
BRW1::MAJ:Description  LIKE(MAJ:Description)               ! Queue Display field
BRW1::TEA:LastName     LIKE(TEA:LastName)                  ! Queue Display field
BRW1::TEA:FirstName    LIKE(TEA:FirstName)                 ! Queue Display field
BRW1::TEA:Address      LIKE(TEA:Address)                   ! Queue Display field
BRW1::TEA:City         LIKE(TEA:City)                      ! Queue Display field
BRW1::TEA:State        LIKE(TEA:State)                     ! Queue Display field
BRW1::TEA:Zip          LIKE(TEA:Zip)                       ! Queue Display field
BRW1::TEA:Telephone    LIKE(TEA:Telephone)                 ! Queue Display field
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
WinResize            WindowResizeType
QuickWindow          WINDOW('Browse the Teachers File'),AT(,,358,188),FONT('MS Sans Serif',8,COLOR:Black),RESIZE, |
  GRAY,IMM,MDI,HLP('~BrowseTeachers'),SYSTEM
                       LIST,AT(8,20,342,143),USE(?Browse:1),HVSCROLL,FORMAT('80L(2)|M~Department~@S20@80L(2)|M' & |
  '~Last Name~@S20@80L(2)|M~First Name~@S20@80L(2)|M~Address~@S20@80L(2)|M~City~@S20@24' & |
  'L(2)|M~State~@S2@24R(2)|M~Zip~C(0)@n05@52L(2)|M~Telephone~@s12@'),FROM(Queue:Browse:1), |
  IMM,MSG('Browsing Records')
                       BUTTON('&Insert'),AT(207,148,45,14),USE(?Insert:2),HIDE
                       BUTTON('&Change'),AT(256,148,45,14),USE(?Change:2),DEFAULT,HIDE
                       BUTTON('&Delete'),AT(305,148,45,14),USE(?Delete:2),HIDE
                       SHEET,AT(4,4,350,162),USE(?CurrentTab)
                         TAB('by Last Name')
                         END
                         TAB('by Department')
                         END
                       END
                       BUTTON('Close'),AT(260,170,45,14),USE(?Close)
                       BUTTON('Help'),AT(309,170,45,14),USE(?Help),STD(STD:Help)
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
      IF ToolBarMode = BrowseMode THEN
        DO UpdateDispatch
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
      END
    OF ?Insert:2
      CASE EVENT()
      OF EVENT:Accepted
        DO SyncWindow
        DO BRW1::ButtonInsert
      END
    OF ?Change:2
      CASE EVENT()
      OF EVENT:Accepted
        DO SyncWindow
        DO BRW1::ButtonChange
      END
    OF ?Delete:2
      CASE EVENT()
      OF EVENT:Accepted
        DO SyncWindow
        DO BRW1::ButtonDelete
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
  WinResize.Init(AppStrategy:Spread)
  Do DefineListboxStyle
  ?Browse:1{PROP:Alrt,252} = MouseLeft2
  ?Browse:1{PROP:Alrt,255} = AppsKey
  ?Browse:1{PROP:Alrt,253} = MouseRight
  ?Browse:1{PROP:Alrt,255} = InsertKey
  ?Browse:1{PROP:Alrt,254} = DeleteKey
  ?Browse:1{PROP:Alrt,253} = CtrlEnter
  ?Browse:1{PROP:Alrt,252} = MouseLeft2

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
  BrowseButtons.InsertButton = ?Insert:2
  BrowseButtons.ChangeButton = ?Change:2
  BrowseButtons.DeleteButton = ?Delete:2
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
  MAJ:Description = BRW1::MAJ:Description
  TEA:LastName = BRW1::TEA:LastName
  TEA:FirstName = BRW1::TEA:FirstName
  TEA:Address = BRW1::TEA:Address
  TEA:City = BRW1::TEA:City
  TEA:State = BRW1::TEA:State
  TEA:Zip = BRW1::TEA:Zip
  TEA:Telephone = BRW1::TEA:Telephone
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
  BRW1::MAJ:Description = MAJ:Description
  BRW1::TEA:LastName = TEA:LastName
  BRW1::TEA:FirstName = TEA:FirstName
  BRW1::TEA:Address = TEA:Address
  BRW1::TEA:City = TEA:City
  BRW1::TEA:State = TEA:State
  BRW1::TEA:Zip = TEA:Zip
  BRW1::TEA:Telephone = TEA:Telephone
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
      IF BRW1::PopupText
        BRW1::PopupText = '&Insert|&Change|&Delete|-|' & BRW1::PopupText
      ELSE
        BRW1::PopupText = '&Insert|&Change|&Delete'
      END
    ELSE
      IF BRW1::PopupText
        BRW1::PopupText = '&Insert|~&Change|~&Delete|-|' & BRW1::PopupText
      ELSE
        BRW1::PopupText = '&Insert|~&Change|~&Delete'
      END
    END
    EXECUTE(POPUP(BRW1::PopupText))
      POST(EVENT:Accepted,?Insert:2)
      POST(EVENT:Accepted,?Change:2)
      POST(EVENT:Accepted,?Delete:2)
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
      POST(EVENT:Accepted,?Change:2)
      DO BRW1::FillBuffer
    OF InsertKey
      POST(EVENT:Accepted,?Insert:2)
    OF DeleteKey
      POST(EVENT:Accepted,?Delete:2)
    OF CtrlEnter
      POST(EVENT:Accepted,?Change:2)
    ELSE                                                   ! ELSE (What keycode was hit)
      CASE BRW1::SortOrder
      OF 1
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
    OF InsertKey
      POST(EVENT:Accepted,?Insert:2)
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
    ?Change:2{PROP:Disable} = 0
    ?Delete:2{PROP:Disable} = 0
  ELSE
    CLEAR(TEA:Record)
    BRW1::CurrentChoice = 0
    ?Change:2{PROP:Disable} = 1
    ?Delete:2{PROP:Disable} = 1
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
  IF INRANGE(ACCEPTED(),TBarBrwInsert,TBarBrwDelete) THEN         !trap remote browse update control calls
    EXECUTE(ACCEPTED()-TBarBrwInsert+1)
      POST(EVENT:Accepted,BrowseButtons.InsertButton)
      POST(EVENT:Accepted,BrowseButtons.ChangeButton)
      POST(EVENT:Accepted,BrowseButtons.DeleteButton)
    END
  END
!----------------------------------------------------------------
BRW1::ButtonInsert ROUTINE
!|
!| This routine calls the BrowseBox's update procedure (as specified in the
!| BrowseUpdateButtons control template) to insert a new record.
!|
!| First, the primary file's record  buffer is cleared, as well as any memos
!| or BLOBs. Next, any range-limit values are restored so that the inserted
!| record defaults to being added to the current display set.
!|
!| Next, LocalRequest is set to InsertRecord, and the BRW1::CallRecord routine
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
  LocalRequest = InsertRecord
  DO BRW1::CallUpdate
  IF GlobalResponse = RequestCompleted
    BRW1::LocateMode = LocateOnEdit
    DO BRW1::LocateRecord
  ELSE
    BRW1::RefreshMode = RefreshOnQueue
    DO BRW1::RefreshPage
  END
  DO BRW1::InitializeBrowse
  DO BRW1::PostNewSelection
  SELECT(?Browse:1)
  LocalRequest = OriginalRequest
  LocalResponse = RequestCancelled
  DO RefreshWindow
!----------------------------------------------------------------
BRW1::ButtonChange ROUTINE
!|
!| This routine calls the BrowseBox's update procedure (as specified in the
!| BrowseUpdateButtons control template) to change a selected record.
!|
!| Whenever a button is pressed, the first thing that happens is that the
!| SyncWindow routine is called. This routine insures that the BrowseBox's
!| VIEW corresponds to the highlighted record by calling the BRW1::GetRecord routine.
!|
!| First, LocalRequest is set to ChangeRecord, and the BRW1::CallRecord routine
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
  DO BRW1::CallUpdate
  IF GlobalResponse = RequestCompleted
    BRW1::LocateMode = LocateOnEdit
    DO BRW1::LocateRecord
  ELSE
    BRW1::RefreshMode = RefreshOnQueue
    DO BRW1::RefreshPage
  END
  DO BRW1::InitializeBrowse
  DO BRW1::PostNewSelection
  SELECT(?Browse:1)
  LocalRequest = OriginalRequest
  LocalResponse = RequestCancelled
  DO RefreshWindow
!----------------------------------------------------------------
BRW1::ButtonDelete ROUTINE
!|
!| This routine calls the BrowseBox's update procedure (as specified in the
!| BrowseUpdateButtons control template) to delete a selected record.
!|
!| Whenever a button is pressed, the first thing that happens is that the
!| SyncWindow routine is called. This routine insures that the BrowseBox's
!| VIEW corresponds to the highlighted record by calling the BRW1::GetRecord routine.
!|
!| First, LocalRequest is set to DeleteRecord, and the BRW1::CallRecord routine
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
  DO BRW1::CallUpdate
  IF GlobalResponse = RequestCompleted
    DELETE(Queue:Browse:1)
    BRW1::RecordCount -= 1
  END
  BRW1::RefreshMode = RefreshOnQueue
  DO BRW1::RefreshPage
  DO BRW1::InitializeBrowse
  DO BRW1::PostNewSelection
  SELECT(?Browse:1)
  LocalRequest = OriginalRequest
  LocalResponse = RequestCancelled
  DO RefreshWindow
!----------------------------------------------------------------
BRW1::CallUpdate ROUTINE
!|
!| This routine performs the actual call to the update procedure.
!|
!| The first thing that happens is that the VIEW is closed. This is performed just in case
!| the VIEW is still open.
!|
!| Next, GlobalRequest is set the the value of LocalRequest, and the update procedure
!| (UpdateTeachers) is called.
!|
!| Upon return from the update, the routine BRW1::Reset is called to reset the VIEW
!| and reopen it.
!|
  CLOSE(BRW1::View:Browse)
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
        DO BRW1::PostVCREdit1
        BRW1::CurrentEvent = EVENT:ScrollDown
        DO BRW1::ScrollOne
        DO BRW1::PostVCREdit2
      END
    OF VCRBackward
      DO BRW1::PostVCREdit1
      BRW1::CurrentEvent = EVENT:ScrollUp
      DO BRW1::ScrollOne
      DO BRW1::PostVCREdit2
    OF VCRPageForward
      DO BRW1::PostVCREdit1
      BRW1::CurrentEvent = EVENT:PageDown
      DO BRW1::ScrollPage
      DO BRW1::PostVCREdit2
    OF VCRPageBackward
      DO BRW1::PostVCREdit1
      BRW1::CurrentEvent = EVENT:PageUp
      DO BRW1::ScrollPage
      DO BRW1::PostVCREdit2
    OF VCRFirst
      DO BRW1::PostVCREdit1
      BRW1::CurrentEvent = EVENT:ScrollTop
      DO BRW1::ScrollEnd
      DO BRW1::PostVCREdit2
    OF VCRLast
      DO BRW1::PostVCREdit1
      BRW1::CurrentEvent = EVENT:ScrollBottom
      DO BRW1::ScrollEND
      DO BRW1::PostVCREdit2
    END
  END
  DO BRW1::Reset

BRW1::PostVCREdit1 ROUTINE
  DO BRW1::Reset
  BRW1::LocateMode = LocateOnEdit
  DO BRW1::LocateRecord
  DO RefreshWindow

BRW1::PostVCREdit2 ROUTINE
  ?Browse:1{PROP:SelStart} = BRW1::CurrentChoice
  DO BRW1::NewSelection
  REGET(BRW1::View:Browse,BRW1::Position)
  CLOSE(BRW1::View:Browse)

!!! <summary>
!!! Generated from procedure template - Browse
!!! </summary>
UpdateGrades PROCEDURE

LocalRequest         LONG                                  ! 
OriginalRequest      LONG                                  ! 
LocalResponse        LONG                                  ! 
FilesOpened          LONG                                  ! 
WindowOpened         LONG                                  ! 
WindowInitialized    LONG                                  ! 
ForceRefresh         LONG                                  ! 
CurrentTab           STRING(80)                            ! 
RecordFiltered       LONG                                  ! 
FullName             STRING(25)                            ! 
EditColumn           BYTE                                  ! 
DummyColumn          STRING(1)                             ! 

BRW1::View:Browse    VIEW(Enrollment)
                       PROJECT(ENR:MidtermExam)
                       PROJECT(ENR:FinalExam)
                       PROJECT(ENR:TermPaper)
                       PROJECT(ENR:ClassNumber)
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
                       JOIN(STU:KeyStudentNumber,ENR:StudentNumber)
                         PROJECT(STU:Number)
                       END
                     END
Queue:Browse         QUEUE,PRE()                           ! Browsing Queue
BRW1::COU:Description  LIKE(COU:Description)               ! Queue Display field
BRW1::COU:Description:NormalFG LONG                        ! Normal Foreground
BRW1::COU:Description:NormalBG LONG                        ! Normal Background
BRW1::COU:Description:SelectedFG LONG                      ! Selected Foreground
BRW1::COU:Description:SelectedBG LONG                      ! Selected Background
BRW1::CLA:ScheduledTime LIKE(CLA:ScheduledTime)            ! Queue Display field
BRW1::CLA:ScheduledTime:NormalFG LONG                      ! Normal Foreground
BRW1::CLA:ScheduledTime:NormalBG LONG                      ! Normal Background
BRW1::CLA:ScheduledTime:SelectedFG LONG                    ! Selected Foreground
BRW1::CLA:ScheduledTime:SelectedBG LONG                    ! Selected Background
BRW1::FullName         LIKE(FullName)                      ! Queue Display field
BRW1::FullName:NormalFG LONG                               ! Normal Foreground
BRW1::FullName:NormalBG LONG                               ! Normal Background
BRW1::FullName:SelectedFG LONG                             ! Selected Foreground
BRW1::FullName:SelectedBG LONG                             ! Selected Background
BRW1::ENR:MidtermExam  LIKE(ENR:MidtermExam)               ! Queue Display field
BRW1::ENR:FinalExam    LIKE(ENR:FinalExam)                 ! Queue Display field
BRW1::ENR:TermPaper    LIKE(ENR:TermPaper)                 ! Queue Display field
BRW1::DummyColumn      LIKE(DummyColumn)                   ! Queue Display field
BRW1::ENR:ClassNumber  LIKE(ENR:ClassNumber)               ! Queue Display field
BRW1::ENR:StudentNumber LIKE(ENR:StudentNumber)            ! Queue Display field
BRW1::CLA:ClassNumber  LIKE(CLA:ClassNumber)               ! Queue Display field
BRW1::COU:Number       LIKE(COU:Number)                    ! Queue Display field
BRW1::STU:Number       LIKE(STU:Number)                    ! Queue Display field
BRW1::Mark             BYTE                                ! Record mark flag
BRW1::Position         STRING(1024)                        ! Queue POSITION information
                     END                                   ! END (Browsing Queue)
BRW1::UsingAdditionalSortOrder BYTE                        ! When true the view is using PROP:Order
BRW1::CurrentScroll  BYTE                                  ! Queue position of scroll thumb
BRW1::ScrollRecordCount LONG                               ! Queue position of scroll thumb
BRW1::SkipFirst      BYTE                                  ! Skip first retrieved record in page fill
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
BrowseWindow         WINDOW('Update Grades for a Class'),AT(0,0,288,140),GRAY,MDI,HLP('~UpdateGrades'),SYSTEM
                       LIST,AT(5,5,275,116),USE(?List),HVSCROLL,COLUMN,FORMAT('67L(1)|_M*~Class~@S30@44L(1)|_M' & |
  '*~Time~@s20@51L(1)|_M*~Student~@s25@33R(8)|_M~Midterm~L(1)@n3@25R(8)|_M~Final~L(1)@n' & |
  '3@41R(8)_~Term Paper~L(1)@n3@4R(2)_@s1@'),FROM(Queue:Browse),IMM,MSG('Browsing Records')
                       BUTTON('Close'),AT(240,125,40,12),USE(?Close)
                       ENTRY(@n3),AT(128,126),USE(?EditEntry),RIGHT(7),HIDE
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
      SELECT(?List)
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
    OF ?List
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
    OF ?EditEntry
      CASE EVENT()
      OF EVENT:Accepted
        !Write the changed data to disk
        GET(Enrollment,ENR:StuSeq)                  !Retrieve a record from the Enrollment file
        IF ERRORCODE() THEN STOP(ERROR()).          !Check for error
        IF ENR:MidtermExam  <> BRW1::ENR:MidtermExam  OR |   !Detect changes and only write if changed
           ENR:FinalExam    <> BRW1::ENR:FinalExam    OR |
           ENR:TermPaper    <> BRW1::ENR:TermPaper
          ENR:MidtermExam  = BRW1::ENR:MidtermExam    !Assign changed data to the Enrollment record
          ENR:FinalExam    = BRW1::ENR:FinalExam
          ENR:TermPaper    = BRW1::ENR:TermPaper
          PUT(Enrollment)                             !Write changes to the Enrollment file
          IF ERRORCODE() THEN STOP(ERROR()).          !Check for error
          PUT(Queue:Browse)                           !Write changes to the display queue
          IF ERRORCODE() THEN STOP(ERROR()).          !Check for error
        END
        ?List{Prop:edit,EditColumn} = FALSE         !Turn off edit in place
      OF EVENT:Selected
        !Force EVENT:Accepted for the edit control
        ?EditEntry{PROP:Touched} = TRUE
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
  OPEN(BrowseWindow)
  WindowOpened=True
  BRW1::AddQueue = True
  BRW1::RecordCount = 0
  Do DefineListboxStyle
  ?List{PROP:Alrt,252} = MouseLeft2
  ?List{PROP:Alrt,255} = AppsKey
  ?List{PROP:Alrt,253} = MouseRight

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
    CLOSE(BrowseWindow)
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
  IF BrowseWindow{Prop:AcceptAll} THEN EXIT.
  Do LookupRelated
  DO BRW1::SelectSort
  ?List{Prop:VScrollPos} = BRW1::CurrentScroll
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
  BrowseButtons.ListBox = ?List
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
  IF BRW1::SortOrder <> BRW1::LastSortOrder OR BRW1::Changed OR ForceRefresh OR (BRW1::LoadPending AND ?List{PROP:VISIBLE})
    DO BRW1::GetRecord
    DO BRW1::Reset
    IF BRW1::LastSortOrder = 0
      IF LocalRequest = SelectRecord
        BRW1::LocateMode = LocateOnValue
        DO BRW1::LocateRecord
      ELSE
        FREE(Queue:Browse)
        BRW1::RefreshMode = RefreshOnTop
        DO BRW1::RefreshPage
        DO BRW1::PostNewSelection
      END
    ELSE
      IF BRW1::Changed
        FREE(Queue:Browse)
        BRW1::RefreshMode = RefreshOnTop
        DO BRW1::RefreshPage
        DO BRW1::PostNewSelection
      ELSE
        BRW1::LocateMode = LocateOnValue
        DO BRW1::LocateRecord
      END
    END
    IF BRW1::RecordCount
      GET(Queue:Browse,BRW1::CurrentChoice)
      DO BRW1::FillBuffer
    END
    DO BRW1::InitializeBrowse
  ELSE
    IF BRW1::RecordCount
      GET(Queue:Browse,BRW1::CurrentChoice)
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
     IF NOT ?List{PROP:Visible} THEN
        BRW1::LoadPending = True
        EXIT
     END
  END
!----------------------------------------------------------------------
BRW1::FillBuffer ROUTINE
!|
!| This routine fills the record buffer from the BrowseBox queue. This gives the appearance
!| that the record is "fresh" from the disk, without the disk access required.
!|
  COU:Description = BRW1::COU:Description
  CLA:ScheduledTime = BRW1::CLA:ScheduledTime
  FullName = BRW1::FullName
  ENR:MidtermExam = BRW1::ENR:MidtermExam
  ENR:FinalExam = BRW1::ENR:FinalExam
  ENR:TermPaper = BRW1::ENR:TermPaper
  DummyColumn = BRW1::DummyColumn
  ENR:ClassNumber = BRW1::ENR:ClassNumber
  ENR:StudentNumber = BRW1::ENR:StudentNumber
  CLA:ClassNumber = BRW1::CLA:ClassNumber
  COU:Number = BRW1::COU:Number
  STU:Number = BRW1::STU:Number
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
!|    If the field is colorized, the colors are computed and applied.
!|
!| Finally, the POSITION of the current VIEW record is added to the QUEUE
!|
  FullName = CLIP(STU:LastName) & ', ' & STU:FirstName
  BRW1::COU:Description = COU:Description
  BRW1::COU:Description:NormalFG = -2147483645
  BRW1::COU:Description:NormalBG = -1
  BRW1::COU:Description:SelectedFG = -2147483645
  BRW1::COU:Description:SelectedBG = -1
  BRW1::CLA:ScheduledTime = CLA:ScheduledTime
  BRW1::CLA:ScheduledTime:NormalFG = -2147483645
  BRW1::CLA:ScheduledTime:NormalBG = -1
  BRW1::CLA:ScheduledTime:SelectedFG = -2147483645
  BRW1::CLA:ScheduledTime:SelectedBG = -1
  BRW1::FullName = FullName
  BRW1::FullName:NormalFG = -2147483645
  BRW1::FullName:NormalBG = -1
  BRW1::FullName:SelectedFG = -2147483645
  BRW1::FullName:SelectedBG = -1
  BRW1::ENR:MidtermExam = ENR:MidtermExam
  BRW1::ENR:FinalExam = ENR:FinalExam
  BRW1::ENR:TermPaper = ENR:TermPaper
  BRW1::DummyColumn = DummyColumn
  BRW1::ENR:ClassNumber = ENR:ClassNumber
  BRW1::ENR:StudentNumber = ENR:StudentNumber
  BRW1::CLA:ClassNumber = CLA:ClassNumber
  BRW1::COU:Number = COU:Number
  BRW1::STU:Number = STU:Number
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
    POST(EVENT:NewSelection,?List)
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
     IF NOT ?List{PROP:Visible} THEN
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
    BRW1::CurrentChoice = CHOICE(?List)
    GET(Queue:Browse,BRW1::CurrentChoice)
    DO BRW1::FillBuffer
    IF BRW1::RecordCount = ?List{PROP:Items}
      IF NOT ?List{PROP:VScroll}
        ?List{PROP:VScroll} = True
      END
    ELSE
      IF ?List{PROP:VScroll}
        ?List{PROP:VScroll} = False
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
    ?List{PROP:SelStart} = BRW1::CurrentChoice
    DO BRW1::PostNewSelection
  CASE BRW1::SortOrder
  OF 1
    BRW1::CurrentScroll = 50                               ! Move Thumb to center
    IF BRW1::RecordCount = ?List{PROP:Items}
      IF BRW1::ItemsToFill
        IF BRW1::CurrentEvent = EVENT:ScrollUp
          BRW1::CurrentScroll = 0
        ELSE
          BRW1::CurrentScroll = 100
        END
      END
    ELSE
      BRW1::CurrentScroll = 0
    END
  END
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
  BRW1::ItemsToFill = ?List{PROP:Items}
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
  FREE(Queue:Browse)
  BRW1::RecordCount = 0
  DO BRW1::Reset
  BRW1::ItemsToFill = ?List{PROP:Items}
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
       IF ?List{PROPLIST:MouseDownRow}>0
         ?List{PROP:Selected} = ?List{PROPLIST:MouseDownRow}
         BRW1::CurrentChoice = CHOICE(?List)
       END
       DO BRW1::NewSelection
    OF MouseLeft2
      !Setup Edit-in-place
      GET(Queue:Browse, CHOICE())                 !Get highlighted queue record
      ENR:ClassNumber = BRW1::ENR:ClassNumber     !Assign linking fields to get Enrollment record
      ENR:StudentNumber = BRW1::STU:Number        ! when it's time to write changes to disk
      CASE ?List{PROP:Column}                     !Detect which column the user double-clicked
      OF 1                                        ! disallow editting columns 1, 2, and 3
      OROF 2                  
      OROF 3
        ?List{PROP:Column} = 4                    ! by setting edit column to 4
      OROF 4
        ?EditEntry{PROP:Use} = BRW1::ENR:MidtermExam ! then assign the appropriate USE variable
      OF 5
        ?EditEntry{PROP:Use} = BRW1::ENR:FinalExam    
      OF 6
        ?EditEntry{PROP:Use} = BRW1::ENR:TermPaper    
      END                                         !Close CASE structure
      EditColumn = ?List{Prop:Column}             !Save the edit column number
      ?List{Prop:Edit,EditColumn} = ?EditEntry    !Activate edit in place
      SELECT(?EditEntry)
      EXIT
    ELSE                                                   ! ELSE (What keycode was hit)
      CASE BRW1::SortOrder
      OF 1
        IF CHR(KEYCHAR())
          IF UPPER(SUB(ENR:ClassNumber,1,1)) = UPPER(CHR(KEYCHAR()))
            BRW1::CurrentEvent = EVENT:ScrollDown
            DO BRW1::ScrollOne
            GET(Queue:Browse,BRW1::CurrentChoice)
            DO BRW1::FillBuffer
          END
          IF UPPER(SUB(ENR:ClassNumber,1,1)) = UPPER(CHR(KEYCHAR()))
            ?List{PROP:SelStart} = BRW1::CurrentChoice
          ELSE
            ENR:ClassNumber = CHR(KEYCHAR())
            CLEAR(ENR:StudentNumber)
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
  IF ?List{PROP:VScrollPos} <= 1
    POST(EVENT:ScrollTop,?List)
  ELSIF ?List{PROP:VScrollPos} = 100
    POST(EVENT:ScrollBottom,?List)
  ELSE
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
      GET(Queue:Browse,BRW1::RecordCount)                  ! Get the first queue item
    ELSE
      GET(Queue:Browse,1)                                  ! Get the first queue item
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
        StandardWarning(Warn:RecordFetchError,'Enrollment')
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
      IF BRW1::RecordCount = ?List{PROP:Items}
        IF BRW1::FillDirection = FillForward
          GET(Queue:Browse,1)                              ! Get the first queue item
        ELSE
          GET(Queue:Browse,BRW1::RecordCount)              ! Get the first queue item
        END
        DELETE(Queue:Browse)
        BRW1::RecordCount -= 1
      END
      DO BRW1::FillQueue
      IF BRW1::FillDirection = FillForward
        ADD(Queue:Browse)
      ELSE
        ADD(Queue:Browse,1)
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
       BRW1::View:Browse{PROP:Order} = '+ENR:ClassNumber,+ENR:StudentNumber'
    END
    IF BRW1::LocateMode = LocateOnEdit
      BRW1::HighlightedPosition = POSITION(ENR:SeqStu)
      RESET(ENR:SeqStu,BRW1::HighlightedPosition)
    ELSE
      SET(ENR:SeqStu,ENR:SeqStu)
    END
    BRW1::View:Browse{Prop:Filter} = 'CLA:ClassNumber <<> 0'
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
  FREE(Queue:Browse)
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
!| records in the browse queue (Queue:Browse), then the current page is reloaded, and the
!| currently selected item remains selected.
!|
  IF NOT BRW1::ActiveInvisible THEN
     IF NOT ?List{PROP:Visible} THEN
        BRW1::LoadPending = True
        EXIT
     END
  END
  SETCURSOR(Cursor:Wait)
  IF BRW1::RefreshMode = RefreshOnPosition
    BRW1::HighlightedPosition = POSITION(BRW1::View:Browse)
    RESET(BRW1::View:Browse,BRW1::HighlightedPosition)
    BRW1::RefreshMode = RefreshOnTop
  ELSIF RECORDS(Queue:Browse)
    GET(Queue:Browse,BRW1::CurrentChoice)
    IF ERRORCODE()
      GET(Queue:Browse,RECORDS(Queue:Browse))
    END
    BRW1::HighlightedPosition = BRW1::Position
    GET(Queue:Browse,1)
    RESET(BRW1::View:Browse,BRW1::Position)
    BRW1::RefreshMode = RefreshOnCurrent
  ELSE
    BRW1::HighlightedPosition = ''
    DO BRW1::Reset
  END
  FREE(Queue:Browse)
  BRW1::RecordCount = 0
  BRW1::ItemsToFill = ?List{PROP:Items}
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
        GET(Queue:Browse,BRW1::CurrentChoice)
        IF BRW1::Position = BRW1::HighlightedPosition THEN BREAK.
      END
      IF BRW1::CurrentChoice > BRW1::RecordCount
        BRW1::CurrentChoice = BRW1::RecordCount
      END
    ELSE
      IF BRW1::RefreshMode = RefreshOnBottom
        BRW1::CurrentChoice = RECORDS(Queue:Browse)
      ELSE
        BRW1::CurrentChoice = 1
      END
    END
    ?List{Prop:Selected} = BRW1::CurrentChoice
    DO BRW1::FillBuffer
  ELSE
    CLEAR(ENR:Record)
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
     IF NOT ?List{PROP:Visible} THEN
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
       BRW1::View:Browse{PROP:Order} = '+ENR:ClassNumber,+ENR:StudentNumber'
    END
    SET(ENR:SeqStu)
    BRW1::View:Browse{Prop:Filter} = 'CLA:ClassNumber <<> 0'
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
    BRW1::CurrentChoice = CHOICE(?List)
    GET(Queue:Browse,BRW1::CurrentChoice)
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
!!! Generated from procedure template - Window
!!! </summary>
StudentTree PROCEDURE

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
window               WINDOW('Students and Classes'),AT(,,162,183),RESIZE,GRAY,IMM,MDI,HLP('~StudentTree'),SYSTEM
                       LIST,AT(3,5,154,160),USE(?RelTree),VSCROLL,FORMAT('800L*ITS(99)@s200@'),FROM(Queue:RelTree)
                       BUTTON('&Insert'),AT(8,109,45,14),USE(?Insert),HIDE
                       BUTTON('&Change'),AT(58,109,45,14),USE(?Change),HIDE
                       BUTTON('&Delete'),AT(108,109,45,14),USE(?Delete),HIDE
                       BUTTON('&Expand All'),AT(8,168,45,14),USE(?Expand)
                       BUTTON('Co&ntract All'),AT(58,168,45,14),USE(?Contract)
                       BUTTON('Close'),AT(108,168,45,14),USE(?Close)
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
          EXECUTE(POPUP('&Insert|&Change|&Delete'))
            DO REL1::AddEntry
            DO REL1::EditEntry
            DO REL1::RemoveEntry
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
  window{PROP:MinHeight}=183                               ! Restrict the minimum window height
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
REL1::Load:Students ROUTINE
!|
!| This routine is used to load the base level of the RelationTree.
!|
!| First, the Title line is added.
!|
!| Next, each record of the file Students is read. If the record is not filtered,
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
!|   branch can be expanded, so REL1::Level gets a -1. This
!|   value is used by the list box to display a "closed" box next to the entry.
!|
!|   Finally, the queue record that corresponds to the Students record read is
!|   formatted and added to the queue Queue:RelTree. This is done in the routine
!|   REL1::Format:Students.
!|
  REL1::Display = 'Students and Enrollment'
  REL1::Loaded = 0
  REL1::Position = ''
  REL1::Level = 0
  REL1::Icon = 3
  REL1::NormalFG = -1
  REL1::NormalBG = -1
  REL1::SelectedFG = -1
  REL1::SelectedBG = -1
  ADD(Queue:RelTree)
  SET(Students)
  LOOP
    NEXT(Students)
    IF ERRORCODE()
      IF ERRORCODE() = BadRecErr
        BREAK
      ELSE
        StandardWarning(Warn:RecordFetchError,'Courses')
        POST(Event:CloseWindow)
        EXIT
      END
    END
    REL1::Loaded = 0
    REL1::Position = POSITION(Students)
    REL1::Level = 1
    REL1::LoadedLevel = ABS(REL1::Level)
    REL1::LoadedPosition = REL1::Position
    GET(REL1::LoadedQueue,REL1::LoadedLevel,REL1::LoadedPosition)
    IF ERRORCODE() AND NOT REL1::LoadAll
      ENR:StudentNumber = STU:Number
      CLEAR(ENR:ClassNumber,0)
      SET(ENR:StuSeq,ENR:StuSeq)
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
        IF UPPER(ENR:StudentNumber) <> UPPER(STU:Number) THEN BREAK.
        REL1::Level = -1
        BREAK
      END
      DO REL1::Format:Students
      ADD(Queue:RelTree,POINTER(Queue:RelTree)+1)
    ELSE
      IF REL1::LoadAll
        ADD(REL1::LoadedQueue,REL1::LoadedLevel,REL1::LoadedPosition)
      END
      REL1::Level = 1
      REL1::Loaded = True
      DO REL1::Format:Students
      ADD(Queue:RelTree,POINTER(Queue:RelTree)+1)
      DO REL1::Load:Enrollment
    END
  END

!---------------------------------------------------------------------------
REL1::Format:Students ROUTINE
!|
!| This routine formats a line of the display queue Queue:RelTree to display the
!| contents of a record of Students.
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
  DisplayString = CLIP(STU:LastName) & ', ' & STU:FirstName
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
        REGET(Students,REL1::Position)
        DO REL1::Format:Students
      END
      BEGIN
        REGET(Enrollment,REL1::Position)
        DO REL1::Format:Enrollment
      END
    END
    PUT(Queue:RelTree)
    EXECUTE(ABS(REL1::Level))
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
        REGET(Students,REL1::Position)
        DO REL1::Format:Students
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
REL1::Load:Enrollment ROUTINE
!|
!| This routine is used to load the base level of the RelationTree.
!|
!| Next, each record of the file Enrollment is read. If the record is not filtered,
!| the queue record that corresponds to this record is formatted and added to the queue
!| Queue:RelTree. This is done in the routine REL1::Format:Enrollment.
!|
  ENR:StudentNumber = STU:Number
  CLEAR(ENR:ClassNumber)
  SET(ENR:StuSeq,ENR:StuSeq)
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
    IF ENR:StudentNumber <> STU:Number THEN BREAK.
    REL1::Loaded = 0
    REL1::Position = POSITION(Enrollment)
    REL1::Level = 2
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
   CLA:ClassNumber = ENR:ClassNumber                       ! Move value for lookup
   GET(Classes,CLA:KeyClassNumber)                         ! Get value from file
   IF ERRORCODE()                                          ! IF record not found
     CLEAR(CLA:Record)                                     ! Clear the record buffer
   END                                                     ! END (IF record not found)
   COU:Number = CLA:CourseNumber                           ! Move value for lookup
   GET(Courses,COU:KeyNumber)                              ! Get value from file
   IF ERRORCODE()                                          ! IF record not found
     CLEAR(COU:Record)                                     ! Clear the record buffer
   END                                                     ! END (IF record not found)
  DisplayString = CLIP(COU:Description) & ' ' & CLA:ScheduledTime
  REL1::Display = DisplayString
  IF ENR:MidTermExam < 70 OR ENR:FinalExam < 70 OR ENR:TermPaper < 70
    REL1::NormalFG = 255
    REL1::NormalBG = -1
    REL1::SelectedFG = 255
    REL1::SelectedBG = -1
  ELSE
    REL1::NormalFG = -1
    REL1::NormalBG = -1
    REL1::SelectedFG = -1
    REL1::SelectedBG = -1
  END
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
    GET(Students,0)
    CLEAR(STU:Record)
    GlobalRequest = InsertRecord
    UpdateStudents
    IF GlobalResponse = RequestCompleted
      REL1::NewItemLevel = 1
      REL1::NewItemPosition = POSITION(Students)
      DO REL1::RefreshTree
    END
  OF 1
  OROF 2
    LOOP WHILE ABS(REL1::Level) = 2
      REL1::CurrentChoice -= 1
      GET(Queue:RelTree,REL1::CurrentChoice)
    UNTIL ERRORCODE()
    REGET(Students,REL1::Position)
    GET(Enrollment,0)
    CLEAR(ENR:Record)
    ENR:StudentNumber = STU:Number
    GlobalRequest = InsertRecord
    UpdateEnrollment
    IF GlobalResponse = RequestCompleted
      REL1::NewItemLevel = 2
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
    REGET(Students,REL1::Position)
    GlobalRequest = ChangeRecord
    UpdateStudents
    IF GlobalResponse = RequestCompleted
      REL1::NewItemLevel = 1
      REL1::NewItemPosition = POSITION(Students)
      DO REL1::RefreshTree
    END
  OF 2
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
    REGET(Students,REL1::Position)
    GlobalRequest = DeleteRecord
    UpdateStudents
    IF GlobalResponse = RequestCompleted
      DO REL1::RefreshTree
    END
  OF 2
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
!| Next, the routine REL1::Load:Students is called. This routine will
!| call any other routines necessary to rebuild the display.
!|
!| Finally, if a new item has been added (via REL1::AddEntry), then the
!| queue is searched for that entry, and the record is highlighted.
!|
  FREE(Queue:RelTree)
  DO REL1::Load:Students
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
!| are FREEd, and the routine REL1::Load:Students is called, which loads
!| the first level of the RelationTree.
!|
  FREE(Queue:RelTree)
  FREE(REL1::LoadedQueue)
  DO REL1::Load:Students
!---------------------------------------------------------------------------
REL1::ExpandAll ROUTINE
!|
!| This routine expands every branch of the RelationTree.
!|
!| First, The two queues used by the RelationTree (Queue:RelTree and REL1::LoadedQueue)
!| are FREEd.
!|
!| Next, the variable REL1::LoadAll is set to true, and the routine REL1::Load:Students
!| is called. Since REL1::LoadAll is True, all branches are completely loaded.
!|
  FREE(Queue:RelTree)
  FREE(REL1::LoadedQueue)
  REL1::LoadAll = True
  DO REL1::Load:Students
  REL1::LoadAll = False
