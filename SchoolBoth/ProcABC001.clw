

   MEMBER('ProcABC.clw')                                   ! This is a MEMBER module


   INCLUDE('ABBROWSE.INC'),ONCE
   INCLUDE('ABPOPUP.INC'),ONCE
   INCLUDE('ABRESIZE.INC'),ONCE
   INCLUDE('ABTOOLBA.INC'),ONCE
   INCLUDE('ABWINDOW.INC'),ONCE

!!! <summary>
!!! Generated from procedure template - Window
!!! </summary>
AbcClassTree PROCEDURE 

LocalRequest         LONG                                  ! 
OriginalRequest      LONG                                  ! 
LocalResponse        LONG                                  ! 
FilesOpened          BYTE                                  ! 
WindowOpened         LONG                                  ! 
WindowInitialized    LONG                                  ! 
ForceRefresh         LONG                                  ! 
DisplayString        STRING(255)                           ! 
REL1::Toolbar        CLASS(ToolbarReltreeClass)
TakeEvent            PROCEDURE(<*LONG VCR>,WindowManager WM),VIRTUAL
  END
REL1::SaveLevel      BYTE,AUTO
REL1::Action         LONG,AUTO
Queue:RelTree        QUEUE,PRE()                           ! Browsing Queue
REL1::Display        STRING(200)                           ! Queue display string
REL1::NormalFG       LONG
REL1::NormalBG       LONG
REL1::SelectedFG     LONG
REL1::SelectedBG     LONG
REL1::Icon           SHORT
REL1::Level          LONG                                  ! Record level in the tree
REL1::Loaded         SHORT                                 ! Inferior level is loaded
REL1::Position       STRING(1024)                          ! Record POSITION in VIEW
                END
REL1::LoadedQueue    QUEUE,PRE()                           ! Status Queue
REL1::LoadedLevel    LONG                                  ! Record level
REL1::LoadedPosition STRING(1024)                          ! Record POSITION in VIEW
               END
REL1::CurrentLevel   LONG                                  ! Current loaded level
REL1::CurrentChoice  LONG                                  ! Current record
REL1::NewItemLevel   LONG                                  ! Level for a new item
REL1::NewItemPosition STRING(1024)                         ! POSITION of a new record
REL1::LoadAll        LONG
window               WINDOW('ABC Tree List of Classes and Enrollments'),AT(,,162,174),FONT('Segoe UI',10),RESIZE, |
  GRAY,IMM,MDI,HLP('~ClassTree'),SYSTEM
                       LIST,AT(4,5,150,147),USE(?RelTree),VSCROLL,FORMAT('800L|M*ITS(99)@s200@'),FROM(Queue:RelTree)
                       BUTTON('&Expand All'),AT(8,155,45,14),USE(?Expand)
                       BUTTON('Co&ntract All'),AT(58,155,45,14),USE(?Contract)
                       BUTTON('&Insert'),AT(7,103,45,14),USE(?Insert),HIDE
                       BUTTON('&Change'),AT(57,103,45,14),USE(?Change),HIDE
                       BUTTON('&Delete'),AT(107,103,45,14),USE(?Delete),HIDE
                       BUTTON('Close'),AT(108,155,45,14),USE(?Close)
                       BUTTON('Help'),AT(0,0,45,14),USE(?Help),HIDE,STD(STD:Help)
                     END

ThisWindow           CLASS(WindowManager)
Init                   PROCEDURE(),BYTE,PROC,DERIVED
Kill                   PROCEDURE(),BYTE,PROC,DERIVED
TakeAccepted           PROCEDURE(),BYTE,PROC,DERIVED
TakeFieldEvent         PROCEDURE(),BYTE,PROC,DERIVED
TakeNewSelection       PROCEDURE(),BYTE,PROC,DERIVED
TakeWindowEvent        PROCEDURE(),BYTE,PROC,DERIVED
                     END

Toolbar              ToolbarClass
Resizer              CLASS(WindowResizeClass)
Init                   PROCEDURE(BYTE AppStrategy=AppStrategy:Resize,BYTE SetWindowMinSize=False,BYTE SetWindowMaxSize=False)
                     END


  CODE
  GlobalResponse = ThisWindow.Run()                        ! Opens the window and starts an Accept Loop

!---------------------------------------------------------------------------
DefineListboxStyle ROUTINE
!|
!| This routine create all the styles to be shared in this window
!| It`s called after the window open
!|
!---------------------------------------------------------------------------
REL1::NextParent ROUTINE
  GET(Queue:RelTree,CHOICE(?RelTree))
  IF ABS(REL1::Level) > 1
    REL1::SaveLevel = ABS(REL1::Level)-1
    DO REL1::NextSavedLevel
  END

REL1::PreviousParent ROUTINE
  GET(Queue:RelTree,CHOICE(?RelTree))
  IF ABS(REL1::Level) > 1
    REL1::SaveLevel = ABS(REL1::Level)-1
    DO REL1::PreviousSavedLevel
  END

REL1::NextLevel ROUTINE
  GET(Queue:RelTree,CHOICE(?RelTree))
  REL1::SaveLevel = ABS(REL1::Level)
  DO REL1::NextSavedLevel

REL1::NextSavedLevel ROUTINE
  DATA
SavePointer LONG,AUTO
  CODE
  LOOP
    LOOP
      GET(Queue:RelTree,POINTER(Queue:RelTree)+1)
      IF ERRORCODE()
        EXIT                ! Unable to find another record on similar level
      END
    WHILE ABS(REL1::Level) > REL1::SaveLevel
    IF ABS(REL1::Level) = REL1::SaveLevel
      SELECT(?RelTree,POINTER(Queue:RelTree))
      EXIT
    END
    SavePointer = POINTER(Queue:RelTree)
    ?RelTree{PROPLIST:MouseDownRow} = SavePointer
    DO REL1::LoadLevel
    GET(Queue:RelTree,SavePointer)
  END

REL1::PreviousSavedLevel ROUTINE
  DATA
SaveRecords LONG,AUTO
SavePointer LONG,AUTO
  CODE
  LOOP
    LOOP
      GET(Queue:RelTree,POINTER(Queue:RelTree)-1)
      IF ERRORCODE()
        EXIT                ! Unable to find another record on similar level
      END
    WHILE ABS(REL1::Level) > REL1::SaveLevel
    IF ABS(REL1::Level) = REL1::SaveLevel
      SELECT(?RelTree,POINTER(Queue:RelTree))
      EXIT
    END
    SavePointer = POINTER(Queue:RelTree)
    SaveRecords = RECORDS(Queue:RelTree)
    ?RelTree{PROPLIST:MouseDownRow} = SavePointer
    DO REL1::LoadLevel
    IF RECORDS(Queue:RelTree) <> SaveRecords
      SavePointer += 1 + RECORDS(Queue:RelTree) - SaveRecords
    END
    GET(Queue:RelTree,SavePointer)
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
  DATA
SaveRecords LONG,AUTO
SavePointer LONG,AUTO
  CODE
  SavePointer = CHOICE(?RelTree)-1
  LOOP
    SaveRecords = RECORDS(Queue:RelTree)
    ?RelTree{PROPLIST:MouseDownRow} = SavePointer
    DO REL1::LoadLevel
    IF RECORDS(Queue:RelTree) = SaveRecords
      BREAK
    END
    SavePointer += RECORDS(Queue:RelTree) - SaveRecords
  END
  SELECT(?RelTree,SavePointer)

REL1::AssignButtons ROUTINE
  REL1::Toolbar.DeleteButton = ?Delete
  REL1::Toolbar.InsertButton = ?Insert
  REL1::Toolbar.ChangeButton = ?Change
  REL1::Toolbar.HelpButton = ?Help
  Toolbar.SetTarget(?RelTree)

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
  Access:Courses.UseFile
  SET(Courses)
  LOOP
    IF Access:Courses.Next() NOT= Level:Benign
      IF Access:Courses.GetEOF()
        BREAK
      ELSE
        POST(EVENT:CloseWindow)
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
      Access:Classes.UseFile
      SET(CLA:KeyCourseNumber,CLA:KeyCourseNumber)
      LOOP
        IF Access:Classes.Next()
          IF Access:Classes.GetEOF()
            BREAK
          ELSE
            POST(EVENT:CloseWindow)
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
  REL1::CurrentChoice = ?RelTree{PROPLIST:MouseDownRow}
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
  REL1::CurrentChoice = ?RelTree{PROPLIST:MouseDownRow}
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
  Access:Classes.UseFile
  SET(CLA:KeyCourseNumber,CLA:KeyCourseNumber)
  LOOP
    IF Access:Classes.Next()
      IF Access:Classes.GetEOF()
        BREAK
      ELSE
        POST(EVENT:CloseWindow)
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
      Access:Enrollment.UseFile
      SET(ENR:SeqStu,ENR:SeqStu)
      LOOP
        IF Access:Enrollment.Next()
          IF Access:Enrollment.GetEOF()
            BREAK
          ELSE
            POST(EVENT:CloseWindow)
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
  Access:Enrollment.UseFile
  SET(ENR:SeqStu,ENR:SeqStu)
  LOOP
    IF Access:Enrollment.Next()
      IF Access:Enrollment.GetEOF()
        BREAK
      ELSE
        POST(EVENT:CloseWindow)
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
   Access:Students.Fetch(STU:KeyStudentNumber)             ! Get value from file
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
    VCRRequest = VCR:None
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
      OF VCR:Forward
        DO REL1::NextRecord
      OF VCR:Backward
        DO REL1::PreviousRecord
      OF VCR:PageForward
        DO REL1::NextLevel
      OF VCR:PageBackward
        DO REL1::PreviousLevel
      OF VCR:First
        DO REL1::PreviousParent
      OF VCR:Last
        DO REL1::NextParent
      OF VCR:Insert
        DO REL1::PreviousParent
        REL1::Action = InsertRecord
      OF VCR:None
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
  REL1::CurrentChoice = ?RelTree{PROPLIST:MouseDownRow}
  GET(Queue:RelTree,REL1::CurrentChoice)
  CASE ABS(REL1::Level)
  OF 0
    Access:Courses.PrimeRecord
    GlobalRequest = InsertRecord
    UpdateCourses
    IF GlobalResponse = RequestCompleted
      REL1::NewItemLevel = 1
      REL1::NewItemPosition = POSITION(Enrollment)
      DO REL1::RefreshTree
    END
  OF 1
    REGET(Courses,REL1::Position)
    GET(Classes,0)
    CLEAR(Classes)
    CLA:CourseNumber = COU:Number
    Access:Classes.PrimeRecord(1)
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
    CLEAR(Enrollment)
    ENR:ClassNumber = CLA:ClassNumber
    Access:Enrollment.PrimeRecord(1)
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
  IF ?Change{PROP:Disable}
    EXIT
  END
  REL1::CurrentChoice = ?RelTree{PROPLIST:MouseDownRow}
  GET(Queue:RelTree,REL1::CurrentChoice)
  CASE ABS(REL1::Level)
  OF 1
    WATCH(Courses)
    REGET(Courses,REL1::Position)
    GlobalRequest = ChangeRecord
    UpdateCourses
    IF GlobalResponse = RequestCompleted
      REL1::NewItemLevel = 1
      REL1::NewItemPosition = POSITION(Courses)
      DO REL1::RefreshTree
    END
  OF 2
    WATCH(Classes)
    REGET(Classes,REL1::Position)
    GlobalRequest = ChangeRecord
    UpdateClasses
    IF GlobalResponse = RequestCompleted
      REL1::NewItemLevel = 1
      REL1::NewItemPosition = POSITION(Classes)
      DO REL1::RefreshTree
    END
  OF 3
    WATCH(Enrollment)
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
  IF ?Delete{PROP:Disable}
    EXIT
  END
  REL1::CurrentChoice = ?RelTree{PROPLIST:MouseDownRow}
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

ThisWindow.Init PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  GlobalErrors.SetProcedureName('AbcClassTree')
  SELF.Request = GlobalRequest                             ! Store the incoming request
  ReturnValue = PARENT.Init()
  IF ReturnValue THEN RETURN ReturnValue.
  SELF.FirstField = ?RelTree
  SELF.VCRRequest &= VCRRequest
  SELF.Errors &= GlobalErrors                              ! Set this windows ErrorManager to the global ErrorManager
  SELF.AddItem(Toolbar)
  CLEAR(GlobalRequest)                                     ! Clear GlobalRequest after storing locally
  CLEAR(GlobalResponse)
  IF SELF.Request = SelectRecord
     SELF.AddItem(?Close,RequestCancelled)                 ! Add the close control to the window manger
  ELSE
     SELF.AddItem(?Close,RequestCompleted)                 ! Add the close control to the window manger
  END
  Relate:Courses.SetOpenRelated()
  Relate:Courses.Open                                      ! File Courses used by this procedure, so make sure it's RelationManager is open
  Access:Students.UseFile                                  ! File referenced in 'Other Files' so need to inform it's FileManager
  SELF.FilesOpened = True
  DO REL1::ContractAll
  SELF.Open(window)                                        ! Open window
  Do DefineListboxStyle
  window{PROP:MinWidth} = 162                              ! Restrict the minimum window width
  window{PROP:MinHeight} = 174                             ! Restrict the minimum window height
  Resizer.Init(AppStrategy:Spread)                         ! Controls will spread out as the window gets bigger
  SELF.AddItem(Resizer)                                    ! Add resizer to window manager
  Toolbar.AddTarget(REL1::Toolbar, ?RelTree)
  DO REL1::AssignButtons
  ?RelTree{PROP:IconList,1} = '~Closed.ico'
  ?RelTree{PROP:IconList,2} = '~Open.ico'
  ?RelTree{PROP:IconList,3} = '~~open.ico'
  ?RelTree{Prop:Selected} = 1
  ?RelTree{PROP:Alrt,255} = CtrlRight
  ?RelTree{PROP:Alrt,254} = CtrlLeft
  ?RelTree{PROP:Alrt,253} = MouseLeft2
  SELF.SetAlerts()
  RETURN ReturnValue


ThisWindow.Kill PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  ReturnValue = PARENT.Kill()
  IF ReturnValue THEN RETURN ReturnValue.
  IF SELF.FilesOpened
    Relate:Courses.Close
  END
  GlobalErrors.SetProcedureName
  RETURN ReturnValue


ThisWindow.TakeAccepted PROCEDURE

ReturnValue          BYTE,AUTO

Looped BYTE
  CODE
  LOOP                                                     ! This method receive all EVENT:Accepted's
    IF Looped
      RETURN Level:Notify
    ELSE
      Looped = 1
    END
    CASE ACCEPTED()
    OF ?Expand
      SETCURSOR(CURSOR:Wait)
      
    OF ?Contract
      SETCURSOR(CURSOR:Wait)
      
    END
  ReturnValue = PARENT.TakeAccepted()
    CASE ACCEPTED()
    OF ?Expand
      ThisWindow.Update()
      ?RelTree{PROPLIST:MouseDownRow} = CHOICE(?RelTree)
      DO REL1::ExpandAll
      SETCURSOR
      
    OF ?Contract
      ThisWindow.Update()
      ?RelTree{PROPLIST:MouseDownRow} = CHOICE(?RelTree)
      DO REL1::ContractAll
      SETCURSOR
      
    OF ?Insert
      ThisWindow.Update()
      ?RelTree{PropList:MouseDownRow} = CHOICE(?RelTree)
      DO REL1::AddEntry
    OF ?Change
      ThisWindow.Update()
      ?RelTree{PropList:MouseDownRow} = CHOICE(?RelTree)
      DO REL1::EditEntry
    OF ?Delete
      ThisWindow.Update()
      ?RelTree{PropList:MouseDownRow} = CHOICE(?RelTree)
      DO REL1::RemoveEntry
    END
    RETURN ReturnValue
  END
  ReturnValue = Level:Fatal
  RETURN ReturnValue


ThisWindow.TakeFieldEvent PROCEDURE

ReturnValue          BYTE,AUTO

Looped BYTE
  CODE
  LOOP                                                     ! This method receives all field specific events
    IF Looped
      RETURN Level:Notify
    ELSE
      Looped = 1
    END
  CASE FIELD()
  OF ?RelTree
    CASE EVENT()
    ELSE
      CASE EVENT()
      OF EVENT:AlertKey
        CASE KEYCODE()
        OF CtrlRight
          ?RelTree{PropList:MouseDownRow} = CHOICE(?RelTree)
          POST(EVENT:Expanded,?RelTree)
        OF CtrlLeft
          ?RelTree{PropList:MouseDownRow} = CHOICE(?RelTree)
          POST(EVENT:Contracted,?RelTree)
        OF MouseLeft2
          DO REL1::EditEntry
        END
      END
    END
  END
  ReturnValue = PARENT.TakeFieldEvent()
  CASE FIELD()
  OF ?RelTree
    CASE EVENT()
    OF EVENT:Expanded
      DO REL1::LoadLevel
    OF EVENT:Contracted
      DO REL1::UnloadLevel
    END
  END
    RETURN ReturnValue
  END
  ReturnValue = Level:Fatal
  RETURN ReturnValue


ThisWindow.TakeNewSelection PROCEDURE

ReturnValue          BYTE,AUTO

Looped BYTE
  CODE
  LOOP                                                     ! This method receives all NewSelection events
    IF Looped
      RETURN Level:Notify
    ELSE
      Looped = 1
    END
  ReturnValue = PARENT.TakeNewSelection()
    CASE FIELD()
    OF ?RelTree
      CASE KEYCODE()
      OF MouseRight
      OROF AppsKey
        EXECUTE(POPUP('&Insert|&Change|&Delete|-|&Expand All|Co&ntract All'))
          DO REL1::AddEntry
          DO REL1::EditEntry
          DO REL1::RemoveEntry
          DO REL1::ExpandAll
          DO REL1::ContractAll
        END
      END
    END
    RETURN ReturnValue
  END
  ReturnValue = Level:Fatal
  RETURN ReturnValue


ThisWindow.TakeWindowEvent PROCEDURE

ReturnValue          BYTE,AUTO

Looped BYTE
  CODE
  LOOP                                                     ! This method receives all window specific events
    IF Looped
      RETURN Level:Notify
    ELSE
      Looped = 1
    END
  ReturnValue = PARENT.TakeWindowEvent()
    CASE EVENT()
    OF EVENT:GainFocus
      REL1::CurrentChoice = CHOICE(?RelTree)
      GET(Queue:RelTree,REL1::CurrentChoice)
      REL1::NewItemLevel = REL1::Level
      REL1::NewItemPosition = REL1::Position
      DO REL1::RefreshTree
    END
    RETURN ReturnValue
  END
  ReturnValue = Level:Fatal
  RETURN ReturnValue

REL1::Toolbar.TakeEvent PROCEDURE(<*LONG VCR>,WindowManager WM)
  CODE
  CASE ACCEPTED()
  OF Toolbar:Bottom TO Toolbar:Up
    SELF.Control{PROPLIST:MouseDownRow} = CHOICE(SELF.Control) !! Server routines assume this
    EXECUTE(ACCEPTED()-Toolbar:Bottom+1)
      DO REL1::NextParent
      DO REL1::PreviousParent
      DO REL1::NextLevel
      DO REL1::PreviousLevel
      DO REL1::NextRecord
      DO REL1::PreviousRecord
    END
  OF Toolbar:Insert TO Toolbar:Delete
    SELF.Control{PROPLIST:MouseDownRow} = CHOICE(SELF.Control) !! Server routines assume this
    EXECUTE(ACCEPTED()-Toolbar:Insert+1)
      DO REL1::AddEntry
      DO REL1::EditEntry
      DO REL1::RemoveEntry
    END
  ELSE
    PARENT.TakeEvent(VCR,ThisWindow)
  END

Resizer.Init PROCEDURE(BYTE AppStrategy=AppStrategy:Resize,BYTE SetWindowMinSize=False,BYTE SetWindowMaxSize=False)


  CODE
  PARENT.Init(AppStrategy,SetWindowMinSize,SetWindowMaxSize)
  SELF.DeferMoves = False
  SELF.SetParentDefaults()                                 ! Calculate default control parent-child relationships based upon their positions on the window

!!! <summary>
!!! Generated from procedure template - Window
!!! Browse the Classes File
!!! </summary>
AbcBrowseClasses PROCEDURE 

CurrentTab           STRING(80)                            ! 
LocalRequest         LONG                                  ! 
OriginalRequest      LONG                                  ! 
LocalResponse        LONG                                  ! 
FilesOpened          BYTE                                  ! 
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
Queue:Browse:1       QUEUE                            !Queue declaration for browse/combo box using ?Browse:1
CLA:ClassNumber        LIKE(CLA:ClassNumber)          !List box control field - type derived from field
COU:Description        LIKE(COU:Description)          !List box control field - type derived from field
CLA:RoomNumber         LIKE(CLA:RoomNumber)           !List box control field - type derived from field
CLA:ScheduledTime      LIKE(CLA:ScheduledTime)        !List box control field - type derived from field
TEA:LastName           LIKE(TEA:LastName)             !List box control field - type derived from field
CLA:CourseNumber       LIKE(CLA:CourseNumber)         !Browse key field - type derived from field
CLA:TeacherNumber      LIKE(CLA:TeacherNumber)        !Browse key field - type derived from field
TEA:Number             LIKE(TEA:Number)               !Related join file key field - type derived from field
COU:Number             LIKE(COU:Number)               !Related join file key field - type derived from field
Mark                   BYTE                           !Entry's marked status
ViewPosition           STRING(1024)                   !Entry's view position
                     END
QuickWindow          WINDOW('ABC Browse the Classes File'),AT(,,255,198),FONT('Segoe UI',9),RESIZE,GRAY,IMM,MDI, |
  HLP('~BrowseClasses'),SYSTEM
                       LIST,AT(8,30,239,143),USE(?Browse:1),HVSCROLL,FORMAT('[49L(1)|M~Class Number~R@P##-####' & |
  '#P@120L|M~Course~@S30@/49R(1)|M~Room~@n4@80R(1)|M~Scheduled Time~R(0)@s20@]|M[80L|M~' & |
  'Instructor~L(2)@S20@/]|M'),FROM(Queue:Browse:1),IMM,MSG('Browsing Records')
                       BUTTON('&Insert'),AT(57,158,45,14),USE(?Insert:2),HIDE
                       BUTTON('&Change'),AT(106,158,45,14),USE(?Change:2),DEFAULT,HIDE
                       BUTTON('&Delete'),AT(155,158,45,14),USE(?Delete:2),HIDE
                       SHEET,AT(4,4,247,172),USE(?CurrentTab)
                         TAB('by Class Number'),USE(?TAB1)
                         END
                         TAB('by Course Number'),USE(?TAB2)
                         END
                       END
                       BUTTON('Close'),AT(155,180,45,14),USE(?Close)
                       BUTTON('Help'),AT(205,180,45,14),USE(?Help),STD(STD:Help)
                     END

ThisWindow           CLASS(WindowManager)
Init                   PROCEDURE(),BYTE,PROC,DERIVED
Kill                   PROCEDURE(),BYTE,PROC,DERIVED
Run                    PROCEDURE(USHORT Number,BYTE Request),BYTE,PROC,DERIVED
                     END

Toolbar              ToolbarClass
BRW1                 CLASS(BrowseClass)                    ! Browse using ?Browse:1
Q                      &Queue:Browse:1                !Reference to browse queue
Init                   PROCEDURE(SIGNED ListBox,*STRING Posit,VIEW V,QUEUE Q,RelationManager RM,WindowManager WM)
ResetSort              PROCEDURE(BYTE Force),BYTE,PROC,DERIVED
                     END

BRW1::Sort0:Locator  StepLocatorClass                      ! Default Locator
BRW1::Sort1:Locator  StepLocatorClass                      ! Conditional Locator - CHOICE(?CurrentTab) = 2
BRW1::Sort0:StepClass StepLongClass                        ! Default Step Manager
BRW1::Sort1:StepClass StepLongClass                        ! Conditional Step Manager - CHOICE(?CurrentTab) = 2
BRW1::Sort2:StepClass StepLongClass                        ! Conditional Step Manager - CHOICE(?CurrentTab) = 3
Resizer              CLASS(WindowResizeClass)
Init                   PROCEDURE(BYTE AppStrategy=AppStrategy:Resize,BYTE SetWindowMinSize=False,BYTE SetWindowMaxSize=False)
                     END


  CODE
  GlobalResponse = ThisWindow.Run()                        ! Opens the window and starts an Accept Loop

!---------------------------------------------------------------------------
DefineListboxStyle ROUTINE
!|
!| This routine create all the styles to be shared in this window
!| It`s called after the window open
!|
!---------------------------------------------------------------------------

ThisWindow.Init PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  GlobalErrors.SetProcedureName('AbcBrowseClasses')
  SELF.Request = GlobalRequest                             ! Store the incoming request
  ReturnValue = PARENT.Init()
  IF ReturnValue THEN RETURN ReturnValue.
  SELF.FirstField = ?Browse:1
  SELF.VCRRequest &= VCRRequest
  SELF.Errors &= GlobalErrors                              ! Set this windows ErrorManager to the global ErrorManager
  SELF.AddItem(Toolbar)
  CLEAR(GlobalRequest)                                     ! Clear GlobalRequest after storing locally
  CLEAR(GlobalResponse)
  IF SELF.Request = SelectRecord
     SELF.AddItem(?Close,RequestCancelled)                 ! Add the close control to the window manger
  ELSE
     SELF.AddItem(?Close,RequestCompleted)                 ! Add the close control to the window manger
  END
  Relate:Classes.SetOpenRelated()
  Relate:Classes.Open                                      ! File Classes used by this procedure, so make sure it's RelationManager is open
  Access:Courses.UseFile                                   ! File referenced in 'Other Files' so need to inform it's FileManager
  Access:Teachers.UseFile                                  ! File referenced in 'Other Files' so need to inform it's FileManager
  SELF.FilesOpened = True
  BRW1.Init(?Browse:1,Queue:Browse:1.ViewPosition,BRW1::View:Browse,Queue:Browse:1,Relate:Classes,SELF) ! Initialize the browse manager
  SELF.Open(QuickWindow)                                   ! Open window
  ListPropFromQ(?Browse:1,Queue:Browse:1,'Queue:Browse:1')
  Do DefineListboxStyle
  BRW1.Q &= Queue:Browse:1
  BRW1::Sort1:StepClass.Init(+ScrollSort:AllowAlpha)       ! Moveable thumb based upon CLA:CourseNumber for sort order 1
  BRW1.AddSortOrder(BRW1::Sort1:StepClass,CLA:KeyCourseNumber) ! Add the sort order for CLA:KeyCourseNumber for sort order 1
  BRW1.AddLocator(BRW1::Sort1:Locator)                     ! Browse has a locator for sort order 1
  BRW1::Sort1:Locator.Init(,CLA:CourseNumber,1,BRW1)       ! Initialize the browse locator using  using key: CLA:KeyCourseNumber , CLA:CourseNumber
  BRW1::Sort2:StepClass.Init(+ScrollSort:AllowAlpha)       ! Moveable thumb based upon CLA:TeacherNumber for sort order 2
  BRW1.AddSortOrder(BRW1::Sort2:StepClass,CLA:KeyTeacherNumber) ! Add the sort order for CLA:KeyTeacherNumber for sort order 2
  BRW1::Sort0:StepClass.Init(+ScrollSort:AllowAlpha)       ! Moveable thumb based upon CLA:ClassNumber for sort order 3
  BRW1.AddSortOrder(BRW1::Sort0:StepClass,CLA:KeyClassNumber) ! Add the sort order for CLA:KeyClassNumber for sort order 3
  BRW1.AddLocator(BRW1::Sort0:Locator)                     ! Browse has a locator for sort order 3
  BRW1::Sort0:Locator.Init(,CLA:ClassNumber,1,BRW1)        ! Initialize the browse locator using  using key: CLA:KeyClassNumber , CLA:ClassNumber
  BRW1.AddField(CLA:ClassNumber,BRW1.Q.CLA:ClassNumber)    ! Field CLA:ClassNumber is a hot field or requires assignment from browse
  BRW1.AddField(COU:Description,BRW1.Q.COU:Description)    ! Field COU:Description is a hot field or requires assignment from browse
  BRW1.AddField(CLA:RoomNumber,BRW1.Q.CLA:RoomNumber)      ! Field CLA:RoomNumber is a hot field or requires assignment from browse
  BRW1.AddField(CLA:ScheduledTime,BRW1.Q.CLA:ScheduledTime) ! Field CLA:ScheduledTime is a hot field or requires assignment from browse
  BRW1.AddField(TEA:LastName,BRW1.Q.TEA:LastName)          ! Field TEA:LastName is a hot field or requires assignment from browse
  BRW1.AddField(CLA:CourseNumber,BRW1.Q.CLA:CourseNumber)  ! Field CLA:CourseNumber is a hot field or requires assignment from browse
  BRW1.AddField(CLA:TeacherNumber,BRW1.Q.CLA:TeacherNumber) ! Field CLA:TeacherNumber is a hot field or requires assignment from browse
  BRW1.AddField(TEA:Number,BRW1.Q.TEA:Number)              ! Field TEA:Number is a hot field or requires assignment from browse
  BRW1.AddField(COU:Number,BRW1.Q.COU:Number)              ! Field COU:Number is a hot field or requires assignment from browse
  Resizer.Init(AppStrategy:Spread)                         ! Controls will spread out as the window gets bigger
  SELF.AddItem(Resizer)                                    ! Add resizer to window manager
  BRW1.AskProcedure = 1                                    ! Will call: UpdateClasses
  BRW1.AddToolbarTarget(Toolbar)                           ! Browse accepts toolbar control
  BRW1.ToolbarItem.HelpButton = ?Help
  SELF.SetAlerts()
  RETURN ReturnValue


ThisWindow.Kill PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  ReturnValue = PARENT.Kill()
  IF ReturnValue THEN RETURN ReturnValue.
  IF SELF.FilesOpened
    Relate:Classes.Close
  END
  GlobalErrors.SetProcedureName
  RETURN ReturnValue


ThisWindow.Run PROCEDURE(USHORT Number,BYTE Request)

ReturnValue          BYTE,AUTO

  CODE
  ReturnValue = PARENT.Run(Number,Request)
  IF SELF.Request = ViewRecord
    ReturnValue = RequestCancelled                         ! Always return RequestCancelled if the form was opened in ViewRecord mode
  ELSE
    GlobalRequest = Request
    UpdateClasses
    ReturnValue = GlobalResponse
  END
  RETURN ReturnValue


BRW1.Init PROCEDURE(SIGNED ListBox,*STRING Posit,VIEW V,QUEUE Q,RelationManager RM,WindowManager WM)

  CODE
  PARENT.Init(ListBox,Posit,V,Q,RM,WM)
  IF WM.Request <> ViewRecord                              ! If called for anything other than ViewMode, make the insert, change & delete controls available
    SELF.InsertControl=?Insert:2
    SELF.ChangeControl=?Change:2
    SELF.DeleteControl=?Delete:2
  END


BRW1.ResetSort PROCEDURE(BYTE Force)

ReturnValue          BYTE,AUTO

  CODE
  IF CHOICE(?CurrentTab) = 2
    RETURN SELF.SetSort(1,Force)
  ELSIF CHOICE(?CurrentTab) = 3
    RETURN SELF.SetSort(2,Force)
  ELSE
    RETURN SELF.SetSort(3,Force)
  END
  ReturnValue = PARENT.ResetSort(Force)
  RETURN ReturnValue


Resizer.Init PROCEDURE(BYTE AppStrategy=AppStrategy:Resize,BYTE SetWindowMinSize=False,BYTE SetWindowMaxSize=False)


  CODE
  PARENT.Init(AppStrategy,SetWindowMinSize,SetWindowMaxSize)
  SELF.DeferMoves = False
  SELF.SetParentDefaults()                                 ! Calculate default control parent-child relationships based upon their positions on the window

!!! <summary>
!!! Generated from procedure template - Window
!!! Browse the Courses File
!!! </summary>
AbcBrowseCourses PROCEDURE 

CurrentTab           STRING(80)                            ! 
LocalRequest         LONG                                  ! 
OriginalRequest      LONG                                  ! 
LocalResponse        LONG                                  ! 
FilesOpened          BYTE                                  ! 
WindowOpened         LONG                                  ! 
WindowInitialized    LONG                                  ! 
ForceRefresh         LONG                                  ! 
RecordFiltered       LONG                                  ! 
BRW1::View:Browse    VIEW(Courses)
                       PROJECT(COU:Number)
                       PROJECT(COU:Description)
                     END
Queue:Browse:1       QUEUE                            !Queue declaration for browse/combo box using ?Browse:1
COU:Number             LIKE(COU:Number)               !List box control field - type derived from field
COU:Description        LIKE(COU:Description)          !List box control field - type derived from field
Mark                   BYTE                           !Entry's marked status
ViewPosition           STRING(1024)                   !Entry's view position
                     END
QuickWindow          WINDOW('ABC Browse the Courses File'),AT(,,159,188),FONT('Segoe UI',9),RESIZE,GRAY,IMM,MDI, |
  HLP('~BrowseCourses'),SYSTEM
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

ThisWindow           CLASS(WindowManager)
Init                   PROCEDURE(),BYTE,PROC,DERIVED
Kill                   PROCEDURE(),BYTE,PROC,DERIVED
Run                    PROCEDURE(USHORT Number,BYTE Request),BYTE,PROC,DERIVED
                     END

Toolbar              ToolbarClass
BRW1                 CLASS(BrowseClass)                    ! Browse using ?Browse:1
Q                      &Queue:Browse:1                !Reference to browse queue
Init                   PROCEDURE(SIGNED ListBox,*STRING Posit,VIEW V,QUEUE Q,RelationManager RM,WindowManager WM)
                     END

BRW1::Sort0:Locator  StepLocatorClass                      ! Default Locator
BRW1::Sort0:StepClass StepStringClass                      ! Default Step Manager
Resizer              CLASS(WindowResizeClass)
Init                   PROCEDURE(BYTE AppStrategy=AppStrategy:Resize,BYTE SetWindowMinSize=False,BYTE SetWindowMaxSize=False)
                     END


  CODE
  GlobalResponse = ThisWindow.Run()                        ! Opens the window and starts an Accept Loop

!---------------------------------------------------------------------------
DefineListboxStyle ROUTINE
!|
!| This routine create all the styles to be shared in this window
!| It`s called after the window open
!|
!---------------------------------------------------------------------------

ThisWindow.Init PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  GlobalErrors.SetProcedureName('AbcBrowseCourses')
  SELF.Request = GlobalRequest                             ! Store the incoming request
  ReturnValue = PARENT.Init()
  IF ReturnValue THEN RETURN ReturnValue.
  SELF.FirstField = ?Browse:1
  SELF.VCRRequest &= VCRRequest
  SELF.Errors &= GlobalErrors                              ! Set this windows ErrorManager to the global ErrorManager
  SELF.AddItem(Toolbar)
  CLEAR(GlobalRequest)                                     ! Clear GlobalRequest after storing locally
  CLEAR(GlobalResponse)
  IF SELF.Request = SelectRecord
     SELF.AddItem(?Close,RequestCancelled)                 ! Add the close control to the window manger
  ELSE
     SELF.AddItem(?Close,RequestCompleted)                 ! Add the close control to the window manger
  END
  Relate:Courses.SetOpenRelated()
  Relate:Courses.Open                                      ! File Courses used by this procedure, so make sure it's RelationManager is open
  SELF.FilesOpened = True
  BRW1.Init(?Browse:1,Queue:Browse:1.ViewPosition,BRW1::View:Browse,Queue:Browse:1,Relate:Courses,SELF) ! Initialize the browse manager
  SELF.Open(QuickWindow)                                   ! Open window
  Do DefineListboxStyle
  BRW1.Q &= Queue:Browse:1
  BRW1::Sort0:StepClass.Init(+ScrollSort:AllowAlpha,ScrollBy:Alpha) ! Moveable thumb based upon COU:Description for sort order 1
  BRW1.AddSortOrder(BRW1::Sort0:StepClass,COU:KeyDescription) ! Add the sort order for COU:KeyDescription for sort order 1
  BRW1.AddLocator(BRW1::Sort0:Locator)                     ! Browse has a locator for sort order 1
  BRW1::Sort0:Locator.Init(,COU:Description,1,BRW1)        ! Initialize the browse locator using  using key: COU:KeyDescription , COU:Description
  BRW1.AddField(COU:Number,BRW1.Q.COU:Number)              ! Field COU:Number is a hot field or requires assignment from browse
  BRW1.AddField(COU:Description,BRW1.Q.COU:Description)    ! Field COU:Description is a hot field or requires assignment from browse
  Resizer.Init(AppStrategy:Spread)                         ! Controls will spread out as the window gets bigger
  SELF.AddItem(Resizer)                                    ! Add resizer to window manager
  BRW1.AskProcedure = 1                                    ! Will call: UpdateCourses
  BRW1.AddToolbarTarget(Toolbar)                           ! Browse accepts toolbar control
  BRW1.ToolbarItem.HelpButton = ?Help
  SELF.SetAlerts()
  RETURN ReturnValue


ThisWindow.Kill PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  ReturnValue = PARENT.Kill()
  IF ReturnValue THEN RETURN ReturnValue.
  IF SELF.FilesOpened
    Relate:Courses.Close
  END
  GlobalErrors.SetProcedureName
  RETURN ReturnValue


ThisWindow.Run PROCEDURE(USHORT Number,BYTE Request)

ReturnValue          BYTE,AUTO

  CODE
  ReturnValue = PARENT.Run(Number,Request)
  IF SELF.Request = ViewRecord
    ReturnValue = RequestCancelled                         ! Always return RequestCancelled if the form was opened in ViewRecord mode
  ELSE
    GlobalRequest = Request
    UpdateCourses
    ReturnValue = GlobalResponse
  END
  RETURN ReturnValue


BRW1.Init PROCEDURE(SIGNED ListBox,*STRING Posit,VIEW V,QUEUE Q,RelationManager RM,WindowManager WM)

  CODE
  PARENT.Init(ListBox,Posit,V,Q,RM,WM)
  IF WM.Request <> ViewRecord                              ! If called for anything other than ViewMode, make the insert, change & delete controls available
    SELF.InsertControl=?Insert:2
    SELF.ChangeControl=?Change:2
    SELF.DeleteControl=?Delete:2
  END


Resizer.Init PROCEDURE(BYTE AppStrategy=AppStrategy:Resize,BYTE SetWindowMinSize=False,BYTE SetWindowMaxSize=False)


  CODE
  PARENT.Init(AppStrategy,SetWindowMinSize,SetWindowMaxSize)
  SELF.DeferMoves = False
  SELF.SetParentDefaults()                                 ! Calculate default control parent-child relationships based upon their positions on the window

!!! <summary>
!!! Generated from procedure template - Window
!!! Browse the Enrollment File
!!! </summary>
AbcBrowseEnrollment PROCEDURE 

CurrentTab           STRING(80)                            ! 
LocalRequest         LONG                                  ! 
OriginalRequest      LONG                                  ! 
LocalResponse        LONG                                  ! 
FilesOpened          BYTE                                  ! 
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
Queue:Browse:1       QUEUE                            !Queue declaration for browse/combo box using ?Browse:1
ENR:StudentNumber      LIKE(ENR:StudentNumber)        !List box control field - type derived from field
STU:LastName           LIKE(STU:LastName)             !List box control field - type derived from field
STU:FirstName          LIKE(STU:FirstName)            !List box control field - type derived from field
ENR:MidtermExam        LIKE(ENR:MidtermExam)          !List box control field - type derived from field
ENR:FinalExam          LIKE(ENR:FinalExam)            !List box control field - type derived from field
ENR:TermPaper          LIKE(ENR:TermPaper)            !List box control field - type derived from field
ENR:ClassNumber        LIKE(ENR:ClassNumber)          !Browse key field - type derived from field
STU:Number             LIKE(STU:Number)               !Related join file key field - type derived from field
Mark                   BYTE                           !Entry's marked status
ViewPosition           STRING(1024)                   !Entry's view position
                     END
QuickWindow          WINDOW('ABC Browse the Enrollment File'),AT(,,260,188),FONT('Segoe UI',9),RESIZE,GRAY,IMM, |
  MDI,HLP('~BrowseEnrollment'),SYSTEM
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

ThisWindow           CLASS(WindowManager)
Init                   PROCEDURE(),BYTE,PROC,DERIVED
Kill                   PROCEDURE(),BYTE,PROC,DERIVED
Run                    PROCEDURE(USHORT Number,BYTE Request),BYTE,PROC,DERIVED
                     END

Toolbar              ToolbarClass
BRW1                 CLASS(BrowseClass)                    ! Browse using ?Browse:1
Q                      &Queue:Browse:1                !Reference to browse queue
Init                   PROCEDURE(SIGNED ListBox,*STRING Posit,VIEW V,QUEUE Q,RelationManager RM,WindowManager WM)
ResetSort              PROCEDURE(BYTE Force),BYTE,PROC,DERIVED
                     END

BRW1::Sort0:StepClass StepLongClass                        ! Default Step Manager
BRW1::Sort1:StepClass StepLongClass                        ! Conditional Step Manager - CHOICE(?CurrentTab) = 2
Resizer              CLASS(WindowResizeClass)
Init                   PROCEDURE(BYTE AppStrategy=AppStrategy:Resize,BYTE SetWindowMinSize=False,BYTE SetWindowMaxSize=False)
                     END


  CODE
  GlobalResponse = ThisWindow.Run()                        ! Opens the window and starts an Accept Loop

!---------------------------------------------------------------------------
DefineListboxStyle ROUTINE
!|
!| This routine create all the styles to be shared in this window
!| It`s called after the window open
!|
!---------------------------------------------------------------------------

ThisWindow.Init PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  GlobalErrors.SetProcedureName('AbcBrowseEnrollment')
  SELF.Request = GlobalRequest                             ! Store the incoming request
  ReturnValue = PARENT.Init()
  IF ReturnValue THEN RETURN ReturnValue.
  SELF.FirstField = ?Browse:1
  SELF.VCRRequest &= VCRRequest
  SELF.Errors &= GlobalErrors                              ! Set this windows ErrorManager to the global ErrorManager
  SELF.AddItem(Toolbar)
  CLEAR(GlobalRequest)                                     ! Clear GlobalRequest after storing locally
  CLEAR(GlobalResponse)
  IF SELF.Request = SelectRecord
     SELF.AddItem(?Close,RequestCancelled)                 ! Add the close control to the window manger
  ELSE
     SELF.AddItem(?Close,RequestCompleted)                 ! Add the close control to the window manger
  END
  Relate:Classes.SetOpenRelated()
  Relate:Classes.Open                                      ! File Classes used by this procedure, so make sure it's RelationManager is open
  Access:Students.UseFile                                  ! File referenced in 'Other Files' so need to inform it's FileManager
  SELF.FilesOpened = True
  BRW1.Init(?Browse:1,Queue:Browse:1.ViewPosition,BRW1::View:Browse,Queue:Browse:1,Relate:Enrollment,SELF) ! Initialize the browse manager
  SELF.Open(QuickWindow)                                   ! Open window
  Do DefineListboxStyle
  BRW1.Q &= Queue:Browse:1
  BRW1::Sort1:StepClass.Init(+ScrollSort:AllowAlpha)       ! Moveable thumb based upon ENR:ClassNumber for sort order 1
  BRW1.AddSortOrder(BRW1::Sort1:StepClass,ENR:SeqStu)      ! Add the sort order for ENR:SeqStu for sort order 1
  BRW1::Sort0:StepClass.Init(+ScrollSort:AllowAlpha)       ! Moveable thumb based upon ENR:StudentNumber for sort order 2
  BRW1.AddSortOrder(BRW1::Sort0:StepClass,ENR:StuSeq)      ! Add the sort order for ENR:StuSeq for sort order 2
  BRW1.AddField(ENR:StudentNumber,BRW1.Q.ENR:StudentNumber) ! Field ENR:StudentNumber is a hot field or requires assignment from browse
  BRW1.AddField(STU:LastName,BRW1.Q.STU:LastName)          ! Field STU:LastName is a hot field or requires assignment from browse
  BRW1.AddField(STU:FirstName,BRW1.Q.STU:FirstName)        ! Field STU:FirstName is a hot field or requires assignment from browse
  BRW1.AddField(ENR:MidtermExam,BRW1.Q.ENR:MidtermExam)    ! Field ENR:MidtermExam is a hot field or requires assignment from browse
  BRW1.AddField(ENR:FinalExam,BRW1.Q.ENR:FinalExam)        ! Field ENR:FinalExam is a hot field or requires assignment from browse
  BRW1.AddField(ENR:TermPaper,BRW1.Q.ENR:TermPaper)        ! Field ENR:TermPaper is a hot field or requires assignment from browse
  BRW1.AddField(ENR:ClassNumber,BRW1.Q.ENR:ClassNumber)    ! Field ENR:ClassNumber is a hot field or requires assignment from browse
  BRW1.AddField(STU:Number,BRW1.Q.STU:Number)              ! Field STU:Number is a hot field or requires assignment from browse
  Resizer.Init(AppStrategy:Spread)                         ! Controls will spread out as the window gets bigger
  SELF.AddItem(Resizer)                                    ! Add resizer to window manager
  BRW1.AskProcedure = 1                                    ! Will call: UpdateEnrollment
  BRW1.AddToolbarTarget(Toolbar)                           ! Browse accepts toolbar control
  BRW1.ToolbarItem.HelpButton = ?Help
  SELF.SetAlerts()
  RETURN ReturnValue


ThisWindow.Kill PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  ReturnValue = PARENT.Kill()
  IF ReturnValue THEN RETURN ReturnValue.
  IF SELF.FilesOpened
    Relate:Classes.Close
  END
  GlobalErrors.SetProcedureName
  RETURN ReturnValue


ThisWindow.Run PROCEDURE(USHORT Number,BYTE Request)

ReturnValue          BYTE,AUTO

  CODE
  ReturnValue = PARENT.Run(Number,Request)
  IF SELF.Request = ViewRecord
    ReturnValue = RequestCancelled                         ! Always return RequestCancelled if the form was opened in ViewRecord mode
  ELSE
    GlobalRequest = Request
    UpdateEnrollment
    ReturnValue = GlobalResponse
  END
  RETURN ReturnValue


BRW1.Init PROCEDURE(SIGNED ListBox,*STRING Posit,VIEW V,QUEUE Q,RelationManager RM,WindowManager WM)

  CODE
  PARENT.Init(ListBox,Posit,V,Q,RM,WM)
  IF WM.Request <> ViewRecord                              ! If called for anything other than ViewMode, make the insert, change & delete controls available
    SELF.InsertControl=?Insert:2
    SELF.ChangeControl=?Change:2
    SELF.DeleteControl=?Delete:2
  END


BRW1.ResetSort PROCEDURE(BYTE Force)

ReturnValue          BYTE,AUTO

  CODE
  IF CHOICE(?CurrentTab) = 2
    RETURN SELF.SetSort(1,Force)
  ELSE
    RETURN SELF.SetSort(2,Force)
  END
  ReturnValue = PARENT.ResetSort(Force)
  RETURN ReturnValue


Resizer.Init PROCEDURE(BYTE AppStrategy=AppStrategy:Resize,BYTE SetWindowMinSize=False,BYTE SetWindowMaxSize=False)


  CODE
  PARENT.Init(AppStrategy,SetWindowMinSize,SetWindowMaxSize)
  SELF.DeferMoves = False
  SELF.SetParentDefaults()                                 ! Calculate default control parent-child relationships based upon their positions on the window

!!! <summary>
!!! Generated from procedure template - Window
!!! Browse the Majors File
!!! </summary>
AbcBrowseMajors PROCEDURE 

CurrentTab           STRING(80)                            ! 
LocalRequest         LONG                                  ! 
OriginalRequest      LONG                                  ! 
LocalResponse        LONG                                  ! 
FilesOpened          BYTE                                  ! 
WindowOpened         LONG                                  ! 
WindowInitialized    LONG                                  ! 
ForceRefresh         LONG                                  ! 
RecordFiltered       LONG                                  ! 
BRW1::View:Browse    VIEW(Majors)
                       PROJECT(MAJ:Description)
                       PROJECT(MAJ:Number)
                     END
Queue:Browse:1       QUEUE                            !Queue declaration for browse/combo box using ?Browse:1
MAJ:Description        LIKE(MAJ:Description)          !List box control field - type derived from field
MAJ:Number             LIKE(MAJ:Number)               !Primary key field - type derived from field
Mark                   BYTE                           !Entry's marked status
ViewPosition           STRING(1024)                   !Entry's view position
                     END
QuickWindow          WINDOW('ABC Browse the Majors File'),AT(,,200,188),FONT('Segoe UI',9),RESIZE,GRAY,IMM,MDI, |
  HLP('~BrowseMajors'),SYSTEM
                       LIST,AT(8,20,184,143),USE(?Browse:1),HVSCROLL,FORMAT('80L(2)|M~Description~L(2)@S20@'),FROM(Queue:Browse:1), |
  IMM,MSG('Browsing Records')
                       BUTTON('&Insert'),AT(49,148,45,14),USE(?Insert:2),HIDE
                       BUTTON('&Change'),AT(98,148,45,14),USE(?Change:2),DEFAULT,HIDE
                       BUTTON('&Delete'),AT(147,148,45,14),USE(?Delete:2),HIDE
                       SHEET,AT(4,4,192,162),USE(?CurrentTab)
                         TAB('by Major Description')
                         END
                       END
                       BUTTON('Close'),AT(102,170,45,14),USE(?Close)
                       BUTTON('Help'),AT(151,170,45,14),USE(?Help),STD(STD:Help)
                     END

ThisWindow           CLASS(WindowManager)
Init                   PROCEDURE(),BYTE,PROC,DERIVED
Kill                   PROCEDURE(),BYTE,PROC,DERIVED
Run                    PROCEDURE(USHORT Number,BYTE Request),BYTE,PROC,DERIVED
                     END

Toolbar              ToolbarClass
BRW1                 CLASS(BrowseClass)                    ! Browse using ?Browse:1
Q                      &Queue:Browse:1                !Reference to browse queue
Init                   PROCEDURE(SIGNED ListBox,*STRING Posit,VIEW V,QUEUE Q,RelationManager RM,WindowManager WM)
ResetSort              PROCEDURE(BYTE Force),BYTE,PROC,DERIVED
                     END

BRW1::Sort0:Locator  StepLocatorClass                      ! Default Locator
BRW1::Sort1:Locator  StepLocatorClass                      ! Conditional Locator - CHOICE(?CurrentTab) = 2
BRW1::Sort0:StepClass StepLongClass                        ! Default Step Manager
BRW1::Sort1:StepClass StepStringClass                      ! Conditional Step Manager - CHOICE(?CurrentTab) = 2
Resizer              CLASS(WindowResizeClass)
Init                   PROCEDURE(BYTE AppStrategy=AppStrategy:Resize,BYTE SetWindowMinSize=False,BYTE SetWindowMaxSize=False)
                     END


  CODE
  GlobalResponse = ThisWindow.Run()                        ! Opens the window and starts an Accept Loop

!---------------------------------------------------------------------------
DefineListboxStyle ROUTINE
!|
!| This routine create all the styles to be shared in this window
!| It`s called after the window open
!|
!---------------------------------------------------------------------------

ThisWindow.Init PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  GlobalErrors.SetProcedureName('AbcBrowseMajors')
  SELF.Request = GlobalRequest                             ! Store the incoming request
  ReturnValue = PARENT.Init()
  IF ReturnValue THEN RETURN ReturnValue.
  SELF.FirstField = ?Browse:1
  SELF.VCRRequest &= VCRRequest
  SELF.Errors &= GlobalErrors                              ! Set this windows ErrorManager to the global ErrorManager
  SELF.AddItem(Toolbar)
  CLEAR(GlobalRequest)                                     ! Clear GlobalRequest after storing locally
  CLEAR(GlobalResponse)
  IF SELF.Request = SelectRecord
     SELF.AddItem(?Close,RequestCancelled)                 ! Add the close control to the window manger
  ELSE
     SELF.AddItem(?Close,RequestCompleted)                 ! Add the close control to the window manger
  END
  Relate:Majors.SetOpenRelated()
  Relate:Majors.Open                                       ! File Majors used by this procedure, so make sure it's RelationManager is open
  SELF.FilesOpened = True
  BRW1.Init(?Browse:1,Queue:Browse:1.ViewPosition,BRW1::View:Browse,Queue:Browse:1,Relate:Majors,SELF) ! Initialize the browse manager
  SELF.Open(QuickWindow)                                   ! Open window
  ListPropFromQ(?Browse:1,Queue:Browse:1,'Queue:Browse:1')
  Do DefineListboxStyle
  BRW1.Q &= Queue:Browse:1
  BRW1::Sort1:StepClass.Init(+ScrollSort:AllowAlpha,ScrollBy:Runtime) ! Moveable thumb based upon MAJ:Description for sort order 1
  BRW1.AddSortOrder(BRW1::Sort1:StepClass,MAJ:KeyDescription) ! Add the sort order for MAJ:KeyDescription for sort order 1
  BRW1.AddLocator(BRW1::Sort1:Locator)                     ! Browse has a locator for sort order 1
  BRW1::Sort1:Locator.Init(,MAJ:Description,1,BRW1)        ! Initialize the browse locator using  using key: MAJ:KeyDescription , MAJ:Description
  BRW1::Sort0:StepClass.Init(+ScrollSort:AllowAlpha)       ! Moveable thumb based upon MAJ:Number for sort order 2
  BRW1.AddSortOrder(BRW1::Sort0:StepClass,MAJ:KeyNumber)   ! Add the sort order for MAJ:KeyNumber for sort order 2
  BRW1.AddLocator(BRW1::Sort0:Locator)                     ! Browse has a locator for sort order 2
  BRW1::Sort0:Locator.Init(,MAJ:Number,1,BRW1)             ! Initialize the browse locator using  using key: MAJ:KeyNumber , MAJ:Number
  BRW1.AddField(MAJ:Description,BRW1.Q.MAJ:Description)    ! Field MAJ:Description is a hot field or requires assignment from browse
  BRW1.AddField(MAJ:Number,BRW1.Q.MAJ:Number)              ! Field MAJ:Number is a hot field or requires assignment from browse
  Resizer.Init(AppStrategy:Spread)                         ! Controls will spread out as the window gets bigger
  SELF.AddItem(Resizer)                                    ! Add resizer to window manager
  BRW1.AskProcedure = 1                                    ! Will call: UpdateMajors
  BRW1.AddToolbarTarget(Toolbar)                           ! Browse accepts toolbar control
  BRW1.ToolbarItem.HelpButton = ?Help
  SELF.SetAlerts()
  RETURN ReturnValue


ThisWindow.Kill PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  ReturnValue = PARENT.Kill()
  IF ReturnValue THEN RETURN ReturnValue.
  IF SELF.FilesOpened
    Relate:Majors.Close
  END
  GlobalErrors.SetProcedureName
  RETURN ReturnValue


ThisWindow.Run PROCEDURE(USHORT Number,BYTE Request)

ReturnValue          BYTE,AUTO

  CODE
  ReturnValue = PARENT.Run(Number,Request)
  IF SELF.Request = ViewRecord
    ReturnValue = RequestCancelled                         ! Always return RequestCancelled if the form was opened in ViewRecord mode
  ELSE
    GlobalRequest = Request
    UpdateMajors
    ReturnValue = GlobalResponse
  END
  RETURN ReturnValue


BRW1.Init PROCEDURE(SIGNED ListBox,*STRING Posit,VIEW V,QUEUE Q,RelationManager RM,WindowManager WM)

  CODE
  PARENT.Init(ListBox,Posit,V,Q,RM,WM)
  IF WM.Request <> ViewRecord                              ! If called for anything other than ViewMode, make the insert, change & delete controls available
    SELF.InsertControl=?Insert:2
    SELF.ChangeControl=?Change:2
    SELF.DeleteControl=?Delete:2
  END


BRW1.ResetSort PROCEDURE(BYTE Force)

ReturnValue          BYTE,AUTO

  CODE
  IF CHOICE(?CurrentTab) = 2
    RETURN SELF.SetSort(1,Force)
  ELSE
    RETURN SELF.SetSort(2,Force)
  END
  ReturnValue = PARENT.ResetSort(Force)
  RETURN ReturnValue


Resizer.Init PROCEDURE(BYTE AppStrategy=AppStrategy:Resize,BYTE SetWindowMinSize=False,BYTE SetWindowMaxSize=False)


  CODE
  PARENT.Init(AppStrategy,SetWindowMinSize,SetWindowMaxSize)
  SELF.DeferMoves = False
  SELF.SetParentDefaults()                                 ! Calculate default control parent-child relationships based upon their positions on the window

!!! <summary>
!!! Generated from procedure template - Window
!!! Browse the Students File
!!! </summary>
AbcBrowseStudents PROCEDURE 

CurrentTab           STRING(80)                            ! 
LocalRequest         LONG                                  ! 
OriginalRequest      LONG                                  ! 
LocalResponse        LONG                                  ! 
FilesOpened          BYTE                                  ! 
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
Queue:Browse:1       QUEUE                            !Queue declaration for browse/combo box using ?Browse:1
STU:LastName           LIKE(STU:LastName)             !List box control field - type derived from field
STU:FirstName          LIKE(STU:FirstName)            !List box control field - type derived from field
MAJ:Description        LIKE(MAJ:Description)          !List box control field - type derived from field
STU:GradYear           LIKE(STU:GradYear)             !List box control field - type derived from field
Junk                   LIKE(Junk)                     !List box control field - type derived from local data
STU:Address            LIKE(STU:Address)              !List box control field - type derived from field
STU:Address2           LIKE(STU:Address2)             !List box control field - type derived from field
STU:City               LIKE(STU:City)                 !List box control field - type derived from field
STU:State              LIKE(STU:State)                !List box control field - type derived from field
STU:Zip                LIKE(STU:Zip)                  !List box control field - type derived from field
STU:Telephone          LIKE(STU:Telephone)            !List box control field - type derived from field
STU:Number             LIKE(STU:Number)               !Primary key field - type derived from field
STU:Major              LIKE(STU:Major)                !Browse key field - type derived from field
MAJ:Number             LIKE(MAJ:Number)               !Related join file key field - type derived from field
Mark                   BYTE                           !Entry's marked status
ViewPosition           STRING(1024)                   !Entry's view position
                     END
QuickWindow          WINDOW('ABC Browse the Students File'),AT(,,358,188),FONT('Segoe UI',9),RESIZE,GRAY,IMM,MDI, |
  HLP('~BrowseStudents'),SYSTEM
                       LIST,AT(8,20,342,143),USE(?Browse:1),HVSCROLL,FORMAT('[80L(2)M~Last Name~@S20@80L(2)|M~' & |
  'First Name~@S20@/80C(2)M~Major~C(0)@S20@40C(2)|M~Grad Year~C(0)@n4@/80C(2)|_M@s20@]|' & |
  'M[123L(2)|M@S20@/80L(2)|M@s20@/83L(2)_M@S20@16L(2)_M@S2@24R(2)|_M@n05@]|M~Address~[5' & |
  '2L(2)~Telephone~@s12@/]|M'),FROM(Queue:Browse:1),IMM,MSG('Browsing Records')
                       BUTTON('&Insert'),AT(207,148,45,14),USE(?Insert:2),HIDE
                       BUTTON('&Change'),AT(256,148,45,14),USE(?Change:2),DEFAULT,HIDE
                       BUTTON('&Delete'),AT(305,148,45,14),USE(?Delete:2),HIDE
                       SHEET,AT(4,4,350,162),USE(?CurrentTab)
                         TAB('by Major'),USE(?TAB1)
                         END
                         TAB('by Last Name'),USE(?TAB2)
                         END
                         TAB('by Grad Year'),USE(?TAB3)
                         END
                       END
                       BUTTON('Close'),AT(260,170,45,14),USE(?Close)
                       BUTTON('Help'),AT(309,170,45,14),USE(?Help),STD(STD:Help)
                       BUTTON('Class Reflection'),AT(3,170,,14),USE(?ReflectionBtn),TIP('CBWndPreview.ClassReflection')
                     END

ThisWindow           CLASS(WindowManager)
Init                   PROCEDURE(),BYTE,PROC,DERIVED
Kill                   PROCEDURE(),BYTE,PROC,DERIVED
Run                    PROCEDURE(USHORT Number,BYTE Request),BYTE,PROC,DERIVED
TakeAccepted           PROCEDURE(),BYTE,PROC,DERIVED
                     END

Toolbar              ToolbarClass
BRW1                 CLASS(BrowseClass)                    ! Browse using ?Browse:1
Q                      &Queue:Browse:1                !Reference to browse queue
Init                   PROCEDURE(SIGNED ListBox,*STRING Posit,VIEW V,QUEUE Q,RelationManager RM,WindowManager WM)
ResetSort              PROCEDURE(BYTE Force),BYTE,PROC,DERIVED
                     END

BRW1::Sort0:Locator  StepLocatorClass                      ! Default Locator
BRW1::Sort1:Locator  StepLocatorClass                      ! Conditional Locator - CHOICE(?CurrentTab) = 2
BRW1::Sort2:Locator  StepLocatorClass                      ! Conditional Locator - CHOICE(?CurrentTab) = 3
BRW1::Sort0:StepClass StepLongClass                        ! Default Step Manager
BRW1::Sort1:StepClass StepStringClass                      ! Conditional Step Manager - CHOICE(?CurrentTab) = 2
BRW1::Sort2:StepClass StepLongClass                        ! Conditional Step Manager - CHOICE(?CurrentTab) = 3
Resizer              CLASS(WindowResizeClass)
Init                   PROCEDURE(BYTE AppStrategy=AppStrategy:Resize,BYTE SetWindowMinSize=False,BYTE SetWindowMaxSize=False)
                     END


  CODE
  GlobalResponse = ThisWindow.Run()                        ! Opens the window and starts an Accept Loop

!---------------------------------------------------------------------------
DefineListboxStyle ROUTINE
!|
!| This routine create all the styles to be shared in this window
!| It`s called after the window open
!|
!---------------------------------------------------------------------------
ReflecttRtn ROUTINE
    DATA
WndPrvCls  CbWndPreviewClass 
ThisWindowRef &ThisWindow    !No matching prototype available 
BRW1Ref &BRW1    !No matching prototype available 
BRW1::Sort1:LocatorRef &BRW1::Sort1:Locator    !No matching prototype available 
ResizerRef &Resizer    !No matching prototype available 
!ThisWindowRef &Group   !No matching prototype available 
    
    CODE
    ThisWindowRef &= ThisWindow
    BRW1Ref &= BRW1
    BRW1::Sort1:LocatorRef &= BRW1::Sort1:Locator
    ResizerRef &= Resizer
    EXECUTE POPUP('ThisWindow' & |
            '|BRW1 BrowseClass' & |
            '|BRW1::Sort1:Locator StepLocatorClass' & |
            '|Resizer WindowResizeClass' & |
            '|Queue:Browse:1 QueueReflection' & |
            '')
         WndPrvCls.ClassReflection(ThisWindowRef,'ThisWindowRef')
         WndPrvCls.ClassReflection(BRW1Ref,'BRW1 BrowseClass')
         WndPrvCls.ClassReflection(BRW1::Sort1:LocatorRef,'BRW1::Sort1:Locator')
         WndPrvCls.ClassReflection(ResizerRef,'Resizer WindowResizeClass')
         WndPrvCls.QueueReflection(Queue:Browse:1,'Queue:Browse:1')
    END

!BRW1::Sort1:Locator  StepLocatorClass                      ! Conditional Locator - CHOICE(?CurrentTab) = 2
!BRW1::Sort2:Locator  StepLocatorClass                      ! Conditional Locator - CHOICE(?CurrentTab) = 3
!BRW1::Sort0:StepClass StepLongClass                        ! Default Step Manager
!BRW1::Sort1:StepClass StepStringClass                      ! Conditional Step Manager - CHOICE(?CurrentTab) = 2
!BRW1::Sort2:StepClass StepLongClass                        ! Conditional Step Manager - CHOICE(?CurrentTab) = 3
!Resizer              CLASS(WindowResizeClass)
    

ThisWindow.Init PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  GlobalErrors.SetProcedureName('AbcBrowseStudents')
  SELF.Request = GlobalRequest                             ! Store the incoming request
  ReturnValue = PARENT.Init()
  IF ReturnValue THEN RETURN ReturnValue.
  SELF.FirstField = ?Browse:1
  SELF.VCRRequest &= VCRRequest
  SELF.Errors &= GlobalErrors                              ! Set this windows ErrorManager to the global ErrorManager
  BIND('Junk',Junk)                                        ! Added by: BrowseBox(ABC)
  SELF.AddItem(Toolbar)
  CLEAR(GlobalRequest)                                     ! Clear GlobalRequest after storing locally
  CLEAR(GlobalResponse)
  IF SELF.Request = SelectRecord
     SELF.AddItem(?Close,RequestCancelled)                 ! Add the close control to the window manger
  ELSE
     SELF.AddItem(?Close,RequestCompleted)                 ! Add the close control to the window manger
  END
  Relate:Students.SetOpenRelated()
  Relate:Students.Open                                     ! File Students used by this procedure, so make sure it's RelationManager is open
  SELF.FilesOpened = True
  BRW1.Init(?Browse:1,Queue:Browse:1.ViewPosition,BRW1::View:Browse,Queue:Browse:1,Relate:Students,SELF) ! Initialize the browse manager
  SELF.Open(QuickWindow)                                   ! Open window
  ListPropFromQ(?Browse:1,Queue:Browse:1,'Queue:Browse:1')
  Do DefineListboxStyle
  BRW1.Q &= Queue:Browse:1
  BRW1::Sort1:StepClass.Init(+ScrollSort:AllowAlpha,ScrollBy:Name) ! Moveable thumb based upon STU:LastName for sort order 1
  BRW1.AddSortOrder(BRW1::Sort1:StepClass,STU:KeyLastName) ! Add the sort order for STU:KeyLastName for sort order 1
  BRW1.AddLocator(BRW1::Sort1:Locator)                     ! Browse has a locator for sort order 1
  BRW1::Sort1:Locator.Init(,STU:LastName,1,BRW1)           ! Initialize the browse locator using  using key: STU:KeyLastName , STU:LastName
  BRW1::Sort2:StepClass.Init(+ScrollSort:AllowAlpha+ScrollSort:Descending) ! Moveable thumb based upon STU:GradYear for sort order 2
  BRW1.AddSortOrder(BRW1::Sort2:StepClass,STU:KeyGradYear) ! Add the sort order for STU:KeyGradYear for sort order 2
  BRW1.AddLocator(BRW1::Sort2:Locator)                     ! Browse has a locator for sort order 2
  BRW1::Sort2:Locator.Init(,STU:GradYear,1,BRW1)           ! Initialize the browse locator using  using key: STU:KeyGradYear , STU:GradYear
  BRW1::Sort0:StepClass.Init(+ScrollSort:AllowAlpha)       ! Moveable thumb based upon STU:Major for sort order 3
  BRW1.AddSortOrder(BRW1::Sort0:StepClass,STU:MajorKey)    ! Add the sort order for STU:MajorKey for sort order 3
  BRW1.AddLocator(BRW1::Sort0:Locator)                     ! Browse has a locator for sort order 3
  BRW1::Sort0:Locator.Init(,STU:Major,1,BRW1)              ! Initialize the browse locator using  using key: STU:MajorKey , STU:Major
  BRW1.AddField(STU:LastName,BRW1.Q.STU:LastName)          ! Field STU:LastName is a hot field or requires assignment from browse
  BRW1.AddField(STU:FirstName,BRW1.Q.STU:FirstName)        ! Field STU:FirstName is a hot field or requires assignment from browse
  BRW1.AddField(MAJ:Description,BRW1.Q.MAJ:Description)    ! Field MAJ:Description is a hot field or requires assignment from browse
  BRW1.AddField(STU:GradYear,BRW1.Q.STU:GradYear)          ! Field STU:GradYear is a hot field or requires assignment from browse
  BRW1.AddField(Junk,BRW1.Q.Junk)                          ! Field Junk is a hot field or requires assignment from browse
  BRW1.AddField(STU:Address,BRW1.Q.STU:Address)            ! Field STU:Address is a hot field or requires assignment from browse
  BRW1.AddField(STU:Address2,BRW1.Q.STU:Address2)          ! Field STU:Address2 is a hot field or requires assignment from browse
  BRW1.AddField(STU:City,BRW1.Q.STU:City)                  ! Field STU:City is a hot field or requires assignment from browse
  BRW1.AddField(STU:State,BRW1.Q.STU:State)                ! Field STU:State is a hot field or requires assignment from browse
  BRW1.AddField(STU:Zip,BRW1.Q.STU:Zip)                    ! Field STU:Zip is a hot field or requires assignment from browse
  BRW1.AddField(STU:Telephone,BRW1.Q.STU:Telephone)        ! Field STU:Telephone is a hot field or requires assignment from browse
  BRW1.AddField(STU:Number,BRW1.Q.STU:Number)              ! Field STU:Number is a hot field or requires assignment from browse
  BRW1.AddField(STU:Major,BRW1.Q.STU:Major)                ! Field STU:Major is a hot field or requires assignment from browse
  BRW1.AddField(MAJ:Number,BRW1.Q.MAJ:Number)              ! Field MAJ:Number is a hot field or requires assignment from browse
  Resizer.Init(AppStrategy:Spread)                         ! Controls will spread out as the window gets bigger
  SELF.AddItem(Resizer)                                    ! Add resizer to window manager
  BRW1.AskProcedure = 1                                    ! Will call: UpdateStudents
  BRW1.AddToolbarTarget(Toolbar)                           ! Browse accepts toolbar control
  BRW1.ToolbarItem.HelpButton = ?Help
  SELF.SetAlerts()
  RETURN ReturnValue


ThisWindow.Kill PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  ReturnValue = PARENT.Kill()
  IF ReturnValue THEN RETURN ReturnValue.
  IF SELF.FilesOpened
    Relate:Students.Close
  END
  GlobalErrors.SetProcedureName
  RETURN ReturnValue


ThisWindow.Run PROCEDURE(USHORT Number,BYTE Request)

ReturnValue          BYTE,AUTO

  CODE
  ReturnValue = PARENT.Run(Number,Request)
  IF SELF.Request = ViewRecord
    ReturnValue = RequestCancelled                         ! Always return RequestCancelled if the form was opened in ViewRecord mode
  ELSE
    GlobalRequest = Request
    UpdateStudents
    ReturnValue = GlobalResponse
  END
  RETURN ReturnValue


ThisWindow.TakeAccepted PROCEDURE

ReturnValue          BYTE,AUTO

Looped BYTE
  CODE
  LOOP                                                     ! This method receive all EVENT:Accepted's
    IF Looped
      RETURN Level:Notify
    ELSE
      Looped = 1
    END
  ReturnValue = PARENT.TakeAccepted()
    CASE ACCEPTED()
    OF ?ReflectionBtn
      ThisWindow.Update()
      DO ReflecttRtn
    END
    RETURN ReturnValue
  END
  ReturnValue = Level:Fatal
  RETURN ReturnValue


BRW1.Init PROCEDURE(SIGNED ListBox,*STRING Posit,VIEW V,QUEUE Q,RelationManager RM,WindowManager WM)

  CODE
  PARENT.Init(ListBox,Posit,V,Q,RM,WM)
  IF WM.Request <> ViewRecord                              ! If called for anything other than ViewMode, make the insert, change & delete controls available
    SELF.InsertControl=?Insert:2
    SELF.ChangeControl=?Change:2
    SELF.DeleteControl=?Delete:2
  END


BRW1.ResetSort PROCEDURE(BYTE Force)

ReturnValue          BYTE,AUTO

  CODE
  IF CHOICE(?CurrentTab) = 2
    RETURN SELF.SetSort(1,Force)
  ELSIF CHOICE(?CurrentTab) = 3
    RETURN SELF.SetSort(2,Force)
  ELSE
    RETURN SELF.SetSort(3,Force)
  END
  ReturnValue = PARENT.ResetSort(Force)
  RETURN ReturnValue


Resizer.Init PROCEDURE(BYTE AppStrategy=AppStrategy:Resize,BYTE SetWindowMinSize=False,BYTE SetWindowMaxSize=False)


  CODE
  PARENT.Init(AppStrategy,SetWindowMinSize,SetWindowMaxSize)
  SELF.DeferMoves = False
  SELF.SetParentDefaults()                                 ! Calculate default control parent-child relationships based upon their positions on the window

!!! <summary>
!!! Generated from procedure template - Window
!!! Browse the Teachers File
!!! </summary>
AbcBrowseTeachers PROCEDURE 

CurrentTab           STRING(80)                            ! 
LocalRequest         LONG                                  ! 
OriginalRequest      LONG                                  ! 
LocalResponse        LONG                                  ! 
FilesOpened          BYTE                                  ! 
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
Queue:Browse:1       QUEUE                            !Queue declaration for browse/combo box using ?Browse:1
MAJ:Description        LIKE(MAJ:Description)          !List box control field - type derived from field
TEA:LastName           LIKE(TEA:LastName)             !List box control field - type derived from field
TEA:FirstName          LIKE(TEA:FirstName)            !List box control field - type derived from field
TEA:Address            LIKE(TEA:Address)              !List box control field - type derived from field
TEA:City               LIKE(TEA:City)                 !List box control field - type derived from field
TEA:State              LIKE(TEA:State)                !List box control field - type derived from field
TEA:Zip                LIKE(TEA:Zip)                  !List box control field - type derived from field
TEA:Telephone          LIKE(TEA:Telephone)            !List box control field - type derived from field
TEA:Number             LIKE(TEA:Number)               !Primary key field - type derived from field
TEA:Department         LIKE(TEA:Department)           !Browse key field - type derived from field
MAJ:Number             LIKE(MAJ:Number)               !Related join file key field - type derived from field
Mark                   BYTE                           !Entry's marked status
ViewPosition           STRING(1024)                   !Entry's view position
                     END
QuickWindow          WINDOW('ABC Browse the Teachers File'),AT(,,358,188),FONT('Segoe UI',9),RESIZE,GRAY,IMM,MDI, |
  HLP('~BrowseTeachers'),SYSTEM
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

ThisWindow           CLASS(WindowManager)
Init                   PROCEDURE(),BYTE,PROC,DERIVED
Kill                   PROCEDURE(),BYTE,PROC,DERIVED
Run                    PROCEDURE(USHORT Number,BYTE Request),BYTE,PROC,DERIVED
                     END

Toolbar              ToolbarClass
BRW1                 CLASS(BrowseClass)                    ! Browse using ?Browse:1
Q                      &Queue:Browse:1                !Reference to browse queue
Init                   PROCEDURE(SIGNED ListBox,*STRING Posit,VIEW V,QUEUE Q,RelationManager RM,WindowManager WM)
ResetSort              PROCEDURE(BYTE Force),BYTE,PROC,DERIVED
                     END

BRW1::Sort0:Locator  StepLocatorClass                      ! Default Locator
BRW1::Sort0:StepClass StepStringClass                      ! Default Step Manager
BRW1::Sort1:StepClass StepLongClass                        ! Conditional Step Manager - CHOICE(?CurrentTab) = 2
Resizer              CLASS(WindowResizeClass)
Init                   PROCEDURE(BYTE AppStrategy=AppStrategy:Resize,BYTE SetWindowMinSize=False,BYTE SetWindowMaxSize=False)
                     END


  CODE
  GlobalResponse = ThisWindow.Run()                        ! Opens the window and starts an Accept Loop

!---------------------------------------------------------------------------
DefineListboxStyle ROUTINE
!|
!| This routine create all the styles to be shared in this window
!| It`s called after the window open
!|
!---------------------------------------------------------------------------

ThisWindow.Init PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  GlobalErrors.SetProcedureName('AbcBrowseTeachers')
  SELF.Request = GlobalRequest                             ! Store the incoming request
  ReturnValue = PARENT.Init()
  IF ReturnValue THEN RETURN ReturnValue.
  SELF.FirstField = ?Browse:1
  SELF.VCRRequest &= VCRRequest
  SELF.Errors &= GlobalErrors                              ! Set this windows ErrorManager to the global ErrorManager
  SELF.AddItem(Toolbar)
  CLEAR(GlobalRequest)                                     ! Clear GlobalRequest after storing locally
  CLEAR(GlobalResponse)
  IF SELF.Request = SelectRecord
     SELF.AddItem(?Close,RequestCancelled)                 ! Add the close control to the window manger
  ELSE
     SELF.AddItem(?Close,RequestCompleted)                 ! Add the close control to the window manger
  END
  Relate:Majors.SetOpenRelated()
  Relate:Majors.Open                                       ! File Majors used by this procedure, so make sure it's RelationManager is open
  SELF.FilesOpened = True
  BRW1.Init(?Browse:1,Queue:Browse:1.ViewPosition,BRW1::View:Browse,Queue:Browse:1,Relate:Teachers,SELF) ! Initialize the browse manager
  SELF.Open(QuickWindow)                                   ! Open window
  ListPropFromQ(?Browse:1,Queue:Browse:1,'Queue:Browse:1')
  Do DefineListboxStyle
  BRW1.Q &= Queue:Browse:1
  BRW1::Sort1:StepClass.Init(+ScrollSort:AllowAlpha)       ! Moveable thumb based upon TEA:Department for sort order 1
  BRW1.AddSortOrder(BRW1::Sort1:StepClass,TEA:KeyDepartment) ! Add the sort order for TEA:KeyDepartment for sort order 1
  BRW1::Sort0:StepClass.Init(+ScrollSort:AllowAlpha,ScrollBy:Name) ! Moveable thumb based upon TEA:LastName for sort order 2
  BRW1.AddSortOrder(BRW1::Sort0:StepClass,TEA:KeyLastName) ! Add the sort order for TEA:KeyLastName for sort order 2
  BRW1.AddLocator(BRW1::Sort0:Locator)                     ! Browse has a locator for sort order 2
  BRW1::Sort0:Locator.Init(,TEA:LastName,1,BRW1)           ! Initialize the browse locator using  using key: TEA:KeyLastName , TEA:LastName
  BRW1.AddField(MAJ:Description,BRW1.Q.MAJ:Description)    ! Field MAJ:Description is a hot field or requires assignment from browse
  BRW1.AddField(TEA:LastName,BRW1.Q.TEA:LastName)          ! Field TEA:LastName is a hot field or requires assignment from browse
  BRW1.AddField(TEA:FirstName,BRW1.Q.TEA:FirstName)        ! Field TEA:FirstName is a hot field or requires assignment from browse
  BRW1.AddField(TEA:Address,BRW1.Q.TEA:Address)            ! Field TEA:Address is a hot field or requires assignment from browse
  BRW1.AddField(TEA:City,BRW1.Q.TEA:City)                  ! Field TEA:City is a hot field or requires assignment from browse
  BRW1.AddField(TEA:State,BRW1.Q.TEA:State)                ! Field TEA:State is a hot field or requires assignment from browse
  BRW1.AddField(TEA:Zip,BRW1.Q.TEA:Zip)                    ! Field TEA:Zip is a hot field or requires assignment from browse
  BRW1.AddField(TEA:Telephone,BRW1.Q.TEA:Telephone)        ! Field TEA:Telephone is a hot field or requires assignment from browse
  BRW1.AddField(TEA:Number,BRW1.Q.TEA:Number)              ! Field TEA:Number is a hot field or requires assignment from browse
  BRW1.AddField(TEA:Department,BRW1.Q.TEA:Department)      ! Field TEA:Department is a hot field or requires assignment from browse
  BRW1.AddField(MAJ:Number,BRW1.Q.MAJ:Number)              ! Field MAJ:Number is a hot field or requires assignment from browse
  Resizer.Init(AppStrategy:Spread)                         ! Controls will spread out as the window gets bigger
  SELF.AddItem(Resizer)                                    ! Add resizer to window manager
  BRW1.AskProcedure = 1                                    ! Will call: UpdateTeachers
  BRW1.AddToolbarTarget(Toolbar)                           ! Browse accepts toolbar control
  BRW1.ToolbarItem.HelpButton = ?Help
  SELF.SetAlerts()
  RETURN ReturnValue


ThisWindow.Kill PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  ReturnValue = PARENT.Kill()
  IF ReturnValue THEN RETURN ReturnValue.
  IF SELF.FilesOpened
    Relate:Majors.Close
  END
  GlobalErrors.SetProcedureName
  RETURN ReturnValue


ThisWindow.Run PROCEDURE(USHORT Number,BYTE Request)

ReturnValue          BYTE,AUTO

  CODE
  ReturnValue = PARENT.Run(Number,Request)
  IF SELF.Request = ViewRecord
    ReturnValue = RequestCancelled                         ! Always return RequestCancelled if the form was opened in ViewRecord mode
  ELSE
    GlobalRequest = Request
    UpdateTeachers
    ReturnValue = GlobalResponse
  END
  RETURN ReturnValue


BRW1.Init PROCEDURE(SIGNED ListBox,*STRING Posit,VIEW V,QUEUE Q,RelationManager RM,WindowManager WM)

  CODE
  PARENT.Init(ListBox,Posit,V,Q,RM,WM)
  IF WM.Request <> ViewRecord                              ! If called for anything other than ViewMode, make the insert, change & delete controls available
    SELF.InsertControl=?Insert:2
    SELF.ChangeControl=?Change:2
    SELF.DeleteControl=?Delete:2
  END


BRW1.ResetSort PROCEDURE(BYTE Force)

ReturnValue          BYTE,AUTO

  CODE
  IF CHOICE(?CurrentTab) = 2
    RETURN SELF.SetSort(1,Force)
  ELSE
    RETURN SELF.SetSort(2,Force)
  END
  ReturnValue = PARENT.ResetSort(Force)
  RETURN ReturnValue


Resizer.Init PROCEDURE(BYTE AppStrategy=AppStrategy:Resize,BYTE SetWindowMinSize=False,BYTE SetWindowMaxSize=False)


  CODE
  PARENT.Init(AppStrategy,SetWindowMinSize,SetWindowMaxSize)
  SELF.DeferMoves = False
  SELF.SetParentDefaults()                                 ! Calculate default control parent-child relationships based upon their positions on the window

!!! <summary>
!!! Generated from procedure template - Window
!!! </summary>
AbcStudentTree PROCEDURE 

LocalRequest         LONG                                  ! 
OriginalRequest      LONG                                  ! 
LocalResponse        LONG                                  ! 
FilesOpened          BYTE                                  ! 
WindowOpened         LONG                                  ! 
WindowInitialized    LONG                                  ! 
ForceRefresh         LONG                                  ! 
DisplayString        STRING(255)                           ! 
REL1::Toolbar        CLASS(ToolbarReltreeClass)
TakeEvent            PROCEDURE(<*LONG VCR>,WindowManager WM),VIRTUAL
  END
REL1::SaveLevel      BYTE,AUTO
REL1::Action         LONG,AUTO
Queue:RelTree        QUEUE,PRE()                           ! Browsing Queue
REL1::Display        STRING(200)                           ! Queue display string
REL1::NormalFG       LONG
REL1::NormalBG       LONG
REL1::SelectedFG     LONG
REL1::SelectedBG     LONG
REL1::Icon           SHORT
REL1::Level          LONG                                  ! Record level in the tree
REL1::Loaded         SHORT                                 ! Inferior level is loaded
REL1::Position       STRING(1024)                          ! Record POSITION in VIEW
                END
REL1::LoadedQueue    QUEUE,PRE()                           ! Status Queue
REL1::LoadedLevel    LONG                                  ! Record level
REL1::LoadedPosition STRING(1024)                          ! Record POSITION in VIEW
               END
REL1::CurrentLevel   LONG                                  ! Current loaded level
REL1::CurrentChoice  LONG                                  ! Current record
REL1::NewItemLevel   LONG                                  ! Level for a new item
REL1::NewItemPosition STRING(1024)                         ! POSITION of a new record
REL1::LoadAll        LONG
window               WINDOW('ABC Students and Classes'),AT(,,162,183),FONT('Segoe UI',10),RESIZE,GRAY,IMM,MDI,HLP('~StudentTree'), |
  SYSTEM
                       LIST,AT(3,5,154,160),USE(?RelTree),VSCROLL,FORMAT('800L*ITS(99)@s200@'),FROM(Queue:RelTree)
                       BUTTON('&Insert'),AT(8,109,45,14),USE(?Insert),HIDE
                       BUTTON('&Change'),AT(58,109,45,14),USE(?Change),HIDE
                       BUTTON('&Delete'),AT(108,109,45,14),USE(?Delete),HIDE
                       BUTTON('&Expand All'),AT(8,168,45,14),USE(?Expand)
                       BUTTON('Co&ntract All'),AT(58,168,45,14),USE(?Contract)
                       BUTTON('Close'),AT(108,168,45,14),USE(?Close)
                       BUTTON('Help'),AT(0,0,45,14),USE(?Help),HIDE,STD(STD:Help)
                     END

ThisWindow           CLASS(WindowManager)
Init                   PROCEDURE(),BYTE,PROC,DERIVED
Kill                   PROCEDURE(),BYTE,PROC,DERIVED
TakeAccepted           PROCEDURE(),BYTE,PROC,DERIVED
TakeFieldEvent         PROCEDURE(),BYTE,PROC,DERIVED
TakeNewSelection       PROCEDURE(),BYTE,PROC,DERIVED
TakeWindowEvent        PROCEDURE(),BYTE,PROC,DERIVED
                     END

Toolbar              ToolbarClass
Resizer              CLASS(WindowResizeClass)
Init                   PROCEDURE(BYTE AppStrategy=AppStrategy:Resize,BYTE SetWindowMinSize=False,BYTE SetWindowMaxSize=False)
                     END


  CODE
  GlobalResponse = ThisWindow.Run()                        ! Opens the window and starts an Accept Loop

!---------------------------------------------------------------------------
DefineListboxStyle ROUTINE
!|
!| This routine create all the styles to be shared in this window
!| It`s called after the window open
!|
!---------------------------------------------------------------------------
REL1::NextParent ROUTINE
  GET(Queue:RelTree,CHOICE(?RelTree))
  IF ABS(REL1::Level) > 1
    REL1::SaveLevel = ABS(REL1::Level)-1
    DO REL1::NextSavedLevel
  END

REL1::PreviousParent ROUTINE
  GET(Queue:RelTree,CHOICE(?RelTree))
  IF ABS(REL1::Level) > 1
    REL1::SaveLevel = ABS(REL1::Level)-1
    DO REL1::PreviousSavedLevel
  END

REL1::NextLevel ROUTINE
  GET(Queue:RelTree,CHOICE(?RelTree))
  REL1::SaveLevel = ABS(REL1::Level)
  DO REL1::NextSavedLevel

REL1::NextSavedLevel ROUTINE
  DATA
SavePointer LONG,AUTO
  CODE
  LOOP
    LOOP
      GET(Queue:RelTree,POINTER(Queue:RelTree)+1)
      IF ERRORCODE()
        EXIT                ! Unable to find another record on similar level
      END
    WHILE ABS(REL1::Level) > REL1::SaveLevel
    IF ABS(REL1::Level) = REL1::SaveLevel
      SELECT(?RelTree,POINTER(Queue:RelTree))
      EXIT
    END
    SavePointer = POINTER(Queue:RelTree)
    ?RelTree{PROPLIST:MouseDownRow} = SavePointer
    DO REL1::LoadLevel
    GET(Queue:RelTree,SavePointer)
  END

REL1::PreviousSavedLevel ROUTINE
  DATA
SaveRecords LONG,AUTO
SavePointer LONG,AUTO
  CODE
  LOOP
    LOOP
      GET(Queue:RelTree,POINTER(Queue:RelTree)-1)
      IF ERRORCODE()
        EXIT                ! Unable to find another record on similar level
      END
    WHILE ABS(REL1::Level) > REL1::SaveLevel
    IF ABS(REL1::Level) = REL1::SaveLevel
      SELECT(?RelTree,POINTER(Queue:RelTree))
      EXIT
    END
    SavePointer = POINTER(Queue:RelTree)
    SaveRecords = RECORDS(Queue:RelTree)
    ?RelTree{PROPLIST:MouseDownRow} = SavePointer
    DO REL1::LoadLevel
    IF RECORDS(Queue:RelTree) <> SaveRecords
      SavePointer += 1 + RECORDS(Queue:RelTree) - SaveRecords
    END
    GET(Queue:RelTree,SavePointer)
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
  DATA
SaveRecords LONG,AUTO
SavePointer LONG,AUTO
  CODE
  SavePointer = CHOICE(?RelTree)-1
  LOOP
    SaveRecords = RECORDS(Queue:RelTree)
    ?RelTree{PROPLIST:MouseDownRow} = SavePointer
    DO REL1::LoadLevel
    IF RECORDS(Queue:RelTree) = SaveRecords
      BREAK
    END
    SavePointer += RECORDS(Queue:RelTree) - SaveRecords
  END
  SELECT(?RelTree,SavePointer)

REL1::AssignButtons ROUTINE
  REL1::Toolbar.DeleteButton = ?Delete
  REL1::Toolbar.InsertButton = ?Insert
  REL1::Toolbar.ChangeButton = ?Change
  REL1::Toolbar.HelpButton = ?Help
  Toolbar.SetTarget(?RelTree)

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
  Access:Students.UseFile
  SET(Students)
  LOOP
    IF Access:Students.Next() NOT= Level:Benign
      IF Access:Students.GetEOF()
        BREAK
      ELSE
        POST(EVENT:CloseWindow)
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
      Access:Enrollment.UseFile
      SET(ENR:StuSeq,ENR:StuSeq)
      LOOP
        IF Access:Enrollment.Next()
          IF Access:Enrollment.GetEOF()
            BREAK
          ELSE
            POST(EVENT:CloseWindow)
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
  REL1::CurrentChoice = ?RelTree{PROPLIST:MouseDownRow}
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
  REL1::CurrentChoice = ?RelTree{PROPLIST:MouseDownRow}
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
  Access:Enrollment.UseFile
  SET(ENR:StuSeq,ENR:StuSeq)
  LOOP
    IF Access:Enrollment.Next()
      IF Access:Enrollment.GetEOF()
        BREAK
      ELSE
        POST(EVENT:CloseWindow)
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
   Access:Classes.Fetch(CLA:KeyClassNumber)                ! Get value from file
   COU:Number = CLA:CourseNumber                           ! Move value for lookup
   Access:Courses.Fetch(COU:KeyNumber)                     ! Get value from file
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
    VCRRequest = VCR:None
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
      OF VCR:Forward
        DO REL1::NextRecord
      OF VCR:Backward
        DO REL1::PreviousRecord
      OF VCR:PageForward
        DO REL1::NextLevel
      OF VCR:PageBackward
        DO REL1::PreviousLevel
      OF VCR:First
        DO REL1::PreviousParent
      OF VCR:Last
        DO REL1::NextParent
      OF VCR:Insert
        DO REL1::PreviousParent
        REL1::Action = InsertRecord
      OF VCR:None
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
  REL1::CurrentChoice = ?RelTree{PROPLIST:MouseDownRow}
  GET(Queue:RelTree,REL1::CurrentChoice)
  CASE ABS(REL1::Level)
  OF 0
    Access:Students.PrimeRecord
    GlobalRequest = InsertRecord
    UpdateStudents
    IF GlobalResponse = RequestCompleted
      REL1::NewItemLevel = 1
      REL1::NewItemPosition = POSITION(Enrollment)
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
    CLEAR(Enrollment)
    ENR:StudentNumber = STU:Number
    Access:Enrollment.PrimeRecord(1)
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
  IF ?Change{PROP:Disable}
    EXIT
  END
  REL1::CurrentChoice = ?RelTree{PROPLIST:MouseDownRow}
  GET(Queue:RelTree,REL1::CurrentChoice)
  CASE ABS(REL1::Level)
  OF 1
    WATCH(Students)
    REGET(Students,REL1::Position)
    GlobalRequest = ChangeRecord
    UpdateStudents
    IF GlobalResponse = RequestCompleted
      REL1::NewItemLevel = 1
      REL1::NewItemPosition = POSITION(Students)
      DO REL1::RefreshTree
    END
  OF 2
    WATCH(Enrollment)
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
  IF ?Delete{PROP:Disable}
    EXIT
  END
  REL1::CurrentChoice = ?RelTree{PROPLIST:MouseDownRow}
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

ThisWindow.Init PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  GlobalErrors.SetProcedureName('AbcStudentTree')
  SELF.Request = GlobalRequest                             ! Store the incoming request
  ReturnValue = PARENT.Init()
  IF ReturnValue THEN RETURN ReturnValue.
  SELF.FirstField = ?RelTree
  SELF.VCRRequest &= VCRRequest
  SELF.Errors &= GlobalErrors                              ! Set this windows ErrorManager to the global ErrorManager
  SELF.AddItem(Toolbar)
  CLEAR(GlobalRequest)                                     ! Clear GlobalRequest after storing locally
  CLEAR(GlobalResponse)
  IF SELF.Request = SelectRecord
     SELF.AddItem(?Close,RequestCancelled)                 ! Add the close control to the window manger
  ELSE
     SELF.AddItem(?Close,RequestCompleted)                 ! Add the close control to the window manger
  END
  Relate:Classes.SetOpenRelated()
  Relate:Classes.Open                                      ! File Classes used by this procedure, so make sure it's RelationManager is open
  Access:Courses.UseFile                                   ! File referenced in 'Other Files' so need to inform it's FileManager
  SELF.FilesOpened = True
  DO REL1::ContractAll
  SELF.Open(window)                                        ! Open window
  ListPropFromQ(?RelTree,Queue:RelTree,'Queue:RelTree')
  Do DefineListboxStyle
  window{PROP:MinWidth} = 162                              ! Restrict the minimum window width
  window{PROP:MinHeight} = 183                             ! Restrict the minimum window height
  Resizer.Init(AppStrategy:Spread)                         ! Controls will spread out as the window gets bigger
  SELF.AddItem(Resizer)                                    ! Add resizer to window manager
  Toolbar.AddTarget(REL1::Toolbar, ?RelTree)
  DO REL1::AssignButtons
  ?RelTree{PROP:IconList,1} = '~Closed.ico'
  ?RelTree{PROP:IconList,2} = '~Open.ico'
  ?RelTree{PROP:IconList,3} = '~~open.ico'
  ?RelTree{Prop:Selected} = 1
  ?RelTree{PROP:Alrt,255} = CtrlRight
  ?RelTree{PROP:Alrt,254} = CtrlLeft
  ?RelTree{PROP:Alrt,253} = MouseLeft2
  SELF.SetAlerts()
  RETURN ReturnValue


ThisWindow.Kill PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  ReturnValue = PARENT.Kill()
  IF ReturnValue THEN RETURN ReturnValue.
  IF SELF.FilesOpened
    Relate:Classes.Close
  END
  GlobalErrors.SetProcedureName
  RETURN ReturnValue


ThisWindow.TakeAccepted PROCEDURE

ReturnValue          BYTE,AUTO

Looped BYTE
  CODE
  LOOP                                                     ! This method receive all EVENT:Accepted's
    IF Looped
      RETURN Level:Notify
    ELSE
      Looped = 1
    END
    CASE ACCEPTED()
    OF ?Expand
      SETCURSOR(CURSOR:Wait)
      
    OF ?Contract
      SETCURSOR(CURSOR:Wait)
      
    END
  ReturnValue = PARENT.TakeAccepted()
    CASE ACCEPTED()
    OF ?Insert
      ThisWindow.Update()
      ?RelTree{PropList:MouseDownRow} = CHOICE(?RelTree)
      DO REL1::AddEntry
    OF ?Change
      ThisWindow.Update()
      ?RelTree{PropList:MouseDownRow} = CHOICE(?RelTree)
      DO REL1::EditEntry
    OF ?Delete
      ThisWindow.Update()
      ?RelTree{PropList:MouseDownRow} = CHOICE(?RelTree)
      DO REL1::RemoveEntry
    OF ?Expand
      ThisWindow.Update()
      ?RelTree{PROPLIST:MouseDownRow} = CHOICE(?RelTree)
      DO REL1::ExpandAll
      SETCURSOR
      
    OF ?Contract
      ThisWindow.Update()
      ?RelTree{PROPLIST:MouseDownRow} = CHOICE(?RelTree)
      DO REL1::ContractAll
      SETCURSOR
      
    END
    RETURN ReturnValue
  END
  ReturnValue = Level:Fatal
  RETURN ReturnValue


ThisWindow.TakeFieldEvent PROCEDURE

ReturnValue          BYTE,AUTO

Looped BYTE
  CODE
  LOOP                                                     ! This method receives all field specific events
    IF Looped
      RETURN Level:Notify
    ELSE
      Looped = 1
    END
  CASE FIELD()
  OF ?RelTree
    CASE EVENT()
    ELSE
      CASE EVENT()
      OF EVENT:AlertKey
        CASE KEYCODE()
        OF CtrlRight
          ?RelTree{PropList:MouseDownRow} = CHOICE(?RelTree)
          POST(EVENT:Expanded,?RelTree)
        OF CtrlLeft
          ?RelTree{PropList:MouseDownRow} = CHOICE(?RelTree)
          POST(EVENT:Contracted,?RelTree)
        OF MouseLeft2
          DO REL1::EditEntry
        END
      END
    END
  END
  ReturnValue = PARENT.TakeFieldEvent()
  CASE FIELD()
  OF ?RelTree
    CASE EVENT()
    OF EVENT:Expanded
      DO REL1::LoadLevel
    OF EVENT:Contracted
      DO REL1::UnloadLevel
    END
  END
    RETURN ReturnValue
  END
  ReturnValue = Level:Fatal
  RETURN ReturnValue


ThisWindow.TakeNewSelection PROCEDURE

ReturnValue          BYTE,AUTO

Looped BYTE
  CODE
  LOOP                                                     ! This method receives all NewSelection events
    IF Looped
      RETURN Level:Notify
    ELSE
      Looped = 1
    END
  ReturnValue = PARENT.TakeNewSelection()
    CASE FIELD()
    OF ?RelTree
      CASE KEYCODE()
      OF MouseRight
      OROF AppsKey
        EXECUTE(POPUP('&Insert|&Change|&Delete'))
          DO REL1::AddEntry
          DO REL1::EditEntry
          DO REL1::RemoveEntry
        END
      END
    END
    RETURN ReturnValue
  END
  ReturnValue = Level:Fatal
  RETURN ReturnValue


ThisWindow.TakeWindowEvent PROCEDURE

ReturnValue          BYTE,AUTO

Looped BYTE
  CODE
  LOOP                                                     ! This method receives all window specific events
    IF Looped
      RETURN Level:Notify
    ELSE
      Looped = 1
    END
  ReturnValue = PARENT.TakeWindowEvent()
    CASE EVENT()
    OF EVENT:GainFocus
      REL1::CurrentChoice = CHOICE(?RelTree)
      GET(Queue:RelTree,REL1::CurrentChoice)
      REL1::NewItemLevel = REL1::Level
      REL1::NewItemPosition = REL1::Position
      DO REL1::RefreshTree
    END
    RETURN ReturnValue
  END
  ReturnValue = Level:Fatal
  RETURN ReturnValue

REL1::Toolbar.TakeEvent PROCEDURE(<*LONG VCR>,WindowManager WM)
  CODE
  CASE ACCEPTED()
  OF Toolbar:Bottom TO Toolbar:Up
    SELF.Control{PROPLIST:MouseDownRow} = CHOICE(SELF.Control) !! Server routines assume this
    EXECUTE(ACCEPTED()-Toolbar:Bottom+1)
      DO REL1::NextParent
      DO REL1::PreviousParent
      DO REL1::NextLevel
      DO REL1::PreviousLevel
      DO REL1::NextRecord
      DO REL1::PreviousRecord
    END
  OF Toolbar:Insert TO Toolbar:Delete
    SELF.Control{PROPLIST:MouseDownRow} = CHOICE(SELF.Control) !! Server routines assume this
    EXECUTE(ACCEPTED()-Toolbar:Insert+1)
      DO REL1::AddEntry
      DO REL1::EditEntry
      DO REL1::RemoveEntry
    END
  ELSE
    PARENT.TakeEvent(VCR,ThisWindow)
  END

Resizer.Init PROCEDURE(BYTE AppStrategy=AppStrategy:Resize,BYTE SetWindowMinSize=False,BYTE SetWindowMaxSize=False)


  CODE
  PARENT.Init(AppStrategy,SetWindowMinSize,SetWindowMaxSize)
  SELF.DeferMoves = False
  SELF.SetParentDefaults()                                 ! Calculate default control parent-child relationships based upon their positions on the window

!!! <summary>
!!! Generated from procedure template - Window
!!! </summary>
AbcUpdateGrades PROCEDURE 

LocalRequest         LONG                                  ! 
OriginalRequest      LONG                                  ! 
LocalResponse        LONG                                  ! 
FilesOpened          BYTE                                  ! 
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
Queue:Browse         QUEUE                            !Queue declaration for browse/combo box using ?List
COU:Description        LIKE(COU:Description)          !List box control field - type derived from field
COU:Description_NormalFG LONG                         !Normal forground color
COU:Description_NormalBG LONG                         !Normal background color
COU:Description_SelectedFG LONG                       !Selected forground color
COU:Description_SelectedBG LONG                       !Selected background color
CLA:ScheduledTime      LIKE(CLA:ScheduledTime)        !List box control field - type derived from field
CLA:ScheduledTime_NormalFG LONG                       !Normal forground color
CLA:ScheduledTime_NormalBG LONG                       !Normal background color
CLA:ScheduledTime_SelectedFG LONG                     !Selected forground color
CLA:ScheduledTime_SelectedBG LONG                     !Selected background color
FullName               LIKE(FullName)                 !List box control field - type derived from local data
FullName_NormalFG      LONG                           !Normal forground color
FullName_NormalBG      LONG                           !Normal background color
FullName_SelectedFG    LONG                           !Selected forground color
FullName_SelectedBG    LONG                           !Selected background color
ENR:MidtermExam        LIKE(ENR:MidtermExam)          !List box control field - type derived from field
ENR:FinalExam          LIKE(ENR:FinalExam)            !List box control field - type derived from field
ENR:TermPaper          LIKE(ENR:TermPaper)            !List box control field - type derived from field
DummyColumn            LIKE(DummyColumn)              !List box control field - type derived from local data
ENR:ClassNumber        LIKE(ENR:ClassNumber)          !Browse hot field - type derived from field
ENR:StudentNumber      LIKE(ENR:StudentNumber)        !Browse key field - type derived from field
CLA:ClassNumber        LIKE(CLA:ClassNumber)          !Related join file key field - type derived from field
COU:Number             LIKE(COU:Number)               !Related join file key field - type derived from field
STU:Number             LIKE(STU:Number)               !Related join file key field - type derived from field
Mark                   BYTE                           !Entry's marked status
ViewPosition           STRING(1024)                   !Entry's view position
                     END
BrowseWindow         WINDOW('ABC Update Grades for a Class'),AT(,,288,140),FONT('Segoe UI',10),CENTER,GRAY,MDI, |
  HLP('~UpdateGrades'),SYSTEM
                       LIST,AT(5,5,275,116),USE(?List),HVSCROLL,COLUMN,FORMAT('67L(1)|_M*~Class~@S30@44L(1)|_M' & |
  '*~Time~@s20@51L(1)|_M*~Student~@s25@33R(8)|_M~Midterm~L(1)@n3@25R(8)|_M~Final~L(1)@n' & |
  '3@41R(8)_~Term Paper~L(1)@n3@4R(2)_@s1@'),FROM(Queue:Browse),IMM,MSG('Browsing Records')
                       BUTTON('&Change'),AT(75,70,42,12),USE(?Change),HIDE
                       BUTTON('Close'),AT(240,125,40,12),USE(?Close)
                       ENTRY(@n3),AT(128,126),USE(?EditEntry),RIGHT(7),HIDE
                       BUTTON('Help'),AT(0,0,45,14),USE(?Help),HIDE,STD(STD:Help)
                     END

ThisWindow           CLASS(WindowManager)
Init                   PROCEDURE(),BYTE,PROC,DERIVED
Kill                   PROCEDURE(),BYTE,PROC,DERIVED
                     END

Toolbar              ToolbarClass
BRW1                 CLASS(BrowseClass)                    ! Browse using ?List
Q                      &Queue:Browse                  !Reference to browse queue
Init                   PROCEDURE(SIGNED ListBox,*STRING Posit,VIEW V,QUEUE Q,RelationManager RM,WindowManager WM)
SetQueueRecord         PROCEDURE(),DERIVED
                     END

BRW1::Sort0:Locator  StepLocatorClass                      ! Default Locator
BRW1::Sort0:StepClass StepLongClass                        ! Default Step Manager
BRW1::EIPManager     BrowseEIPManager                      ! Browse EIP Manager for Browse using ?List

  CODE
  GlobalResponse = ThisWindow.Run()                        ! Opens the window and starts an Accept Loop

!---------------------------------------------------------------------------
DefineListboxStyle ROUTINE
!|
!| This routine create all the styles to be shared in this window
!| It`s called after the window open
!|
!---------------------------------------------------------------------------

ThisWindow.Init PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  GlobalErrors.SetProcedureName('AbcUpdateGrades')
  SELF.Request = GlobalRequest                             ! Store the incoming request
  ReturnValue = PARENT.Init()
  IF ReturnValue THEN RETURN ReturnValue.
  SELF.FirstField = ?List
  SELF.VCRRequest &= VCRRequest
  SELF.Errors &= GlobalErrors                              ! Set this windows ErrorManager to the global ErrorManager
  BIND('FullName',FullName)                                ! Added by: BrowseBox(ABC)
  BIND('DummyColumn',DummyColumn)                          ! Added by: BrowseBox(ABC)
  SELF.AddItem(Toolbar)
  CLEAR(GlobalRequest)                                     ! Clear GlobalRequest after storing locally
  CLEAR(GlobalResponse)
  IF SELF.Request = SelectRecord
     SELF.AddItem(?Close,RequestCancelled)                 ! Add the close control to the window manger
  ELSE
     SELF.AddItem(?Close,RequestCompleted)                 ! Add the close control to the window manger
  END
  Relate:Enrollment.SetOpenRelated()
  Relate:Enrollment.Open                                   ! File Enrollment used by this procedure, so make sure it's RelationManager is open
  SELF.FilesOpened = True
  BRW1.Init(?List,Queue:Browse.ViewPosition,BRW1::View:Browse,Queue:Browse,Relate:Enrollment,SELF) ! Initialize the browse manager
  SELF.Open(BrowseWindow)                                  ! Open window
  Do DefineListboxStyle
  BRW1.Q &= Queue:Browse
  BRW1::Sort0:StepClass.Init(+ScrollSort:AllowNumeric)     ! Moveable thumb based upon ENR:ClassNumber for sort order 1
  BRW1.AddSortOrder(BRW1::Sort0:StepClass,ENR:SeqStu)      ! Add the sort order for ENR:SeqStu for sort order 1
  BRW1.AddLocator(BRW1::Sort0:Locator)                     ! Browse has a locator for sort order 1
  BRW1::Sort0:Locator.Init(,ENR:ClassNumber,1,BRW1)        ! Initialize the browse locator using  using key: ENR:SeqStu , ENR:ClassNumber
  BRW1.SetFilter('(CLA:ClassNumber <<> 0)')                ! Apply filter expression to browse
  BRW1.AddField(COU:Description,BRW1.Q.COU:Description)    ! Field COU:Description is a hot field or requires assignment from browse
  BRW1.AddField(CLA:ScheduledTime,BRW1.Q.CLA:ScheduledTime) ! Field CLA:ScheduledTime is a hot field or requires assignment from browse
  BRW1.AddField(FullName,BRW1.Q.FullName)                  ! Field FullName is a hot field or requires assignment from browse
  BRW1.AddField(ENR:MidtermExam,BRW1.Q.ENR:MidtermExam)    ! Field ENR:MidtermExam is a hot field or requires assignment from browse
  BRW1.AddField(ENR:FinalExam,BRW1.Q.ENR:FinalExam)        ! Field ENR:FinalExam is a hot field or requires assignment from browse
  BRW1.AddField(ENR:TermPaper,BRW1.Q.ENR:TermPaper)        ! Field ENR:TermPaper is a hot field or requires assignment from browse
  BRW1.AddField(DummyColumn,BRW1.Q.DummyColumn)            ! Field DummyColumn is a hot field or requires assignment from browse
  BRW1.AddField(ENR:ClassNumber,BRW1.Q.ENR:ClassNumber)    ! Field ENR:ClassNumber is a hot field or requires assignment from browse
  BRW1.AddField(ENR:StudentNumber,BRW1.Q.ENR:StudentNumber) ! Field ENR:StudentNumber is a hot field or requires assignment from browse
  BRW1.AddField(CLA:ClassNumber,BRW1.Q.CLA:ClassNumber)    ! Field CLA:ClassNumber is a hot field or requires assignment from browse
  BRW1.AddField(COU:Number,BRW1.Q.COU:Number)              ! Field COU:Number is a hot field or requires assignment from browse
  BRW1.AddField(STU:Number,BRW1.Q.STU:Number)              ! Field STU:Number is a hot field or requires assignment from browse
  BRW1.AddToolbarTarget(Toolbar)                           ! Browse accepts toolbar control
  BRW1.ToolbarItem.HelpButton = ?Help
  SELF.SetAlerts()
  RETURN ReturnValue


ThisWindow.Kill PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  ReturnValue = PARENT.Kill()
  IF ReturnValue THEN RETURN ReturnValue.
  IF SELF.FilesOpened
    Relate:Enrollment.Close
  END
  GlobalErrors.SetProcedureName
  RETURN ReturnValue


BRW1.Init PROCEDURE(SIGNED ListBox,*STRING Posit,VIEW V,QUEUE Q,RelationManager RM,WindowManager WM)

  CODE
  PARENT.Init(ListBox,Posit,V,Q,RM,WM)
  SELF.EIP &= BRW1::EIPManager                             ! Set the EIP manager
  SELF.AddEditControl(,1)
  SELF.AddEditControl(,6)
  SELF.AddEditControl(,11)
  SELF.AddEditControl(,19)
  SELF.DeleteAction = EIPAction:Always
  SELF.ArrowAction = EIPAction:Default+EIPAction:Remain+EIPAction:RetainColumn
  IF WM.Request <> ViewRecord                              ! If called for anything other than ViewMode, make the insert, change & delete controls available
    SELF.ChangeControl=?Change
  END


BRW1.SetQueueRecord PROCEDURE

  CODE
  FullName = CLIP(STU:LastName) & ', ' & STU:FirstName
  PARENT.SetQueueRecord
  
  SELF.Q.COU:Description_NormalFG = -2147483645            ! Set color values for COU:Description
  SELF.Q.COU:Description_NormalBG = -1
  SELF.Q.COU:Description_SelectedFG = -2147483645
  SELF.Q.COU:Description_SelectedBG = -1
  SELF.Q.CLA:ScheduledTime_NormalFG = -2147483645          ! Set color values for CLA:ScheduledTime
  SELF.Q.CLA:ScheduledTime_NormalBG = -1
  SELF.Q.CLA:ScheduledTime_SelectedFG = -2147483645
  SELF.Q.CLA:ScheduledTime_SelectedBG = -1
  SELF.Q.FullName_NormalFG = -2147483645                   ! Set color values for FullName
  SELF.Q.FullName_NormalBG = -1
  SELF.Q.FullName_SelectedFG = -2147483645
  SELF.Q.FullName_SelectedBG = -1
  SELF.Q.FullName = FullName                               !Assign formula result to display queue

