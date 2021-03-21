

   MEMBER('ProcABC.clw')                                   ! This is a MEMBER module


   INCLUDE('ABBROWSE.INC'),ONCE
   INCLUDE('ABPOPUP.INC'),ONCE
   INCLUDE('ABRESIZE.INC'),ONCE
   INCLUDE('ABTOOLBA.INC'),ONCE
   INCLUDE('ABUTIL.INC'),ONCE
   INCLUDE('ABWINDOW.INC'),ONCE
   INCLUDE('MENUStyle.INC'),ONCE

!!! <summary>
!!! Generated from procedure template - Splash
!!! </summary>
SplashIt PROCEDURE 

LocalRequest         LONG                                  ! 
OriginalRequest      LONG                                  ! 
LocalResponse        LONG                                  ! 
FilesOpened          BYTE                                  ! 
WindowOpened         LONG                                  ! 
WindowInitialized    LONG                                  ! 
ForceRefresh         LONG                                  ! 
window               WINDOW,AT(,,209,109),FONT('MS Sans Serif',8,COLOR:Black,FONT:regular),NOFRAME,CENTER,GRAY, |
  MDI,PALETTE(256)
                       PANEL,AT(0,0,209,108),BEVEL(6)
                       PANEL,AT(7,6,195,95),BEVEL(-2,1)
                       STRING('SoftVelocity University'),AT(11,18,188,20),USE(?String2),FONT('Arial',18,COLOR:Red, |
  FONT:bold+FONT:italic),CENTER
                       PANEL,AT(13,41,182,12),BEVEL(-1,1,9)
                       STRING('SoftVelocity, Inc. '),AT(11,72,188,18),USE(?String3),FONT('Arial',14,,FONT:bold),CENTER
                     END

ThisWindow           CLASS(WindowManager)
Init                   PROCEDURE(),BYTE,PROC,DERIVED
Kill                   PROCEDURE(),BYTE,PROC,DERIVED
TakeWindowEvent        PROCEDURE(),BYTE,PROC,DERIVED
                     END

Toolbar              ToolbarClass

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
  GlobalErrors.SetProcedureName('SplashIt')
  SELF.Request = GlobalRequest                             ! Store the incoming request
  ReturnValue = PARENT.Init()
  IF ReturnValue THEN RETURN ReturnValue.
  SELF.FirstField = ?String2
  SELF.VCRRequest &= VCRRequest
  SELF.Errors &= GlobalErrors                              ! Set this windows ErrorManager to the global ErrorManager
  CLEAR(GlobalRequest)                                     ! Clear GlobalRequest after storing locally
  CLEAR(GlobalResponse)
  SELF.AddItem(Toolbar)
  SELF.Open(window)                                        ! Open window
  Do DefineListboxStyle
  TARGET{Prop:Timer} = 1000                                ! Close window on timer event, so configure timer
  TARGET{Prop:Alrt,255} = MouseLeft                        ! Alert mouse clicks that will close window
  TARGET{Prop:Alrt,254} = MouseLeft2
  TARGET{Prop:Alrt,253} = MouseRight
  SELF.SetAlerts()
  RETURN ReturnValue


ThisWindow.Kill PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  ReturnValue = PARENT.Kill()
  IF ReturnValue THEN RETURN ReturnValue.
  GlobalErrors.SetProcedureName
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
    OF EVENT:AlertKey
      CASE KEYCODE()
      OF MouseLeft
      OROF MouseLeft2
      OROF MouseRight
        POST(Event:CloseWindow)                            ! Splash window will close on mouse click
      END
    OF EVENT:LoseFocus
        POST(Event:CloseWindow)                            ! Splash window will close when focus is lost
    OF Event:Timer
      POST(Event:CloseWindow)                              ! Splash window will close on event timer
    OF Event:AlertKey
      CASE KEYCODE()                                       ! Splash window will close on mouse click
      OF MouseLeft
      OROF MouseLeft2
      OROF MouseRight
        POST(Event:CloseWindow)
      END
    ELSE
    END
    RETURN ReturnValue
  END
  ReturnValue = Level:Fatal
  RETURN ReturnValue

!!! <summary>
!!! Generated from procedure template - Window
!!! Select a Classes Record
!!! </summary>
SelectClasses PROCEDURE 

CurrentTab           STRING(80)                            ! 
LocalRequest         LONG                                  ! 
OriginalRequest      LONG                                  ! 
LocalResponse        LONG                                  ! 
FilesOpened          BYTE                                  ! 
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
QuickWindow          WINDOW('Drag Class to Enroll in'),AT(,,189,191),FONT('MS Sans Serif',8,COLOR:Black),CENTER, |
  GRAY,IMM,MDI,HLP('~SelectClasses'),SYSTEM
                       LIST,AT(8,30,172,142),USE(?Browse:1),HVSCROLL,DRAGID('Classes'),FORMAT('[52L(2)|M~Class' & |
  ' Number~@P##-#####P@120L(2)|M~Description~@S30@/52C(2)|M~Room~C(0)@n4@80L(2)|M~Sched' & |
  'uled Time~@s20@80L(2)|M~Instructor~@S20@]|M'),FROM(Queue:Browse:1),IMM,MSG('Browsing Records')
                       SHEET,AT(4,4,180,172),USE(?CurrentTab)
                         TAB('by Class Number'),USE(?TAB1)
                         END
                         TAB('by Course Number'),USE(?TAB2)
                         END
                         TAB('by Teacher Number'),USE(?TAB3)
                         END
                       END
                       BUTTON('Help'),AT(0,0,45,14),USE(?Help),HIDE,STD(STD:Help)
                     END

ThisWindow           CLASS(WindowManager)
Init                   PROCEDURE(),BYTE,PROC,DERIVED
Kill                   PROCEDURE(),BYTE,PROC,DERIVED
TakeFieldEvent         PROCEDURE(),BYTE,PROC,DERIVED
TakeWindowEvent        PROCEDURE(),BYTE,PROC,DERIVED
                     END

Toolbar              ToolbarClass
BRW1                 CLASS(BrowseClass)                    ! Browse using ?Browse:1
Q                      &Queue:Browse:1                !Reference to browse queue
ResetSort              PROCEDURE(BYTE Force),BYTE,PROC,DERIVED
TakeKey                PROCEDURE(),BYTE,PROC,DERIVED
                     END

BRW1::Sort0:Locator  StepLocatorClass                      ! Default Locator
BRW1::Sort1:Locator  StepLocatorClass                      ! Conditional Locator - CHOICE(?CurrentTab) = 2
BRW1::Sort2:Locator  StepLocatorClass                      ! Conditional Locator - CHOICE(?CurrentTab) = 3
BRW1::Sort0:StepClass StepLongClass                        ! Default Step Manager
BRW1::Sort1:StepClass StepLongClass                        ! Conditional Step Manager - CHOICE(?CurrentTab) = 2
BRW1::Sort2:StepClass StepLongClass                        ! Conditional Step Manager - CHOICE(?CurrentTab) = 3

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
  GlobalErrors.SetProcedureName('SelectClasses')
  SELF.Request = GlobalRequest                             ! Store the incoming request
  ReturnValue = PARENT.Init()
  IF ReturnValue THEN RETURN ReturnValue.
  SELF.FirstField = ?Browse:1
  SELF.VCRRequest &= VCRRequest
  SELF.Errors &= GlobalErrors                              ! Set this windows ErrorManager to the global ErrorManager
  !Setup inter-thread processes
  LOC:DropThread  = GLO:DropThread
  LOC:DropControl = GLO:DropControl
  LOC:ThreadRef &= GLO:ThreadRef
  
  CLEAR(GlobalRequest)                                     ! Clear GlobalRequest after storing locally
  CLEAR(GlobalResponse)
  SELF.AddItem(Toolbar)
  Relate:Classes.SetOpenRelated()
  Relate:Classes.Open                                      ! File Classes used by this procedure, so make sure it's RelationManager is open
  SELF.FilesOpened = True
  BRW1.Init(?Browse:1,Queue:Browse:1.ViewPosition,BRW1::View:Browse,Queue:Browse:1,Relate:Classes,SELF) ! Initialize the browse manager
  SELF.Open(QuickWindow)                                   ! Open window
  Do DefineListboxStyle
  BRW1.Q &= Queue:Browse:1
  BRW1::Sort1:StepClass.Init(+ScrollSort:AllowAlpha)       ! Moveable thumb based upon CLA:CourseNumber for sort order 1
  BRW1.AddSortOrder(BRW1::Sort1:StepClass,CLA:KeyCourseNumber) ! Add the sort order for CLA:KeyCourseNumber for sort order 1
  BRW1.AddLocator(BRW1::Sort1:Locator)                     ! Browse has a locator for sort order 1
  BRW1::Sort1:Locator.Init(,CLA:CourseNumber,1,BRW1)       ! Initialize the browse locator using  using key: CLA:KeyCourseNumber , CLA:CourseNumber
  BRW1::Sort2:StepClass.Init(+ScrollSort:AllowAlpha)       ! Moveable thumb based upon CLA:TeacherNumber for sort order 2
  BRW1.AddSortOrder(BRW1::Sort2:StepClass,CLA:KeyTeacherNumber) ! Add the sort order for CLA:KeyTeacherNumber for sort order 2
  BRW1.AddLocator(BRW1::Sort2:Locator)                     ! Browse has a locator for sort order 2
  BRW1::Sort2:Locator.Init(,CLA:TeacherNumber,1,BRW1)      ! Initialize the browse locator using  using key: CLA:KeyTeacherNumber , CLA:TeacherNumber
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
  OF ?Browse:1
    CASE EVENT()
    OF EVENT:Drag
      !Pass dragged data
      BRW1.TakeNewSelection()
      SETDROPID(FORMAT(CLA:ClassNumber,@P##-#####P))
      
    END
  END
  ReturnValue = PARENT.TakeFieldEvent()
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
    CASE EVENT()
    OF EVENT:CloseWindow
      !Flag thread closed
      LOC:ThreadRef = 0
    END
  ReturnValue = PARENT.TakeWindowEvent()
    RETURN ReturnValue
  END
  ReturnValue = Level:Fatal
  RETURN ReturnValue


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


BRW1.TakeKey PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  IF RECORDS(SELF.ListQueue) AND KEYCODE() = MouseLeft2
    !Pass dragged data
    SELF.TakeNewSelection()
    SETDROPID(FORMAT(CLA:ClassNumber,@P##-#####P))
    POST(EVENT:Drop,LOC:DropControl,LOC:DropThread)
  END
  
  ReturnValue = PARENT.TakeKey()
  RETURN ReturnValue

!!! <summary>
!!! Generated from procedure template - Window
!!! Select a Courses Record
!!! </summary>
SelectCourses PROCEDURE 

CurrentTab           STRING(80)                            ! 
LocalRequest         LONG                                  ! 
OriginalRequest      LONG                                  ! 
LocalResponse        LONG                                  ! 
FilesOpened          BYTE                                  ! 
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
Queue:Browse:1       QUEUE                            !Queue declaration for browse/combo box using ?Browse:1
COU:Description        LIKE(COU:Description)          !List box control field - type derived from field
COU:Number             LIKE(COU:Number)               !Primary key field - type derived from field
Mark                   BYTE                           !Entry's marked status
ViewPosition           STRING(1024)                   !Entry's view position
                     END
QuickWindow          WINDOW('Drag Course'),AT(,,158,195),FONT('MS Sans Serif',8,COLOR:Black),CENTER,GRAY,IMM,MDI, |
  HLP('~SelectCourses'),SYSTEM
                       LIST,AT(8,20,142,154),USE(?Browse:1),HVSCROLL,DRAGID('Courses'),FORMAT('80L(2)|M~Descri' & |
  'ption~L(2)@S30@'),FROM(Queue:Browse:1),IMM,MSG('Browsing Records')
                       SHEET,AT(4,4,150,177),USE(?CurrentTab)
                         TAB('by Course Description'),USE(?TAB1)
                         END
                       END
                       BUTTON('Help'),AT(0,0,45,14),USE(?Help),HIDE,STD(STD:Help)
                     END

ThisWindow           CLASS(WindowManager)
Init                   PROCEDURE(),BYTE,PROC,DERIVED
Kill                   PROCEDURE(),BYTE,PROC,DERIVED
TakeFieldEvent         PROCEDURE(),BYTE,PROC,DERIVED
TakeWindowEvent        PROCEDURE(),BYTE,PROC,DERIVED
                     END

Toolbar              ToolbarClass
BRW1                 CLASS(BrowseClass)                    ! Browse using ?Browse:1
Q                      &Queue:Browse:1                !Reference to browse queue
TakeKey                PROCEDURE(),BYTE,PROC,DERIVED
                     END

BRW1::Sort0:Locator  StepLocatorClass                      ! Default Locator
BRW1::Sort0:StepClass StepStringClass                      ! Default Step Manager

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
  GlobalErrors.SetProcedureName('SelectCourses')
  SELF.Request = GlobalRequest                             ! Store the incoming request
  ReturnValue = PARENT.Init()
  IF ReturnValue THEN RETURN ReturnValue.
  SELF.FirstField = ?Browse:1
  SELF.VCRRequest &= VCRRequest
  SELF.Errors &= GlobalErrors                              ! Set this windows ErrorManager to the global ErrorManager
  !Setup inter-thread processing
  LOC:DropThread  = GLO:DropThread
  LOC:DropControl = GLO:DropControl
  LOC:ThreadRef &= GLO:ThreadRef
  
  CLEAR(GlobalRequest)                                     ! Clear GlobalRequest after storing locally
  CLEAR(GlobalResponse)
  SELF.AddItem(Toolbar)
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
  BRW1.AddField(COU:Description,BRW1.Q.COU:Description)    ! Field COU:Description is a hot field or requires assignment from browse
  BRW1.AddField(COU:Number,BRW1.Q.COU:Number)              ! Field COU:Number is a hot field or requires assignment from browse
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
  OF ?Browse:1
    CASE EVENT()
    OF EVENT:Drag
      !Pass dragged data
      BRW1.TakeNewSelection()
      SETDROPID(COU:Number)
      
    END
  END
  ReturnValue = PARENT.TakeFieldEvent()
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
    CASE EVENT()
    OF EVENT:CloseWindow
      !Flag thread closed
      LOC:ThreadRef = 0
    END
  ReturnValue = PARENT.TakeWindowEvent()
    RETURN ReturnValue
  END
  ReturnValue = Level:Fatal
  RETURN ReturnValue


BRW1.TakeKey PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  IF RECORDS(SELF.ListQueue) AND KEYCODE() = MouseLeft2
    !Pass dragged data
    SELF.TakeNewSelection()
    SETDROPID(COU:Number)
    POST(EVENT:Drop,LOC:DropControl,LOC:DropThread)
  END
  
  ReturnValue = PARENT.TakeKey()
  RETURN ReturnValue

!!! <summary>
!!! Generated from procedure template - Window
!!! Select a Majors Record
!!! </summary>
SelectMajors PROCEDURE 

CurrentTab           STRING(80)                            ! 
LocalRequest         LONG                                  ! 
OriginalRequest      LONG                                  ! 
LocalResponse        LONG                                  ! 
FilesOpened          BYTE                                  ! 
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
Queue:Browse:1       QUEUE                            !Queue declaration for browse/combo box using ?Browse:1
MAJ:Description        LIKE(MAJ:Description)          !List box control field - type derived from field
MAJ:Number             LIKE(MAJ:Number)               !Primary key field - type derived from field
Mark                   BYTE                           !Entry's marked status
ViewPosition           STRING(1024)                   !Entry's view position
                     END
QuickWindow          WINDOW('Drag Major'),AT(,,159,195),FONT('MS Sans Serif',8,COLOR:Black),CENTER,GRAY,IMM,MDI, |
  HLP('~SelectMajors'),SYSTEM
                       LIST,AT(8,23,142,148),USE(?Browse:1),HVSCROLL,DRAGID('Majors'),FORMAT('80L(2)|M~Descrip' & |
  'tion~L(2)@S20@'),FROM(Queue:Browse:1),IMM,MSG('Browsing Records')
                       SHEET,AT(4,4,150,172),USE(?CurrentTab)
                         TAB('by Major Number'),USE(?TAB1)
                         END
                         TAB('by Major Description'),USE(?TAB2)
                         END
                       END
                       BUTTON('Help'),AT(0,0,45,14),USE(?Help),HIDE,STD(STD:Help)
                     END

ThisWindow           CLASS(WindowManager)
Init                   PROCEDURE(),BYTE,PROC,DERIVED
Kill                   PROCEDURE(),BYTE,PROC,DERIVED
TakeFieldEvent         PROCEDURE(),BYTE,PROC,DERIVED
TakeWindowEvent        PROCEDURE(),BYTE,PROC,DERIVED
                     END

Toolbar              ToolbarClass
BRW1                 CLASS(BrowseClass)                    ! Browse using ?Browse:1
Q                      &Queue:Browse:1                !Reference to browse queue
ResetSort              PROCEDURE(BYTE Force),BYTE,PROC,DERIVED
TakeKey                PROCEDURE(),BYTE,PROC,DERIVED
                     END

BRW1::Sort0:Locator  StepLocatorClass                      ! Default Locator
BRW1::Sort1:Locator  StepLocatorClass                      ! Conditional Locator - CHOICE(?CurrentTab) = 2
BRW1::Sort0:StepClass StepLongClass                        ! Default Step Manager
BRW1::Sort1:StepClass StepStringClass                      ! Conditional Step Manager - CHOICE(?CurrentTab) = 2

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
  GlobalErrors.SetProcedureName('SelectMajors')
  SELF.Request = GlobalRequest                             ! Store the incoming request
  ReturnValue = PARENT.Init()
  IF ReturnValue THEN RETURN ReturnValue.
  SELF.FirstField = ?Browse:1
  SELF.VCRRequest &= VCRRequest
  SELF.Errors &= GlobalErrors                              ! Set this windows ErrorManager to the global ErrorManager
  !Setup inter-thread processing
  LOC:DropThread  = GLO:DropThread
  LOC:DropControl = GLO:DropControl
  LOC:ThreadRef &= GLO:ThreadRef
  
  CLEAR(GlobalRequest)                                     ! Clear GlobalRequest after storing locally
  CLEAR(GlobalResponse)
  SELF.AddItem(Toolbar)
  Relate:Majors.SetOpenRelated()
  Relate:Majors.Open                                       ! File Majors used by this procedure, so make sure it's RelationManager is open
  SELF.FilesOpened = True
  BRW1.Init(?Browse:1,Queue:Browse:1.ViewPosition,BRW1::View:Browse,Queue:Browse:1,Relate:Majors,SELF) ! Initialize the browse manager
  SELF.Open(QuickWindow)                                   ! Open window
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
  OF ?Browse:1
    CASE EVENT()
    OF EVENT:Drag
      !Pass dragged data
      BRW1.TakeNewSelection()
      SETDROPID(MAJ:Number)
      
    END
  END
  ReturnValue = PARENT.TakeFieldEvent()
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
    CASE EVENT()
    OF EVENT:CloseWindow
      !Flag thread closed
      LOC:ThreadRef = 0
    END
  ReturnValue = PARENT.TakeWindowEvent()
    RETURN ReturnValue
  END
  ReturnValue = Level:Fatal
  RETURN ReturnValue


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


BRW1.TakeKey PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  IF RECORDS(SELF.ListQueue) AND KEYCODE() = MouseLeft2
    !Pass dragged data
    SELF.TakeNewSelection()
    SETDROPID(MAJ:Number)
    POST(EVENT:Drop,GLO:DropControl,GLO:DropThread)
  END
  
  ReturnValue = PARENT.TakeKey()
  RETURN ReturnValue

!!! <summary>
!!! Generated from procedure template - Window
!!! Select a Students Record
!!! </summary>
SelectStudents PROCEDURE 

CurrentTab           STRING(80)                            ! 
LocalRequest         LONG                                  ! 
OriginalRequest      LONG                                  ! 
LocalResponse        LONG                                  ! 
FilesOpened          BYTE                                  ! 
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
Queue:Browse:1       QUEUE                            !Queue declaration for browse/combo box using ?Browse:1
STU:LastName           LIKE(STU:LastName)             !List box control field - type derived from field
STU:FirstName          LIKE(STU:FirstName)            !List box control field - type derived from field
STU:Number             LIKE(STU:Number)               !List box control field - type derived from field
MAJ:Description        LIKE(MAJ:Description)          !List box control field - type derived from field
STU:GradYear           LIKE(STU:GradYear)             !List box control field - type derived from field
STU:Major              LIKE(STU:Major)                !Browse key field - type derived from field
MAJ:Number             LIKE(MAJ:Number)               !Related join file key field - type derived from field
Mark                   BYTE                           !Entry's marked status
ViewPosition           STRING(1024)                   !Entry's view position
                     END
QuickWindow          WINDOW('Drag Student to Enroll'),AT(,,257,187),FONT('MS Sans Serif',8,COLOR:Black),CENTER, |
  GRAY,IMM,MDI,HLP('~SelectStudents'),SYSTEM
                       LIST,AT(8,20,237,141),USE(?Browse:1),HVSCROLL,DRAGID('Students'),FORMAT('[80L(2)|M~Last' & |
  ' Name~@S20@80L(2)|M~First Name~@S20@/80C(1)|M~Number~C(0)@P###-##-####P@93C(3)|M~Maj' & |
  'or~C(0)@S20@40C(2)|M~Grad Year~@n4@]|M'),FROM(Queue:Browse:1),IMM,MSG('Browsing Records')
                       SHEET,AT(4,4,247,162),USE(?CurrentTab)
                         TAB('by Major'),USE(?TAB1)
                         END
                         TAB('by Last Name'),USE(?TAB2)
                         END
                         TAB('by Grad Year'),USE(?TAB3)
                         END
                       END
                       BUTTON('Help'),AT(0,0,45,14),USE(?Help),HIDE,STD(STD:Help)
                     END

ThisWindow           CLASS(WindowManager)
Init                   PROCEDURE(),BYTE,PROC,DERIVED
Kill                   PROCEDURE(),BYTE,PROC,DERIVED
TakeFieldEvent         PROCEDURE(),BYTE,PROC,DERIVED
TakeWindowEvent        PROCEDURE(),BYTE,PROC,DERIVED
                     END

Toolbar              ToolbarClass
BRW1                 CLASS(BrowseClass)                    ! Browse using ?Browse:1
Q                      &Queue:Browse:1                !Reference to browse queue
ResetSort              PROCEDURE(BYTE Force),BYTE,PROC,DERIVED
TakeKey                PROCEDURE(),BYTE,PROC,DERIVED
                     END

BRW1::Sort0:Locator  StepLocatorClass                      ! Default Locator
BRW1::Sort1:Locator  StepLocatorClass                      ! Conditional Locator - CHOICE(?CurrentTab) = 2
BRW1::Sort2:Locator  StepLocatorClass                      ! Conditional Locator - CHOICE(?CurrentTab) = 3
BRW1::Sort0:StepClass StepLongClass                        ! Default Step Manager
BRW1::Sort1:StepClass StepStringClass                      ! Conditional Step Manager - CHOICE(?CurrentTab) = 2
BRW1::Sort2:StepClass StepLongClass                        ! Conditional Step Manager - CHOICE(?CurrentTab) = 3

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
  GlobalErrors.SetProcedureName('SelectStudents')
  SELF.Request = GlobalRequest                             ! Store the incoming request
  ReturnValue = PARENT.Init()
  IF ReturnValue THEN RETURN ReturnValue.
  SELF.FirstField = ?Browse:1
  SELF.VCRRequest &= VCRRequest
  SELF.Errors &= GlobalErrors                              ! Set this windows ErrorManager to the global ErrorManager
  !Setup inter-thread processing
  LOC:DropThread  = GLO:DropThread
  LOC:DropControl = GLO:DropControl
  LOC:ThreadRef &= GLO:ThreadRef
  
  CLEAR(GlobalRequest)                                     ! Clear GlobalRequest after storing locally
  CLEAR(GlobalResponse)
  SELF.AddItem(Toolbar)
  Relate:Students.SetOpenRelated()
  Relate:Students.Open                                     ! File Students used by this procedure, so make sure it's RelationManager is open
  SELF.FilesOpened = True
  BRW1.Init(?Browse:1,Queue:Browse:1.ViewPosition,BRW1::View:Browse,Queue:Browse:1,Relate:Students,SELF) ! Initialize the browse manager
  SELF.Open(QuickWindow)                                   ! Open window
  Do DefineListboxStyle
  BRW1.Q &= Queue:Browse:1
  BRW1::Sort1:StepClass.Init(+ScrollSort:AllowAlpha,ScrollBy:Runtime) ! Moveable thumb based upon STU:LastName for sort order 1
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
  BRW1.AddField(STU:Number,BRW1.Q.STU:Number)              ! Field STU:Number is a hot field or requires assignment from browse
  BRW1.AddField(MAJ:Description,BRW1.Q.MAJ:Description)    ! Field MAJ:Description is a hot field or requires assignment from browse
  BRW1.AddField(STU:GradYear,BRW1.Q.STU:GradYear)          ! Field STU:GradYear is a hot field or requires assignment from browse
  BRW1.AddField(STU:Major,BRW1.Q.STU:Major)                ! Field STU:Major is a hot field or requires assignment from browse
  BRW1.AddField(MAJ:Number,BRW1.Q.MAJ:Number)              ! Field MAJ:Number is a hot field or requires assignment from browse
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
  OF ?Browse:1
    CASE EVENT()
    OF EVENT:Drag
      !Pass dragged data
      BRW1.TakeNewSelection()
      SETDROPID(FORMAT(STU:Number,@P###-##-####P))
      
    END
  END
  ReturnValue = PARENT.TakeFieldEvent()
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
    CASE EVENT()
    OF EVENT:CloseWindow
      !Flag thread closed
      LOC:ThreadRef = 0
    END
  ReturnValue = PARENT.TakeWindowEvent()
    RETURN ReturnValue
  END
  ReturnValue = Level:Fatal
  RETURN ReturnValue


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


BRW1.TakeKey PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  IF RECORDS(SELF.ListQueue) AND KEYCODE() = MouseLeft2
    !Pass dragged data
    SELF.TakeNewSelection()
    SETDROPID(FORMAT(STU:Number,@P###-##-####P))
    POST(EVENT:Drop,LOC:DropControl,LOC:DropThread)
  END
  
  ReturnValue = PARENT.TakeKey()
  RETURN ReturnValue

!!! <summary>
!!! Generated from procedure template - Window
!!! Select a Teachers Record
!!! </summary>
SelectTeachers PROCEDURE 

CurrentTab           STRING(80)                            ! 
LocalRequest         LONG                                  ! 
OriginalRequest      LONG                                  ! 
LocalResponse        LONG                                  ! 
FilesOpened          BYTE                                  ! 
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
Queue:Browse:1       QUEUE                            !Queue declaration for browse/combo box using ?Browse:1
TEA:LastName           LIKE(TEA:LastName)             !List box control field - type derived from field
TEA:FirstName          LIKE(TEA:FirstName)            !List box control field - type derived from field
MAJ:Description        LIKE(MAJ:Description)          !List box control field - type derived from field
TEA:Number             LIKE(TEA:Number)               !Primary key field - type derived from field
TEA:Department         LIKE(TEA:Department)           !Browse key field - type derived from field
MAJ:Number             LIKE(MAJ:Number)               !Related join file key field - type derived from field
Mark                   BYTE                           !Entry's marked status
ViewPosition           STRING(1024)                   !Entry's view position
                     END
QuickWindow          WINDOW('Drag a Teacher'),AT(,,251,171),FONT('MS Sans Serif',8,COLOR:Black),CENTER,GRAY,IMM, |
  MDI,HLP('~SelectTeachers'),SYSTEM
                       LIST,AT(8,20,230,124),USE(?Browse:1),HVSCROLL,DRAGID('Teachers'),FORMAT('80L(2)|M~Last ' & |
  'Name~@S20@80L(2)|M~First Name~@S20@80L(2)|M~Department~@S20@'),FROM(Queue:Browse:1),IMM, |
  MSG('Browsing Records')
                       SHEET,AT(4,4,242,162),USE(?CurrentTab)
                         TAB('by Last Name'),USE(?TAB1)
                         END
                         TAB('by Department'),USE(?TAB2)
                         END
                       END
                       BUTTON('Help'),AT(0,0,45,14),USE(?Help),HIDE,STD(STD:Help)
                     END

ThisWindow           CLASS(WindowManager)
Init                   PROCEDURE(),BYTE,PROC,DERIVED
Kill                   PROCEDURE(),BYTE,PROC,DERIVED
TakeFieldEvent         PROCEDURE(),BYTE,PROC,DERIVED
TakeWindowEvent        PROCEDURE(),BYTE,PROC,DERIVED
                     END

Toolbar              ToolbarClass
BRW1                 CLASS(BrowseClass)                    ! Browse using ?Browse:1
Q                      &Queue:Browse:1                !Reference to browse queue
ResetSort              PROCEDURE(BYTE Force),BYTE,PROC,DERIVED
TakeKey                PROCEDURE(),BYTE,PROC,DERIVED
                     END

BRW1::Sort0:Locator  StepLocatorClass                      ! Default Locator
BRW1::Sort1:Locator  StepLocatorClass                      ! Conditional Locator - CHOICE(?CurrentTab) = 2
BRW1::Sort0:StepClass StepStringClass                      ! Default Step Manager
BRW1::Sort1:StepClass StepLongClass                        ! Conditional Step Manager - CHOICE(?CurrentTab) = 2

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
  GlobalErrors.SetProcedureName('SelectTeachers')
  SELF.Request = GlobalRequest                             ! Store the incoming request
  ReturnValue = PARENT.Init()
  IF ReturnValue THEN RETURN ReturnValue.
  SELF.FirstField = ?Browse:1
  SELF.VCRRequest &= VCRRequest
  SELF.Errors &= GlobalErrors                              ! Set this windows ErrorManager to the global ErrorManager
  !Setup inter-thread processing
  LOC:DropThread  = GLO:DropThread
  LOC:DropControl = GLO:DropControl
  LOC:ThreadRef &= GLO:ThreadRef
  
  CLEAR(GlobalRequest)                                     ! Clear GlobalRequest after storing locally
  CLEAR(GlobalResponse)
  SELF.AddItem(Toolbar)
  Relate:Teachers.SetOpenRelated()
  Relate:Teachers.Open                                     ! File Teachers used by this procedure, so make sure it's RelationManager is open
  SELF.FilesOpened = True
  BRW1.Init(?Browse:1,Queue:Browse:1.ViewPosition,BRW1::View:Browse,Queue:Browse:1,Relate:Teachers,SELF) ! Initialize the browse manager
  SELF.Open(QuickWindow)                                   ! Open window
  Do DefineListboxStyle
  BRW1.Q &= Queue:Browse:1
  BRW1::Sort1:StepClass.Init(+ScrollSort:AllowAlpha)       ! Moveable thumb based upon TEA:Department for sort order 1
  BRW1.AddSortOrder(BRW1::Sort1:StepClass,TEA:KeyDepartment) ! Add the sort order for TEA:KeyDepartment for sort order 1
  BRW1.AddLocator(BRW1::Sort1:Locator)                     ! Browse has a locator for sort order 1
  BRW1::Sort1:Locator.Init(,TEA:Department,1,BRW1)         ! Initialize the browse locator using  using key: TEA:KeyDepartment , TEA:Department
  BRW1::Sort0:StepClass.Init(+ScrollSort:AllowAlpha,ScrollBy:Name) ! Moveable thumb based upon TEA:LastName for sort order 2
  BRW1.AddSortOrder(BRW1::Sort0:StepClass,TEA:KeyLastName) ! Add the sort order for TEA:KeyLastName for sort order 2
  BRW1.AddLocator(BRW1::Sort0:Locator)                     ! Browse has a locator for sort order 2
  BRW1::Sort0:Locator.Init(,TEA:LastName,1,BRW1)           ! Initialize the browse locator using  using key: TEA:KeyLastName , TEA:LastName
  BRW1.AddField(TEA:LastName,BRW1.Q.TEA:LastName)          ! Field TEA:LastName is a hot field or requires assignment from browse
  BRW1.AddField(TEA:FirstName,BRW1.Q.TEA:FirstName)        ! Field TEA:FirstName is a hot field or requires assignment from browse
  BRW1.AddField(MAJ:Description,BRW1.Q.MAJ:Description)    ! Field MAJ:Description is a hot field or requires assignment from browse
  BRW1.AddField(TEA:Number,BRW1.Q.TEA:Number)              ! Field TEA:Number is a hot field or requires assignment from browse
  BRW1.AddField(TEA:Department,BRW1.Q.TEA:Department)      ! Field TEA:Department is a hot field or requires assignment from browse
  BRW1.AddField(MAJ:Number,BRW1.Q.MAJ:Number)              ! Field MAJ:Number is a hot field or requires assignment from browse
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
    Relate:Teachers.Close
  END
  GlobalErrors.SetProcedureName
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
  OF ?Browse:1
    CASE EVENT()
    OF EVENT:Drag
      !Pass dragged data
      BRW1.TakeNewSelection()
      SETDROPID(Tea:Number)
      
    END
  END
  ReturnValue = PARENT.TakeFieldEvent()
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
    CASE EVENT()
    OF EVENT:CloseWindow
      !Flag thread closed
      LOC:ThreadRef = 0
    END
  ReturnValue = PARENT.TakeWindowEvent()
    RETURN ReturnValue
  END
  ReturnValue = Level:Fatal
  RETURN ReturnValue


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


BRW1.TakeKey PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  IF RECORDS(SELF.ListQueue) AND KEYCODE() = MouseLeft2
    !Pass dragged data
    SELF.TakeNewSelection()
    SETDROPID(Tea:Number)
    POST(EVENT:Drop,LOC:DropControl,LOC:DropThread)
  END
  
  ReturnValue = PARENT.TakeKey()
  RETURN ReturnValue

!!! <summary>
!!! Generated from procedure template - Window
!!! Update the Classes File
!!! </summary>
UpdateClasses PROCEDURE 

CurrentTab           STRING(80)                            ! 
LocalRequest         LONG                                  ! 
OriginalRequest      LONG                                  ! 
LocalResponse        LONG                                  ! 
FilesOpened          BYTE                                  ! 
WindowOpened         LONG                                  ! 
WindowInitialized    LONG                                  ! 
ForceRefresh         LONG                                  ! 
RecordFiltered       LONG                                  ! 
ActionMessage        CSTRING(40)                           ! 
RecordChanged        BYTE,AUTO                             ! 
SelectCoursesThread  LONG                                  ! 
SelectTeachersThread LONG                                  ! 
History::CLA:Record  LIKE(CLA:RECORD),THREAD
QuickWindow          WINDOW('Update the Classes File'),AT(,,164,138),FONT('MS Sans Serif',8,COLOR:Black),CENTER, |
  GRAY,IMM,MDI,HLP('~UpdateClasses'),SYSTEM
                       SHEET,AT(2,3,158,114),USE(?CurrentTab)
                         TAB('General'),USE(?TAB1)
                           PROMPT('&Class Number:'),AT(7,21),USE(?CLA:ClassNumber:Prompt),TRN
                           ENTRY(@P##-#####P),AT(71,21,40,10),USE(CLA:ClassNumber),RIGHT(1)
                           PROMPT('&Course Number:'),AT(7,34),USE(?CLA:CourseNumber:Prompt),TRN
                           ENTRY(@n4),AT(71,34,24,10),USE(CLA:CourseNumber),RIGHT(1),ALRT(F10Key),DROPID('Courses')
                           BUTTON('...'),AT(98,33,12,12),USE(?Button7)
                           STRING(@S20),AT(71,47),USE(COU:Description),TRN
                           ENTRY(@p###-##-####p),AT(71,60,,10),USE(CLA:TeacherNumber),RIGHT(1),ALRT(F10Key),DROPID('Teachers')
                           BUTTON('...'),AT(128,60,12,12),USE(?Button7:2)
                           PROMPT('&Teacher Number:'),AT(7,60),USE(?CLA:TeacherNumber:Prompt),TRN
                           STRING(@S20),AT(71,73),USE(TEA:LastName),TRN
                           PROMPT('Room Number:'),AT(7,90),USE(?CLA:RoomNumber:Prompt),TRN
                           ENTRY(@n4),AT(71,90,40,10),USE(CLA:RoomNumber)
                           PROMPT('Scheduled Time:'),AT(7,103),USE(?CLA:ScheduledTime:Prompt),TRN
                           ENTRY(@s20),AT(71,103,84,10),USE(CLA:ScheduledTime)
                         END
                       END
                       BUTTON('OK'),AT(17,120,45,14),USE(?OK),DEFAULT
                       BUTTON('Cancel'),AT(66,120,45,14),USE(?Cancel)
                       BUTTON('Help'),AT(115,120,45,14),USE(?Help),STD(STD:Help)
                     END

ThisWindow           CLASS(WindowManager)
Ask                    PROCEDURE(),DERIVED
Init                   PROCEDURE(),BYTE,PROC,DERIVED
Kill                   PROCEDURE(),BYTE,PROC,DERIVED
Reset                  PROCEDURE(BYTE Force=0),DERIVED
Run                    PROCEDURE(),BYTE,PROC,DERIVED
TakeAccepted           PROCEDURE(),BYTE,PROC,DERIVED
TakeFieldEvent         PROCEDURE(),BYTE,PROC,DERIVED
TakeSelected           PROCEDURE(),BYTE,PROC,DERIVED
TakeWindowEvent        PROCEDURE(),BYTE,PROC,DERIVED
                     END

Toolbar              ToolbarClass
ToolbarForm          ToolbarUpdateClass                    ! Form Toolbar Manager
CurCtrlFeq          LONG
FieldColorQueue     QUEUE
Feq                   LONG
OldColor              LONG
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

ThisWindow.Ask PROCEDURE

  CODE
  CASE SELF.Request                                        ! Configure the action message text
  OF ViewRecord
    ActionMessage = 'View Record'
  OF InsertRecord
    ActionMessage = 'Adding a Classes Record'
  OF ChangeRecord
    ActionMessage = 'Changing a Classes Record'
  END
  QuickWindow{PROP:Text} = ActionMessage                   ! Display status message in title bar
  PARENT.Ask


ThisWindow.Init PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  GlobalErrors.SetProcedureName('UpdateClasses')
  SELF.Request = GlobalRequest                             ! Store the incoming request
  ReturnValue = PARENT.Init()
  IF ReturnValue THEN RETURN ReturnValue.
  SELF.FirstField = ?CLA:ClassNumber:Prompt
  SELF.VCRRequest &= VCRRequest
  SELF.Errors &= GlobalErrors                              ! Set this windows ErrorManager to the global ErrorManager
  CLEAR(GlobalRequest)                                     ! Clear GlobalRequest after storing locally
  CLEAR(GlobalResponse)
  SELF.AddItem(Toolbar)
  SELF.HistoryKey = 734
  SELF.AddHistoryFile(CLA:Record,History::CLA:Record)
  SELF.AddHistoryField(?CLA:ClassNumber,1)
  SELF.AddHistoryField(?CLA:CourseNumber,2)
  SELF.AddHistoryField(?CLA:TeacherNumber,3)
  SELF.AddHistoryField(?CLA:RoomNumber,4)
  SELF.AddHistoryField(?CLA:ScheduledTime,5)
  SELF.AddUpdateFile(Access:Classes)
  SELF.AddItem(?Cancel,RequestCancelled)                   ! Add the cancel control to the window manager
  Relate:Classes.SetOpenRelated()
  Relate:Classes.Open                                      ! File Classes used by this procedure, so make sure it's RelationManager is open
  SELF.FilesOpened = True
  SELF.Primary &= Relate:Classes
  IF SELF.Request = ViewRecord AND NOT SELF.BatchProcessing ! Setup actions for ViewOnly Mode
    SELF.InsertAction = Insert:None
    SELF.DeleteAction = Delete:None
    SELF.ChangeAction = Change:None
    SELF.CancelAction = Cancel:Cancel
    SELF.OkControl = 0
  ELSE
    SELF.InsertAction = Insert:Query
    SELF.ChangeAction = Change:Caller                      ! Changes allowed
    SELF.OkControl = ?OK
    IF SELF.PrimeUpdate() THEN RETURN Level:Notify.
  END
  SELF.Open(QuickWindow)                                   ! Open window
  Do DefineListboxStyle
  ToolBarForm.HelpButton=?Help
  SELF.AddItem(ToolbarForm)
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


ThisWindow.Reset PROCEDURE(BYTE Force=0)

  CODE
  SELF.ForcedReset += Force
  IF QuickWindow{Prop:AcceptAll} THEN RETURN.
  TEA:Number = CLA:TeacherNumber                           ! Assign linking field value
  Access:Teachers.Fetch(TEA:KeyTeacherNumber)
  COU:Number = CLA:CourseNumber                            ! Assign linking field value
  Access:Courses.Fetch(COU:KeyNumber)
  PARENT.Reset(Force)


ThisWindow.Run PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  ReturnValue = PARENT.Run()
  IF SELF.Request = ViewRecord                             ! In View Only mode always signal RequestCancelled
    ReturnValue = RequestCancelled
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
    CASE ACCEPTED()
    OF ?CLA:CourseNumber
      !Close drag toolbox
      IF SelectCoursesThread
        POST(EVENT:CloseWindow,,SelectCoursesThread)
      END
      
    OF ?Button7
      !Call drag toolbox
      POST(EVENT:AlertKey,?CLA:CourseNumber)
      
    OF ?CLA:TeacherNumber
      !Close drag toolbox
      IF SelectTeachersThread
        POST(EVENT:CloseWindow,,SelectTeachersThread)
        SelectTeachersThread = 0
      END
      
    OF ?Button7:2
      !Call drag toolbox
      POST(EVENT:AlertKey,?CLA:TeacherNumber)
      
    END
  ReturnValue = PARENT.TakeAccepted()
    CASE ACCEPTED()
    OF ?CLA:CourseNumber
      IF Access:Classes.TryValidateField(2)                ! Attempt to validate CLA:CourseNumber in Classes
        SELECT(?CLA:CourseNumber)
        QuickWindow{PROP:AcceptAll} = False
        CYCLE
      ELSE
        FieldColorQueue.Feq = ?CLA:CourseNumber
        GET(FieldColorQueue, FieldColorQueue.Feq)
        IF ERRORCODE() = 0
          ?CLA:CourseNumber{PROP:FontColor} = FieldColorQueue.OldColor
          DELETE(FieldColorQueue)
        END
      END
    OF ?CLA:TeacherNumber
      IF Access:Classes.TryValidateField(3)                ! Attempt to validate CLA:TeacherNumber in Classes
        SELECT(?CLA:TeacherNumber)
        QuickWindow{PROP:AcceptAll} = False
        CYCLE
      ELSE
        FieldColorQueue.Feq = ?CLA:TeacherNumber
        GET(FieldColorQueue, FieldColorQueue.Feq)
        IF ERRORCODE() = 0
          ?CLA:TeacherNumber{PROP:FontColor} = FieldColorQueue.OldColor
          DELETE(FieldColorQueue)
        END
      END
    OF ?OK
      ThisWindow.Update()
      IF SELF.Request = ViewRecord AND NOT SELF.BatchProcessing THEN
         POST(EVENT:CloseWindow)
      END
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
  OF ?CLA:CourseNumber
    CASE EVENT()
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
      SELF.Reset
      PRESSKEY(TabKey)
      PRESSKEY(TabKey)
      
    END
  OF ?CLA:TeacherNumber
    CASE EVENT()
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
      SELF.Reset
      PRESSKEY(TabKey)
      PRESSKEY(TabKey)
      
    END
  END
  ReturnValue = PARENT.TakeFieldEvent()
    RETURN ReturnValue
  END
  ReturnValue = Level:Fatal
  RETURN ReturnValue


ThisWindow.TakeSelected PROCEDURE

ReturnValue          BYTE,AUTO

Looped BYTE
  CODE
  LOOP                                                     ! This method receives all Selected events
    IF Looped
      RETURN Level:Notify
    ELSE
      Looped = 1
    END
    CASE FIELD()
    OF ?CLA:CourseNumber
      !Call drag toolbox
      IF SELF.OriginalRequest = InsertRecord AND NOT CLA:CourseNumber
        GLO:DropThread = THREAD()
        GLO:DropControl = ?CLA:CourseNumber
        SelectCoursesThread = START(SelectCourses)
        GLO:ThreadRef &= SelectCoursesThread
      END
      
    OF ?CLA:TeacherNumber
      !Call drag toolbox
      IF SELF.OriginalRequest = InsertRecord AND NOT CLA:TeacherNumber
        GLO:DropThread = THREAD()
        GLO:DropControl = ?CLA:TeacherNumber
        SelectTeachersThread = START(SelectTeachers)
        GLO:ThreadRef &= SelectTeachersThread
      END
      
    END
  ReturnValue = PARENT.TakeSelected()
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
    CASE EVENT()
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
    END
  ReturnValue = PARENT.TakeWindowEvent()
    RETURN ReturnValue
  END
  ReturnValue = Level:Fatal
  RETURN ReturnValue

!!! <summary>
!!! Generated from procedure template - Window
!!! Update the Courses File
!!! </summary>
UpdateCourses PROCEDURE 

CurrentTab           STRING(80)                            ! 
LocalRequest         LONG                                  ! 
OriginalRequest      LONG                                  ! 
LocalResponse        LONG                                  ! 
FilesOpened          BYTE                                  ! 
WindowOpened         LONG                                  ! 
WindowInitialized    LONG                                  ! 
ForceRefresh         LONG                                  ! 
RecordFiltered       LONG                                  ! 
ActionMessage        CSTRING(40)                           ! 
RecordChanged        BYTE,AUTO                             ! 
History::COU:Record  LIKE(COU:RECORD),THREAD
QuickWindow          WINDOW('Update the Courses File'),AT(,,232,150),FONT('MS Sans Serif',8,COLOR:Black),CENTER, |
  GRAY,IMM,MDI,HLP('~UpdateCourses'),SYSTEM
                       SHEET,AT(3,2,226,125),USE(?CurrentTab),WIZARD
                         TAB('General'),USE(?TAB1)
                           PROMPT('&Description:'),AT(9,15,46,10),USE(?COU:Description:Prompt),TRN
                           ENTRY(@S30),AT(69,15,148,10),USE(COU:Description)
                           PROMPT('Complete Description:'),AT(9,33),USE(?COU:CompleteDescription:Prompt),TRN
                           TEXT,AT(9,45,212,76),USE(COU:CompleteDescription),VSCROLL,BOXED
                         END
                       END
                       BUTTON('OK'),AT(81,130,45,14),USE(?OK),DEFAULT
                       BUTTON('Cancel'),AT(130,130,45,14),USE(?Cancel)
                       BUTTON('Help'),AT(179,130,45,14),USE(?Help),STD(STD:Help)
                     END

ThisWindow           CLASS(WindowManager)
Ask                    PROCEDURE(),DERIVED
Init                   PROCEDURE(),BYTE,PROC,DERIVED
Kill                   PROCEDURE(),BYTE,PROC,DERIVED
Run                    PROCEDURE(),BYTE,PROC,DERIVED
TakeAccepted           PROCEDURE(),BYTE,PROC,DERIVED
                     END

Toolbar              ToolbarClass
ToolbarForm          ToolbarUpdateClass                    ! Form Toolbar Manager
CurCtrlFeq          LONG
FieldColorQueue     QUEUE
Feq                   LONG
OldColor              LONG
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

ThisWindow.Ask PROCEDURE

  CODE
  CASE SELF.Request                                        ! Configure the action message text
  OF ViewRecord
    ActionMessage = 'View Record'
  OF InsertRecord
    ActionMessage = 'Adding a Courses Record'
  OF ChangeRecord
    ActionMessage = 'Changing a Courses Record'
  END
  QuickWindow{PROP:Text} = ActionMessage                   ! Display status message in title bar
  PARENT.Ask


ThisWindow.Init PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  GlobalErrors.SetProcedureName('UpdateCourses')
  SELF.Request = GlobalRequest                             ! Store the incoming request
  ReturnValue = PARENT.Init()
  IF ReturnValue THEN RETURN ReturnValue.
  SELF.FirstField = ?COU:Description:Prompt
  SELF.VCRRequest &= VCRRequest
  SELF.Errors &= GlobalErrors                              ! Set this windows ErrorManager to the global ErrorManager
  CLEAR(GlobalRequest)                                     ! Clear GlobalRequest after storing locally
  CLEAR(GlobalResponse)
  SELF.AddItem(Toolbar)
  SELF.HistoryKey = 734
  SELF.AddHistoryFile(COU:Record,History::COU:Record)
  SELF.AddHistoryField(?COU:Description,2)
  SELF.AddHistoryField(?COU:CompleteDescription,3)
  SELF.AddUpdateFile(Access:Courses)
  SELF.AddItem(?Cancel,RequestCancelled)                   ! Add the cancel control to the window manager
  Relate:Courses.SetOpenRelated()
  Relate:Courses.Open                                      ! File Courses used by this procedure, so make sure it's RelationManager is open
  SELF.FilesOpened = True
  SELF.Primary &= Relate:Courses
  IF SELF.Request = ViewRecord AND NOT SELF.BatchProcessing ! Setup actions for ViewOnly Mode
    SELF.InsertAction = Insert:None
    SELF.DeleteAction = Delete:None
    SELF.ChangeAction = Change:None
    SELF.CancelAction = Cancel:Cancel
    SELF.OkControl = 0
  ELSE
    SELF.InsertAction = Insert:Query
    SELF.ChangeAction = Change:Caller                      ! Changes allowed
    SELF.OkControl = ?OK
    IF SELF.PrimeUpdate() THEN RETURN Level:Notify.
  END
  SELF.Open(QuickWindow)                                   ! Open window
  Do DefineListboxStyle
  ToolBarForm.HelpButton=?Help
  SELF.AddItem(ToolbarForm)
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


ThisWindow.Run PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  ReturnValue = PARENT.Run()
  IF SELF.Request = ViewRecord                             ! In View Only mode always signal RequestCancelled
    ReturnValue = RequestCancelled
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
    OF ?OK
      ThisWindow.Update()
      IF SELF.Request = ViewRecord AND NOT SELF.BatchProcessing THEN
         POST(EVENT:CloseWindow)
      END
    END
    RETURN ReturnValue
  END
  ReturnValue = Level:Fatal
  RETURN ReturnValue

!!! <summary>
!!! Generated from procedure template - Window
!!! Update the Teachers File
!!! </summary>
UpdateTeachers PROCEDURE 

CurrentTab           STRING(80)                            ! 
LocalRequest         LONG                                  ! 
OriginalRequest      LONG                                  ! 
LocalResponse        LONG                                  ! 
FilesOpened          BYTE                                  ! 
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
Queue:Browse:2       QUEUE                            !Queue declaration for browse/combo box using ?Browse:2
CLA:ClassNumber        LIKE(CLA:ClassNumber)          !List box control field - type derived from field
COU:Description        LIKE(COU:Description)          !List box control field - type derived from field
CLA:RoomNumber         LIKE(CLA:RoomNumber)           !List box control field - type derived from field
CLA:ScheduledTime      LIKE(CLA:ScheduledTime)        !List box control field - type derived from field
CLA:TeacherNumber      LIKE(CLA:TeacherNumber)        !Browse key field - type derived from field
COU:Number             LIKE(COU:Number)               !Related join file key field - type derived from field
Mark                   BYTE                           !Entry's marked status
ViewPosition           STRING(1024)                   !Entry's view position
                     END
History::TEA:Record  LIKE(TEA:RECORD),THREAD
QuickWindow          WINDOW('Update the Teachers File'),AT(,,159,177),FONT('MS Sans Serif',8,COLOR:Black),GRAY, |
  IMM,MDI,HLP('~UpdateTeachers'),SYSTEM
                       SHEET,AT(4,6,151,151),USE(?CurrentTab)
                         TAB('General'),USE(?TAB1)
                           PROMPT('&Number:'),AT(8,26),USE(?Tea:Number:Prompt)
                           ENTRY(@P###-##-####P),AT(53,26,,10),USE(TEA:Number),RIGHT(1)
                           PROMPT('&First Name:'),AT(8,39),USE(?Tea:FirstName:Prompt)
                           ENTRY(@S20),AT(53,39,84,10),USE(TEA:FirstName)
                           PROMPT('&Last Name:'),AT(8,52),USE(?Tea:LastName:Prompt)
                           ENTRY(@S20),AT(53,52,84,10),USE(TEA:LastName)
                           PROMPT('&Address:'),AT(8,66),USE(?Tea:Address:Prompt)
                           ENTRY(@S20),AT(53,66,84,10),USE(TEA:Address)
                           PROMPT('&City:'),AT(8,81),USE(?Tea:City:Prompt)
                           ENTRY(@S20),AT(53,81,84,10),USE(TEA:City)
                           PROMPT('&State:'),AT(8,95),USE(?Tea:State:Prompt)
                           ENTRY(@S2),AT(53,95,40,10),USE(TEA:State)
                           PROMPT('&Zip:'),AT(8,108),USE(?Tea:Zip:Prompt)
                           ENTRY(@n05),AT(53,108,40,10),USE(TEA:Zip)
                           PROMPT('&Telephone:'),AT(8,122),USE(?Tea:Telephone:Prompt)
                           ENTRY(@s12),AT(53,122,52,10),USE(TEA:Telephone)
                           PROMPT('Department:'),AT(8,137),USE(?Tea:Department:Prompt)
                           ENTRY(@n4),AT(53,137,21,10),USE(TEA:Department),ALRT(F10Key),DROPID('Majors')
                           BUTTON('...'),AT(77,137,12,12),USE(?Button7)
                           STRING(@S15),AT(90,137),USE(MAJ:Description)
                         END
                         TAB('Classes'),USE(?ClassesTab)
                           LIST,AT(8,26,143,124),USE(?Browse:2),HVSCROLL,FORMAT('52L(2)|M~Class Number~@P##-#####P' & |
  '@120L(2)|M~Description~@S30@48R(2)|M~Room Number~C(0)@n4@80L(2)|M~Scheduled Time~@s20@'), |
  FROM(Queue:Browse:2),IMM,MSG('Browsing Records')
                           BUTTON('&Insert'),AT(8,119,45,14),USE(?Insert:3),HIDE
                           BUTTON('&Change'),AT(57,119,45,14),USE(?Change:3),HIDE
                           BUTTON('&Delete'),AT(106,119,45,14),USE(?Delete:3),HIDE
                         END
                       END
                       BUTTON('OK'),AT(12,159,45,14),USE(?OK),DEFAULT
                       BUTTON('Cancel'),AT(61,159,45,14),USE(?Cancel)
                       BUTTON('Help'),AT(110,159,45,14),USE(?Help),STD(STD:Help)
                     END

ThisWindow           CLASS(WindowManager)
Ask                    PROCEDURE(),DERIVED
Init                   PROCEDURE(),BYTE,PROC,DERIVED
Kill                   PROCEDURE(),BYTE,PROC,DERIVED
Reset                  PROCEDURE(BYTE Force=0),DERIVED
Run                    PROCEDURE(),BYTE,PROC,DERIVED
Run                    PROCEDURE(USHORT Number,BYTE Request),BYTE,PROC,DERIVED
TakeAccepted           PROCEDURE(),BYTE,PROC,DERIVED
TakeFieldEvent         PROCEDURE(),BYTE,PROC,DERIVED
TakeSelected           PROCEDURE(),BYTE,PROC,DERIVED
TakeWindowEvent        PROCEDURE(),BYTE,PROC,DERIVED
                     END

Toolbar              ToolbarClass
BRW2                 CLASS(BrowseClass)                    ! Browse using ?Browse:2
Q                      &Queue:Browse:2                !Reference to browse queue
Init                   PROCEDURE(SIGNED ListBox,*STRING Posit,VIEW V,QUEUE Q,RelationManager RM,WindowManager WM)
                     END

BRW2::Sort0:StepClass StepLongClass                        ! Default Step Manager
ToolbarForm          ToolbarUpdateClass                    ! Form Toolbar Manager
CurCtrlFeq          LONG
FieldColorQueue     QUEUE
Feq                   LONG
OldColor              LONG
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

ThisWindow.Ask PROCEDURE

  CODE
  CASE SELF.Request                                        ! Configure the action message text
  OF ViewRecord
    ActionMessage = 'View Record'
  OF InsertRecord
    ActionMessage = 'Adding a Teachers Record'
  OF ChangeRecord
    ActionMessage = 'Changing a Teachers Record'
  END
  QuickWindow{PROP:Text} = ActionMessage                   ! Display status message in title bar
  PARENT.Ask


ThisWindow.Init PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  GlobalErrors.SetProcedureName('UpdateTeachers')
  SELF.Request = GlobalRequest                             ! Store the incoming request
  ReturnValue = PARENT.Init()
  IF ReturnValue THEN RETURN ReturnValue.
  SELF.FirstField = ?Tea:Number:Prompt
  SELF.VCRRequest &= VCRRequest
  SELF.Errors &= GlobalErrors                              ! Set this windows ErrorManager to the global ErrorManager
  CLEAR(GlobalRequest)                                     ! Clear GlobalRequest after storing locally
  CLEAR(GlobalResponse)
  SELF.AddItem(Toolbar)
  SELF.HistoryKey = 734
  SELF.AddHistoryFile(TEA:Record,History::TEA:Record)
  SELF.AddHistoryField(?TEA:Number,1)
  SELF.AddHistoryField(?TEA:FirstName,2)
  SELF.AddHistoryField(?TEA:LastName,3)
  SELF.AddHistoryField(?TEA:Address,4)
  SELF.AddHistoryField(?TEA:City,5)
  SELF.AddHistoryField(?TEA:State,6)
  SELF.AddHistoryField(?TEA:Zip,7)
  SELF.AddHistoryField(?TEA:Telephone,8)
  SELF.AddHistoryField(?TEA:Department,9)
  SELF.AddUpdateFile(Access:Teachers)
  SELF.AddItem(?Cancel,RequestCancelled)                   ! Add the cancel control to the window manager
  Relate:Classes.SetOpenRelated()
  Relate:Classes.Open                                      ! File Classes used by this procedure, so make sure it's RelationManager is open
  SELF.FilesOpened = True
  SELF.Primary &= Relate:Teachers
  IF SELF.Request = ViewRecord AND NOT SELF.BatchProcessing ! Setup actions for ViewOnly Mode
    SELF.InsertAction = Insert:None
    SELF.DeleteAction = Delete:None
    SELF.ChangeAction = Change:None
    SELF.CancelAction = Cancel:Cancel
    SELF.OkControl = 0
  ELSE
    SELF.InsertAction = Insert:Query
    SELF.ChangeAction = Change:Caller                      ! Changes allowed
    SELF.OkControl = ?OK
    IF SELF.PrimeUpdate() THEN RETURN Level:Notify.
  END
  BRW2.Init(?Browse:2,Queue:Browse:2.ViewPosition,BRW2::View:Browse,Queue:Browse:2,Relate:Classes,SELF) ! Initialize the browse manager
  SELF.Open(QuickWindow)                                   ! Open window
  Do DefineListboxStyle
  BRW2.Q &= Queue:Browse:2
  BRW2::Sort0:StepClass.Init(+ScrollSort:AllowAlpha)       ! Moveable thumb based upon CLA:TeacherNumber for sort order 1
  BRW2.AddSortOrder(BRW2::Sort0:StepClass,CLA:KeyTeacherNumber) ! Add the sort order for CLA:KeyTeacherNumber for sort order 1
  BRW2.AddRange(CLA:TeacherNumber,Relate:Classes,Relate:Teachers) ! Add file relationship range limit for sort order 1
  BRW2.AddField(CLA:ClassNumber,BRW2.Q.CLA:ClassNumber)    ! Field CLA:ClassNumber is a hot field or requires assignment from browse
  BRW2.AddField(COU:Description,BRW2.Q.COU:Description)    ! Field COU:Description is a hot field or requires assignment from browse
  BRW2.AddField(CLA:RoomNumber,BRW2.Q.CLA:RoomNumber)      ! Field CLA:RoomNumber is a hot field or requires assignment from browse
  BRW2.AddField(CLA:ScheduledTime,BRW2.Q.CLA:ScheduledTime) ! Field CLA:ScheduledTime is a hot field or requires assignment from browse
  BRW2.AddField(CLA:TeacherNumber,BRW2.Q.CLA:TeacherNumber) ! Field CLA:TeacherNumber is a hot field or requires assignment from browse
  BRW2.AddField(COU:Number,BRW2.Q.COU:Number)              ! Field COU:Number is a hot field or requires assignment from browse
  BRW2.AskProcedure = 1                                    ! Will call: UpdateClasses
  ToolBarForm.HelpButton=?Help
  SELF.AddItem(ToolbarForm)
  BRW2.AddToolbarTarget(Toolbar)                           ! Browse accepts toolbar control
  BRW2.ToolbarItem.HelpButton = ?Help
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


ThisWindow.Reset PROCEDURE(BYTE Force=0)

  CODE
  SELF.ForcedReset += Force
  IF QuickWindow{Prop:AcceptAll} THEN RETURN.
  MAJ:Number = TEA:Department                              ! Assign linking field value
  Access:Majors.Fetch(MAJ:KeyNumber)
  PARENT.Reset(Force)


ThisWindow.Run PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  ReturnValue = PARENT.Run()
  IF SELF.Request = ViewRecord                             ! In View Only mode always signal RequestCancelled
    ReturnValue = RequestCancelled
  END
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
    OF ?Button7
      !Call drag toolbox
      POST(EVENT:AlertKey,?Tea:Department)
      
    END
  ReturnValue = PARENT.TakeAccepted()
    CASE ACCEPTED()
    OF ?TEA:Department
      IF Access:Teachers.TryValidateField(9)               ! Attempt to validate TEA:Department in Teachers
        SELECT(?TEA:Department)
        QuickWindow{PROP:AcceptAll} = False
        CYCLE
      ELSE
        FieldColorQueue.Feq = ?TEA:Department
        GET(FieldColorQueue, FieldColorQueue.Feq)
        IF ERRORCODE() = 0
          ?TEA:Department{PROP:FontColor} = FieldColorQueue.OldColor
          DELETE(FieldColorQueue)
        END
      END
    OF ?OK
      ThisWindow.Update()
      IF SELF.Request = ViewRecord AND NOT SELF.BatchProcessing THEN
         POST(EVENT:CloseWindow)
      END
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
  OF ?TEA:Department
    CASE EVENT()
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
      SELF.Reset
      PRESSKEY(TabKey)
      PRESSKEY(TabKey)
      
    END
  END
  ReturnValue = PARENT.TakeFieldEvent()
    RETURN ReturnValue
  END
  ReturnValue = Level:Fatal
  RETURN ReturnValue


ThisWindow.TakeSelected PROCEDURE

ReturnValue          BYTE,AUTO

Looped BYTE
  CODE
  LOOP                                                     ! This method receives all Selected events
    IF Looped
      RETURN Level:Notify
    ELSE
      Looped = 1
    END
    CASE FIELD()
    OF ?TEA:Department
      !Call drag toolbox
      IF SELF.OriginalRequest = InsertRecord AND NOT Tea:Department
        GLO:DropThread = THREAD()
        GLO:DropControl = ?Tea:Department
        SelectMajorsThread = START(SelectMajors)
        GLO:ThreadRef &= SelectMajorsThread
      END
      
    END
  ReturnValue = PARENT.TakeSelected()
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
    CASE EVENT()
    OF EVENT:CloseWindow
      !Close drag toolbox
      IF SelectMajorsThread
        POST(EVENT:CloseWindow,,SelectMajorsThread)
      END
    END
  ReturnValue = PARENT.TakeWindowEvent()
    RETURN ReturnValue
  END
  ReturnValue = Level:Fatal
  RETURN ReturnValue


BRW2.Init PROCEDURE(SIGNED ListBox,*STRING Posit,VIEW V,QUEUE Q,RelationManager RM,WindowManager WM)

  CODE
  PARENT.Init(ListBox,Posit,V,Q,RM,WM)
  IF WM.Request <> ViewRecord                              ! If called for anything other than ViewMode, make the insert, change & delete controls available
    SELF.InsertControl=?Insert:3
    SELF.ChangeControl=?Change:3
    SELF.DeleteControl=?Delete:3
  END

!!! <summary>
!!! Generated from procedure template - Window
!!! Update the Students File
!!! </summary>
UpdateStudents PROCEDURE 

CurrentTab           STRING(80)                            ! 
LocalRequest         LONG                                  ! 
OriginalRequest      LONG                                  ! 
LocalResponse        LONG                                  ! 
FilesOpened          BYTE                                  ! 
WindowOpened         LONG                                  ! 
WindowInitialized    LONG                                  ! 
ForceRefresh         LONG                                  ! 
RecordFiltered       LONG                                  ! 
ActionMessage        CSTRING(40)                           ! 
RecordChanged        BYTE,AUTO                             ! 
DOSDialogHeader      CSTRING(40)                           ! 
DOSExtParameter      CSTRING(250)                          ! 
DOSTargetVariable    CSTRING(80)                           ! 
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
Queue:Browse:2       QUEUE                            !Queue declaration for browse/combo box using ?Browse:2
ENR:ClassNumber        LIKE(ENR:ClassNumber)          !List box control field - type derived from field
CLA:ScheduledTime      LIKE(CLA:ScheduledTime)        !List box control field - type derived from field
COU:Description        LIKE(COU:Description)          !List box control field - type derived from field
ENR:MidtermExam        LIKE(ENR:MidtermExam)          !List box control field - type derived from field
ENR:FinalExam          LIKE(ENR:FinalExam)            !List box control field - type derived from field
ENR:TermPaper          LIKE(ENR:TermPaper)            !List box control field - type derived from field
ENR:StudentNumber      LIKE(ENR:StudentNumber)        !Browse key field - type derived from field
CLA:ClassNumber        LIKE(CLA:ClassNumber)          !Related join file key field - type derived from field
COU:Number             LIKE(COU:Number)               !Related join file key field - type derived from field
Mark                   BYTE                           !Entry's marked status
ViewPosition           STRING(1024)                   !Entry's view position
                     END
History::STU:Record  LIKE(STU:RECORD),THREAD
QuickWindow          WINDOW('Update the Students File'),AT(,,303,204),FONT('MS Sans Serif',8,COLOR:Black),RESIZE, |
  GRAY,IMM,MDI,HLP('~UpdateStudents'),PALETTE(256),SYSTEM
                       SHEET,AT(3,4,298,173),USE(?CurrentTab)
                         TAB('General'),USE(?TAB1)
                           PROMPT('&First Name:'),AT(7,20),USE(?STU:FirstName:Prompt),TRN
                           ENTRY(@S20),AT(47,20,84,10),USE(STU:FirstName)
                           PROMPT('&Last Name:'),AT(7,34),USE(?STU:LastName:Prompt),TRN
                           ENTRY(@S20),AT(47,34,84,10),USE(STU:LastName)
                           PROMPT('&Address:'),AT(7,48),USE(?STU:Address:Prompt),TRN
                           ENTRY(@S20),AT(47,48,84,10),USE(STU:Address)
                           PROMPT('Address 2:'),AT(7,62),USE(?STU:Address2:Prompt),TRN
                           ENTRY(@s20),AT(47,62,84,10),USE(STU:Address2)
                           PROMPT('&City:'),AT(7,76),USE(?STU:City:Prompt),TRN
                           ENTRY(@S20),AT(47,76,84,10),USE(STU:City)
                           PROMPT('&State:'),AT(7,90),USE(?STU:State:Prompt),TRN
                           ENTRY(@S2),AT(47,90,40,10),USE(STU:State)
                           PROMPT('&Zip:'),AT(7,104),USE(?STU:Zip:Prompt),TRN
                           ENTRY(@n05),AT(47,104,40,10),USE(STU:Zip)
                           PROMPT('&Telephone:'),AT(7,118),USE(?STU:Telephone:Prompt),TRN
                           ENTRY(@s12),AT(47,118,52,10),USE(STU:Telephone)
                           PROMPT('Grad Year:'),AT(7,132),USE(?STU:GradYear:Prompt),TRN
                           ENTRY(@n4),AT(47,132,40,10),USE(STU:GradYear)
                           PROMPT('&Number:'),AT(7,146),USE(?STU:Number:Prompt),TRN
                           ENTRY(@P###-##-####P),AT(47,146,,10),USE(STU:Number),RIGHT(1),REQ
                           STRING(@s20),AT(74,160,77,10),USE(MAJ:Description),TRN
                           PROMPT('Major:'),AT(7,160),USE(?STU:Major:Prompt),TRN
                           ENTRY(@n4),AT(47,160,13,10),USE(STU:Major),ALRT(F10Key),DROPID('Majors')
                           BUTTON('...'),AT(61,159,12,12),USE(?Button8)
                         END
                         TAB('Enrollment'),USE(?EnrollmentTab)
                           LIST,AT(7,22,290,148),USE(?Browse:2),HVSCROLL,FORMAT('[52L(2)|M~Class Number~@p##-#####' & |
  'p@80L(2)|M~Scheduled Time~@s20@/120L(2)|M~Description~@S30@]|M~Class~[52R(2)|M~Midte' & |
  'rm Exam~C(0)@n3@44R(2)|M~Final Exam~C(0)@n3@44R(2)|M~Term Paper~C(0)@n3@]|M~Grades~'),FROM(Queue:Browse:2), |
  IMM,MSG('Browsing Records')
                           BUTTON('&Insert'),AT(7,128,45,14),USE(?Insert:3),HIDE
                           BUTTON('&Change'),AT(57,128,45,14),USE(?Change:3),HIDE
                           BUTTON('&Delete'),AT(105,128,45,14),USE(?Delete:3),HIDE
                         END
                         TAB('Photo'),USE(?Tab3)
                           IMAGE,AT(8,24),USE(?Image1)
                           ENTRY(@s64),AT(7,158,129,12),USE(GLO:FileName)
                           PROMPT('Image File Name:'),AT(7,147),USE(?GLO:FileName:Prompt),TRN
                           BUTTON('...'),AT(137,158,12,12),USE(?LookupFile)
                         END
                       END
                       BUTTON('OK'),AT(12,185,45,14),USE(?OK),DEFAULT
                       BUTTON('Cancel'),AT(61,185,45,14),USE(?Cancel)
                       BUTTON('Help'),AT(110,185,45,14),USE(?Help),STD(STD:Help)
                     END

ThisWindow           CLASS(WindowManager)
Ask                    PROCEDURE(),DERIVED
Init                   PROCEDURE(),BYTE,PROC,DERIVED
Kill                   PROCEDURE(),BYTE,PROC,DERIVED
Reset                  PROCEDURE(BYTE Force=0),DERIVED
Run                    PROCEDURE(),BYTE,PROC,DERIVED
Run                    PROCEDURE(USHORT Number,BYTE Request),BYTE,PROC,DERIVED
TakeAccepted           PROCEDURE(),BYTE,PROC,DERIVED
TakeCompleted          PROCEDURE(),BYTE,PROC,DERIVED
TakeFieldEvent         PROCEDURE(),BYTE,PROC,DERIVED
TakeSelected           PROCEDURE(),BYTE,PROC,DERIVED
TakeWindowEvent        PROCEDURE(),BYTE,PROC,DERIVED
                     END

Toolbar              ToolbarClass
BRW2                 CLASS(BrowseClass)                    ! Browse using ?Browse:2
Q                      &Queue:Browse:2                !Reference to browse queue
Init                   PROCEDURE(SIGNED ListBox,*STRING Posit,VIEW V,QUEUE Q,RelationManager RM,WindowManager WM)
                     END

BRW2::Sort0:StepClass StepLongClass                        ! Default Step Manager
ToolbarForm          ToolbarUpdateClass                    ! Form Toolbar Manager
FileLookup6          SelectFileClass
Resizer              CLASS(WindowResizeClass)
Init                   PROCEDURE(BYTE AppStrategy=AppStrategy:Resize,BYTE SetWindowMinSize=False,BYTE SetWindowMaxSize=False)
                     END

CurCtrlFeq          LONG
FieldColorQueue     QUEUE
Feq                   LONG
OldColor              LONG
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
!Display photo maintaining aspect ratio
DisplayImage  ROUTINE
  IF ?Image1{PROP:Height} > 100
    AspectRatio$ = ?Image1{PROP:Width}/?Image1{PROP:Height}
    ?Image1{PROP:Height} = 100
    ?Image1{PROP:Width} = 100 * AspectRatio$
  END
  IF ?Image1{PROP:Width} > 100
    AspectRatio$ = ?Image1{PROP:Height}/?Image1{PROP:Width}
    ?Image1{PROP:Width} = 100
    ?Image1{PROP:Height} = 100 * AspectRatio$
  END

ThisWindow.Ask PROCEDURE

  CODE
  CASE SELF.Request                                        ! Configure the action message text
  OF ViewRecord
    ActionMessage = 'View Record'
  OF InsertRecord
    ActionMessage = 'Adding a Students Record'
  OF ChangeRecord
    ActionMessage = 'Changing a Students Record'
  END
  QuickWindow{PROP:Text} = ActionMessage                   ! Display status message in title bar
  PARENT.Ask


ThisWindow.Init PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  GlobalErrors.SetProcedureName('UpdateStudents')
  SELF.Request = GlobalRequest                             ! Store the incoming request
  ReturnValue = PARENT.Init()
  IF ReturnValue THEN RETURN ReturnValue.
  SELF.FirstField = ?STU:FirstName:Prompt
  SELF.VCRRequest &= VCRRequest
  SELF.Errors &= GlobalErrors                              ! Set this windows ErrorManager to the global ErrorManager
  CLEAR(GlobalRequest)                                     ! Clear GlobalRequest after storing locally
  CLEAR(GlobalResponse)
  SELF.AddItem(Toolbar)
  SELF.HistoryKey = 734
  SELF.AddHistoryFile(STU:Record,History::STU:Record)
  SELF.AddHistoryField(?STU:FirstName,3)
  SELF.AddHistoryField(?STU:LastName,4)
  SELF.AddHistoryField(?STU:Address,5)
  SELF.AddHistoryField(?STU:Address2,6)
  SELF.AddHistoryField(?STU:City,7)
  SELF.AddHistoryField(?STU:State,8)
  SELF.AddHistoryField(?STU:Zip,9)
  SELF.AddHistoryField(?STU:Telephone,10)
  SELF.AddHistoryField(?STU:GradYear,12)
  SELF.AddHistoryField(?STU:Number,2)
  SELF.AddHistoryField(?STU:Major,11)
  SELF.AddUpdateFile(Access:Students)
  SELF.AddItem(?Cancel,RequestCancelled)                   ! Add the cancel control to the window manager
  Relate:Enrollment.SetOpenRelated()
  Relate:Enrollment.Open                                   ! File Enrollment used by this procedure, so make sure it's RelationManager is open
  SELF.FilesOpened = True
  SELF.Primary &= Relate:Students
  IF SELF.Request = ViewRecord AND NOT SELF.BatchProcessing ! Setup actions for ViewOnly Mode
    SELF.InsertAction = Insert:None
    SELF.DeleteAction = Delete:None
    SELF.ChangeAction = Change:None
    SELF.CancelAction = Cancel:Cancel
    SELF.OkControl = 0
  ELSE
    SELF.InsertAction = Insert:Query
    SELF.ChangeAction = Change:Caller                      ! Changes allowed
    SELF.OkControl = ?OK
    IF SELF.PrimeUpdate() THEN RETURN Level:Notify.
  END
  BRW2.Init(?Browse:2,Queue:Browse:2.ViewPosition,BRW2::View:Browse,Queue:Browse:2,Relate:Enrollment,SELF) ! Initialize the browse manager
  SELF.Open(QuickWindow)                                   ! Open window
  Do DefineListboxStyle
  BRW2.Q &= Queue:Browse:2
  BRW2::Sort0:StepClass.Init(+ScrollSort:AllowAlpha)       ! Moveable thumb based upon ENR:ClassNumber for sort order 1
  BRW2.AddSortOrder(BRW2::Sort0:StepClass,ENR:StuSeq)      ! Add the sort order for ENR:StuSeq for sort order 1
  BRW2.AddRange(ENR:StudentNumber,Relate:Enrollment,Relate:Students) ! Add file relationship range limit for sort order 1
  BRW2.AddField(ENR:ClassNumber,BRW2.Q.ENR:ClassNumber)    ! Field ENR:ClassNumber is a hot field or requires assignment from browse
  BRW2.AddField(CLA:ScheduledTime,BRW2.Q.CLA:ScheduledTime) ! Field CLA:ScheduledTime is a hot field or requires assignment from browse
  BRW2.AddField(COU:Description,BRW2.Q.COU:Description)    ! Field COU:Description is a hot field or requires assignment from browse
  BRW2.AddField(ENR:MidtermExam,BRW2.Q.ENR:MidtermExam)    ! Field ENR:MidtermExam is a hot field or requires assignment from browse
  BRW2.AddField(ENR:FinalExam,BRW2.Q.ENR:FinalExam)        ! Field ENR:FinalExam is a hot field or requires assignment from browse
  BRW2.AddField(ENR:TermPaper,BRW2.Q.ENR:TermPaper)        ! Field ENR:TermPaper is a hot field or requires assignment from browse
  BRW2.AddField(ENR:StudentNumber,BRW2.Q.ENR:StudentNumber) ! Field ENR:StudentNumber is a hot field or requires assignment from browse
  BRW2.AddField(CLA:ClassNumber,BRW2.Q.CLA:ClassNumber)    ! Field CLA:ClassNumber is a hot field or requires assignment from browse
  BRW2.AddField(COU:Number,BRW2.Q.COU:Number)              ! Field COU:Number is a hot field or requires assignment from browse
  QuickWindow{PROP:MinWidth} = 160                         ! Restrict the minimum window width
  QuickWindow{PROP:MinHeight} = 204                        ! Restrict the minimum window height
  Resizer.Init(AppStrategy:Spread)                         ! Controls will spread out as the window gets bigger
  SELF.AddItem(Resizer)                                    ! Add resizer to window manager
  !Display photo from stored BLOB
  ?Image1{PROP:NoWidth} = TRUE
  ?Image1{PROP:NoHeight} = TRUE
  IF SELF.OriginalRequest <> InsertRecord
    ?Image1{PROP:ImageBlob} = STU:Photograph{PROP:Handle}
  ELSE
    ?Image1{PROP:Text} = 'NoPhoto.BMP'
    PhotoChanged = TRUE
  END
  GLO:FileName = ''
  DO DisplayImage
  
  BRW2.AskProcedure = 1                                    ! Will call: UpdateEnrollment
  ToolBarForm.HelpButton=?Help
  SELF.AddItem(ToolbarForm)
  FileLookup6.Init
  FileLookup6.ClearOnCancel = True
  FileLookup6.SetMask('Image Files','*.GIF;*.BMP;*.JPG;*.PCX') ! Set the file mask
  FileLookup6.AddMask('All Files','*.*')                   ! Add additional masks
  FileLookup6.WindowTitle='Choose a File'
  BRW2.AddToolbarTarget(Toolbar)                           ! Browse accepts toolbar control
  BRW2.ToolbarItem.HelpButton = ?Help
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


ThisWindow.Reset PROCEDURE(BYTE Force=0)

  CODE
  SELF.ForcedReset += Force
  IF QuickWindow{Prop:AcceptAll} THEN RETURN.
  MAJ:Number = STU:Major                                   ! Assign linking field value
  Access:Majors.Fetch(MAJ:KeyNumber)
  PARENT.Reset(Force)


ThisWindow.Run PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  ReturnValue = PARENT.Run()
  IF SELF.Request = ViewRecord                             ! In View Only mode always signal RequestCancelled
    ReturnValue = RequestCancelled
  END
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
    OF ?Button8
      !Call drag toolbox
      POST(EVENT:AlertKey,?STU:Major)
      
    END
  ReturnValue = PARENT.TakeAccepted()
    CASE ACCEPTED()
    OF ?STU:Major
      IF Access:Students.TryValidateField(11)              ! Attempt to validate STU:Major in Students
        SELECT(?STU:Major)
        QuickWindow{PROP:AcceptAll} = False
        CYCLE
      ELSE
        FieldColorQueue.Feq = ?STU:Major
        GET(FieldColorQueue, FieldColorQueue.Feq)
        IF ERRORCODE() = 0
          ?STU:Major{PROP:FontColor} = FieldColorQueue.OldColor
          DELETE(FieldColorQueue)
        END
      END
    OF ?GLO:FileName
      !Display photo from selected disk file
      IF NOT TARGET{PROP:AcceptAll}
        ?Image1{PROP:NoWidth} = TRUE
        ?Image1{PROP:NoHeight} = TRUE
        ?Image1{PROP:Text} = GLO:FileName
        DO DisplayImage
        PhotoChanged = TRUE
      END
      
    OF ?LookupFile
      ThisWindow.Update()
      GLO:FileName = FileLookup6.Ask(1)
      DISPLAY
      !Display photo from selected disk file
      IF NOT TARGET{PROP:AcceptAll} AND GLO:FileName <> ''
        ?Image1{PROP:NoWidth} = TRUE
        ?Image1{PROP:NoHeight} = TRUE
        ?Image1{PROP:Text} = GLO:FileName
        DO DisplayImage
        PhotoChanged = TRUE
      END
    OF ?OK
      ThisWindow.Update()
      IF SELF.Request = ViewRecord AND NOT SELF.BatchProcessing THEN
         POST(EVENT:CloseWindow)
      END
    END
    RETURN ReturnValue
  END
  ReturnValue = Level:Fatal
  RETURN ReturnValue


ThisWindow.TakeCompleted PROCEDURE

ReturnValue          BYTE,AUTO

Looped BYTE
  CODE
  LOOP
    IF Looped
      RETURN Level:Notify
    ELSE
      Looped = 1
    END
  !Update Photo BLOB on disk
  IF PhotoChanged = TRUE
    STU:Photograph{PROP:size} = 0
    STU:Photograph{PROP:Handle} = ?Image1{PROP:ImageBlob}
  END
  
  ReturnValue = PARENT.TakeCompleted()
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
  OF ?STU:Major
    CASE EVENT()
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
      SELF.Reset
      PRESSKEY(TabKey)
      
    END
  END
  ReturnValue = PARENT.TakeFieldEvent()
    RETURN ReturnValue
  END
  ReturnValue = Level:Fatal
  RETURN ReturnValue


ThisWindow.TakeSelected PROCEDURE

ReturnValue          BYTE,AUTO

Looped BYTE
  CODE
  LOOP                                                     ! This method receives all Selected events
    IF Looped
      RETURN Level:Notify
    ELSE
      Looped = 1
    END
    CASE FIELD()
    OF ?STU:Major
      !Call drag toolbox
      IF SELF.OriginalRequest = InsertRecord AND NOT STU:Major
        GLO:DropThread = THREAD()
        GLO:DropControl = ?STU:Major
        SelectMajorsThread = START(SelectMajors)
        GLO:ThreadRef &= SelectMajorsThread
      END
      
    END
  ReturnValue = PARENT.TakeSelected()
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
    CASE EVENT()
    OF EVENT:CloseWindow
      !Close drag toolbox
      IF SelectMajorsThread
        POST(EVENT:CloseWindow,,SelectMajorsThread)
      END
    END
  ReturnValue = PARENT.TakeWindowEvent()
    RETURN ReturnValue
  END
  ReturnValue = Level:Fatal
  RETURN ReturnValue


BRW2.Init PROCEDURE(SIGNED ListBox,*STRING Posit,VIEW V,QUEUE Q,RelationManager RM,WindowManager WM)

  CODE
  PARENT.Init(ListBox,Posit,V,Q,RM,WM)
  IF WM.Request <> ViewRecord                              ! If called for anything other than ViewMode, make the insert, change & delete controls available
    SELF.InsertControl=?Insert:3
    SELF.ChangeControl=?Change:3
    SELF.DeleteControl=?Delete:3
  END


Resizer.Init PROCEDURE(BYTE AppStrategy=AppStrategy:Resize,BYTE SetWindowMinSize=False,BYTE SetWindowMaxSize=False)


  CODE
  PARENT.Init(AppStrategy,SetWindowMinSize,SetWindowMaxSize)
  SELF.DeferMoves = False
  SELF.SetParentDefaults()                                 ! Calculate default control parent-child relationships based upon their positions on the window

!!! <summary>
!!! Generated from procedure template - Window
!!! Update the Majors File
!!! </summary>
UpdateMajors PROCEDURE 

CurrentTab           STRING(80)                            ! 
LocalRequest         LONG                                  ! 
OriginalRequest      LONG                                  ! 
LocalResponse        LONG                                  ! 
FilesOpened          BYTE                                  ! 
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
Queue:Browse:2       QUEUE                            !Queue declaration for browse/combo box using ?Browse:2
TEA:FirstName          LIKE(TEA:FirstName)            !List box control field - type derived from field
TEA:LastName           LIKE(TEA:LastName)             !List box control field - type derived from field
TEA:Address            LIKE(TEA:Address)              !List box control field - type derived from field
TEA:City               LIKE(TEA:City)                 !List box control field - type derived from field
TEA:State              LIKE(TEA:State)                !List box control field - type derived from field
TEA:Zip                LIKE(TEA:Zip)                  !List box control field - type derived from field
TEA:Telephone          LIKE(TEA:Telephone)            !List box control field - type derived from field
TEA:Number             LIKE(TEA:Number)               !Primary key field - type derived from field
TEA:Department         LIKE(TEA:Department)           !Browse key field - type derived from field
Mark                   BYTE                           !Entry's marked status
ViewPosition           STRING(1024)                   !Entry's view position
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
Queue:Browse:4       QUEUE                            !Queue declaration for browse/combo box using ?Browse:4
STU:FirstName          LIKE(STU:FirstName)            !List box control field - type derived from field
STU:LastName           LIKE(STU:LastName)             !List box control field - type derived from field
STU:Address            LIKE(STU:Address)              !List box control field - type derived from field
STU:Address2           LIKE(STU:Address2)             !List box control field - type derived from field
STU:City               LIKE(STU:City)                 !List box control field - type derived from field
STU:State              LIKE(STU:State)                !List box control field - type derived from field
STU:Zip                LIKE(STU:Zip)                  !List box control field - type derived from field
STU:Telephone          LIKE(STU:Telephone)            !List box control field - type derived from field
STU:GradYear           LIKE(STU:GradYear)             !List box control field - type derived from field
STU:Number             LIKE(STU:Number)               !Primary key field - type derived from field
STU:Major              LIKE(STU:Major)                !Browse key field - type derived from field
Mark                   BYTE                           !Entry's marked status
ViewPosition           STRING(1024)                   !Entry's view position
                     END
History::MAJ:Record  LIKE(MAJ:RECORD),THREAD
QuickWindow          WINDOW('Update the Majors File'),AT(,,159,138),FONT('MS Sans Serif',8,COLOR:Black),GRAY,IMM, |
  MDI,HLP('~UpdateMajors'),SYSTEM
                       SHEET,AT(4,4,151,112),USE(?CurrentTab)
                         TAB('General'),USE(?TAB1)
                           PROMPT('&Description:'),AT(8,20),USE(?MAJ:Description:Prompt),TRN
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

ThisWindow           CLASS(WindowManager)
Ask                    PROCEDURE(),DERIVED
Init                   PROCEDURE(),BYTE,PROC,DERIVED
Kill                   PROCEDURE(),BYTE,PROC,DERIVED
Run                    PROCEDURE(),BYTE,PROC,DERIVED
Run                    PROCEDURE(USHORT Number,BYTE Request),BYTE,PROC,DERIVED
TakeAccepted           PROCEDURE(),BYTE,PROC,DERIVED
                     END

Toolbar              ToolbarClass
BRW2                 CLASS(BrowseClass)                    ! Browse using ?Browse:2
Q                      &Queue:Browse:2                !Reference to browse queue
Init                   PROCEDURE(SIGNED ListBox,*STRING Posit,VIEW V,QUEUE Q,RelationManager RM,WindowManager WM)
                     END

BRW2::Sort0:StepClass StepLongClass                        ! Default Step Manager
BRW4                 CLASS(BrowseClass)                    ! Browse using ?Browse:4
Q                      &Queue:Browse:4                !Reference to browse queue
Init                   PROCEDURE(SIGNED ListBox,*STRING Posit,VIEW V,QUEUE Q,RelationManager RM,WindowManager WM)
                     END

BRW4::Sort0:Locator  StepLocatorClass                      ! Default Locator
BRW4::Sort0:StepClass StepStringClass                      ! Default Step Manager
ToolbarForm          ToolbarUpdateClass                    ! Form Toolbar Manager
CurCtrlFeq          LONG
FieldColorQueue     QUEUE
Feq                   LONG
OldColor              LONG
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

ThisWindow.Ask PROCEDURE

  CODE
  CASE SELF.Request                                        ! Configure the action message text
  OF ViewRecord
    ActionMessage = 'View Record'
  OF InsertRecord
    ActionMessage = 'Adding a Majors Record'
  OF ChangeRecord
    ActionMessage = 'Changing a Majors Record'
  END
  QuickWindow{PROP:Text} = ActionMessage                   ! Display status message in title bar
  PARENT.Ask


ThisWindow.Init PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  GlobalErrors.SetProcedureName('UpdateMajors')
  SELF.Request = GlobalRequest                             ! Store the incoming request
  ReturnValue = PARENT.Init()
  IF ReturnValue THEN RETURN ReturnValue.
  SELF.FirstField = ?MAJ:Description:Prompt
  SELF.VCRRequest &= VCRRequest
  SELF.Errors &= GlobalErrors                              ! Set this windows ErrorManager to the global ErrorManager
  CLEAR(GlobalRequest)                                     ! Clear GlobalRequest after storing locally
  CLEAR(GlobalResponse)
  SELF.AddItem(Toolbar)
  SELF.HistoryKey = 734
  SELF.AddHistoryFile(MAJ:Record,History::MAJ:Record)
  SELF.AddHistoryField(?MAJ:Description,2)
  SELF.AddUpdateFile(Access:Majors)
  SELF.AddItem(?Cancel,RequestCancelled)                   ! Add the cancel control to the window manager
  Relate:Majors.SetOpenRelated()
  Relate:Majors.Open                                       ! File Majors used by this procedure, so make sure it's RelationManager is open
  SELF.FilesOpened = True
  SELF.Primary &= Relate:Majors
  IF SELF.Request = ViewRecord AND NOT SELF.BatchProcessing ! Setup actions for ViewOnly Mode
    SELF.InsertAction = Insert:None
    SELF.DeleteAction = Delete:None
    SELF.ChangeAction = Change:None
    SELF.CancelAction = Cancel:Cancel
    SELF.OkControl = 0
  ELSE
    SELF.InsertAction = Insert:Query
    SELF.ChangeAction = Change:Caller                      ! Changes allowed
    SELF.OkControl = ?OK
    IF SELF.PrimeUpdate() THEN RETURN Level:Notify.
  END
  BRW2.Init(?Browse:2,Queue:Browse:2.ViewPosition,BRW2::View:Browse,Queue:Browse:2,Relate:Teachers,SELF) ! Initialize the browse manager
  BRW4.Init(?Browse:4,Queue:Browse:4.ViewPosition,BRW4::View:Browse,Queue:Browse:4,Relate:Students,SELF) ! Initialize the browse manager
  SELF.Open(QuickWindow)                                   ! Open window
  Do DefineListboxStyle
  BRW2.Q &= Queue:Browse:2
  BRW2::Sort0:StepClass.Init(+ScrollSort:AllowAlpha)       ! Moveable thumb based upon TEA:Department for sort order 1
  BRW2.AddSortOrder(BRW2::Sort0:StepClass,TEA:KeyDepartment) ! Add the sort order for TEA:KeyDepartment for sort order 1
  BRW2.AddRange(TEA:Department,Relate:Teachers,Relate:Majors) ! Add file relationship range limit for sort order 1
  BRW2.AddField(TEA:FirstName,BRW2.Q.TEA:FirstName)        ! Field TEA:FirstName is a hot field or requires assignment from browse
  BRW2.AddField(TEA:LastName,BRW2.Q.TEA:LastName)          ! Field TEA:LastName is a hot field or requires assignment from browse
  BRW2.AddField(TEA:Address,BRW2.Q.TEA:Address)            ! Field TEA:Address is a hot field or requires assignment from browse
  BRW2.AddField(TEA:City,BRW2.Q.TEA:City)                  ! Field TEA:City is a hot field or requires assignment from browse
  BRW2.AddField(TEA:State,BRW2.Q.TEA:State)                ! Field TEA:State is a hot field or requires assignment from browse
  BRW2.AddField(TEA:Zip,BRW2.Q.TEA:Zip)                    ! Field TEA:Zip is a hot field or requires assignment from browse
  BRW2.AddField(TEA:Telephone,BRW2.Q.TEA:Telephone)        ! Field TEA:Telephone is a hot field or requires assignment from browse
  BRW2.AddField(TEA:Number,BRW2.Q.TEA:Number)              ! Field TEA:Number is a hot field or requires assignment from browse
  BRW2.AddField(TEA:Department,BRW2.Q.TEA:Department)      ! Field TEA:Department is a hot field or requires assignment from browse
  BRW4.Q &= Queue:Browse:4
  BRW4::Sort0:StepClass.Init(+ScrollSort:AllowAlpha,ScrollBy:Runtime) ! Moveable thumb based upon STU:LastName for sort order 1
  BRW4.AddSortOrder(BRW4::Sort0:StepClass,STU:MajorKey)    ! Add the sort order for STU:MajorKey for sort order 1
  BRW4.AddRange(STU:Major,Relate:Students,Relate:Majors)   ! Add file relationship range limit for sort order 1
  BRW4.AddLocator(BRW4::Sort0:Locator)                     ! Browse has a locator for sort order 1
  BRW4::Sort0:Locator.Init(,STU:LastName,1,BRW4)           ! Initialize the browse locator using  using key: STU:MajorKey , STU:LastName
  BRW4.AddField(STU:FirstName,BRW4.Q.STU:FirstName)        ! Field STU:FirstName is a hot field or requires assignment from browse
  BRW4.AddField(STU:LastName,BRW4.Q.STU:LastName)          ! Field STU:LastName is a hot field or requires assignment from browse
  BRW4.AddField(STU:Address,BRW4.Q.STU:Address)            ! Field STU:Address is a hot field or requires assignment from browse
  BRW4.AddField(STU:Address2,BRW4.Q.STU:Address2)          ! Field STU:Address2 is a hot field or requires assignment from browse
  BRW4.AddField(STU:City,BRW4.Q.STU:City)                  ! Field STU:City is a hot field or requires assignment from browse
  BRW4.AddField(STU:State,BRW4.Q.STU:State)                ! Field STU:State is a hot field or requires assignment from browse
  BRW4.AddField(STU:Zip,BRW4.Q.STU:Zip)                    ! Field STU:Zip is a hot field or requires assignment from browse
  BRW4.AddField(STU:Telephone,BRW4.Q.STU:Telephone)        ! Field STU:Telephone is a hot field or requires assignment from browse
  BRW4.AddField(STU:GradYear,BRW4.Q.STU:GradYear)          ! Field STU:GradYear is a hot field or requires assignment from browse
  BRW4.AddField(STU:Number,BRW4.Q.STU:Number)              ! Field STU:Number is a hot field or requires assignment from browse
  BRW4.AddField(STU:Major,BRW4.Q.STU:Major)                ! Field STU:Major is a hot field or requires assignment from browse
  BRW2.AskProcedure = 1                                    ! Will call: UpdateTeachers
  BRW4.AskProcedure = 2                                    ! Will call: UpdateStudents
  ToolBarForm.HelpButton=?Help
  SELF.AddItem(ToolbarForm)
  BRW2.AddToolbarTarget(Toolbar)                           ! Browse accepts toolbar control
  BRW2.ToolbarItem.HelpButton = ?Help
  BRW4.AddToolbarTarget(Toolbar)                           ! Browse accepts toolbar control
  BRW4.ToolbarItem.HelpButton = ?Help
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


ThisWindow.Run PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  ReturnValue = PARENT.Run()
  IF SELF.Request = ViewRecord                             ! In View Only mode always signal RequestCancelled
    ReturnValue = RequestCancelled
  END
  RETURN ReturnValue


ThisWindow.Run PROCEDURE(USHORT Number,BYTE Request)

ReturnValue          BYTE,AUTO

  CODE
  ReturnValue = PARENT.Run(Number,Request)
  IF SELF.Request = ViewRecord
    ReturnValue = RequestCancelled                         ! Always return RequestCancelled if the form was opened in ViewRecord mode
  ELSE
    GlobalRequest = Request
    EXECUTE Number
      UpdateTeachers
      UpdateStudents
    END
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
    OF ?OK
      ThisWindow.Update()
      IF SELF.Request = ViewRecord AND NOT SELF.BatchProcessing THEN
         POST(EVENT:CloseWindow)
      END
    END
    RETURN ReturnValue
  END
  ReturnValue = Level:Fatal
  RETURN ReturnValue


BRW2.Init PROCEDURE(SIGNED ListBox,*STRING Posit,VIEW V,QUEUE Q,RelationManager RM,WindowManager WM)

  CODE
  PARENT.Init(ListBox,Posit,V,Q,RM,WM)
  IF WM.Request <> ViewRecord                              ! If called for anything other than ViewMode, make the insert, change & delete controls available
    SELF.InsertControl=?Insert:3
    SELF.ChangeControl=?Change:3
    SELF.DeleteControl=?Delete:3
  END


BRW4.Init PROCEDURE(SIGNED ListBox,*STRING Posit,VIEW V,QUEUE Q,RelationManager RM,WindowManager WM)

  CODE
  PARENT.Init(ListBox,Posit,V,Q,RM,WM)
  IF WM.Request <> ViewRecord                              ! If called for anything other than ViewMode, make the insert, change & delete controls available
    SELF.InsertControl=?Insert:5
    SELF.ChangeControl=?Change:5
    SELF.DeleteControl=?Delete:5
  END

!!! <summary>
!!! Generated from procedure template - Window
!!! Update the Enrollment File
!!! </summary>
UpdateEnrollment PROCEDURE 

CurrentTab           STRING(80)                            ! 
LocalRequest         LONG                                  ! 
OriginalRequest      LONG                                  ! 
LocalResponse        LONG                                  ! 
FilesOpened          BYTE                                  ! 
WindowOpened         LONG                                  ! 
WindowInitialized    LONG                                  ! 
ForceRefresh         LONG                                  ! 
ActionMessage        CSTRING(40)                           ! 
RecordChanged        BYTE,AUTO                             ! 
SelectStudentsThread LONG                                  ! 
SelectClassesThread  LONG                                  ! 
History::ENR:Record  LIKE(ENR:RECORD),THREAD
QuickWindow          WINDOW('Update the Enrollment File'),AT(,,263,102),FONT('MS Sans Serif',8,COLOR:Black),CENTER, |
  GRAY,IMM,MDI,HLP('~UpdateEnrollment'),SYSTEM
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

ThisWindow           CLASS(WindowManager)
Ask                    PROCEDURE(),DERIVED
Init                   PROCEDURE(),BYTE,PROC,DERIVED
Kill                   PROCEDURE(),BYTE,PROC,DERIVED
Reset                  PROCEDURE(BYTE Force=0),DERIVED
Run                    PROCEDURE(),BYTE,PROC,DERIVED
Run                    PROCEDURE(USHORT Number,BYTE Request),BYTE,PROC,DERIVED
TakeAccepted           PROCEDURE(),BYTE,PROC,DERIVED
TakeFieldEvent         PROCEDURE(),BYTE,PROC,DERIVED
TakeSelected           PROCEDURE(),BYTE,PROC,DERIVED
TakeWindowEvent        PROCEDURE(),BYTE,PROC,DERIVED
                     END

Toolbar              ToolbarClass
ToolbarForm          ToolbarUpdateClass                    ! Form Toolbar Manager
CurCtrlFeq          LONG
FieldColorQueue     QUEUE
Feq                   LONG
OldColor              LONG
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

ThisWindow.Ask PROCEDURE

  CODE
  CASE SELF.Request                                        ! Configure the action message text
  OF ViewRecord
    ActionMessage = 'View Record'
  OF InsertRecord
    ActionMessage = 'Adding a Enrollment Record'
  OF ChangeRecord
    ActionMessage = 'Changing a Enrollment Record'
  END
  QuickWindow{PROP:Text} = ActionMessage                   ! Display status message in title bar
  PARENT.Ask


ThisWindow.Init PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  GlobalErrors.SetProcedureName('UpdateEnrollment')
  SELF.Request = GlobalRequest                             ! Store the incoming request
  ReturnValue = PARENT.Init()
  IF ReturnValue THEN RETURN ReturnValue.
  SELF.FirstField = ?ENR:StudentNumber:Prompt
  SELF.VCRRequest &= VCRRequest
  SELF.Errors &= GlobalErrors                              ! Set this windows ErrorManager to the global ErrorManager
  CLEAR(GlobalRequest)                                     ! Clear GlobalRequest after storing locally
  CLEAR(GlobalResponse)
  SELF.AddItem(Toolbar)
  SELF.HistoryKey = 734
  SELF.AddHistoryFile(ENR:Record,History::ENR:Record)
  SELF.AddHistoryField(?ENR:StudentNumber,1)
  SELF.AddHistoryField(?ENR:ClassNumber,2)
  SELF.AddHistoryField(?ENR:MidtermExam,3)
  SELF.AddHistoryField(?ENR:FinalExam,4)
  SELF.AddHistoryField(?ENR:TermPaper,5)
  SELF.AddUpdateFile(Access:Enrollment)
  SELF.AddItem(?Cancel,RequestCancelled)                   ! Add the cancel control to the window manager
  Relate:Classes.SetOpenRelated()
  Relate:Classes.Open                                      ! File Classes used by this procedure, so make sure it's RelationManager is open
  SELF.FilesOpened = True
  SELF.Primary &= Relate:Enrollment
  IF SELF.Request = ViewRecord AND NOT SELF.BatchProcessing ! Setup actions for ViewOnly Mode
    SELF.InsertAction = Insert:None
    SELF.DeleteAction = Delete:None
    SELF.ChangeAction = Change:None
    SELF.CancelAction = Cancel:Cancel
    SELF.OkControl = 0
  ELSE
    SELF.InsertAction = Insert:Query
    SELF.ChangeAction = Change:Caller                      ! Changes allowed
    SELF.OkControl = ?OK
    IF SELF.PrimeUpdate() THEN RETURN Level:Notify.
  END
  SELF.Open(QuickWindow)                                   ! Open window
  Do DefineListboxStyle
  ToolBarForm.HelpButton=?Help
  SELF.AddItem(ToolbarForm)
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


ThisWindow.Reset PROCEDURE(BYTE Force=0)

  CODE
  SELF.ForcedReset += Force
  IF QuickWindow{Prop:AcceptAll} THEN RETURN.
  STU:Number = ENR:StudentNumber                           ! Assign linking field value
  Access:Students.Fetch(STU:KeyStudentNumber)
  CLA:ClassNumber = ENR:ClassNumber                        ! Assign linking field value
  Access:Classes.Fetch(CLA:KeyClassNumber)
  COU:Number = CLA:CourseNumber                            ! Assign linking field value
  Access:Courses.Fetch(COU:KeyNumber)
  PARENT.Reset(Force)


ThisWindow.Run PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  ReturnValue = PARENT.Run()
  IF SELF.Request = ViewRecord                             ! In View Only mode always signal RequestCancelled
    ReturnValue = RequestCancelled
  END
  RETURN ReturnValue


ThisWindow.Run PROCEDURE(USHORT Number,BYTE Request)

ReturnValue          BYTE,AUTO

  CODE
  ReturnValue = PARENT.Run(Number,Request)
  IF SELF.Request = ViewRecord
    ReturnValue = RequestCancelled                         ! Always return RequestCancelled if the form was opened in ViewRecord mode
  ELSE
    GlobalRequest = Request
    SelectClasses
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
    CASE ACCEPTED()
    OF ?Button4
      !Call drag toolbox
      POST(EVENT:AlertKey,?ENR:StudentNumber)
      
    OF ?Button5
      !Call drag toolbox
      POST(EVENT:AlertKey,?ENR:ClassNumber)
      
    END
  ReturnValue = PARENT.TakeAccepted()
    CASE ACCEPTED()
    OF ?ENR:StudentNumber
      IF Access:Enrollment.TryValidateField(1)             ! Attempt to validate ENR:StudentNumber in Enrollment
        SELECT(?ENR:StudentNumber)
        QuickWindow{PROP:AcceptAll} = False
        CYCLE
      ELSE
        FieldColorQueue.Feq = ?ENR:StudentNumber
        GET(FieldColorQueue, FieldColorQueue.Feq)
        IF ERRORCODE() = 0
          ?ENR:StudentNumber{PROP:FontColor} = FieldColorQueue.OldColor
          DELETE(FieldColorQueue)
        END
      END
    OF ?ENR:ClassNumber
      IF NOT QuickWindow{PROP:AcceptAll}
        CLA:ClassNumber = ENR:ClassNumber
        IF Access:Classes.TryFetch(CLA:KeyClassNumber)
          IF SELF.Run(1,SelectRecord) = RequestCompleted
            ENR:ClassNumber = CLA:ClassNumber
          ELSE
            SELECT(?ENR:ClassNumber)
            CYCLE
          END
        END
      END
      ThisWindow.Reset(0)
      IF Access:Enrollment.TryValidateField(2)             ! Attempt to validate ENR:ClassNumber in Enrollment
        SELECT(?ENR:ClassNumber)
        QuickWindow{PROP:AcceptAll} = False
        CYCLE
      ELSE
        FieldColorQueue.Feq = ?ENR:ClassNumber
        GET(FieldColorQueue, FieldColorQueue.Feq)
        IF ERRORCODE() = 0
          ?ENR:ClassNumber{PROP:FontColor} = FieldColorQueue.OldColor
          DELETE(FieldColorQueue)
        END
      END
    OF ?ENR:MidtermExam
      IF Access:Enrollment.TryValidateField(3)             ! Attempt to validate ENR:MidtermExam in Enrollment
        SELECT(?ENR:MidtermExam)
        QuickWindow{PROP:AcceptAll} = False
        CYCLE
      ELSE
        FieldColorQueue.Feq = ?ENR:MidtermExam
        GET(FieldColorQueue, FieldColorQueue.Feq)
        IF ERRORCODE() = 0
          ?ENR:MidtermExam{PROP:FontColor} = FieldColorQueue.OldColor
          DELETE(FieldColorQueue)
        END
      END
    OF ?ENR:FinalExam
      IF Access:Enrollment.TryValidateField(4)             ! Attempt to validate ENR:FinalExam in Enrollment
        SELECT(?ENR:FinalExam)
        QuickWindow{PROP:AcceptAll} = False
        CYCLE
      ELSE
        FieldColorQueue.Feq = ?ENR:FinalExam
        GET(FieldColorQueue, FieldColorQueue.Feq)
        IF ERRORCODE() = 0
          ?ENR:FinalExam{PROP:FontColor} = FieldColorQueue.OldColor
          DELETE(FieldColorQueue)
        END
      END
    OF ?ENR:TermPaper
      IF Access:Enrollment.TryValidateField(5)             ! Attempt to validate ENR:TermPaper in Enrollment
        SELECT(?ENR:TermPaper)
        QuickWindow{PROP:AcceptAll} = False
        CYCLE
      ELSE
        FieldColorQueue.Feq = ?ENR:TermPaper
        GET(FieldColorQueue, FieldColorQueue.Feq)
        IF ERRORCODE() = 0
          ?ENR:TermPaper{PROP:FontColor} = FieldColorQueue.OldColor
          DELETE(FieldColorQueue)
        END
      END
    OF ?OK
      ThisWindow.Update()
      IF SELF.Request = ViewRecord AND NOT SELF.BatchProcessing THEN
         POST(EVENT:CloseWindow)
      END
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
  OF ?ENR:StudentNumber
    CASE EVENT()
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
      SELF.Reset
      PRESSKEY(TabKey)
      PRESSKEY(TabKey)
      
    END
  OF ?ENR:ClassNumber
    CASE EVENT()
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
      SELF.Reset
      PRESSKEY(TabKey)
      PRESSKEY(TabKey)
      
    END
  END
  ReturnValue = PARENT.TakeFieldEvent()
    RETURN ReturnValue
  END
  ReturnValue = Level:Fatal
  RETURN ReturnValue


ThisWindow.TakeSelected PROCEDURE

ReturnValue          BYTE,AUTO

Looped BYTE
  CODE
  LOOP                                                     ! This method receives all Selected events
    IF Looped
      RETURN Level:Notify
    ELSE
      Looped = 1
    END
    CASE FIELD()
    OF ?ENR:StudentNumber
      !Call drag toolbox
      IF SELF.OriginalRequest = InsertRecord AND NOT ENR:StudentNumber
        GLO:DropThread = THREAD()
        GLO:DropControl = ?ENR:StudentNumber
        SelectStudentsThread = START(SelectStudents)
        GLO:ThreadRef &= SelectStudentsThread
      END
      
    OF ?ENR:ClassNumber
      !Call drag toolbox
      IF SELF.OriginalRequest = InsertRecord AND NOT ENR:ClassNumber
        GLO:DropThread = THREAD()
        GLO:DropControl = ?ENR:ClassNumber
        SelectClassesThread = START(SelectClasses)
        GLO:ThreadRef &= SelectClassesThread
      END
      
    END
  ReturnValue = PARENT.TakeSelected()
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
    CASE EVENT()
    OF EVENT:CloseWindow
      !Close drag toolbox
      IF SelectStudentsThread
        POST(EVENT:CloseWindow,,SelectStudentsThread)
      END
      IF SelectClassesThread
        POST(EVENT:CloseWindow,,SelectClassesThread)
      END
    END
  ReturnValue = PARENT.TakeWindowEvent()
    RETURN ReturnValue
  END
  ReturnValue = Level:Fatal
  RETURN ReturnValue

!!! <summary>
!!! Generated from procedure template - Frame
!!! Clarion for Windows Wizard Application
!!! </summary>
MainABC PROCEDURE 

LocalRequest         LONG                                  ! 
OriginalRequest      LONG                                  ! 
LocalResponse        LONG                                  ! 
FilesOpened          BYTE                                  ! 
WindowOpened         LONG                                  ! 
WindowInitialized    LONG                                  ! 
ForceRefresh         LONG                                  ! 
CurrentTab           STRING(80)                            ! 
SplashProcedureThread LONG
MenuStyleMgr MenuStyleManager
DisplayDayString STRING('Sunday   Monday   Tuesday  WednesdayThursday Friday   Saturday ')
DisplayDayText   STRING(9),DIM(7),OVER(DisplayDayString)
AppFrame             APPLICATION('SoftVelocity University'),AT(,,635,351),FONT('MS Sans Serif',8,COLOR:Black),RESIZE, |
  ALRT(MouseLeft2),CENTER,IMM,MAX,HLP('~TopSpeedUniversity'),STATUS(-1,80,120,45),SYSTEM
                       MENUBAR,USE(?MENUBAR),FONT(,,COLOR:MENUTEXT)
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
                         END
                       END
                       TOOLBAR,AT(0,0,635,16),USE(?TOOLBAR1)
                         BUTTON,AT(2,2,16,14),USE(?Toolbar:Top, Toolbar:Top),ICON('VCRFIRST.ICO'),DISABLE,FLAT,TIP('Go to the ' & |
  'First Page')
                         BUTTON,AT(18,2,16,14),USE(?Toolbar:PageUp, Toolbar:PageUp),ICON('VCRPRIOR.ICO'),DISABLE,FLAT, |
  TIP('Go to the Prior Page')
                         BUTTON,AT(34,2,16,14),USE(?Toolbar:Up, Toolbar:Up),ICON('VCRUP.ICO'),DISABLE,FLAT,TIP('Go to the ' & |
  'Prior Record')
                         BUTTON,AT(50,2,16,14),USE(?Toolbar:Locate, Toolbar:Locate),ICON('FIND.ICO'),DISABLE,FLAT,TIP('Locate record')
                         BUTTON,AT(66,2,16,14),USE(?Toolbar:Down, Toolbar:Down),ICON('VCRDOWN.ICO'),DISABLE,FLAT,TIP('Go to the ' & |
  'Next Record')
                         BUTTON,AT(82,2,16,14),USE(?Toolbar:PageDown, Toolbar:PageDown),ICON('VCRNEXT.ICO'),DISABLE, |
  FLAT,TIP('Go to the Next Page')
                         BUTTON,AT(98,2,16,14),USE(?Toolbar:Bottom, Toolbar:Bottom),ICON('VCRLAST.ICO'),DISABLE,FLAT, |
  TIP('Go to the Last Page')
                         BUTTON,AT(118,2,16,14),USE(?Toolbar:Select, Toolbar:Select),ICON('MARK.ICO'),DISABLE,FLAT, |
  TIP('Select This Record')
                         BUTTON,AT(134,2,16,14),USE(?Toolbar:Insert, Toolbar:Insert),ICON('INSERT.ICO'),DISABLE,FLAT, |
  TIP('Insert a New Record')
                         BUTTON,AT(150,2,16,14),USE(?Toolbar:Change, Toolbar:Change),ICON('EDIT.ICO'),DISABLE,FLAT, |
  TIP('Edit This Record')
                         BUTTON,AT(166,2,16,14),USE(?Toolbar:Delete, Toolbar:Delete),ICON('DELETE.ICO'),DISABLE,FLAT, |
  TIP('Delete This Record')
                         BUTTON,AT(186,2,16,14),USE(?Toolbar:History, Toolbar:History),ICON('DITTO.ICO'),DISABLE,FLAT, |
  TIP('Previous value')
                         BUTTON,AT(202,2,16,14),USE(?Toolbar:Help, Toolbar:Help),ICON('HELP.ICO'),DISABLE,FLAT,TIP('Get Help')
                       END
                     END

ThisWindow           CLASS(WindowManager)
Ask                    PROCEDURE(),DERIVED
Init                   PROCEDURE(),BYTE,PROC,DERIVED
Kill                   PROCEDURE(),BYTE,PROC,DERIVED
TakeAccepted           PROCEDURE(),BYTE,PROC,DERIVED
TakeWindowEvent        PROCEDURE(),BYTE,PROC,DERIVED
                     END

Toolbar              ToolbarClass

  CODE
  GlobalResponse = ThisWindow.Run()                        ! Opens the window and starts an Accept Loop

!---------------------------------------------------------------------------
DefineListboxStyle ROUTINE
!|
!| This routine create all the styles to be shared in this window
!| It`s called after the window open
!|
!---------------------------------------------------------------------------
Menu::MENUBAR ROUTINE                                      ! Code for menu items on ?MENUBAR
Menu::MENU1 ROUTINE                                        ! Code for menu items on ?MENU1
Menu::MENU2 ROUTINE                                        ! Code for menu items on ?MENU2
Menu::Trees ROUTINE                                        ! Code for menu items on ?Trees
  CASE ACCEPTED()
  OF ?BrowseClassTree
    START(AbcClassTree, 25000)
  OF ?BrowseStudentsTree
    START(AbcStudentTree, 25000)
  END
Menu::MENU3 ROUTINE                                        ! Code for menu items on ?MENU3
  CASE ACCEPTED()
  OF ?BrowseStudents
    START(AbcBrowseStudents, 050000)
  OF ?BrowseTeachers
    START(AbcBrowseTeachers, 050000)
  OF ?BrowseClasses
    START(AbcBrowseClasses, 050000)
  OF ?BrowseEnrollment
    START(AbcBrowseEnrollment, 050000)
  OF ?BrowseCourses
    START(AbcBrowseCourses, 050000)
  OF ?BrowseMajors
    START(AbcBrowseMajors, 050000)
  OF ?BrowseUpdateGrades
    START(AbcUpdateGrades, 25000)
  END
Menu::ReportMenu ROUTINE                                   ! Code for menu items on ?ReportMenu
  CASE ACCEPTED()
  OF ?PrintENR:StuSeq
    START(AbcClassSchedules1, 050000)
  OF ?ReportsTeacherClassSchedules
    START(AbcClassSchedules2, 25000)
  OF ?PrintENR:SeqStu
    START(AbcAttendanceSheets, 050000)
  OF ?PrintCOU:KeyDescription
    START(AbcCourseEnrollment, 050000)
  OF ?ReportsCourseEnrollmentSummary
    START(AbcEnrollSummary, 25000)
  OF ?ReportsFinalGrades
    START(AbcFinalGrades, 25000)
  OF ?ReportsStudentIDs
    START(AbcStudentIDs, 25000)
  END
Menu::MENU4 ROUTINE                                        ! Code for menu items on ?MENU4
Menu::MENU5 ROUTINE                                        ! Code for menu items on ?MENU5

ThisWindow.Ask PROCEDURE

  CODE
  SYSTEM{PROP:Icon} = '~_TpSpdU.ICO'
  IF NOT INRANGE(AppFrame{PROP:Timer},1,100)
    AppFrame{PROP:Timer} = 100
  END
    AppFrame{Prop:StatusText,3} = CLIP(DisplayDayText[(TODAY()%7)+1]) & ', ' & FORMAT(TODAY(),@D4)
    AppFrame{PROP:StatusText,4} = FORMAT(CLOCK(),@T3)
  PARENT.Ask


ThisWindow.Init PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  GlobalErrors.SetProcedureName('MainABC')
  SELF.Request = GlobalRequest                             ! Store the incoming request
  ReturnValue = PARENT.Init()
  IF ReturnValue THEN RETURN ReturnValue.
  SELF.FirstField = 1
  SELF.VCRRequest &= VCRRequest
  SELF.Errors &= GlobalErrors                              ! Set this windows ErrorManager to the global ErrorManager
  CLEAR(GlobalRequest)                                     ! Clear GlobalRequest after storing locally
  CLEAR(GlobalResponse)
  SELF.AddItem(Toolbar)
  SELF.Open(AppFrame)                                      ! Open window
  Do DefineListboxStyle
  SELF.SetAlerts()
      AppFrame{PROP:TabBarVisible}  = False
      MenuStyleMgr.Init(?MENUBAR)
      MenuStyleMgr.SuspendRefresh()
      MenuStyleMgr.SetThemeColors('XPLunaBlue')
      MenuStyleMgr.SetImageBar(False)
      MenuStyleMgr.ApplyTheme()
      MenuStyleMgr.Refresh(TRUE)
  RETURN ReturnValue


ThisWindow.Kill PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  ReturnValue = PARENT.Kill()
  IF ReturnValue THEN RETURN ReturnValue.
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
    OF ?Toolbar:Top
    OROF ?Toolbar:PageUp
    OROF ?Toolbar:Up
    OROF ?Toolbar:Locate
    OROF ?Toolbar:Down
    OROF ?Toolbar:PageDown
    OROF ?Toolbar:Bottom
    OROF ?Toolbar:Select
    OROF ?Toolbar:Insert
    OROF ?Toolbar:Change
    OROF ?Toolbar:Delete
    OROF ?Toolbar:History
    OROF ?Toolbar:Help
      IF SYSTEM{PROP:Active} <> THREAD()
        POST(EVENT:Accepted,ACCEPTED(),SYSTEM{Prop:Active} )
        CYCLE
      END
    ELSE
      DO Menu::MENUBAR                                     ! Process menu items on ?MENUBAR menu
      DO Menu::MENU1                                       ! Process menu items on ?MENU1 menu
      DO Menu::MENU2                                       ! Process menu items on ?MENU2 menu
      DO Menu::Trees                                       ! Process menu items on ?Trees menu
      DO Menu::MENU3                                       ! Process menu items on ?MENU3 menu
      DO Menu::ReportMenu                                  ! Process menu items on ?ReportMenu menu
      DO Menu::MENU4                                       ! Process menu items on ?MENU4 menu
      DO Menu::MENU5                                       ! Process menu items on ?MENU5 menu
    END
  ReturnValue = PARENT.TakeAccepted()
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
    OF EVENT:OpenWindow
      SplashProcedureThread = START(SplashIt)              ! Run the splash window procedure
    OF EVENT:Timer
      AppFrame{Prop:StatusText,3} = CLIP(DisplayDayText[(TODAY()%7)+1]) & ', ' & FORMAT(TODAY(),@D4)
      AppFrame{PROP:StatusText,4} = FORMAT(CLOCK(),@T3)
    ELSE
      IF SplashProcedureThread
        IF EVENT() = Event:Accepted
          POST(Event:CloseWindow,,SplashProcedureThread)   ! Close the splash window
          SplashPRocedureThread = 0
        END
     END
    END
    RETURN ReturnValue
  END
  ReturnValue = Level:Fatal
  RETURN ReturnValue

!!! <summary>
!!! Generated from procedure template - Source
!!! Strore LIST From(Q) Reference in User Property {FromQ}. Not needed 11.13505
!!! </summary>
ListPropFromQ        PROCEDURE  (LONG FEQ,*QUEUE FrmQ,<STRING NameQ>) ! Declare Procedure
Ref GROUP,AUTO
Q    &QUEUE
L    LONG,OVER(Q)
  END


  CODE
  Ref.Q &=FrmQ 
  FEQ{'FromQ'}=Ref.L 
  FEQ{'FromWho'}=CHOOSE(~OMITTED(NameQ),NameQ,'Queue' & Ref.L)
  RETURN
  
!This is Not needed in 11.13505
