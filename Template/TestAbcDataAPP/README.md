This APP was generated using the CBAbc4Legacy.tpl 

In TestAbcData.clw you see ::Used LONG's

```Clarion
!Region CBAbc4Legacy File::Used Counters
Students::Used LONG
Teachers::Used LONG
Classes::Used LONG
Enrollment::Used LONG
Courses::Used LONG
Majors::Used LONG
!endregion  
```

In TestAbcData_BC0.clw the FileManager .Init show's 2 lines from CBAbc4Legacy 

```Clarion
Hide:Access:Majors.Init PROCEDURE
  CODE
    SELF.Opened &= Majors::Used         !CBAbc4Legacy
    SELF.DoNotAssertOnCloseMisuse=True  !CBAbc4Legacy optional, not sure required
  SELF.Initialized = False
  SELF.Buffer &= MAJ:Record
  SELF.FileNameValue = 'Majors'
  SELF.SetErrors(GlobalErrors)
  SELF.File &= Majors
  PARENT.Init
  Access:Majors &= SELF
  SELF.LazyOpen = False
```

In TestANCData.EXP you see the exports
```
 ; CBAbc4Legacy File::Used Counters
$STUDENTS::USED                                         @?
$TEACHERS::USED                                         @?
$CLASSES::USED                                          @?
$ENROLLMENT::USED                                       @?
$COURSES::USED                                          @?
$MAJORS::USED                                           @?
```