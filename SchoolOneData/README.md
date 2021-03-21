## School One Data

This version of BothSchool has only the ABC Data DLL and it has been modified to export the File::Used required by Legacy.

This does not use the CBAbc4Legacy.TPL. It implement those changes has embed code. 

The App MUST NOT Check "Defer Opening files" i.e. Lazy Open.

1. Declare File::Used LONG in Global Data
2. Export File::Used
3. FileManage.Init() has a "SELF.Opened &= File::Used" line e.g. Hide:Access:Majors.Init has "SELF.Opened &= Majors::Used"

ABFile.INC requires these changes:
```Clarion
  1. Find      "Opened UNSIGNED"
  2. Change to "Opened &LONG       !CBAbc4Legacy change to &LONG Reference"

  3. Add below "Opened::Used LONG  !CBAbc4Legacy true .Opened Count"  

  4. Add below "CONSTRUCT PROCEDURE() !CBAbc4Legacy Sets Opened &= Opened::Used"   
```

ABFile.CLW this CONSTRUCT added:
```Clarion
FileManager.CONSTRUCT PROCEDURE
    CODE 
    SELF.Opened &= SELF.Opened::Used  !CBAbc4Legacy &Reference, allows &= File::Used in  Legacy
    RETURN 
```

