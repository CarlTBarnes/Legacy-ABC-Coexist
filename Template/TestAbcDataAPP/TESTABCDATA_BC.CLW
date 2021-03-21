  MEMBER('TestAbcData.clw')

  PRAGMA('define(init_priority=>3)')

  MAP
    MODULE('TestAbcData_BC0.CLW')
TestAbcData_BC0:DctInit             PROCEDURE()
TestAbcData_BC0:DctKill             PROCEDURE()
TestAbcData_BC0:FilesInit           PROCEDURE()
    END
  END

DLLInit              DllInitializer                          !This object is used to initialize the dll, it is defined in the main program module

DctInit PROCEDURE()
  CODE
  TestAbcData_BC0:DctInit
  TestAbcData_BC0:FilesInit


DctKill PROCEDURE()
  CODE
  TestAbcData_BC0:DctKill

