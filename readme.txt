Basic utilities:

WayfireConfigTool -p:  prints the current config
WayfireConfigTool -g [section] [name]: prints the value of the specified option
WayfireConfigTool -c [section] [name] [newval] [optional output]: changes the specified option to have a new value.  optionally writes it to the output file
WayfireConfigTool -cx [section] [name] [newval]: changes the specified option to a new value.  backs up the old wayfire.ini to wayfire.ini.bak
WayfireConfigTool -b:  restores wayfire.ini from wayfire.ini.bak
