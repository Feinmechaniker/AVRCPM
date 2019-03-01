{{

Info            : I2C EEPROM
Autor           : Joe G.
Version         : 0.1
Subversion      : 00
Funktion        : Routines for reading and writing to EEPROM 
Komponenten     : Basic_I2C_Driver.spin

Log             

25.10.2014      Start

}}


CON
    '' Saved config in EEPROM
    EEPROMAddr = %1010_0000
    EEPROM_Base = $7FC0

    i2cSCL = 28                                         'SCL Pin   

    '' Value which shows valid values are present
    CfgMagic = 59

    '' Number of longs in config memory array
    CfgSize = 16


OBJ
  i2c:  "basic_i2c_driver"      ' I2C serial bus


VAR
    long cfg[CfgSize]


'' One time setup
PUB initialize
    i2c.Initialize(i2cSCL)

'' Read the configuration
'' params" points to an array of CfgSize words:
'' [baud, color, force-7bit, cursor, auto-crlf, caps-opt, timeout]
'' Return value is 1 if a config is available, 0 if not.
PUB readCfg(params) : res2 | loc, x

    '' Start at base of config, see if there's a config
    loc := EEPROM_Base
    x := i2c.ReadLong(i2cSCL, EEPROMAddr, loc)
    res2 := 0
    if x <> CfgMagic
        return

    '' There is.  Get the config values.  They are read into a
    ''  sequence of memory locations which mirrors how they
    ''  will lie in the array "params".
    repeat x from 0 to CfgSize-1
        loc += 4
        cfg[x] := i2c.ReadLong(i2cSCL, EEPROMAddr, loc)

    '' Success; move result to caller's memory and return success
    longmove(params, @cfg, CfgSize)
    res2 := 1
    waitcnt(clkfreq/200 + cnt)

'' Write a config back to EEPROM
'' ("params" is as above.)
'' This routine assumes a readCfg() will always precede this write
PUB writeCfg(params) | loc, x
    longmove(@cfg, params, CfgSize)
    loc := EEPROM_Base
    i2c.WriteLong(i2cSCL, EEPROMAddr, loc, CfgMagic)
    repeat x from 0 to CfgSize-1
        loc += 4
        i2c.WriteLong(i2cSCL, EEPROMAddr, loc, cfg[x])
    waitcnt(clkfreq/200 + cnt)