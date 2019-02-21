# AVR-ASM Makefile, derived from the WinAVR template (which
# is public domain), believed to be neutral to any flavor of "make"
# (GNU make, BSD make, SysV make)

#For a description of of the following built options see 'config.inc'
#Defining options here will override the defaults from the config file.

#MCU = atmega88
MCU = atmega328p
#F_CPU = 20000000
#DRAM_8BIT = 0
#BAUD = 57600
#BAUD = 115200
#I2C_SUPPORT = 0
ADC_SUPPORT = 0
#EM_Z80	= 0
#FAT16_SUPPORT = 0
#CPMDSK_SUPPORT = 0
#MMCBOOTLOADER = 0


# Version defined in 'config.inc'.
VMAJOR = $(call conf-val, VMAJOR, config.inc)
VMINOR = $(call conf-val, VMINOR, config.inc)


TARGET = avrcpm
ASRC0  = avrcpm.asm 

ASRC0 += config.inc macros.inc init.asm dram-refresh.asm timer.asm utils.asm 
ASRC0 += mmc.asm mmc-old.asm virt_ports.asm
ASRC0 += dsk_cpm.asm dsk_fat16.asm dsk_fsys.asm dsk_mgr.asm dsk_ram.asm 
ASRC0 += 8080int-orig.asm 8080int.asm 8080int-jmp.asm 8080int-t3.asm 8080int-t3-jmp.asm Z80int-jmp.asm

ifneq ($(DRAM_8BIT),0)
  ASRC0 += dram-8bit.inc dram-8bit.asm sw-uart.asm i2c.asm
else
  ASRC0 += dram-4bit.inc dram-4bit.asm hw-uart.asm 
endif

ASRC = $(ASRC0) svnrev.inc
#ASRC := $(ASRC0) svnrev.inc

# Place -D or -U options here
CDEFS = -D$(MCU) 

ifdef F_CPU
  CDEFS += -DF_CPU=$(F_CPU)
endif
ifdef DRAM_8BIT
  CDEFS += -DDRAM_8BIT=$(DRAM_8BIT)
endif
ifdef BAUD
  CDEFS += -DBAUD=$(BAUD)
endif
ifdef I2C_SUPPORT
  CDEFS += -DI2C_SUPPORT=$(I2C_SUPPORT)
endif
ifdef ADC_SUPPORT
  CDEFS += -DADC_SUPPORT=$(ADC_SUPPORT)
endif
ifdef EM_Z80
  CDEFS += -DEM_Z80=$(EM_Z80)
endif
ifdef FAT16_SUPPORT
  CDEFS += -DFAT16_SUPPORT=$(FAT16_SUPPORT)
endif
ifdef CPMDSK_SUPPORT
  CDEFS += -DCPMDSK_SUPPORT=$(CPMDSK_SUPPORT)
endif
ifdef MMCBOOTLOADER
  CDEFS += -DMMCBOOTLOADER=$(MMCBOOTLOADER)
endif
ifdef TESTVERSION
  CDEFS += -DTESTVERSION=$(TESTVERSION)
endif

ASPATH = C:/Programme/Atmel/AVR\ Tools/AvrAssembler2
DEFS = $(ASPATH)/Appnotes

ifeq "$(OS)" "Windows_NT"
  PLATFORM=win32
else
  PLATFORM=Linux
endif

WINE =
ifeq ($(PLATFORM),Linux)
  WINE = wine
endif

AS = $(WINE) $(ASPATH)/avrasm2.exe
ASFLAGS = -I $(DEFS) $(CDEFS)

AWK = gawk
OBJCOPY = avr-objcopy
CRCGEN = crcgen

HEXTOBIN = $(OBJCOPY) -I ihex -O binary --gap-fill 0xff 

#(call conf-val,config-id,config-file)
#conf-val = $(shell $(AWK) -vID=$(strip $1) '$$0 ~ "^[ \t]*\#define[ \t]+" ID "[ \t]+" {print $$3}' $2 )
conf-val = $(shell $(AWK) -vID=$(strip $1) '$$1$$2 ~ "\#define"ID {print $$3}' $2)


# Programming support using avrdude. Settings and variables.

AVRDUDE_PROGRAMMER = dragon_isp
AVRDUDE_PORT = usb

AVRDUDE_WRITE_FLASH = -U flash:w:$(TARGET).hex
AVRDUDE_WRITE_EEPROM = -U eeprom:w:$(TARGET).eep


# Uncomment the following if you want avrdude's erase cycle counter.
# Note that this counter needs to be initialized first using -Yn,
# see avrdude manual.
#AVRDUDE_ERASE_COUNTER = -y

# Uncomment the following if you do /not/ wish a verification to be
# performed after programming the device.
#AVRDUDE_NO_VERIFY = -V

# Increase verbosity level.  Please use this when submitting bug
# reports about avrdude. See <http://savannah.nongnu.org/projects/avrdude> 
# to submit bug reports.
#AVRDUDE_VERBOSE = -v -v

AVRDUDE_BASIC = -p $(MCU) -P $(AVRDUDE_PORT) -c $(AVRDUDE_PROGRAMMER)
AVRDUDE_FLAGS = $(AVRDUDE_BASIC) $(AVRDUDE_NO_VERIFY) $(AVRDUDE_VERBOSE) $(AVRDUDE_ERASE_COUNTER)

AVRDUDE = avrdude
REMOVE = rm -f
MV = mv -f

.PHONY:	all bin hex eep lst map program flash eeprom tags clean

# Default target.
all: hex lst

hex: $(TARGET).hex
eep: $(TARGET).eep
lst: $(TARGET).lst
map: $(TARGET).map
bin: $(TARGET)-$(VMAJOR).$(VMINOR).bin


# Program the device.  
program: $(TARGET).hex $(TARGET).eep
	$(AVRDUDE) $(AVRDUDE_FLAGS) $(AVRDUDE_WRITE_FLASH) $(AVRDUDE_WRITE_EEPROM)

flash: $(TARGET).hex
	$(AVRDUDE) $(AVRDUDE_FLAGS) $(AVRDUDE_WRITE_FLASH)

eeprom: $(TARGET).hex $(TARGET).eep
	$(AVRDUDE) $(AVRDUDE_FLAGS) $(AVRDUDE_WRITE_EEPROM)


$(TARGET).hex: $(ASRC)
$(TARGET).eep: $(ASRC)
$(TARGET).lst: $(ASRC)
$(TARGET).map: $(ASRC)


.SUFFIXES:

%-$(VMAJOR).$(VMINOR).bin: %.hex
	$(HEXTOBIN) $< $@
	$(CRCGEN) $@

%.hex: %.asm
	$(AS) $(ASFLAGS) -fI -o $@ $<

%.lst: %.asm
	@$(AS) $(ASFLAGS) -v0 -f- -l $@ $<

%.map: %.asm
	$(AS) $(ASFLAGS) -v0 -f- -m $@ $<

tags: $(SRC) $(ASRC)
	ctags $(SRC) $(ASRC)

svnrev.inc: $(ASRC0)
	svnrev -osvnrev.inc $^
	touch svnrev.inc

# Target: clean project.
clean:
	$(REMOVE) $(TARGET).hex $(TARGET).eep $(TARGET).obj $(TARGET).map $(TARGET).lst \
		$(TARGET)-$(VMAJOR).$(VMINOR).bin

